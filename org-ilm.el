;;; org-ilm.el --- Incremental learning mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: html, org

;;; Commentary:

;; This package extends org-mode to allow for incremental processing of reading,
;; writing, listening, and watching materials.

;;; Code:

;;;; Requirements
(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'org-element)
(require 'org-ql)
(require 'org-ql-view)
(require 'fsrs)
(require 'org-transclusion)
(require 'cl-lib)
(require 'dash)
(require 'rx)
(require 'peg)
(require 'ts)
(require 'vtable)
(require 'eldoc)
(require 'transient)
(require 'cond-let)

(require 'mochar-utils)
(require 'convtools)
(require 'org-registry)
(require 'ost)

;;;; Global customization

(defgroup org-ilm nil
  "Incremental learning mode."
  :group 'org
  :prefix "org-ilm-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom org-ilm-id-from-attachment-path-func 'org-ilm-infer-id-from-attachment-path
  "Function that accepts a path to an attachment file and returns the Org id of the header."
  :type 'function
  :group 'org-ilm)

(defcustom org-ilm-import-default-method 'cp
  "Default import method, or `nil' to always ask."
  :type '(choice (const :tag "Nil" nil)
                 (const :tag "Move" 'mv)
                 (const :tag "Copy" 'cp))
  :group 'org-ilm)

;; 40 days in supermemo
(defcustom org-ilm-schedule-max-days 60
  "Maximum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-schedule-min-days 1
  "Minimum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-material-states '("MTRL")
  "TODO state of material elements to be processed incrementally."
  :type 'string
  :group 'org-ilm)

(defcustom org-ilm-card-states '("CARD")
  "TODO state of flash cards."
  :type 'string
  :group 'org-ilm)

(defcustom org-ilm-subject-states '("SUBJ")
  "TODO state of subjects."
  :type '(repeat string)
  :group 'org-ilm)

(defcustom org-ilm-update-org-mem-after-capture t
  "Update org-mem cache to include captured entry."
  :type 'boolean
  :group 'org-ilm)

(defcustom org-ilm-midnight-shift 2
  "Number of hours after midnight to count as a new day.

As an example, with a value of 2, elements scheduled for a day will only
be due starting 2am."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-debug nil
  "Print stuff to message buffer."
  :type 'boolean
  :group 'org-ilm)

;;;; Variables

(defface org-ilm-face-extract
  '((t :background "yellow"))
  "Face used to highlight extracts.")

(defface org-ilm-face-card
  '((t :background "pink"))
  "Face used to highlight clozes.")

(defvar-keymap org-ilm-map
  :doc "Keymap for `org-ilm-global-minor-mode'."
  "i" #'org-ilm-import
  "e" #'org-ilm-element-actions
  "a" #'org-ilm-attachment-actions
  "s" #'org-ilm-schedule
  "o" #'org-ilm-open-dwim
  "x" #'org-ilm-extract
  "z" #'org-ilm-cloze
  "t" #'org-ilm-attachment-transclude
  "j" #'org-ilm-subject-dwim
  "r" #'org-ilm-review
  "q" #'org-ilm-queue
  "+" #'org-ilm-queue-add-dwim
  "g" #'org-ilm-registry
  "b" #'org-ilm-bibliography)

;; TODO This autoload doesnt work. Problem because i need this var in dir-locals.
;;;###autoload
(defconst org-ilm-log-drawer-name "ILM")

;;;; Minor mode

;; TODO This hook is called not just after file save but on a timer as well. Do
;; we want the timer to reset cache?
(defun org-ilm--org-mem-hook (parse-results)
  ;; (seq-let (bad-paths file-data entries) parse-results
  ;;   )
  (org-ilm-subject-cache-reset))

;;;###autoload
(define-minor-mode org-ilm-global-minor-mode
  "Prepare some hooks and advices some functions."
  :init-value nil
  :global t
  :lighter nil ;; String to display in mode line
  :group 'org-ilm
  ;; :keymap org-ilm-map
  (if org-ilm-global-minor-mode
      ;; Enable
      (progn
        (add-hook 'after-change-major-mode-hook
                  #'org-ilm--attachment-prepare-buffer)
        (add-hook 'org-mem-post-full-scan-functions
                  #'org-ilm--org-mem-hook)
        (add-hook 'org-mem-post-targeted-scan-functions
                  #'org-ilm--org-mem-hook)
        (define-key pdf-view-mode-map (kbd "A") org-ilm-pdf-map)
        (advice-add 'pdf-annot-create-context-menu
                    :around #'org-ilm--pdf-annot-create-context-menu-advice)
        )
    ;; Disable
    (remove-hook 'after-change-major-mode-hook
                 #'org-ilm--attachment-prepare-buffer)
    (remove-hook 'org-mem-post-full-scan-functions
              #'org-ilm--org-mem-hook)
    (remove-hook 'org-mem-post-targeted-scan-functions
                 #'org-ilm--org-mem-hook)
    (define-key pdf-view-mode-map (kbd "A") nil)
    (advice-remove 'pdf-annot-create-context-menu
                   #'org-ilm--pdf-annot-create-context-menu-advice)
    ))

;;;; Commands

(defun org-ilm-open-attachment ()
  "Open attachment of item at point."
  (interactive)
  (unless (org-ilm--attachment-open)
    (pcase (read-char-choice "No attachments. (n)ew, (r)egistry, (s)elect: " '("n" "r" "s"))
      (?n 
       (let* ((attach-dir (org-attach-dir-get-create))
              (file-path (expand-file-name (concat (org-id-get) ".org") attach-dir)))
         (find-file file-path)))
      (?s
       (if-let* ((attach-dir (org-attach-dir))
                 (attachments (org-attach-file-list attach-dir))
                 (attachment (completing-read "Attachment to dedicate as element's: "
                                              attachments nil t))
                 (attachment (expand-file-name attachment attach-dir))
                 (attachment-new (expand-file-name
                                  (format "%s.%s" (org-id-get)
                                          (file-name-extension attachment))
                                  (file-name-directory attachment))))
           (progn
             (rename-file attachment attachment-new)
             (find-file attachment-new))
         (user-error "No attachment found")))
      (?r
       (if-let* ((registry-id (org-entry-get nil "REGISTRY")))
           (user-error "Not implemented!") ;; TODO
         (user-error "No registry entry"))))))

(defun org-ilm-open-highlight ()
  "Open element associated with highlight at point."
  (interactive)
  (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'org-ilm-id)) (overlays-at (point)))))
    (pcase (length ovs)
      (0 nil)
      (1 (org-ilm--open-from-ov (nth 0 ovs)))
      (_
       (let* ((choices (mapcar
                        (lambda (ov)
                          (cons
                           (format "%s: %s"
                                   (propertize (overlay-get ov 'org-ilm-type) 'face '(:weight bold))
                                   (propertize
                                    (overlay-get ov 'org-ilm-id)
                                    'face '(:slant italic)))
                           ov))
                        ovs))
              (choice (completing-read "Element: " choices nil t))
              (ov (cdr (assoc choice choices))))
         (org-ilm--open-from-ov ov))))))

(defun org-ilm-open-collection ()
  "Open collection file, jump back to it if in attachment."
  (interactive)
  (if-let* ((data (org-ilm--attachment-data))
            (org-id-loc (org-id-find (car data))))
      (org-ilm--org-goto-id (car data))
    (let ((collection (org-ilm--select-collection)))
      (find-file (cdr collection)))))

(defun org-ilm-open-dwim ()
  "Open element of highlight or within collection."
  (interactive)
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
     ('attachment
      (unless (org-ilm-open-highlight)
        (org-ilm-open-collection)))
     ('collection
      (if (org-ilm-element-at-point)
          (org-ilm-open-attachment)
        (user-error "No ilm element at point!")))
     ('registry
      ;; TODO Allow for multiple ilm elements for same registry
      ;; Eg: blog and accompanying video, manuscript and published paper, etc
      (if-let ((entry (seq-find
                       (lambda (entry)
                         (and (org-ilm-type (org-mem-entry-todo-state entry))
                              (string= (org-id-get)
                                       (org-mem-entry-property "REGISTRY" entry))))
                       (hash-table-values org-mem--id<>entry))))
          (org-node-goto-id (org-mem-entry-id entry))
        (user-error "No ilm element derived from this registry entry!")))
     (_ (org-ilm-open-collection)))))

(defun org-ilm-extract ()
  "Extract region depending on file."
  (interactive)
  (if (eq 'attachment (car (org-ilm--where-am-i)))
    (cond
     ((eq major-mode 'org-mode)
      (call-interactively #'org-ilm-org-extract))
     ((org-ilm--pdf-mode-p)
      (call-interactively #'org-ilm-pdf-extract)))
    (user-error "Extracts can only be made from within an attachment")))

(defun org-ilm-cloze ()
  "Create a cloze card"
  (interactive)
  (if (eq 'attachment (car (org-ilm--where-am-i)))
      (cond
       ((eq major-mode 'org-mode)
        (call-interactively #'org-ilm-org-cloze))
       ((org-ilm--pdf-mode-p)
        (call-interactively #'org-ilm-pdf-cloze)))
    (user-error "Clozes can only be made from within an attachment")))

(defun org-ilm-split ()
  "Split document by section."
  (interactive)
  (if (eq 'attachment (car (org-ilm--where-am-i)))
      (cond
       ((eq major-mode 'org-mode)
        (call-interactively #'org-ilm-org-split))
       ((org-ilm--pdf-mode-p)
        (call-interactively #'org-ilm-pdf-split)))
    (user-error "Splitting can only be done from within an attachment")))

(defun org-ilm-registry ()
  "Open collection registry."
  (interactive)
  (org-registry-open))

(defun org-ilm-bibliography ()
  (interactive)
  (if-let ((path (org-ilm--collection-bib)))
      (let ((key (ignore-errors
                   (car (mochar-utils--org-mem-cite-refs)))))
        (with-current-buffer (find-file path)
          (goto-char (point-min))
          (when (re-search-forward key nil t)
            (beginning-of-line))))
    (user-error "Path to bibliography not found")))
  
;;;; Utilities

;;;;; Ilm

(defun org-ilm--debug (&optional fmt &rest args)
  "Print stuff to message buffer when `org-ilm-debug' non-nil."
  (let ((fmt (cond
              ((not fmt) "%s")
              ((s-contains-p "%s" fmt) fmt)
              (t (concat fmt " %s")))))
  (when org-ilm-debug
    (apply #'message
           (concat
            (propertize "[org-ilm] " 'face 'calendar-today)
            fmt)
           args))))

(defun org-ilm--debug-all (vals args-func &optional fmt)
  (dolist (val vals)
    (apply #'org-ilm--debug fmt (funcall args-func val))))

(defun org-ilm--where-am-i ()
  "Returns one of ('collection collection), ('attachment (org-id collection)),
('queue collection org-id), nil."
  (cond-let*
    ([attachment (org-ilm--attachment-data)]
     (cons 'attachment attachment))
    ([collection (org-ilm--collection-file (org-ilm--buffer-file-name))]
     (if (string= (file-name-base (buffer-file-name (buffer-base-buffer))) "registry")
         (cons 'registry collection)
       (cons 'collection collection)))
    ((bound-and-true-p org-ilm-queue)
     (let* ((object (cdr (org-ilm--vtable-get-object)))
            ;; TODO Can't use cl-typecase here because org-ilm-element
            ;; not yet defined
            (id (cond
                  ((stringp object) object)
                  ((org-ilm-element-p object)
                   (org-ilm-element-id object)))))
       (list 'queue (org-ilm-queue--collection org-ilm-queue) id)))))

(defun org-ilm-midnight-shift-minutes ()
  "Midnight shift as number of minutes past (or before) midnight."
  (let* ((whole-hours org-ilm-midnight-shift))
    (when (or (not (numberp whole-hours)) (> whole-hours 12))
      (message "Midnight shift larger than 12 hours. Cutting it to 12.")
      (setq whole-hours 12))
    (* whole-hours 60)))

;;;;; Time and date

(defun org-ilm--ts-to-midnight (ts)
  "Set time to midnight"
  (ts-adjust 'hour (- (ts-hour ts))
             'minute (- (ts-min ts))
             'second (- (ts-sec ts)) ts))

(defun org-ilm--ts-diff-rounded-days (ts1 ts2)
  (plist-get
   (ts-human-duration
    (ts-difference (org-ilm--ts-to-midnight ts1)
                   (org-ilm--ts-to-midnight ts2)))
   :days))

(defun org-ilm--ts-today ()
  (org-ilm--ts-to-midnight (ts-now)))

(defun org-ilm--current-date-utc ()
  "Current date in UTC as string."
  (format-time-string "%Y-%m-%d" (current-time) t))

(defun org-ilm--interval-to-schedule-string (interval)
  "Turn INTERVAL (days) into a date string used in in org headline SCHEDULED."
  (org-format-time-string
   (org-time-stamp-format)
   (ts-unix (ts-adjust 'day interval (ts-now)))))

(defun org-ilm--timestamp-is-format-p (timestamp format &optional timezone)
  "Check if string TIMESTAMP has the required FORMAT with optional TIMEZONE."
  (cl-assert (and (stringp timestamp) (stringp format)))
  (let ((ts (ts-parse timestamp)))
    (string= timestamp (format-time-string format (ts-unix ts) timezone))))

(defun org-ilm--timestamp-is-utc-iso8601-p (timestamp)
  "Check if string TIMESTAMP is formatted according to ISO 8601 with UTC timezone."
  (org-ilm--timestamp-is-format-p timestamp "%FT%TZ" "UTC0"))

(defun org-ilm--timestamp-is-iso8601-date-p (timestamp)
  "Check if string TIMESTAMP is ISO 8601 formatted calendar date (YYYY-MM-DD)."
  (org-ilm--timestamp-is-format-p timestamp "%Y-%m-%d"))

(defun org-ilm--ts-format-utc (ts)
  (if (ts-p ts)
      (format-time-string "%FT%TZ" (ts-unix ts) "UTC0")
    (cl-assert (org-ilm--timestamp-is-utc-iso8601-p ts))
    ts))

(defun org-ilm--ts-format-utc-date (ts)
  (if (ts-p ts)
      (ts-format "%Y-%m-%d" ts)
    (cl-assert (org-ilm--timestamp-is-iso8601-date-p ts))
    ts))

(defun org-ilm--ts-format-utc-date-maybe-time (ts &optional ignore-time)
  (cl-assert (ts-p ts))
  (if (or ignore-time
          (and (= (ts-hour ts) 0) (= (ts-minute ts) 0)))
      (ts-format "%Y-%m-%d" ts)
    (ts-format "%Y-%m-%d %H:%M" ts)))

;;;;; Elisp

(defun org-ilm--select-alist (alist &optional prompt formatter ordered)
  "Prompt user for element in alist to select from.
If empty return nil, and if only one, return it."
  (pcase (length alist)
    (0 nil)
    (1 (nth 0 alist))
    (_
     (let* ((choices
             (mapcar
              (lambda (thing)
                (let ((second (if (consp (cdr thing))
                                  (car (cdr thing))
                                (cdr thing))))
                  (cons
                   (if formatter
                       (funcall formatter thing)
                     (format "%s %s"
                             (propertize
                              (format "%s" (car thing))
                              'face '(:weight bold))
                             (propertize
                              (format "%s" second)
                              'face '(:slant italic))))
                   thing)))
              alist))
            (choice (completing-read
                     (or prompt "Select: ")
                     (if ordered
                         ;; https://emacs.stackexchange.com/a/8177/49372
                         (lambda (string pred action)
                           (if (eq action 'metadata)
                               `(metadata (display-sort-function . ,#'identity))
                             (complete-with-action action choices string pred)))
                       choices)
                     nil t))
            (item (cdr (assoc choice choices))))
       item))))

(defun org-ilm--invert-alist (alist)
  "Turn car into cdr and vice versa for each cons in ALIST."
  (mapcar (lambda (pair)
            (cons (cdr pair) (car pair)))
          alist))

(cl-defun org-ilm--add-hook-once (hook function &optional depth (local t))
  "Add FUNCTION to HOOK and remove it after its first execution.

HOOK is the hook to which FUNCTION will be added.
FUNCTION is the function to run once in the hook.
DEPTH controls where FUNCTION is placed in HOOK and defaults to 0.
LOCAL controls whether to add HOOK buffer-locally and defaults to t.

The returned function can be used to call `remove-hook' if needed."
  (letrec ((hook-function (lambda () (remove-hook hook hook-function local) (funcall function))))
    (add-hook hook hook-function depth local)
    hook-function))

(cl-defun org-ilm--buffer-text-process (&key keep-clozes no-footnotes (source-buffer (current-buffer)) (begin (point-min)) (end (point-max)))
  "Process current buffer for extraction or cloze."
  (replace-regexp org-ilm-target-regexp "" nil begin end)
  (unless keep-clozes
    (org-ilm--card-uncloze-buffer begin end))
  (if no-footnotes
      (replace-regexp org-footnote-re "" nil begin end)
    ;; Copy over footnotes 
    (let ((footnotes '()))
      (goto-char (point-min))
      (while (re-search-forward org-footnote-re nil t)
        (when-let* ((fn (match-string 0))
                    (label (match-string 1))
                    (def (with-current-buffer source-buffer
                           (org-footnote-get-definition label))))
          (push (concat fn " " (nth 3 def)) footnotes)))
      (goto-char (point-max))
      (insert "\n\n")
      (dolist (fn (delete-dups (nreverse footnotes)))
        (insert fn "\n")))))

(defun org-ilm--buffer-text-prepare (&optional region-begin region-end keep-clozes remove-footnotes)
  "Return processed buffer text for new extract or card element."
  (let* ((text (buffer-substring-no-properties (or region-begin (point-min))
                                               (or region-end (point-max))))
         (buffer (current-buffer)))
    (with-temp-buffer
      (insert text)
      (org-ilm--buffer-text-process
       :source-buffer buffer
       :keep-clozes keep-clozes
       :no-footnotes remove-footnotes)
      (buffer-string))))

(defun org-ilm--generate-text-snippet (text)
  "Cleanup TEXT so that it can be used in as an org header."
  (let* ((text (replace-regexp-in-string "\n" " " text))
         (text (org-link-display-format text)) ;; Remove org links
         (text (string-trim text))) ;; Trim whitespace
    (substring text 0 (min 50 (length text)))))

(defun org-ilm--buffer-file-name ()
  "Like `buffer-file-name' but support indirect buffers."
  (or buffer-file-name (buffer-file-name (buffer-base-buffer))))

(defun org-ilm--ov-delete (ov &rest _)
  (delete-overlay ov))


;;;;; Org

(defun org-ilm--org-headline-bounds ()
  "Return (start . end) char that constitutes the headline at point,
 excluding child headlines."
  (save-excursion
    (let ((start (org-back-to-heading))
          (end (progn
                 (org-next-visible-heading 1)
                 (point))))
      (cons start end))))

(defun org-ilm--org-narrow-to-header ()
  "Narrow to headline and content up to next heading or end of buffer."
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))

(defun org-ilm--org-id-p (string)
  "Is STRING an org-id? Return org-mem entry, else nil."
  ;; (org-id-find string)
  (org-mem-entry-by-id string))

(defun org-ilm--org-headline-at-point ()
  "Return headline at point."
  (let ((element (org-element-at-point))
        (headline-text (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
    (when (eq (car element) 'headline)
      
      ;; TODO Seems that when buffer local priority ranges differ from global
      ;; ones, org-element returns nil as priority. Also sometimes priority
      ;; returned as their numerical value, sometimes as char number. Solution
      ;; is to parse the header manually.

      ;; (with-temp-buffer
      ;;   (insert headline-text)
      ;;   (goto-char 0)
      ;;   (looking-at org-priority-regexp)
      ;;   (let ((priority (match-string-no-properties 2)))
      ;;     (when priority
      ;;       (setq priority (string-to-number priority)))
      ;;     (setq element (org-element-put-property element :priority priority))))

      (when (string-match org-priority-regexp headline-text)
        (let ((priority (match-string-no-properties 2 headline-text)))
          (when priority
            (setq priority (string-to-number priority)))
          (setq element (org-element-put-property element :priority priority))))
        
      ;; (save-excursion
      ;;   (beginning-of-line)
      ;;   ;; Previously used org-entry-get but it just gives weird numbers
      ;;   (looking-at org-priority-regexp)
      ;;   (let ((priority (match-string-no-properties 2)))
      ;;     (when priority
      ;;       (setq priority (string-to-number priority)))
      ;;     (setq element (org-element-put-property element :priority priority))))

      element
      )))

(defun org-ilm--org-goto-id (org-id)
  ;; (let ((loc (org-id-find org-id 'marker)))
  ;;   (pop-to-buffer (marker-buffer loc))
  ;;   (goto-char loc)))
  (org-node-goto-id org-id))

(defmacro org-ilm--org-with-headline-contents-visible (&rest body)
  "Necessary when parsing hidden/collapsed data within headline."
  (declare (debug (body)) (indent 1))
  ;; Need to save outline visiblity, otherwise will unfold headers.
  ;; Furthermore, need to set USE-MARKERS to non-nil, otherwise it causes weird
  ;; folding problems.
  `(org-save-outline-visibility 'use-markers
     (org-fold-show-entry)
     ,@body))

;; TODO Export jump logic in own function
(defmacro org-ilm--org-with-point-at (thing &rest body)
  "THING should be an org-id or nil.

Note: Previously used org-id-find but it put point above
headline. org-mem takes me there directly.

Alternatively org-id-goto could be used, but does not seem to respect
save-excursion."
  (declare (debug (body)) (indent 1))
  `(cond
    ;; If id is that of headline at point, dont jump, for performance but also
    ;; so that org-mem is not required to have parsed the headline (useful in
    ;; capture).
    ((or (null ,thing) (string= ,thing (org-id-get)))
     ;; Reveal entry contents, otherwise run into problems parsing the metadata,
     ;; such as with org-srs drawer.
     (org-ilm--org-with-headline-contents-visible ,@body))
    ((stringp ,thing)
     (when-let* ((org-id ,thing)
                 (entry (org-mem-entry-by-id org-id))
                 (file (org-mem-entry-file-truename entry))
                 (buf (or (find-buffer-visiting file)
                          (find-file-noselect file))))
       (with-current-buffer buf
         ;; We need to widen the buffer because `find-buffer-visiting' might
         ;; return an active, narrowed buffer.
         (org-with-wide-buffer
          ;; Jump to correct position
          (goto-char (org-find-property "ID" org-id))
          
          ;; Reveal entry contents, otherwise run into problems parsing the
          ;; metadata, such as with org-srs drawer.
          (org-ilm--org-with-headline-contents-visible ,@body)))))
    (t (error "THING should be org-id or nil"))))

(defun org-ilm--org-headline-element-from-id (org-id)
  "Return headline element from org id."
  (save-excursion
    (org-ilm--org-with-point-at
     org-id
     (org-ilm--org-headline-at-point))))

(defun org-ilm--org-attach-dir-from-id (org-id)
  "Return the attachment directory of element with ORG-ID

Note that we cannot just use org-attach utilities to find this because we might be using buffer local variables or dir-locals.el"
  (save-excursion
    (org-ilm--org-with-point-at
     org-id
     (org-attach-dir))))

(defun org-ilm--org-id-from-string (string)
  "Interpret STRING as org-id, link with org-id, or else nil."
  (cond
   ((org-ilm--org-id-p string) string)
   ((string-match org-link-bracket-re string)
    (let ((link (match-string 1 string)))
      (when (string-prefix-p "id:" link)
        (substring link 3))))))

(defun org-ilm--org-headline-from-thing (&optional thing assert)
  "Return headline from THING: org-id, org-id link, headline, nil -> headline at point.
With non-nil ASSERT, assert headline must be found, else return nil."
  (let ((headline (cond
                   ;; Nil, try getting it from point
                   ((not thing)
                    (org-ilm--org-headline-at-point))
                   ;; Org-element, check if headline
                   ((eq (org-element-type thing) 'headline)
                    thing)
                   ;; String, check if is or contains id
                   ((stringp thing)
                    (org-ilm--org-headline-element-from-id
                     (org-ilm--org-id-from-string thing))))))
    (when assert (cl-assert headline))
    headline))

(defun org-ilm--clock-in-continue-last ()
  "Edit out the clock-out time of the last entry and continue as if never clocked out."
  (when (org-clocking-p)
    (org-clock-out))
  (save-excursion
    (org-back-to-heading 'invisble-ok)
    (save-restriction
      (org-ilm--org-narrow-to-header)
      (when (and
             (re-search-forward org-clock-line-re nil 'noerror)
             (re-search-forward (org-re-timestamp 'inactive) (line-end-position) 'noerror))
        (delete-region (point) (line-end-position))
        (let ((org-clock-in-resume t)
              (org-clock-idle-time nil))
          (org-clock-in))))))

(defun org-ilm--org-priority-set (value)
  "Set the priority of element at point to VALUE.

This function was made to deal with a bug in `org-priority' where a
value of 32 is read as the character ?\s (32) which causes it to remove
the priority, or error if there is no priority."
  (when (numberp value) (setq value (number-to-string value)))
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-priority-regexp)
      (org-priority 'remove))
    (let ((case-fold-search nil))
      (looking-at org-todo-line-regexp))
    (if (match-end 2)
        (progn
	  (goto-char (match-end 2))
	  (insert " [#" value "]"))
      (goto-char (match-beginning 3))
      (insert "[#" value "] "))
    (message "Priority set to %s" value)))

(cl-defun org-ilm--org-schedule (&key timestamp interval-minutes interval-days ignore-time)
  "Call org-schedule with TIMESTAMP."
  (cond
   ((not timestamp)
    (when interval-days
      (setq interval-minutes (* 60 24 interval-days)
            ignore-time t))
    (if (not interval-minutes)
        (error "need timestamp or interval")
      (cl-assert (and (numberp interval-minutes) (>= interval-minutes 0)))
      (setq timestamp (ts-adjust 'minute interval-minutes (ts-now)))))
   ((stringp timestamp)
    (setq timestamp (ts-parse timestamp)))
   ((ts-p timestamp))
   (t (error "timestamp invalid format")))

  (org-schedule
   nil
   (if (or ignore-time
           (and (= (ts-hour timestamp) 0) (= (ts-minute timestamp) 0)))
       (ts-format "%Y-%m-%d" timestamp)
     (ts-format "%Y-%m-%d %H:%M" timestamp))))

(defun org-ilm--org-logbook-parse (logbook)
  "Parses the Org contents of a logbook drawer entry."
  (let ((contents (org-element-contents logbook)))
    (mapcar
     ;; If more data is needed, see:
     ;; https://orgmode.org/worg/dev/org-element-api.html
     ;; See: Timestamp, Clock
     (lambda (clock)
       (let* ((timestamp (org-element-property :value clock))
              (stamps (split-string (org-element-property :raw-value timestamp) "--")))
         (list
          :duration-minutes
          (when-let ((duration (org-element-property :duration clock)))
            (org-duration-to-minutes duration))
          :status (org-element-property :status clock)
          :timestamp-start (ts-parse-org (nth 0 stamps))
           ;; May be nil if active clock
          :timestamp-end (when-let ((s (nth 1 stamps))) (ts-parse-org s))
          :day-end (org-element-property :day-end timestamp)
          :day-start (org-element-property :day-start timestamp)
          :month-end (org-element-property :month-end timestamp)
          :month-start (org-element-property :month-start timestamp)
          :year-end (org-element-property :year-end timestamp)
          :year-start (org-element-property :year-start timestamp))))
     contents)))
    
(defun org-ilm--org-logbook-read (&optional headline)
  "Reads and parses the LOGBOOK drawer of heading at point.

If `HEADLINE' is passed, read it as org-property."
  (unless headline
    (setq headline (save-excursion (org-back-to-heading) (org-element-at-point))))
  (let* ((begin (org-element-property :begin headline))
         (end (save-excursion
                (outline-next-heading)
                (point))))
    (when (and begin end)
      (save-excursion
        (save-restriction
          (narrow-to-region begin end)
          (when-let* ((drawers (org-element-map
                                   (org-element-parse-buffer)
                                   'drawer #'identity))
                      (logbook (seq-find
                                (lambda (drawer)
                                  (string-equal
                                   (org-element-property :drawer-name drawer) "LOGBOOK"))
                                drawers)))
            (org-ilm--org-logbook-parse logbook)))))))

(defun org-ilm--org-delete-headline (&optional delete-attach-dir)
  (when delete-attach-dir (org-attach-delete-all 'force))
  (org-mark-subtree)
  (delete-region (region-beginning) (region-end)))

;;;;; Org-node / org-mem

(defun org-ilm--node-read-candidate-state-only (states &optional prompt blank-ok initial-input)
  "Like `org-node-read-candidate' but for nodes with STATES todo-state only."
  (let* ((states-list (if (stringp states) (list states) states))
         (choice
          (completing-read (or prompt "Node: ")
                           (if blank-ok #'org-node-collection-main
                             #'org-node-collection-basic)
                           (lambda (name node)
                             (member (org-mem-todo-state node) states-list))
                           ()
                           initial-input
                           (if org-node-alter-candidates 'org-node-hist-altered
                             'org-node-hist))))
    (gethash choice org-node--candidate<>entry)))

(defun org-ilm--org-mem-ancestry-ids (entry-or-id &optional with-root-str)
  "Return org ids of ancestors of entry."
  (when-let ((entry (if (stringp entry-or-id)
                        (org-mem-entry-by-id entry-or-id)
                      entry-or-id)))
    (let ((ancestry
           (delq nil ;; Delete nils returned when no org-id
                 (mapcar (lambda (x)
                           (car (last x)))
                         ;; cdr to skip itself
                         (cdr (org-mem-entry-crumbs entry))))))
      (if with-root-str (cons "ROOT" ancestry) ancestry))))

;;;; Citation

(defcustom org-ilm-bibtex-fields nil
  "List of field names to include in the bibtex entry.
See `parsebib-read-entry'."
  :type '(repeat string)
  :group 'org-ilm)

(defun org-ilm--citation-get-bibtex (source &optional as-alist)
  "Return the bibtex entry as string from SOURCE."
  ;; Much better results with citoid backend compared to zotra
  (let ((zotra-backend 'citoid)
        bibtex-string)
    (setq bibtex-string (-some-> (zotra-get-entry source "bibtex")
                          s-trim))
    (if (and bibtex-string as-alist)
        (org-ilm--citation-parse-bibtex bibtex-string)
      bibtex-string)))

(defun org-ilm--citation-parse-bibtex (bibtex-string)
  "Parses BIBTEX-STRING as alist."
  (with-temp-buffer
    (insert bibtex-string)
    (when-let* ((bibtexes (car (parsebib-parse-bib-buffer
                                :fields org-ilm-bibtex-fields
                                :expand-strings t
                                :inheritance t
                                :replace-TeX t)))
                (key (car (hash-table-keys bibtexes))))
      (setq bibtex (gethash key bibtexes)))))

(defun org-ilm--citation-get-zotero (source)
  "Return the zotero reference of SOURCE as an alist."
  (when-let* ((zotra-backend 'citoid)
              (json-string (zotra-get-entry source "zotero")))
    (with-temp-buffer
      (insert json-string)
      (goto-char (point-min))
      (aref (json-read) 0))))



;;;; Ost

;; Shared code for structs that hold an ost.

(defun org-ilm--ost-ost (obj)
  (cond
   ((ost-tree-p obj) obj)
   ((cl-struct-p obj)
    (let ((type (type-of obj)))
      (cl-struct-slot-value type 'ost obj)))))

(defun org-ilm--ost-count (obj &optional offset)
  "Return the number of elements in ost of OBJ.
Optional OFFSET is added to the true count."
  (setq offset (or offset 0))
  (let ((size (+ (ost-size (org-ilm--ost-ost obj)) offset)))
    (cl-assert (>= size 0) nil "OFFSET may not lead to negative size.")
    size))

(defun org-ilm--ost-move (obj id new-rank)
  (ost-tree-move (org-ilm--ost-ost obj) id new-rank))

(defun org-ilm--ost-move-many (obj new-ranks-alist)
  (ost-tree-move-many (org-ilm--ost-ost obj) new-ranks-alist))

(defun org-ilm--ost-contains-p (obj id)
  (when (ost-tree-node-by-id (org-ilm--ost-ost obj) id)
    id))

(defun org-ilm--ost-rank-to-quantile (obj rank &optional offset)
  "Return the quantile of RANK in ost of OBJ.
Optional OFFSET is added to the ost size for calculation."
  (let ((size (org-ilm--ost-count obj offset)))
    (if (= 1 size) 0 (/ (float rank) (1- size)))))

(defun org-ilm--ost-quantile-to-rank (obj quantile &optional offset)
  "Return the rank of QUANTILE in ost of OBJ.
Optional OFFSET is added to the ost size for calculation."
  (cl-assert (<= 0 quantile 1) nil "QUANTILE must be between 0 and 1")
  (let ((size (org-ilm--ost-count obj offset)))
    (round (* quantile (1- size)))))

(defun org-ilm--ost-parse-position (obj position &optional offset)
  "Return (RANK . QUANTILE) given rank or quantile POSITION.

OFFSET artificially increases the size of the queue to correct the
calculations, assuming POSITION was already corrected for this
offset. For example, when POSITION represents the position of a new
element at the end of a queue of size N: OFFSET should be 1, POSITION
should be 1.0 if a quantile or N+1 if a rank, and will return (N+1 . 1.0)."
  (let ((ost-size (org-ilm--ost-count obj offset)))
    (cond
     ;; Already correct
     ((and (listp position) (numberp (car position)) (floatp (cdr position)))
      ;; Make sure the rank fits the quantile
      ;; TODO This calculation might fail due to rounding errors????
      ;; (cl-assert (= (car position)
      ;;               (org-ilm--ost-quantile-to-rank obj (cdr position) offset)))
      position)
     ;; Inputted quantile
     ((and (floatp position) (<= 0.0 position 1.0))
      (let ((rank (org-ilm--ost-quantile-to-rank obj position offset)))
        (cons rank position)))
     ;; Inputted rank
     ((and (numberp position) (<= 0 position (1- ost-size)))
      (let ((quantile (org-ilm--ost-rank-to-quantile obj position offset)))
        (cons position quantile))))))

(defun org-ilm--ost-read-position (obj &optional numnew-or-id)
  (let* ((ost-size (org-ilm--ost-count obj))
         (numnew (cond
                  ((null numnew-or-id) 0)
                  ((numberp numnew-or-id) numnew-or-id)
                  ((stringp numnew-or-id)
                   ;; If element not yet in queue, max position is queue size + 1
                   (if (org-ilm--ost-contains-p obj numnew-or-id) 0 1))
                  (t (error "Invalid value for NUMNEW-OR-ID"))))
         (min 1)
         (max (+ ost-size numnew))
         position)
    (while (not position)
      (let* ((number (read-number
                      (format "Priority (#%s-%s / 0.0-1.0): " min max)))
             (number (if (integerp number) (1- number) number)))
        (setq position (org-ilm--ost-parse-position obj number numnew))))
    position))

(defun org-ilm--ost-format-position (obj position)
  (setq position (org-ilm--ost-parse-position obj position))
  (when position
    (let ((rank (car position))
          (quantile (cdr position)))
      (cl-assert (or rank quantile))
      (unless quantile
        (setq quantile (org-ilm--ost-rank-to-quantile obj rank)))
      (unless rank
        (setq rank (org-ilm--ost-quantile-to-rank obj quantile)))
      (format "#%s/%s (%.2f%s)"
              (1+ rank)
              (org-ilm--ost-count obj)
              (* 100 quantile)
              "%"))))


;;;; Collection

(defcustom org-ilm-collections '((ilm . ((path . "~/ilm/")
                                         (bib . "refs.bib"))))
  "Alist mapping collection name symbol to its configuration alist.

Properties:

`path' (required)

  The path to the collection (file or folder).

`bib'

  The path to the bibtex file. If missing will be named refs.bib."
  :type '(alist :key-type symbol :value-type alist)
  :group 'org-ilm)

(defvar org-ilm--active-collection nil
  "The current collection that is active.")

(defun org-ilm--active-collection ()
  "Return or infer the active collection. If missing, prompt user."
  (or org-ilm--active-collection
      (when-let ((collection (org-ilm--collection-from-context)))
        (setq org-ilm--active-collection collection))
      (let ((collection (org-ilm--select-collection)))
        (setq org-ilm--active-collection (car collection)))))

(defun org-ilm--collection-from-context ()
  "Infer collection based on the current buffer."
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
      ('collection (cdr location))
      ('attachment (caaddr location))
      ('queue (cadr location)))))

(defun org-ilm--collection-path (collection)
  "Return path of COLLECTION."
  (alist-get 'path (alist-get collection org-ilm-collections)))

(defun org-ilm--collection-single-file-p (collection)
  "Return t if COLLECTION is a single file collection."
  (f-file-p (org-ilm--collection-path collection)))

(defun org-ilm--collection-bib (&optional collection)
  "Return path to bibtex file of COLLECTION."
  (let* ((collection (or collection (org-ilm--active-collection)))
         (conf (alist-get collection org-ilm-collections))
         (bib (alist-get 'bib conf)))
    (if (org-ilm--collection-single-file-p collection)
        (cl-assert (f-absolute-p bib) nil "Path to bibtex file must be absolute")
      (setq bib (expand-file-name (or bib "refs.bib") (alist-get 'path conf))))
    (unless (file-exists-p bib) (make-empty-file bib))
    bib))

(defun org-ilm--collection-file (&optional file collection)
  "Return collection of which FILE belongs to.
A collection symbol COLLECTION can be passed to test if file belongs to
 that collection."
  (when-let ((file (or file (buffer-file-name (buffer-base-buffer))))
             (path (expand-file-name file))
             (test (lambda (f place)
                     (if (f-directory-p place)
                         (and
                          (file-in-directory-p f place)
                          (not (file-in-directory-p f org-attach-id-dir)))
                       (file-equal-p f place)))))
    (if collection
        (funcall test path (org-ilm--collection-path collection))
      (car (seq-find (lambda (c) (funcall test path (alist-get 'path (cdr c))))
                     org-ilm-collections)))))

(defun org-ilm--select-collection ()
  "Prompt user for collection to select from.
The collections are stored in `org-ilm-collections'."
  (car 
   (org-ilm--select-alist
    (mapcar (lambda (x) (cons (car x) (alist-get 'path x)))
            org-ilm-collections)
    "Collection: ")))

(defun org-ilm--collection-files (collection)
  "Return all collection org files that belong to COLLECTION."
  (let ((file-or-dir (org-ilm--collection-path collection)))
    (cond
     ((f-file-p file-or-dir)
      (list (expand-file-name file-or-dir)))
     ((f-dir-p file-or-dir)
      (directory-files file-or-dir 'full "^[^.].*\\.org$"))
     (t (error "No files in collection %s" collection)))))

(defun org-ilm--select-collection-file (collection)
  "Prompt user to select a file from COLLECTION."
  (cl-assert (assoc collection org-ilm-collections))
  (let ((files (org-ilm--collection-files collection)))
    (cond
     ((= 1 (length files))
      (car files))
     ((> (length files) 1)
      (let* ((col-path (org-ilm--collection-path collection))
             ;; TODO Make it work with nested files
             (file (completing-read
                    "Collection file: "
                    (mapcar #'file-name-nondirectory files)
                    nil 'require-match)))
        (expand-file-name file col-path))))))

(defun org-ilm--collection-entries (collection)
  "All org-mem entries in COLLECTION."
  (seq-filter
   (lambda (entry)
     (member
      (org-ilm-type (org-mem-entry-todo-state entry))
      '(material card)))
   (org-mem-entries-in-files (org-ilm--collection-files collection))))

(defun org-ilm--collection-tags (collection)
  "List of tags used in COLLECTION."
  (cl-remove-duplicates
   (apply #'append
          (mapcar #'org-mem-entry-tags
                  (org-ilm--collection-entries collection)))
   :test #'string=))


;;;; Priority queue

(defcustom org-ilm-pqueue-data-dir
  (expand-file-name
   (file-name-concat (convert-standard-filename "var/") "org-ilm")
   user-emacs-directory)
  "The directory where the queues are stored for single file collections."
  :type 'directory
  :group 'org-ilm)

(defvar org-ilm-pqueue-priority-changed-hook nil
  "Hook called when the priority of an element was actively changed.

Hook arg is (id . priority-rank) or an alist of such cons's if many moved at once.

Note that priority may change frequently due to the nature of a queue. Therefore not all changes are logged, only when explicitely set (`org-ilm-pqueue--move' and `org-ilm-pqueue--move-many'.")

;;;;; Object

(cl-defstruct (org-ilm-pqueue (:conc-name org-ilm-pqueue--))
  "Priority queue."
  collection ost)

(defun org-ilm--pqueue-dir (collection)
  "Return the directory path where the queue of COLLECTION is stored."
  (when-let ((file-or-dir (org-ilm--collection-path collection)))
    (if (f-file-p file-or-dir)
        org-ilm-pqueue-data-dir
      (let ((dir (expand-file-name ".ilm/" file-or-dir)))
        (make-directory dir 'parents)
        dir))))

(defun org-ilm--pqueue-file (collection)
  (expand-file-name "element-queue.el" (org-ilm--pqueue-dir collection)))

(defun org-ilm--pqueue-read (collection)
  (when-let ((ost (ost-read (org-ilm--pqueue-file collection))))
    (make-org-ilm-pqueue :collection collection :ost ost)))

;; TODO Schedule for write in background process
;; If new scheduled, old one cancelled. Use org-mem thing?
(defun org-ilm-pqueue--write (pqueue)
  (let ((file (org-ilm--pqueue-file (org-ilm-pqueue--collection pqueue))))
    (make-directory (file-name-directory file) 'parents)
    (ost-write (org-ilm-pqueue--ost pqueue) file))
  ;; Return nil prevent printing
  nil)

(defun org-ilm-pqueue--contains-p (pqueue id)
  "Return ID if it is in QUEUE."
  (org-ilm--ost-contains-p pqueue id))

(defun org-ilm-pqueue--insert (pqueue id rank)
  "Insert element with ID in PQUEUE with RANK."
  (ost-tree-insert (org-ilm-pqueue--ost pqueue) rank id)
  (org-ilm-pqueue--write pqueue))

(defun org-ilm-pqueue--remove (pqueue id)
  "Remove element with ID from PQUEUE."
  (when (org-ilm-pqueue--contains-p pqueue id)
    (ost-tree-remove (org-ilm-pqueue--ost pqueue) id)
    (org-ilm-pqueue--write pqueue)))

;; TODO Keep copy of ost to restore in case of error midway?
(defun org-ilm-pqueue--move (pqueue id new-rank)
  "Move element ID in PQUEUE to NEW-RANK."
  (ost-tree-move (org-ilm-pqueue--ost pqueue) id new-rank)
  (run-hook-with-args 'org-ilm-pqueue-priority-changed-hook (cons id new-rank))
  (org-ilm-pqueue--write pqueue))

(defun org-ilm-pqueue--move-many (pqueue new-ranks-alist)
  "Move many elements in PQUEUE to a new rank.

NEW-RANKS-ALIST is an alist of (ID . NEW-RANK) pairs."
  (ost-tree-move-many (org-ilm-pqueue--ost pqueue) new-ranks-alist)
  (run-hook-with-args 'org-ilm-pqueue-priority-changed-hook new-ranks-alist)
  (org-ilm-pqueue--write pqueue))

(defun org-ilm-pqueue--count (pqueue)
  "Return number of elements in PQUEUE."
  (ost-size (org-ilm-pqueue--ost pqueue)))

;;;;; Create

(defvar org-ilm--pqueues nil
  "Alist collection -> pqueue.")

(defun org-ilm--pqueue-new (collection)
  (let* ((ost (make-ost-tree :dynamic t))
         (pqueue (make-org-ilm-pqueue :collection collection :ost ost))
         ;; TODO Use org-mem instead?
         (elements (org-ilm-query-collection collection #'org-ilm-query-all)))
    (dolist (element elements)
      (org-ilm-pqueue--insert pqueue (org-ilm-element-id element) 0))
    pqueue))

(defun org-ilm--pqueue (collection)
  "Return the priority queue of COLLECTION."
  (cond-let*
    ([pqueue (cdr (assoc collection org-ilm--pqueues))]
     pqueue)
    ([pqueue (org-ilm--pqueue-read collection)]
     (setf (alist-get collection org-ilm--pqueues) pqueue))
    (t
     (when (yes-or-no-p (format "No priority queue found for collection \"%s\". Create a new one?" collection))
       (let* ((pqueue (org-ilm--pqueue-new collection)))
         (org-ilm-pqueue--write pqueue)
         (setf (alist-get collection org-ilm--pqueues) pqueue))))))

(defun org-ilm-pqueue (&optional collection)
  "Return the priority queue of COLLECTION, or active collection."
  (setq collection (or collection (org-ilm--active-collection)))
  (org-ilm--pqueue collection))

(defun org-ilm-pqueue-parse-position (position &optional offset collection)
  (org-ilm--ost-parse-position
   (org-ilm-pqueue collection)
   position offset))

(defun org-ilm-pqueue-parse-new-position (position &optional collection)
  (org-ilm-pqueue-parse-position position 1 collection))

(defun org-ilm-pqueue-count (&optional collection)
  (org-ilm-pqueue--count (org-ilm-pqueue collection)))

(defun org-ilm-pqueue-insert (id priority &optional collection)
  (let* ((pqueue (org-ilm-pqueue collection))
         (rank (car (org-ilm--ost-parse-position pqueue priority 1))))
    (org-ilm-pqueue--insert pqueue id rank)))

(defun org-ilm-pqueue-remove (id &optional collection)
  (org-ilm-pqueue--remove (org-ilm-pqueue collection) id))

(defun org-ilm-pqueue-select (priority &optional collection)
  (let* ((pqueue (org-ilm-pqueue collection))
         (rank (car (org-ilm--ost-parse-position pqueue priority))))
    (ost-node-id
     (ost-select (org-ilm-pqueue--ost (org-ilm-pqueue collection)) rank))))

(defun org-ilm-pqueue-read-rank (&optional numnew-or-id collection)
  (car (org-ilm--ost-read-position (org-ilm-pqueue collection) numnew-or-id)))

(defun org-ilm-pqueue-queue (&optional collection)
  "Return `org-ilm-queue' from all elements in the priority queue.

Because element headlines might be deleted, their ids will map to the id
itself in `org-ilm-queue--elements', rather than `org-ilm-element'
object."
  (let* ((pqueue (org-ilm-pqueue collection))
         (collection (org-ilm-pqueue--collection pqueue))
         (queue (make-org-ilm-queue
                 :name (format "Priority queue (%s)" 
                               (symbol-name collection))
                 :collection collection
                 :type 'pqueue))
         (elements (org-ilm-query-collection collection #'org-ilm-query-all)))

    ;; First go through the elements that have been found and add them to ost
    ;; and nodes map as normal.
    (dolist (element elements)
      (org-ilm-queue--insert
       queue element
       (car (org-ilm-pqueue-priority (org-ilm-element-id element) :pqueue pqueue))))

    ;; For ids that have not been found, i.e. were not added in the previous
    ;; loop, put them in the ost as normal, but in the node map use the id also
    ;; as value.
    (dolist (id (hash-table-keys (ost-tree-nodes (org-ilm-pqueue--ost pqueue))))
      (unless (gethash id (org-ilm-queue--elements queue))
        (puthash id id (org-ilm-queue--elements queue))
        (ost-tree-insert (org-ilm-queue--ost queue)
                         (car (org-ilm-pqueue-priority id :pqueue pqueue))
                         id)))
    
    queue))

(defun org-ilm-pqueue-select-position (&optional initial)
  "Select a position in the priority queue interactively."
  (org-ilm--queue-select-read (org-ilm-pqueue-queue) initial))

(cl-defun org-ilm-pqueue-priority (id &key pqueue collection rank quantile nil-if-absent)
  "Position of the ID in PQUEUE as a cons (rank . quantile).
With RANK or QUANTILE, set the new position in the queue, or insert there if not exists."
  (cl-assert (not (and pqueue collection)))
  (cl-assert (not (and rank quantile)))
  (setq pqueue (or pqueue (org-ilm-pqueue collection)))
  (cl-assert (org-ilm-pqueue-p pqueue))
  (let* ((ost (org-ilm-pqueue--ost pqueue))
         (node (ost-tree-node-by-id ost id)))
    (cond
     ;; Position specified: Move or add the element 
     ((or rank quantile)
      (unless rank
        (setq rank (org-ilm--ost-quantile-to-rank pqueue quantile)))
      (if node
          (org-ilm-pqueue--move pqueue id rank)
        (org-ilm-pqueue--insert pqueue id rank)))
     ;; Retrieve position
     (node
      (let* ((rank (ost-rank ost id))
             (quantile (org-ilm--ost-rank-to-quantile pqueue rank)))
        (cons rank quantile)))
     ;; Element not found in priority queue
     (t
      (when (and (not nil-if-absent)
                 (yes-or-no-p 
                  (format "Element \"%s\" not found in priority queue. Add it?" id)))
        (if-let ((element (org-ilm-element-from-id id))
                 (pqueue-collection (org-ilm-pqueue--collection pqueue)))
            (if (eq (org-ilm-element-collection element) pqueue-collection)
                (progn
                  (org-ilm-pqueue--insert pqueue id (org-ilm-pqueue-read-rank))
                  (org-ilm-pqueue-priority id :pqueue pqueue))
              (error "Element \"%s\" not part of collection \"%s\"" id pqueue-collectoin))
          (error "Org heading \"%s\" not an ilm element." id)))))))

;;;; Logging

;; Logging stuff to the ilm drawer table.
;; Fields of the log table in all elements:
;; - timestamp: when it was reviewed (or created)
;; - delay: num days between timestamp and scheduled date
;; - priority: "rank-size" when it was reviewed
;; - due: the next scheduled review (date without time)
;; - state: new state after review
;; Notes:
;; - The creation of an element is also logged.
;; - The timestamp can be compared with the delay field to calculate delay or how
;; - much earlier it was reviewed.
;; - The delay+timestamp fields can be compared with the previous log's due
;;   field to calculate how much it was postponed.

(defconst org-ilm-log-element-fields '(timestamp delay priority due state))
(defconst org-ilm-log-material-fields (append
                                       org-ilm-log-element-fields
                                       '(duration)))
(defconst org-ilm-log-card-fields (append
                                   org-ilm-log-element-fields
                                   '(rating stability difficulty)))

(defcustom org-ilm-log-card-parameter-precision 3
  "The number of decimals to store for the stability and difficulty card parameters.

Stability is measured as number of days. Negligible difference.
Difficulty is between 1 and 10. Want a bit more precision here."
  :type 'number
  :group 'org-ilm-card)

(cl-defstruct org-ilm-log-review
  "A review entry in an ilm element log drawer table."
  type timestamp delay priority due duration state rating stability difficulty)

(defun org-ilm-log-review-format-field (field)
  "Format FIELD column for use in the log."
  (pcase field
    ('stability "S")
    ('difficulty "D")
    ('priority "prior")
    (_ (format "%s" field))))

(defun org-ilm-log-review-format-value (review field)
  "Format the value of FIELD in `org-ilm-log-review' object REVIEW for use in the log."
  (let* ((getter (intern (concat "org-ilm-log-review-" (symbol-name field))))
         (value (funcall getter review)))
    (cond
     ((null value) "")
     ((eq field 'delay)
      (if (= 0 value) "" (format "%s" value)))
     ((eq field 'rating)
      (format "%s" value))
     ((eq field 'duration)
      (org-duration-from-minutes value 'h:mm:ss))
     ((eq field 'state)
      (format "%s" (pcase value
                     (:learning :learn)
                     (:relearning :relearn)
                     (_ value))))
     ((member field '(stability difficulty))
      (format (concat "%." (number-to-string org-ilm-log-card-parameter-precision) "f") value))
     ((eq field 'priority)
      (format "%s-%s" (car value) (cdr value)))
     ((eq field 'due)
      (org-ilm--ts-format-utc-date (ts-parse value)))
     (t value))))

(defun org-ilm-log-review-from-alist (alist &optional type)
  "Make an `org-ilm-log-review' object from an ALIST.
If element TYPE is omitted, infer from headline at point."
  (unless type (setq type (org-ilm-type)))
  (cl-assert (member type '(material card)))
  ;; Empty values are parsed as "" so turn them to nil
  (dolist (pair alist)
    (when (and (stringp (cdr pair)) (string= (cdr pair) ""))
      (setcdr pair nil)))
  (let ((timestamp (alist-get 'timestamp alist))
        (delay (alist-get 'delay alist))
        (priority (or (alist-get 'priority alist)
                      (alist-get 'prior alist)))
        (due (alist-get 'due alist))
        (duration (alist-get 'duration alist))
        (state (alist-get 'state alist))
        (rating (alist-get 'rating alist))
        (stability (or (alist-get 'stability alist)
                       (alist-get 'S alist)))
        (difficulty (or (alist-get 'difficulty alist)
                        (alist-get 'D alist))))
    (cl-assert (org-ilm--timestamp-is-utc-iso8601-p timestamp))
    (cl-assert (org-ilm--timestamp-is-iso8601-date-p due))
    (let* ((parts (string-split priority "-"))
           (rank (string-to-number (car parts)))
           (size (string-to-number (cadr parts))))
      (cl-assert (or (string= (car parts) "0") (> rank 0)))
      (cl-assert (or (string= (cadr parts) "0") (> size 0)))
      (setq priority (cons rank size)))
    (when delay
      (setq delay (string-to-number delay))
      (cl-assert (numberp delay)))
    (when state (setq state (intern state)))
    (pcase type
      ('card
       (setq state (pcase state
                     (:learn :learning)
                     (:relearn :relearning)
                     (_ state)))
       (cl-assert (member state '(:done :learning :review :relearning)))
       (when rating
         (setq rating (intern rating))
         (cl-assert (member rating '(:again :hard :good :easy))))
       ;; Both are absent (new card) or both are present
       (cl-assert (eq (not stability) (not difficulty)))
       (when (and stability difficulty)
         (setq stability (string-to-number stability)
               difficulty (string-to-number difficulty))
         (cl-assert (floatp stability))
         (cl-assert (floatp difficulty))))
      ('material
       (cl-assert (or (not state) (member state '(:done))))
       (cl-assert (null rating))
       (when duration
         (setq duration (org-duration-to-minutes duration)))))
    
    (make-org-ilm-log-review
     :type type :timestamp timestamp :delay delay :priority priority
     :due due :duration duration :state state :rating rating
     :stability stability :difficulty difficulty)))

(defun org-ilm-log-review-ensure (data)
  "Make an `org-ilm-log-review' object from alist DATA, or return if already is one."
  (if (org-ilm-log-review-p data)
      data
    (org-ilm-log-review-from-alist data)))

(defun org-ilm--log-data-ensure (data)
  "Make `org-ilm-log-review' object list from alist or list of alists DATA."
  ;; If alist of one row, put it in a list
  (when (or (org-ilm-log-review-p data) (not (listp (car (car data)))))
    (setq data (list data)))
  (mapcar #'org-ilm-log-review-ensure data))

(defun org-ilm--log-beginning-of-drawer (&optional create-if-missing)
  "Move point to the beginning of the ilm drawer.
If found, return start and end positions as cons."
  (let ((bound (cdr (org-ilm--org-headline-bounds)))
        (drawer-begin-regex (rx bol (* blank) ":" (literal org-ilm-log-drawer-name) ":" (* blank) eol))
        (drawer-end-reg (rx bol (* blank) ":END:" (* blank) eol))
        (point (point)))
    (org-back-to-heading)
    (if (re-search-forward drawer-begin-regex bound 'noerror)
        (let ((begin (progn (beginning-of-line) (point)))
              (end (if-let ((p (save-excursion
                                 (re-search-forward drawer-end-reg bound 'noerror))))
                       p
                     (goto-char point)
                     (error "End of drawer missing"))))
          (cons begin end))
      (goto-char point)
      (when create-if-missing
        (org-end-of-meta-data) ; 'full)
        (cond
         ((and (eolp) (not (bolp)))
          (newline))
         ((not (and (eolp) (bolp)))
          (newline)
          (forward-line -1)))
        (save-excursion
          (insert ":" org-ilm-log-drawer-name ":")
          (newline)
          (insert ":END:"))
        (org-ilm--log-beginning-of-drawer)))))

(defun org-ilm--log-fields ()
  "Return the log table fields of the element at point."
  (pcase (org-ilm-type)
    ('material org-ilm-log-material-fields)
    ('card org-ilm-log-card-fields)
    (_ (error "Not an ilm element"))))

(defun org-ilm--log-append (data)
  "Insert DATA at the end of an org table.

DATA can be an alist for one row, or a list of alists for multiple
rows. The alist maps column name to entry value."
  (cl-assert (org-table-p))
  (setq data (org-ilm--log-data-ensure data))
  (atomic-change-group
    (goto-char (org-table-end))
    (forward-line -1)
    (end-of-line)
    (dolist (review data 2)
      (newline)
      (insert "|")
      (dolist (field (org-ilm--log-fields))
        (insert (org-ilm-log-review-format-value review field) "|")))
    (org-table-align)))

(defun org-ilm--log-beginning-of-table (&optional init-data)
  "Go to the beginning of the log drawer table. Return pos.

With optional INIT-DATA, create a new table with this data. See
`org-ilm--log-append'."
  (when-let ((bounds (org-ilm--log-beginning-of-drawer init-data)))
    (let ((table-pos (save-excursion
                       (re-search-forward org-table-line-regexp (cdr bounds) t))))
      (cond
       (table-pos
        (goto-char table-pos)
        (beginning-of-line)
        (point))
       (init-data
        (atomic-change-group
          (forward-line 1)
          (newline)
          (forward-line -1)
          (insert "|")
          (dolist (field (org-ilm--log-fields))
            (insert (org-ilm-log-review-format-field field) "|"))
          (org-table-insert-hline)
          (org-ilm--log-append init-data)
          (org-ilm--log-beginning-of-table)))))))

(defun org-ilm--log-insert (data)
  "Insert DATA into the log table, creating one if missing."
  (if (org-ilm--log-beginning-of-table)
      (org-ilm--log-append data)
    (org-ilm--log-beginning-of-table data)))

(defun org-ilm--log-read (&optional init-data)
  "Return the data of the log drawer table as an alist.

With optional INIT-DATA, create a new table with this data. See
`org-ilm--log-append'."
  (save-excursion
    (when (org-ilm--log-beginning-of-table init-data)
      (let ((table (org-table-to-lisp)))
        (cl-assert (>= (length table) 3))
        (cl-assert (eq (nth 1 table) 'hline))
        (let* ((columns (mapcar #'intern (car table)))
               (data (mapcar
                      (lambda (row)
                        (mapcar
                         (lambda (i)
                           (cons (nth i columns)
                                 (substring-no-properties (nth i row))))
                         (number-sequence 0 (1- (length columns)))) )
                      (cl-subseq table 2))))
          (org-ilm--log-data-ensure data))))))

(defun org-ilm--log-log (type priority due scheduled &rest review-data)
  "Log a new review for element with TYPE, return the review object.

SCHEDULED is the timestamp when the element was scheduled for
review. For new elements, this should be nil.

DUE is the new scheduled review timestamp."
  (cl-assert (member type '(material card)))
  (when (ts-p due)
    (setq due (pcase type
                ('card (org-ilm--ts-format-utc-date-maybe-time due))
                ('material (org-ilm--ts-format-utc-date due)))))
  (let* ((timestamp (or (plist-get review-data :timestamp) (ts-now)))
         (priority (org-ilm-pqueue-parse-new-position priority))
         (rank (car priority))
         (size (org-ilm-pqueue-count))
         (review-log (org-ilm--log-read)))

    (cl-assert (ts-p timestamp))
    (setf (plist-get review-data :type) type
          (plist-get review-data :priority) (cons rank size)
          (plist-get review-data :due) due
          (plist-get review-data :timestamp) (org-ilm--ts-format-utc timestamp))

    (if (null scheduled)
        ;; Make sure that if SCHEDULED is nil this is a new element
        (cl-assert (null review-log))
      (cl-assert review-log)
      ;; TODO Is this midnight shift calculation right????
      (setf (plist-get review-data :delay)
            (org-ilm--ts-diff-rounded-days
             timestamp
             (ts-adjust 'minute (org-ilm-midnight-shift-minutes) scheduled))))

    (let ((review (apply #'make-org-ilm-log-review review-data)))
      (org-ilm--log-insert review)
      review)))


;;;; Types

;; There are three headline types: Subjects, Materials, and Cards.

(defun org-ilm-type (&optional headline-or-state)
  "Return ilm type from an org headline or its todo state.

See `org-ilm-card-states', `org-ilm-material-states', and `org-ilm-subject-states'."
  (when-let ((state (cond
                     ((org-element-type-p headline-or-state 'headline)
                      (org-element-property :todo-keyword headline-or-state))
                     ((stringp headline-or-state)
                      headline-or-state)
                     ((not headline-or-state)
                      (cl-assert (eq major-mode 'org-mode))
                      (org-get-todo-state))
                     (t (error "Unrecognized arg")))))
    (cond
     ((member state org-ilm-card-states) 'card)
     ((member state org-ilm-material-states) 'material)
     ((member state org-ilm-subject-states) 'subject))))

;;;; Element

(cl-defstruct org-ilm-element
  "A piece of knowledge."
  id collection state level pcookie rawval title tags sched
  type subjects registry media)

(defun org-ilm-element-schedrel (element)
  (when-let ((scheduled (org-ilm-element-sched element)))
    ;; convert from sec to days
    (/ (ts-diff (ts-now) scheduled) 86400)))

(defun org-ilm-element-card-p (element)
  (eq (org-ilm-element-type element) 'card))

(defun org-ilm-element-at-point ()
  "Parse org-ilm data of headline at point.

Was thinking of org-ql--value-at this whole function, but this is
wasteful if headline does not match query."
  (when-let ((headline (org-ilm--org-headline-at-point)))
    (let* (;; `headline' looses the priority property which is set manually in
           ;; `org-ilm--org-headline-at-point', after I access the :todo-keyword
           ;; property below. I think it is because it is a deferred value, which
           ;; might cause org to overwrite the custom set :priority value after
           ;; resolving it when it is accessed. In any case, it is essentialy to
           ;; use the priority before accessing the other
           ;; properties. Alternatively we can just resolve the deferred
           ;; properties by accessing them all in
           ;; `org-ilm--org-headline-at-point'.
           (id (org-element-property :ID headline))
           (todo-keyword (org-element-property :todo-keyword headline))
           (type (org-ilm-type todo-keyword))
           (is-card (eq type 'card))
           (subject-data (org-ilm--subject-cache-gather headline))
           (scheduled (when-let ((s (org-element-property :scheduled headline)))
                        (ts-parse-org-element s)))
           (entry (org-node-at-point)))
      
      (when type ; non-nil only when org-ilm type
        (make-org-ilm-element
         ;; Headline element properties
         :id id
         :state todo-keyword
         :level (org-element-property :level headline)
         :pcookie (org-element-property :priority headline)
         :rawval (org-element-property :raw-value headline)
         :tags (org-element-property :tags headline)
         :title (org-no-properties ; Remove text properties from title
                 (org-element-interpret-data (org-element-property :title headline)))

         ;; Ilm stuff
         :collection (org-ilm--collection-file)
         :sched scheduled
         :type type
         :registry (org-mem-entry-property-with-inheritance "REGISTRY" entry)
         :media (org-ilm--media-compile entry)
         ;; cdr to get rid of headline priority in the car - redundant
         :subjects subject-data)))))

(defun org-ilm-element-from-id (id)
  (org-ilm--org-with-point-at id
    (org-ilm-element-at-point)))

(defun org-ilm-element-from-context ()
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
      ('collection
       (condition-case-unless-debug err
           (org-ilm-element-at-point)
         (error nil)))
      ('attachment
       (org-ilm--org-with-point-at (nth 1 location)
         (org-ilm-element-at-point)))
      ('queue
       (org-ilm--org-with-point-at (nth 2 location)
         (org-ilm-element-at-point))))))

(defun org-ilm-element-entry (element)
  (org-mem-entry-by-id (org-ilm-element-id element)))

(defmacro org-ilm-element-with-point-at (element &rest body)
  (declare (debug (body)) (indent 1))
  `(org-ilm--org-with-point-at (org-ilm-element-id ,element)
     ,@body))

(defmacro org-ilm-element-with-point-at-registry (element &rest body)
  (declare (debug (body)) (indent 1))
  `(org-ilm--org-with-point-at (org-ilm-element-registry ,element)
     ,@body))

(defun org-ilm-element-is-card (element)
  (cl-assert (org-ilm-element-p element))
  (eq 'card (org-ilm-element-type element)))

(defun org-ilm-element-last-review (element-or-id)
  (org-ilm--org-with-point-at
      (if (org-ilm-element-p element-or-id)
          (org-ilm-element-id element-or-id)
        element-or-id)
    (when-let* ((review-log (org-ilm--log-read))
                (last-review (car (last review-log))))
      (ts-parse (org-ilm-log-review-timestamp last-review)))))

(defun org-ilm-element-interval (element)
  "Interval in days."
  (when-let ((last-review (org-ilm-element-last-review element))
             (sched (org-ilm-element-sched element)))
    ;; Seconds to days
    (/ (ts-diff last-review sched) 86400)))
    
(cl-defun org-ilm--element-by-id (org-id &key if-matches-query)
  "Return an element by their org id."
  (cond
   ((null if-matches-query))
   ;; Is a function - fine
   ((and (symbolp if-matches-query) (fboundp if-matches-query)))
   ((eq if-matches-query t)
    (setq if-matches-query (plist-get org-ilm-queue :query))
    (unless if-matches-query
      (error "No query found"))))
  
  (save-excursion
    (org-ilm--org-with-point-at org-id
      (pcase if-matches-query
        (t
         (save-restriction
           (org-ilm--org-narrow-to-header)
           (car (org-ilm-query-buffer
                 (current-buffer)
                 if-matches-query
                 t))))
        (_ (org-ilm-element-at-point))))))

;;;;; Querying

(defun org-ilm--element-children (id &optional return-type include-self all-descendants)
  "Return child elements of element with ID."
  (cl-assert (member return-type '(entry element headline)))
  (let ((marker (org-id-find id 'marker))
        (parent (if all-descendants 'ancestors 'parent)))
    (unless marker
      (error "No entry with ID %s" id))
    (org-ql-select (list (marker-buffer marker))
      `(and
        ,@(if include-self
              `((or (property "ID" ,id) (,parent (property "ID" ,id))))
            `((,parent (property "ID" ,id))))
        (property "ID")
        (property "ILM_PDF")
        (or ,(cons 'todo org-ilm-material-states)
            ,(cons 'todo org-ilm-card-states)))
      :narrow t
      :action
      (pcase (or return-type 'headline)
        ('headline) ;; default: org headline element
        ('entry #'org-node-at-point)
        ('element #'org-ilm-element-at-point)
        (_ (error "Unrecognized return type"))))))

;;;;; Priority

(cl-defun org-ilm-element-priority (element &key rank quantile)
  "Return (rank . quantile) of ELEMENT in the priority queue.
With RANK or QUANTILE, set the new position in the queue.
ELEMENT may be nil, in which case try to read it from point."
  (cl-assert (not (and rank quantile)))
  (unless element (setq element (org-ilm-element-from-context)))
  (cl-assert (org-ilm-element-p element))
  (org-ilm-pqueue-priority
   (org-ilm-element-id element)
   :collection (org-ilm-element-collection element)
   :rank rank :quantile quantile))

(defun org-ilm-element-pqueue (element)
  (org-ilm-pqueue (org-ilm-element-collection element)))

(defun org-ilm-element-prank (element)
  "Position of ELEMENT in the priority queue."
  (car (org-ilm-element-priority element)))

(defun org-ilm-element-pquantile (element)
  "Quantile of ELEMENT in the priority queue."
  (cdr (org-ilm-element-priority element)))

(defun org-ilm-element-priority-formatted (element)
  (org-ilm--ost-format-position
   (org-ilm-element-pqueue element)
   (org-ilm-element-priority element)))

;;;;; Actions

(defun org-ilm-element-set-schedule (element)
  "Set the schedule of an ilm element."
  (interactive
   (list (or org-ilm--element-transient-element (org-ilm-element-from-context))))
  (org-ilm-element-with-point-at element
    (call-interactively #'org-ilm-schedule)))

(defun org-ilm-element-set-priority--cookie (element)
  "Set the priority of an ilm element."
  (interactive
   (list (or org-ilm--element-transient-element (org-ilm-element-from-context))))
  (org-ilm-element-with-point-at element
    (let ((min org-priority-highest)
          (max org-priority-lowest)
          priority)
      (while (null priority)
        (let ((number (read-number (format "Priority (%s-%s): " min max) org-priority-default)))
          (when (<= min number max)
            (setq priority number))))
      (org-ilm--org-priority-set priority))))

(defun org-ilm-element-set-priority (element)
  "Set the priority of an ilm element."
  (interactive
   (list (or org-ilm--element-transient-element (org-ilm-element-from-context))))
  (let ((position (org-ilm-pqueue-select-position (org-ilm-element-priority element))))
    (org-ilm-element-priority element :rank (car position))))

(defun org-ilm-element-delete (element &optional warn-attach)
  "Delete ilm element at point."
  (interactive
   (list (or org-ilm--element-transient-element (org-ilm-element-from-context))))
  (cl-assert (org-ilm-element-p element) nil "Not an ilm element")

  (when (called-interactively-p)
    (org-mark-subtree)
    (unless (yes-or-no-p "Delete element?")
      (user-error "Abort deletion")))
    
  (when (and (org-attach-dir) (or (called-interactively-p) warn-attach))
    (cond-let*
      ((org-entry-get nil "DIR")
       (when (yes-or-no-p "Delete attachment directory?")
         (org-attach-delete-all 'force)))
      ([attachments (f-glob (file-name-concat (org-attach-dir)
                                              (concat (org-ilm-element-id element) "*")))]
       (when (yes-or-no-p (format "Delete element %s attachtment(s)?" (length attachments)))
         (dolist (file attachments)
           (delete-file file))))))
     
  (org-ilm--org-delete-headline)
  (org-ilm-pqueue-remove (org-ilm-element-id element)
                         (org-ilm-element-collection element)))

;;;;; Transient

(defvar org-ilm--element-transient-element nil)

(transient-define-suffix org-ilm--element-transient-schedule ()
  :key "s"
  :description
  (lambda ()
    (concat
     "Schedule "
     (when-let* ((element org-ilm--element-transient-element)
                 (sched (org-ilm-element-sched element)))
       (propertize (ts-format "%Y-%m-%d" sched) 'face 'transient-value))))
  :transient 'transient--do-call
  (interactive)
  (call-interactively #'org-ilm-element-set-schedule)
  (setq org-ilm--element-transient-element (org-ilm-element-from-context)))

(transient-define-suffix org-ilm--element-transient-priority ()
  :key "p"
  :description
  (lambda ()
    (concat
     "Priority "
     (when-let* ((element org-ilm--element-transient-element)
                 (priority (org-ilm-element-priority-formatted element)))
       (propertize priority 'face 'transient-value))))
  :transient 'transient--do-call
  (interactive)
  (call-interactively #'org-ilm-element-set-priority)
  (setq org-ilm--element-transient-element (org-ilm-element-from-context)))

(defun org-ilm--element-transient-registry-attach-read ()
  (org-ilm-element-with-point-at-registry
      org-ilm--element-transient-element
    (when-let* ((attach-dir (org-attach-dir))
                (attachments (org-attach-file-list attach-dir)))
      (completing-read "Attachment: " attachments nil t))))

(transient-define-infix org-ilm--element-transient-registry-attach-attachment ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--attachment="
  :reader
  (lambda (prompt initial-input history)
    (org-ilm--element-transient-registry-attach-read)))

(transient-define-prefix org-ilm--element-transient-registry-attach ()
  :refresh-suffixes t
  :value
  (lambda ()
    (append
     (list "--method=lns" "--main")
     (when-let ((attachment (org-ilm--element-transient-registry-attach-read)))
       (list (concat "--attachment=" attachment)))))
  
  ["Attach from registry"
   ("a" "Attachment" org-ilm--element-transient-registry-attach-attachment)
   ("m" "Method" "--method=" :choices ("cp" "mv" "ln" "lns") :always-read t)
   ("M" "Main attachment (use id)" "--main")
   ("RET" "Attach"
    (lambda ()
      (interactive)
      (let* ((args (transient-args transient-current-command))
             (attachment (transient-arg-value "--attachment=" args))
             (attach-dir (org-ilm-element-with-point-at-registry
                             org-ilm--element-transient-element
                           (org-attach-dir)))
             (method (transient-arg-value "--method=" args))
             (main (transient-arg-value "--main" args)))
        (org-ilm-element-with-point-at org-ilm--element-transient-element
          (org-attach-attach (expand-file-name attachment attach-dir) nil (intern method))
          (when main
            (rename-file
             (expand-file-name attachment (org-attach-dir))
             (expand-file-name 
              (concat (org-id-get) "." (file-name-extension attachment))
              (org-attach-dir)))))))
    :inapt-if-not
    (lambda ()
      (transient-arg-value "--attachment=" (transient-get-value))))
   ]
  )

(transient-define-prefix org-ilm--element-transient ()
  :refresh-suffixes t
  [:description
   (lambda ()
     (pcase (org-ilm-element-type org-ilm--element-transient-element)
       ('material "Material")
       ('card "Card")
       (_ "Element")))
   (:info*
    (lambda ()
      (propertize
       (org-ilm-element-title org-ilm--element-transient-element)
       'face 'italic)))
   (org-ilm--element-transient-schedule)
   (org-ilm--element-transient-priority)
   ]

  [
   ["Attachment"
    ("as" "Set"
     (lambda ()
       (interactive)
       (let* ((attach-dir (org-attach-dir))
              (attachments (org-attach-file-list attach-dir))
              (attachment (completing-read "Attachment: " attachments nil t)))
         (rename-file (expand-file-name attachment attach-dir)
                      (expand-file-name
                       (concat (org-id-get-create) "." (file-name-extension attachment))
                       attach-dir)))))
    ("ao" "Open"
     (lambda ()
       (interactive)
       (org-ilm-element-with-point-at org-ilm--element-transient-element
         (org-ilm-open-attachment))))
    ("ad" "Dired"
     (lambda ()
       (interactive)
       (let* ((element org-ilm--element-transient-element)
              (id (org-ilm-element-id element))
              attach-dir)
         (org-ilm-element-with-point-at element
           (setq attach-dir (org-attach-dir)))
         (unless attach-dir (user-error "No attachment directory"))
         (find-name-dired attach-dir (concat id "*")))))
    ]

   ["Registry"
    :if (lambda () (org-ilm-element-registry org-ilm--element-transient-element))
    ("gj" "Jump"
     (lambda ()
       (interactive)
       (org-node-goto-id (org-ilm-element-registry org-ilm--element-transient-element))))
    ("ga" "Attach..." org-ilm--element-transient-registry-attach
     :inapt-if-not
     (lambda ()
       (org-ilm--org-with-point-at
           (org-ilm-element-registry org-ilm--element-transient-element)
         (org-attach-dir))))
    ]

   ["Media"
    ("ms" "Set"
     (lambda ()
       (interactive)
       (let* ((element org-ilm--element-transient-element)
              (id (org-ilm-element-id element))
              (entry (org-mem-entry-by-id id))
              (registry-entry (org-mem-entry-by-id (org-ilm-element-registry element)))
              (element-medias (org-ilm--entry-media-sources entry))
              (registry-medias (org-ilm--entry-media-sources registry-entry))
              (choice (consult--multi
                       (list
                        (list
                         :name "Element"
                         :narrow ?e
                         :items element-medias
                         :action #'message)
                        (list
                         :name "Registry"
                         :narrow ?r
                         :items registry-medias
                         :action #'message))
                       :require-match t
                       :prompt "Media: "))
              (media (car choice)))
         (org-ilm--org-with-point-at id
           (org-entry-put nil "ILM_MEDIA" media)
           (save-buffer))))
     :transient transient--do-call)
    ("mo" "Open"
     (lambda ()
       (interactive)
       (org-ilm--media-open
        (org-ilm-element-entry org-ilm--element-transient-element)))
     :if (lambda () (org-ilm-element-media org-ilm--element-transient-element)))
    ("mr" "Range"
     (lambda ()
       (interactive)
       (let* ((element org-ilm--element-transient-element)
              (entry (org-ilm-element-entry element))
              (parts (org-ilm--media-compile entry))
              (range (read-string "Range: "
                                  (when (stringp (nth 1 parts))
                                    (if (stringp (nth 2 parts))
                                        (string-join (cdr parts) "-")
                                      (nth 1 parts)))))
              (source (car parts)))
         (org-ilm--org-with-point-at (org-ilm-element-id element)
           (org-entry-put nil "ILM_MEDIA+" range)
           (save-buffer)
           (org-mem-updater-ensure-id-node-at-point-known)
           (setq org-ilm--element-transient-element (org-ilm-element-at-point)))))
     :if (lambda () (org-ilm-element-media org-ilm--element-transient-element))
     :transient transient--do-call)
    ]
   ]

  ["Actions"
   [
    ("o" "Open"
     (lambda ()
       (interactive)
       (org-ilm--org-goto-id (org-ilm-element-id org-ilm--element-transient-element))))
    ("D" "Delete"
     (lambda ()
       (interactive)
       (org-ilm-element-with-point-at org-ilm--element-transient-element
         (call-interactively #'org-ilm-element-delete))))
    ]
   [
    ("C" "New card"
     (lambda ()
       (interactive)
       (org-ilm--capture-cloze
        :target (org-ilm-element-id org-ilm--element-transient-element)
        :content "")
       ))
    ("X" "New extract"
     (lambda ()
       (interactive)
       (org-ilm--capture-extract
        :target (org-ilm-element-id org-ilm--element-transient-element)
        :content "")))
    ]
   ]
  )

(defun org-ilm-element-actions ()
  "Open menu to apply an action on an element."
  (interactive)
  (let ((element (org-ilm-element-from-context)))
    (if (null element)
        (user-error "No ilm element!")
      (setq org-ilm--element-transient-element element)
      ;; Unset var after transient exited. Tried temporarily setting var within
      ;; let, but gets lost somehow.
      (org-ilm--add-hook-once
       'transient-post-exit-hook
       (lambda () (setq org-ilm--element-transient-element nil)))
      (org-ilm--element-transient))))


;;;; Material

;; Source material to be consumed incrementally. Includes extracts.

(defun org-ilm--material-log-new (priority due &optional timestamp)
  "Log the creation of a new material element in the ilm drawer."
  (org-ilm--log-log 'material priority due nil :timestamp timestamp))

(defun org-ilm--material-log-review (priority due scheduled &optional timestamp duration)
  "Log the review of a material element in the ilm drawer."
  (org-ilm--log-log 'material priority due scheduled
                    :duration duration :timestamp timestamp))

(defun org-ilm--material-calculate-interval (priority scheduled last-review)
  "Calculate the new scheduled interval in days."
  (cl-assert (and (ts-p scheduled) (ts-p last-review)))

  (let ((review-interval (org-ilm--ts-diff-rounded-days (ts-now) last-review)))
    ;; If the scheduled date was adjusted to be before the last review, then see
    ;; it as a new element and use initial interval base on priority
    (if (< (org-ilm--ts-diff-rounded-days (ts-now) last-review) 0)
        (org-ilm--initial-schedule-interval-from-priority priority)
      
      (let* ((multiplier 1.5)
             (sched-interval (org-ilm--ts-diff-rounded-days scheduled last-review))
             (new-interval (* sched-interval multiplier)))

        ;; Reviewed earlier than scheduled. Take proportion of time from last review
        ;; to today relative to scheduled date and use that to adjust the new
        ;; interval.
        (when (< review-interval sched-interval)
          (setq new-interval
                (+ sched-interval
                   (* new-interval (/ (float review-interval) sched-interval)))))

        (round new-interval)))))

(defun org-ilm--material-review (&optional org-id duration)
  "Apply a review on the material element.
This will update the log table and the headline scheduled date."
  (org-ilm--org-with-point-at org-id
    (let* ((review-log (org-ilm--log-read))
           (last-review (car (last review-log))))
      (cl-assert last-review nil "Element missing log")
      (let* ((element (org-ilm-element-at-point))
             (scheduled (org-ilm-element-sched element))
             (priority (org-ilm-element-priority element))
             (interval (org-ilm--material-calculate-interval
                        priority scheduled
                        (ts-parse (org-ilm-log-review-timestamp last-review))))
             (due (ts-adjust 'day interval (ts-now))))
        (atomic-change-group
          (org-ilm--material-log-review priority due scheduled nil duration)
          (org-ilm--org-schedule :timestamp due :ignore-time t))))))


;;;; Cards

(defcustom org-ilm-card-fsrs-desired-retention .9
  "Target probability of successful recall (0.0-1.0).

FSRS default: 0.9"
  :type 'number
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-learning-steps '((1 :minute) (10 :minute))
  "List of time intervals for initial learning phase.

FSRS default: '((1 :minute) (10 :minute))"
  :type 'list
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-relearning-steps '((10 :minute))
  "List of time intervals for relearning phase.

FSRS default: '((10 :minute))"
  :type 'list
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-maximum-interval '(36500 :day)
  "Upper bound for scheduling intervals.

FSRS default: '(36500 :day)"
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-fuzzing-p t
  "Randomize intervals within bounds.

FSRS default: t"
  :type 'boolean
  :group 'org-ilm-card)

(defcustom org-ilm-card-first-interval 'priority
  "The first interval for cards."
  :type '(choice (const :tag "Immediate" nil)
                 (const :tag "Priority based interval" priority)
                 (string :tag "Custom interval as %H:%S format")
                 (number :tag "Custom interval in minutes"))
  :group 'org-ilm-card)

;;;;; Logic

(defun org-ilm-card-first-interval ()
  (let ((interval org-ilm-card-first-interval))
    (cond
     ((eq interval 'priority)
      interval)
     ((and (numberp interval) (>= interval 0))
      interval)
     ((stringp interval)
      (ignore-errors (org-duration-to-minutes interval)))
     (t 0))))

(defun org-ilm--card-default-scheduler ()
  (fsrs-make-scheduler
   :desired-retention org-ilm-card-fsrs-desired-retention
   :learning-steps org-ilm-card-fsrs-learning-steps
   :relearning-steps org-ilm-card-fsrs-relearning-steps
   :maximum-interval org-ilm-card-fsrs-maximum-interval
   :enable-fuzzing-p org-ilm-card-fsrs-fuzzing-p))

(defun org-ilm--card-review (rating &optional org-id timestamp duration)
  "Rate a card element with RATING, update log and scheduled date.
If ORG-ID ommitted, assume point at card headline."
  (org-ilm--org-with-point-at org-id
    (if-let* ((headline (org-ilm--org-headline-at-point))
              (org-id (org-id-get))
              (collection (org-ilm--collection-file))
              (priority (org-ilm-pqueue-priority org-id :collection collection))
              (scheduled (org-element-property :scheduled headline)))
        (atomic-change-group
          (cl-assert (eq (org-ilm-type headline) 'card))
          (let ((review (org-ilm--card-log-review
                         priority (ts-parse-org-element scheduled) rating
                         (org-ilm--card-default-scheduler)
                         timestamp duration)))
            (org-ilm--org-schedule
             :timestamp (ts-parse (org-ilm-log-review-due review)))))
      ;; TODO error message sucks
      (error "Cannot rate headline due to lacking info"))))

;;;;; Logging

;; TODO This can be optimized by truncating based on latest :review state
(defun org-ilm--card-step-from-log (scheduler review-log)
  "Calculate the current step based on a REVIEW-LOG.

Returns the current step (integer) or nil if the card is in :review state.
An empty log implies a new card, so step is 0."
  ;; We could also run the simulation with fsrs-scheduler-review-card but that
  ;; calculates also the model parameters which is unnecessary work. So just
  ;; reimplement the step logic.
  (let ((learn-steps (length (fsrs-scheduler-learning-steps scheduler)))
        (relearn-steps (length (fsrs-scheduler-relearning-steps scheduler)))
        (current-step 0))
    (dolist (review review-log)
      (let ((state (org-ilm-log-review-state review))
            (rating (org-ilm-log-review-rating review)))
        ;; Note, :hard rating does not increment step
        (cond
         ((eq state :review)
          (setq current-step nil))
         ((eq rating :again)
          (setq current-step 0))
         ((eq rating :good)
          (if (= current-step (1- (pcase state
                                    (:learning learn-steps)
                                    (:relearning relearn-steps))))
              (setq current-step nil)
            (cl-incf current-step))))))
    current-step))

(defun org-ilm--card-log-new (priority due &optional timestamp)
  "Log the creation of a new card element in the ilm drawer."
  (org-ilm--log-log
   'card priority due nil 
   :timestamp timestamp
   ;; Default state of a new fsrs card
   :state :learning))

(defun org-ilm--card-log-review (priority scheduled rating scheduler &optional timestamp duration)
  "Log the review of a fsrs card in the ilm drawer table, return the review object."
  (cl-assert (ts-p scheduled))
  (setq timestamp (or timestamp (ts-now)))
  (let* ((review-log (org-ilm--log-read))
         (last-review (car (last review-log)))
         card)
    (cl-assert last-review nil "Card missing log")
    
    (setq card
          (car
           (fsrs-scheduler-review-card
            scheduler
            (fsrs-make-card
             :state (org-ilm-log-review-state last-review)
             :step (org-ilm--card-step-from-log scheduler review-log)
             :stability (org-ilm-log-review-stability last-review)
             :difficulty (org-ilm-log-review-difficulty last-review)
             :last-review (org-ilm-log-review-timestamp last-review)
             ;; Due date should be "artificial" one (potentially manually
             ;; edited), not the true date as scheduled in previous review, as
             ;; the algorithm does the calculation based on expected next
             ;; review. Having said that i cannot find this field being accessed
             ;; in `fsrs-scheduler-review-card'.
             :due (org-ilm--ts-format-utc scheduled))
            rating
            (org-ilm--ts-format-utc timestamp)
            duration)))

    (org-ilm--log-log
     'card priority (fsrs-card-due card) scheduled
     :timestamp timestamp :rating rating
     :state (fsrs-card-state card)
     :stability (fsrs-card-stability card)
     :difficulty (fsrs-card-difficulty card))))

;;;;; Cloze

(cl-defstruct org-ilm-cloze
  "Cloze content and optional hint, with positions."
  pos content content-pos hint hint-pos)

;; Cloze syntax
;; - No hint: {{c::content}}
;; - Hint: {{c::content}{hint}}
(define-peg-ruleset org-ilm-card-cloze
  (text () (* (or (and (not ["{}"]) (any)) "\\{" "\\}" (nested (peg text)))))
  (nested (content) (and "{" (funcall content) "}"))
  (cloze () (nested (peg (and (nested (peg (and "c::" (region text))))
                              (opt (nested (peg (region text)))))))
         `(v1 v2 v3 v4 -- (when v1 (cons v1 v2)) (cons v3 v4))))

(defun org-ilm--card-cloze-match-at-point ()
  (with-peg-rules (org-ilm-card-cloze)
    (when-let* ((cloze (peg-run (peg cloze))))
      (setq cloze (if (cadr cloze) (nreverse cloze) cloze))
      (let ((content-pos (car cloze))
            (hint-pos (cadr cloze)))
        (make-org-ilm-cloze
         :content (buffer-substring-no-properties
                   (car content-pos) (cdr content-pos))
         :hint (when hint-pos (buffer-substring-no-properties
                               (car hint-pos) (cdr hint-pos)))
         :content-pos content-pos
         :hint-pos hint-pos
         :pos (cons (- (car content-pos) 5)
                    (+ (cdr (or hint-pos content-pos)) 2)))))))

(defun org-ilm--card-cloze-match-around-point ()
  "Match cloze around point."
  (save-excursion
    (cond-let*
      ([cloze (org-ilm--card-cloze-match-at-point)]
       cloze)
      ([pos (org-in-regexp "{{c::")]
       (goto-char (car pos))
       (org-ilm--card-cloze-match-at-point))
      (t (when-let ((point (point))
                    ((re-search-backward "{{c::" nil t))
                    (cloze (org-ilm--card-cloze-match-at-point)))
           (when (<= point (cdr (org-ilm-cloze-pos cloze)))
             cloze))))))

(defun org-ilm--card-cloze-p ()
  "Return t if point on cloze."
  (if (org-ilm--card-cloze-match-around-point) t nil))

(defun org-ilm--card-cloze-match-forward (&optional end)
  "Jump to and return the first matching cloze."
  (let ((cloze (org-ilm--card-cloze-match-at-point)))
    (while (and (not cloze)
                (re-search-forward "{{c::" end t))
      ;; Back to beginning
      (goto-char (match-beginning 0))
      ;; We might match regex but not cloze
      (setq cloze (org-ilm--card-cloze-match-at-point)))
    cloze))

(defun org-ilm--card-cloze-gather (&optional begin end)
  "Return all clozes found in buffer."
  (let (cloze clozes)
    (save-excursion
      (goto-char (or begin (point-min)))
      (while (setq cloze (org-ilm--card-cloze-match-forward end))
        (push cloze clozes)))
    (nreverse clozes)))

(defun org-ilm-cloze-toggle ()
  "Toggle cloze at point, without creating the card."
  (interactive)
  (if (org-ilm--card-cloze-p)
      (org-ilm--card-uncloze)
    (org-ilm--card-cloze-dwim)))

(defun org-ilm--card-cloze-dwim (&optional hint)
  "Cloze the region or word at point."
  (let ((bounds (org-ilm--card-cloze-bounds)))
    (if (not bounds)
        (error "No active region or word at point")
      (org-ilm--card-cloze-region (car bounds) (cdr bounds) hint)
      bounds)))

(defun org-ilm--card-cloze-bounds ()
  "Return (beg . end) of what will be clozed."
  (let (begin end)
    (if (region-active-p)
        (setq begin (region-beginning)
              end (region-end))
      (if-let ((bounds (bounds-of-thing-at-point 'word)))
          (setq begin (car bounds)
                end (cdr bounds))))
    (cons begin end)))

(defun org-ilm--card-cloze-region (begin end &optional hint)
  "Coze the region between BEGIN and END."
  (save-excursion
    (goto-char end)
    (insert "}")
    (when hint
      (insert "{" hint "}"))
    (insert "}")
    (goto-char begin)
    (insert "{{c::")))

(defun org-ilm--card-uncloze ()
  "Remove cloze at point."
  (when-let ((cloze (org-ilm--card-cloze-match-around-point)))
    (delete-region (car (org-ilm-cloze-pos cloze))
                   (cdr (org-ilm-cloze-pos cloze)))
    (insert (string-trim (org-ilm-cloze-content cloze)))))

(defun org-ilm--card-uncloze-buffer (&optional begin end)
  "Remove clozes in buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (let (cloze)
      (while (setq cloze (org-ilm--card-cloze-match-forward end))
        (org-ilm--card-uncloze)))))

;;;;; Review interaction

;; TODO Extensible system where cloze types can be added based on thing at point
;; like in org-registry. Instead cond in function like now.

(defface org-ilm-cloze-face
  '((t (:foreground "black"
        :background "pink" 
        :weight bold
        :height 1.2)))
  "Face for clozes.")

(defun org-ilm--card-cloze-format-latex (latex)
  "Translate emacs face to latex code and apply to LATEX."
  (when (face-bold-p 'org-ilm-cloze-face)
    (setq latex (format "\\textbf{%s}" latex)))
  (when-let ((bg (face-background 'org-ilm-cloze-face)))
    (setq latex (format "\\fcolorbox{%s}{%s}{%s}" bg bg latex)))
  (when-let ((height (face-attribute 'org-ilm-cloze-face :height))
             (size (cond
                    ((< height 0.8)  "\\tiny")
                    ((< height 0.9)  "\\scriptsize")
                    ((< height 1.0)  "\\footnotesize")
                    ((< height 1.2)  "\\small")
                    ((< height 1.5)  "\\normalsize")
                    ((< height 1.8)  "\\large")
                    ((< height 2.0)  "\\Large")
                    ((< height 2.5)  "\\LARGE")
                    ((< height 3.0)  "\\huge"))))
    (setq latex (format "{%s %s}" size latex)))
  latex)

(defun org-ilm--card-cloze-build-latex (begin end latex &optional hint reveal-p)
  "Place the latex fragment modified by the cloze within it."
  (with-temp-buffer
    (insert latex)
    (goto-char (point-min))
    (while-let ((cloze (org-ilm--card-cloze-match-forward)))
      (delete-region (car (org-ilm-cloze-pos cloze))
                     (cdr (org-ilm-cloze-pos cloze)))
      (insert (org-ilm--card-cloze-format-latex
               (if reveal-p
                   (concat "$" (org-ilm-cloze-content cloze) "$")
                 (concat "[\\dots]" (when hint (concat "(" hint ")")))))))
    (setq latex (buffer-string)))
  
  (org-latex-preview-clear-overlays begin end)
  (org-latex-preview-place
   org-latex-preview-process-default
   (list (list begin end latex))))

(defun org-ilm--card-hide-clozes (&optional begin end)
  "Hide the clozes in the buffer by applying overlays."
  (org-ilm--card-remove-overlays)
  (save-excursion
    (goto-char (or begin (point-min)))
    (while-let ((cloze (org-ilm--card-cloze-match-forward end)))
      (let ((begin (car (org-ilm-cloze-pos cloze)))
            (end (cdr (org-ilm-cloze-pos cloze)))
            (content (org-ilm-cloze-content cloze))
            (hint (org-ilm-cloze-hint cloze)))
        (goto-char begin)
        (let* ((element (org-element-context))
               (ov (make-overlay begin end nil)))
          (overlay-put ov 'org-ilm-cloze cloze)
          (overlay-put ov 'org-ilm-cloze-state 'hidden)
          (overlay-put ov 'evaporate t)
          ;; (overlay-put ov 'modification-hooks '(org-ilm--ov-delete))
          ;; (overlay-put ov 'insert-in-front-hooks '(org-ilm--ov-delete))
          ;; (overlay-put ov 'insert-behind-hooks '(org-ilm--ov-delete))

          (cond
           ;; Latex
           ;; TODO Add modification and insert hooks to entire latex fragment
           ;; that will rebuild the fragment as the cloze is being modified.
           ((member (org-element-type element) '(latex-fragment latex-environment))
            (let ((latex-begin (org-element-begin element))
                  (latex-end (org-element-end element))
                  (latex-text (org-element-property :value element)))

              (org-ilm--card-cloze-build-latex latex-begin latex-end latex-text hint)

              (mochar-utils--add-hook-once
               'org-ilm-review-reveal-hook
               (lambda ()
                 (org-ilm--card-cloze-build-latex
                  latex-begin latex-end latex-text hint t))
               nil 'local)

              ;; We are already building all clozes in the latex fragment, so
              ;; skip entire fragment.
              (goto-char latex-end)))
           (t 
            (overlay-put ov 'face 'org-ilm-cloze-face)
            (overlay-put ov 'display (concat "[...]" (when hint (concat "(" hint ")"))))
            (mochar-utils--add-hook-once
             'org-ilm-review-reveal-hook
             (lambda ()
               (overlay-put ov 'display nil))
             nil 'local)
            (goto-char end))))
        ))))

(defun org-ilm--card-reveal-clozes ()
  )

(defun org-ilm--card-remove-overlays (&optional begin end)
  (dolist (val '(hidden revealed))
    (remove-overlays (or begin (point-min)) (or end (point-max))
                     'org-ilm-cloze-state val))
  (org-latex-preview-clear-overlays)
  (call-interactively #'org-latex-preview))



;;;; Capture

;; Logic related to creating child elements, i.e. extracts and cards.

(defcustom org-ilm-capture-show-menu t
  "When creating an extract or card, always show a menu to configure it
 first, rather than with a prefix argument."
  :type 'boolean
  :group 'org-ilm)

(defvar org-ilm-extract-hook nil
  "Hook run after extract was made.
Arguments are: extract id, collection")

(defvar org-ilm-card-hook nil
  "Hook run after card was made.
Arguments are: extract id, collection")

;;;;; Capture

(cl-defstruct org-ilm-capture
  "Data for a capture (extract or card)."
  type target title id ext content props file method
  priority scheduled template collection state bibtex
  on-success on-abort capture-kwargs)

(defun org-ilm-capture-ensure (&rest data)
  "Parse plist DATA as org-ilm-capture object, or return if it already is one.
Alternatively pass just an org-ilm-capture object, which will be returned.

TYPE should be one of 'extract 'card, or 'source.

TARGET should be the org-capture target. If it is a string, it is
interpreted as the org ID of a heading, which will be the
target. Finally, if it is a symbol, it is interpreted as a collection
name, and the user will be prompted to select a file from the collection
to be used as the target.

DATA is a plist that contains info about the capture entry.

The callback ON-SUCCESS is called when capture is saved.

The callback ON-ABORT is called when capture is cancelled."  
  (if (and (= 1 (length data)) (org-ilm-capture-p (car data)))
      (car data)
    
    (cl-destructuring-bind
        (&key target type title id ext content props file method
              priority scheduled template state on-success on-abort
              bibtex collection capture-kwargs) data
      
      (cl-assert (member type '(extract card source)))
      (unless id (setq id (org-id-new)))
      (unless state
        (setq state (car (if (eq type 'card)
                             org-ilm-card-states
                           org-ilm-material-states))))

      ;; See `org-attach-method'
      (unless method (setq method 'cp))
      (cl-assert (member method '(mv cp ln lns)))

      ;; Set the target.
      (setq target
            (cond
             ;; Target is parent org id 
             ((stringp target)
              ;; If no priority given, set same as parent.
              (unless priority
                (when-let ((p (org-ilm-pqueue-priority target :nil-if-absent t)))
                  (setq priority (org-ilm-pqueue-parse-new-position (car p)))))

              ;; Determine collection
              (when-let* ((file (org-id-find-id-file target))
                          (col (org-ilm--collection-file file)))
                (setq collection col))
              
              ;; Originally used the built-in 'id target, however for some reason
              ;; it sometimes finds the wrong location which messes everything
              ;; up. I noticed this behavior also with org-id-find and such. I
              ;; should probably find the root cause, but org-node finds location
              ;; accurately so i rely on it instead to find the location of a
              ;; heading by id.  `(id ,target)
              (let ((target-id (copy-sequence target)))
                (list 'function
                      (lambda ()
                        (org-node-goto-id target-id)
                        (org-back-to-heading)))))
             ((symbolp target)
              (setq collection target)
              (list 'file (org-ilm--select-collection-file target)))
             ;; TODO Determine collection based on arbitrary target?
             ;; Maybe its ok to leave this
             (t target)))
      ;; Priority. We always need a priority value.
      (setq priority
            (if priority
                (org-ilm-pqueue-parse-new-position priority)
              (org-ilm-pqueue-select-position)))

      ;; Schedule date. Always needed.
      (setq scheduled
            (cond
             ((ts-p scheduled) scheduled)
             ((stringp scheduled) (ts-parse scheduled))
             (scheduled (error "scheduled unrecognized"))
             (t (let ((card-interval (org-ilm-card-first-interval)))
                  (cond
                   ((or (not (eq type 'card)) (eq card-interval 'priority))
                    (org-ilm--initial-schedule-from-priority (cdr priority)))
                   ((numberp card-interval)
                    (ts-adjust 'minute card-interval (ts-now)))
                   (t (error "card-interval unrecognized")))))))

      ;; Generate title from content if no title provided
      (when (and (not title) content)
        (setq title (org-ilm--generate-text-snippet content)))

      ;; Set extension from file when set to t
      (when (eq ext t)
        (if file
            (setq ext (file-name-extension file))
          (error "Cannot infer extension when no file provided (:ext=t)")))

      ;; Either file, content, or neither is given.
      (cl-assert (not (and file content)))
      (when (and (not file) content)
        (setq file (expand-file-name
                    (format "%s.%s" id (or ext "org"))
                    temporary-file-directory)
              method 'cp))

      ;; Make sure there is a collection
      (setq collection (or collection (org-ilm--active-collection)))

      (make-org-ilm-capture
       :target target :type type :title title :id id :ext ext
       :content content :props props :file file :method method
       :priority priority :scheduled scheduled :template template
       :state state :on-success on-success :on-abort on-abort
       :bibtex bibtex :collection collection :capture-kwargs capture-kwargs))))

(defun org-ilm--capture (&rest data)
  "Make an org capture to make a new source heading, extract, or card.

For type of arguments DATA, see `org-ilm-capture-ensure'"
  (let* ((capture (apply #'org-ilm-capture-ensure data))
         (id (org-ilm-capture-id capture))
         (type (org-ilm-capture-type capture))
         (collection (org-ilm-capture-collection capture))
         (scheduled (org-ilm-capture-scheduled capture))
         (priority (org-ilm-capture-priority capture))
         (template (org-ilm-capture-template capture))
         capture-kwargs
         attach-dir) ; Will be set in :hook, and passed to on-success

    (cl-assert (org-ilm-capture-p capture))
    
    ;; Save content in a  file if provided.
    ;; TODO set MUSTBENEW to t?
    (when-let ((content (org-ilm-capture-content capture)))
      (write-region content nil (org-ilm-capture-file capture)))

    ;; If no explicit template is set, build it from title and state
    (unless template
      (let ((title (org-ilm-capture-title capture))
            (state (org-ilm-capture-state capture)))
        (setq template
              (format "* %s%s %s"
                      state
                      (if title (concat " " title) "")
                      "%?"))))

    (setq capture-kwargs
          (list
           :hook
           (lambda ()
             ;; Set attach dir which will be passed to on-success
             ;; callback. Has to be done in the hook so that point is on
             ;; the headline, and respects file-local or .dir-locals
             ;; `org-attach-id-dir'.
             (setq attach-dir
                   ;; If extract/card need to use inherited attach dir. If
                   ;; new source, make new one from id.
                   (if-let ((d (org-attach-dir)))
                       (expand-file-name d)
                     (org-attach-dir-from-id id)))
             
             ;; Regardless of type, every headline will have an id.
             (org-entry-put nil "ID" id)
             ;; Also trigger org-mem to update cache
             ;; (save-buffer)
             ;; (org-mem-updater-ensure-id-node-at-point-known)

             ;; Add id to priority queue. An abort is detected in
             ;; `after-finalize' to remove the id.
             (org-ilm-pqueue-insert id priority)

             ;; Attachment extension if specified
             (when-let ((ext (org-ilm-capture-ext capture)))
               (org-entry-put nil "ILM_EXT" ext))

             ;; Additional properties
             (when-let ((props (org-ilm-capture-props capture)))
               (cl-loop for (p v) on props by #'cddr
                        do (org-entry-put
                            nil
                            (if (stringp p) p (substring (symbol-name p) 1))
                            (format "%s" v))))

             ;; Schedule in the org heading
             (org-schedule
              nil
              (if (eq type 'card)
                  (org-ilm--ts-format-utc-date-maybe-time scheduled)
                (org-ilm--ts-format-utc-date scheduled)))

             ;; Log to drawer
             (pcase type 
               ('card 
                (org-ilm--card-log-new priority scheduled))
               ((or 'source 'extract)
                (org-ilm--material-log-new priority scheduled)))

             ;; Store bibtex
             (when-let ((bibtex (org-ilm-capture-bibtex capture)))
               (write-region (mochar-utils--format-bibtex-entry bibtex) nil
                             (org-ilm--collection-bib collection) 'append))
             )
           :before-finalize
           (lambda ()
             ;; If this is a source header where the attachments will
             ;; live, we need to set the DIR property, otherwise for
             ;; some reason org-attach on children doesn't detect that
             ;; there is a parent attachment header, even with a non-nil
             ;; `org-attach-use-inheritance'.
             (when (eq type 'source)
               (org-entry-put
                nil "DIR"
                (file-relative-name
                 (org-attach-dir-get-create)
                 (file-name-directory (org-ilm--collection-path collection)))))
             
             (when-let ((file (org-ilm-capture-file capture))
                        (method (org-ilm-capture-method capture)))
               ;; Attach the file. Turn of auto tagging if not import source.
               (let ((org-attach-auto-tag (if (eq type 'source)
                                              org-attach-auto-tag
                                            nil)))
                 (org-attach-attach file nil method)

                 ;; Make sure the file name is the org-id
                 (rename-file
                  (expand-file-name (file-name-nondirectory file) attach-dir)
                  (expand-file-name (concat id "." (file-name-extension file))
                                    attach-dir)
                  'ok-if-already-exists))))
           :after-finalize
           (lambda ()
             ;; Deal with success and aborted capture. This can be detected in
             ;; after-finalize hook with the `org-note-abort' flag set to t in
             ;; `org-capture-kill'.
             (if org-note-abort
                 (progn
                   (org-ilm-pqueue-remove id)
                   (when-let ((on-abort (org-ilm-capture-on-abort capture)))
                     (funcall on-abort)))
               (when org-ilm-update-org-mem-after-capture
                 (mochar-utils--org-mem-update-cache-after-capture 'entry))
               (pcase type
                 ('extract
                  (run-hook-with-args 'org-ilm-extract-hook id collection))
                 ('card
                  (run-hook-with-args 'org-ilm-card-hook id collection)))
               (when-let ((on-success (org-ilm-capture-on-success capture)))
                 (funcall on-success id attach-dir
                          (org-ilm-capture-collection capture)))))
           :immediate-finish t))

    (cl-letf (((symbol-value 'org-capture-templates)
               (list
                (append
                 (list
                  "i" "Import" 'entry
                  (org-ilm-capture-target capture)
                  template)
                 (org-combine-plists
                  capture-kwargs
                  (org-ilm-capture-capture-kwargs capture))))))
      (org-capture nil "i"))))

(defun org-ilm--capture-capture (type &rest data)
  (let ((immediate-p (if org-ilm-capture-show-menu
                         current-prefix-arg
                       (not current-prefix-arg)))
        (capture (apply #'org-ilm-capture-ensure
                        (org-combine-plists
                         data (list :type type)))))
    (if immediate-p
        (org-ilm--capture capture)
      (org-ilm--capture-transient capture))))

(defun org-ilm--capture-extract (&rest data)
  "Make an extract with DATA, see `org-ilm-capture-ensure'."
  (apply #'org-ilm--capture-capture 'extract data))

(defun org-ilm--capture-cloze (&rest data)
  "Make a cloze with DATA, see `org-ilm-capture-ensure'."
  (apply #'org-ilm--capture-capture 'card data))

;;;;; Transient

(defun org-ilm--capture-transient-values ()
  (let* ((capture (transient-scope))
         (args (if transient-current-command
                   (transient-args transient-current-command)
                 (transient-get-value)))
         (title (transient-arg-value "--title=" args))
         (rank (transient-arg-value "--priority=" args))
         (scheduled (transient-arg-value "--scheduled=" args)))          

    (if scheduled
        (setq scheduled (ts-parse scheduled))
      (setq scheduled (org-ilm-capture-scheduled (transient-scope))))
    
    (if rank
        (setq rank (1- (string-to-number rank)))
      (setq rank (org-ilm-capture-priority (transient-scope))))

    (list title rank scheduled)))

(transient-define-prefix org-ilm--capture-transient (scope)
  :refresh-suffixes t
  
  [:description
   (lambda ()
     (pcase (org-ilm-capture-type (transient-scope))
       ('card "Cloze")
       ('material "Extract")
       ('source "Import")
       (_ "Capture")))

   ("t" "Title" "--title="
    :transient transient--do-call
    :always-read t
    :allow-empty nil
    :reader
    (lambda (&rest _)
      (let* ((cur-title (nth 0 (org-ilm--capture-transient-values)))
             (title (read-string "Title: " cur-title)))
        (if (string-empty-p title)
            cur-title
          title))))
   ("p" "Priority" "--priority="
    :transient transient--do-call
    :class transient-option
    :reader
    (lambda (&rest _)
      (let ((priority (org-ilm-pqueue-select-position)))
        (unless (transient-arg-value "--scheduled=" (transient-args transient-current-command))
          (mochar-utils--transiet-set-target-value
           "s" (org-ilm--ts-format-utc-date
                (org-ilm--initial-schedule-from-priority (cdr priority)))))
        (number-to-string (1+ (car priority))))))
   (:info
    (lambda ()
      (let ((rank (nth 1 (org-ilm--capture-transient-values))))
        (org-ilm--ost-format-position (org-ilm-pqueue) rank))))
   ("s" "Scheduled" "--scheduled="
    :transient transient--do-call
    :reader
    (lambda (&rest _)
      (org-read-date 'with-time nil nil "Schedule: ")))
   (:info
    (lambda ()
      (let ((scheduled (nth 2 (org-ilm--capture-transient-values))))
        (ts-format "%Y-%m-%d %H:%M" scheduled))))
   ]

  [
   ("RET" "Capture"
    (lambda ()
      (interactive)
      (org-ilm--capture-transient-capture)))
   ("M-RET" "Capture with buffer"
    (lambda ()
      (interactive)
      (org-ilm--capture-transient-capture 'with-buffer-p)))
   ]

  (interactive)
  (transient-setup 'org-ilm--capture-transient
                   nil nil :scope scope
                   :value (append
                           (when-let ((title (org-ilm-capture-title scope)))
                             (list (concat "--title=" title))))))

(defun org-ilm--capture-transient-capture (&optional with-buffer-p)
  (cl-destructuring-bind (title rank scheduled)
      (org-ilm--capture-transient-values)
    (let ((capture (transient-scope))
          (capture-kwargs (list :immediate-finish (not with-buffer-p))))
      (setf (org-ilm-capture-priority capture) rank
            (org-ilm-capture-scheduled capture) scheduled
            (org-ilm-capture-title capture) title
            (org-ilm-capture-capture-kwargs capture) capture-kwargs)
      (org-ilm--capture capture))))



;;;; Org attachment

(cl-defun org-ilm-org-extract (&key title dont-update-priority)
  "Extract text in region.

Will become an attachment Org file that is the child heading of current entry."
  (interactive)
  (unless (use-region-p) (user-error "No region active."))
  (let* ((file-buf (current-buffer))
         (file-path buffer-file-name)
         (file-name (file-name-base file-path))
         (attach-org-id (car (org-ilm--attachment-data)))
         (attach-org-id-marker (org-id-find attach-org-id t))
         (file-org-id file-name)
         ;; TODO This doesnt work great, use what i do in other places, which
         ;; is: i forgot. i think there is a utility function
         (file-org-id-marker (org-id-find file-org-id t)))
    (unless attach-org-id-marker
      (error "Current file is not an attachment, or failed to parse org ID."))
    (unless file-org-id-marker
      (error "Current file name not org ID, or failed to find it."))
    
    (let* ((region-begin (region-beginning))
           (region-end (region-end))
           ;; If region end is beginning of line, reposition to end of previous
           ;; line. This avoids formatting problems.
           (region-end (if (save-excursion (goto-char region-end) (bolp))
                           (max (- region-end 1) 0)
                         region-end))
           (region-text (org-ilm--buffer-text-prepare region-begin region-end))
           (extract-org-id (org-id-new))
           (entry (org-mem-entry-by-id attach-org-id))
           props)

      ;; If the element has media, then extract time.
      (when (org-mem-entry-property-with-inheritance "ILM_MEDIA" entry)
        (when-let ((timer (org-ilm--media-extract-range region-text)))
          (setf (plist-get props :ILM_MEDIA+) timer)))
          
      (org-ilm--capture-extract
       :target file-org-id
       :id extract-org-id
       :content region-text
       :title title
       :props props
       :on-success
       (lambda (&rest _)

         ;; Wrap region with targets.
         (with-current-buffer file-buf
           (save-excursion
             (let* ((target-template (format "<<extr:%s:%s>>" "%s" extract-org-id))
                    (begin-is-bolp (save-excursion
                                     (goto-char region-begin)
                                     (bolp)))
                    (target-begin (concat
                                   (format target-template "beg")
                                   ;; Region starts at beginning of line: insert
                                   ;; on new line above
                                   (when begin-is-bolp "\n")))
                    (target-end (format target-template "end")))
               ;; Insert target before region
               (goto-char region-begin)
               (insert target-begin)

               ;; Insert target after region, adjusting for start target length
               (goto-char (+ region-end (length target-begin)))
               (insert target-end)

               (save-buffer)
               (org-ilm-recreate-overlays region-begin (point))))))
       ))))

(defun org-ilm-org-cloze ()
  "Create a cloze card.
A cloze is made automatically of the element at point or active region."
  (interactive)

  (let* ((attachment (org-ilm--attachment-data))
         (file-org-id (car attachment))
         (file-buf (current-buffer))
         (card-org-id (org-id-new))
         (cloze-bounds (org-ilm--card-cloze-bounds))
         (buffer-text (buffer-string))
         (point (point))
         snippet)

    (cl-assert cloze-bounds)

    ;; Prepare card buffer text
    (with-temp-buffer
      (insert buffer-text)
      (goto-char point)
      (let ((marker (point-marker))
            (content (buffer-substring (car cloze-bounds) (cdr cloze-bounds))))
        (delete-region (car cloze-bounds) (cdr cloze-bounds))
        (org-ilm--buffer-text-process :source-buffer file-buf)
        (setq snippet (org-ilm--generate-text-snippet (buffer-string)))
        (goto-char marker)
        (org-ilm--card-cloze-region (prog1 (point) (insert content)) (point))
        (setq buffer-text (buffer-string))))
        
    (org-ilm--capture-cloze
     :type 'card
     :target file-org-id
     :id card-org-id
     :content buffer-text
     :title snippet
     :on-success
     (lambda (&rest _)
       (with-current-buffer file-buf
         (save-excursion
           (goto-char (cdr cloze-bounds))
           (insert "<<card:end:" card-org-id ">>")
           (goto-char (car cloze-bounds))
           (insert "<<card:beg:" card-org-id ">>"))
         (save-buffer)
         (org-ilm-recreate-overlays))))))

(defun org-ilm-org-split (&optional level)
  "Split org document by heading level."
  (interactive)
  (cl-assert (and (eq (car (org-ilm--where-am-i)) 'attachment)
                  (eq major-mode 'org-mode)))

  (unless level
    (setq level (read-number "Split by level: "  (max (org-outline-level) 2))))

  (let ((org-ilm-capture-show-menu nil))
    (save-excursion
      (goto-char (point-min))
      (let ((re (format "^\\*\\{%d\\} " level))
            title)
        (while (re-search-forward re nil t)
          (setq title (org-get-heading t t t t))
          (beginning-of-line)
          (insert "\n")
          (set-mark (point))
          (org-end-of-subtree t)
          (insert "\n")
          (org-ilm-org-extract :title title :dont-update-priority t))))))

;;;; Media attachment

;; Media attachments are actually just org attachmnets with a ILM_MEDIA property.

(defun org-ilm--source-recover (source element-id &optional registry-id)
  "Return path to media attachment or its url."
  (cond-let*
    ((org-url-p source) source)
    ((file-exists-p source) source)
    ([attach-dir (seq-some
                  (lambda (id)
                    (when id 
                      (org-ilm--org-with-point-at id
                        (when-let ((attach-dir (org-attach-dir)))
                          (when (member source (org-attach-file-list attach-dir))
                            (expand-file-name attach-dir))))))
                  (list element-id registry-id))]
     (expand-file-name source attach-dir))))

(defun org-ilm--media-prop-parse (media)
  "Parse the ILM_MEDIA property MEDIA and return (source start end)."
  (org-media-note--split-link media))

(defun org-ilm--media-compile (entry)
  "Return (source start end)"
  (let ((media (org-mem-entry-property-with-inheritance "ILM_MEDIA" entry))
        (media2 (org-mem-entry-property-with-inheritance "ILM_MEDIA+" entry)))
    (when media
      (cl-destructuring-bind (source start end) (org-ilm--media-prop-parse media)
        (cl-destructuring-bind (start2 end2) (or (ignore-errors (split-string media2 "-"))
                                                 '(nil nil))
          (setq start (or start2 start)
                end (if start2 end2 end))
          (list source start end))))))

(defun org-ilm--media-open (&optional entry)
  "Open the media of ENTRY (or at point) with `org-media-note'."
  (when-let* ((entry (or entry (org-node-at-point)))
              (media (org-ilm--media-compile entry)))
    (cl-destructuring-bind (source start end) media
      (setq source (org-ilm--source-recover
                    source (org-mem-entry-id entry)
                    (org-mem-entry-property-with-inheritance "REGISTRY" entry)))
      (org-media-note--follow-link source start end))))

;; TODO too fragile. should at the very least check in org-media-note org links
(defun org-ilm--media-extract-range (text)
  "Extract the first and last timers in TEXT as start and end points."
  (let (start end)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (re-search-forward org-timer-re nil t)
        (setq start (match-string-no-properties 0))
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (when (re-search-backward org-timer-re nil t)
        (setq end (match-string-no-properties 0))))
    (when start (if end (concat start "-" end) start))))

;;;;; Custom link

(defconst org-ilm-media-link "ilmmedia")

(defun org-ilm--media-link-folow (link)
  (if-let* ((element-id (car (org-ilm--attachment-data)))
            (entry (org-mem-entry-by-id element-id))
            (source (car (org-ilm--media-compile entry))))
      (pcase-let* ((`(,start ,end) (string-split link "-")))
        ;; (string-match org-timer-re start)
        (org-media-note--follow-link source start end))
    (user-error "Cannot find element or its ILM_MEDIA property")))

(org-link-set-parameters
 org-ilm-media-link
 :follow #'org-ilm--media-link-folow
 )

;;;;; Org-media-note advice

;; Advice org-media-note to use our link type when inserting

(defun org-ilm--advice--org-media-note--link (&rest r)
  (pcase-let* ((`(,file-path ,filename ,timestamp) (org-media-note--current-media-info))
               (link-type org-ilm-media-link))
    (if (org-media-note--ab-loop-p)
        ;; ab-loop link
        (let ((time-a (org-media-note--seconds-to-timestamp (mpv-get-property "ab-loop-a")))
              (time-b (org-media-note--seconds-to-timestamp (mpv-get-property "ab-loop-b"))))
          (format "[[%s:%s-%s][%s]]"
                  link-type
                  time-a
                  time-b
                  (org-media-note--link-formatter org-media-note-ab-loop-link-format
                                                  `(("filename" . ,filename)
                                                    ("ab-loop-a" . ,time-a)
                                                    ("ab-loop-b" . ,time-b)
                                                    ("file-path" . ,file-path)))))
      ;; timestamp link
      (format "[[%s:%s][%s]]"
              link-type
              timestamp
              (org-media-note--link-formatter org-media-note-timestamp-link-format
                                              `(("filename" . ,filename)
                                                ("timestamp" . ,timestamp)
                                                ("file-path" . ,file-path)))))))

(defun org-ilm--ilm-hook-media ()
  (if org-ilm-global-minor-mode
      (advice-add #'org-media-note--link
                  :override #'org-ilm--advice--org-media-note--link)
    (advice-remove #'org-media-note--link
                   #'org-ilm--advice--org-media-note--link)))

(add-hook 'org-ilm-global-minor-mode-hook #'org-ilm--ilm-hook-media)

;;;; PDF attachment

;; Nice functions/macros:
;; pdf-util-track-mouse-dragging
;; pdf-view-display-region
;; pdf-virtual-document-page: normalized virtual page + range -> actual

(defvar-keymap org-ilm-pdf-map
  :doc "Keymap for PDF attachments."
  "d" #'org-ilm-pdf-open-full-document
  "x" #'org-ilm-pdf-extract
  "z" #'org-ilm-pdf-cloze
  "c" #'org-ilm-pdf-convert
  "n" #'org-ilm-pdf-toggle-narrow
  "h" #'org-ilm-pdf-toggle-highlights)

(defcustom org-ilm-pdf-minimum-virtual-page-size '(1 . 1)
  "The minimum size of the area to create a virtual PDF extract."
  :type '(cons (symbol :tag "Width")
               (symbol :tag "Height"))
  :group 'org-ilm-pdf)

(defcustom org-ilm-pdf-convert-org-respect-area t
  "When virtual view is an area of a single page, convert just that area.

This is done by converting the area to an image first. Note that this will likely effect the quality of the conversion output, probably for the worse if it contains complex objects other than text."
  :type 'boolean
  :group 'org-ilm-pdf)

(defcustom org-ilm-pdf-highlight-alpha 0.35
  "The alpha value of the extract highlights on the PDF page."
  :type 'number
  :group 'org-ilm-pdf)

(defconst org-ilm--pdf-output-types
  '((virtual . "PDF (virtual)")
    (text . "Text")
    (image . "Image")
    (org . "Org (Marker)")))

(defconst org-ilm--pdf-extract-options
  '((page . "Page")
    (outline . "Outline")
    (region . "Region")
    (section . "Section")))

(defconst org-ilm--pdf-extract-option-to-types
  '((page . (virtual text image org))
    (outline . (virtual))
    (region . (virtual text image org))
    (section . (virtual text org))))


;;;;; Commands

(defun org-ilm-pdf-open-full-document ()
  "Open the full PDF document from an extracted virtual view, jump to current page.

TODO Cute if we can use numeric prefix to jump to that page number"
  (interactive)
  (save-excursion
    (org-ilm--pdf-with-point-on-collection-headline 'of-document
     (org-ilm--attachment-open)
     (pdf-view-goto-page page))))

(defun org-ilm-pdf-toggle-narrow ()
  "When in virtual view that specifies an area, toggle between the area and whole page."
  (interactive)
  (save-excursion
    (org-ilm--pdf-with-point-on-collection-headline nil
     (org-ilm--attachment-open :pdf-no-region region)
     (pdf-view-goto-page virtual-page))))

(defun org-ilm-pdf-toggle-highlights ()
  "Toggle render of extract and cloze highlights."
  (interactive)
  (setq org-ilm-pdf-highlight-captures-p (not org-ilm-pdf-highlight-captures-p))
  (pdf-view-redisplay))

;;;;; Utilities

(defun org-ilm--pdf-mode-p ()
  "Return non-nil if current major mode is `pdf-view-mode' or `pdf-virtual-view-mode'."
  (member major-mode '(pdf-view-mode pdf-virtual-view-mode)))

(defun org-ilm--pdf-mode-assert ()
  "Assert if not in pdf mode."
  (cl-assert (org-ilm--pdf-mode-p) nil "Not in a PDF document"))

(defun org-ilm--pdf-path ()
  "Return path to pdf file of currently open PDF buffer."
  (pcase major-mode
    ('pdf-view-mode
     buffer-file-name)
    ('pdf-virtual-view-mode
     (car (pdf-virtual-document-page 1)))
    (t (error "Not in PDF buffer"))))

(defun org-ilm--pdf-region-below-min-size-p (size-or-page &optional region min-size)
  "Check if normalized coordinates REGION in SIZE is less than SIZE-MAX.
When REGION is nil, use active region.
When MIN-SIZE is nil, compare to `org-ilm-pdf-minimum-virtual-page-size'."
  (when-let* ((size (if (integerp size-or-page) (pdf-info-pagesize size-or-page) size-or-page))
              (min-size (or min-size org-ilm-pdf-minimum-virtual-page-size))
              (region (or region (org-ilm--pdf-region-normalized)))
              (region-width (* (car size) (- (nth 2 region) (nth 0 region))))
              (region-height (* (cdr size) (- (nth 3 region) (nth 1 region)))))
    (org-ilm--debug "W: %s H: %s" region-width region-height)
    (or (< region-width (car min-size))
        (< region-height (cdr min-size)))))

(cl-defun org-ilm--pdf-path (&key directory-p)
  "Infer the path of the PDF file, regardless if in virtual or not.
When DIRECTORY-P, return directory of the file."
  (org-ilm--pdf-mode-assert)
  (let ((path (expand-file-name
               (if (eq major-mode 'pdf-virtual-view-mode)
                   (car (pdf-virtual-document-page 1))
                 buffer-file-name))))
    (if directory-p
        (file-name-directory path)
      path)))

(defun org-ilm--pdf-data ()
  "Return list with some data about the current PDF attachment."
  (cl-assert (and (eq (car (org-ilm--where-am-i)) 'attachment)
                  (org-ilm--pdf-mode-p)))
  (pcase-let* ((`(,_ ,org-id ,collection) (org-ilm--where-am-i))
               (pdf-path (org-ilm--pdf-path))
               (attach-dir (file-name-directory pdf-path))
               (headline (org-ilm--org-headline-element-from-id org-id)))
    (cl-assert headline nil "Collection element not found")
    (list org-id attach-dir pdf-path collection headline)))

(defmacro org-ilm--pdf-with-point-on-collection-headline (of-document &rest body)
  "Place point on the headline belonging to this PDF attachment.
When OF-DOCUMENT non-nil, jump to headline which has the orginal document as attachment."
  (declare (indent 1))
  `(when-let ((headline (nth 4 (org-ilm--pdf-data))))
     (org-with-point-at headline
       ,@body)))

(cl-defun org-ilm--pdf-image-export (filename &key region page dir)
  "Export current PDF buffer as an image.

REGION: Note that if in virtual view with region, already exports that region."
  (org-ilm--pdf-mode-assert)
  (let ((region (or region '(0 0 1 1)))
        (img-buffer (get-buffer-create "*Org Ilm PDF Image*"))
        img-type img-ext img-path)
    ;; TODO Supports combining multiple regions as one image, might be nice for
    ;; region export
    (pdf-view-extract-region-image
     (list region) page nil img-buffer 'no-display)
    (with-current-buffer img-buffer
      (setq img-type (image-type-from-buffer)
            img-ext (symbol-name img-type) ;; TODO ???
            img-path (expand-file-name
                      (concat filename "." img-ext)
                      (or dir temporary-file-directory)))
      (write-region (point-min) (point-max) img-path))
    (kill-buffer img-buffer)
    img-path))

(defun org-ilm-pdf-virtual-refresh ()
  "Refresh the virtual document."
  (interactive)
  (unless (eq major-mode 'pdf-virtual-view-mode)
    (error "This command can only be run in a pdf-view-mode buffer"))

  ;; The `pdf-view-have-image-mode-pixel-vscroll' constant checks
  ;; if the Emacs version supports pixel-based vertical scrolling,
  ;; which is necessary for `window-vscroll' to return the correct value.
  (let* ((pixel-scroll-p (bound-and-true-p pdf-view-have-image-mode-pixel-vscroll))
         (page (pdf-view-current-page))
         (size pdf-view-display-size)
         (hscroll (window-hscroll))
         (vscroll (window-vscroll nil pixel-scroll-p)))
    (pdf-virtual-edit-mode)
    (pdf-virtual-view-mode)
    
    ;; Set the display size (zoom level).
    ;; This needs to be done *before* going to the page so the
    ;; image is rendered at the correct size.
    (setq-local pdf-view-display-size size)

    ;; Go to the specified page. This will also redisplay the buffer.
    (pdf-view-goto-page page)

    ;; After redisplaying, set the scroll positions.
    ;; It's important to do this last, as the scrollable area
    ;; depends on the rendered image size.
    (set-window-hscroll (selected-window) hscroll)
    (set-window-vscroll (selected-window) vscroll pixel-scroll-p)))

(defun org-ilm--pdf-outline-get (&optional file-or-buffer full-p)
  "Parse outline of the PDF in FILE-OR-BUFFER or current buffer.
Same as `pdf-info-outline' but adds in each section info about the next
section at the same or shallower depth."
  ;; TODO Issues running this on virtual pdf buffer so set it to main pdf
  (setq file-or-buffer (or file-or-buffer (org-ilm--pdf-path)))
  (let ((outline (pdf-info-outline file-or-buffer))
        (num-pages (pdf-info-number-of-pages file-or-buffer)))
    (dotimes (index (length outline))
      (let* ((current-item (nth index outline))
             (title (alist-get 'title current-item))
             (page (alist-get 'page current-item))
             (top (alist-get 'top current-item))
             (depth (alist-get 'depth current-item))
             (begin (list page 0 top 1 1))
             next-item range)

        ;; Find the next item at the same or a shallower depth
        (dolist (j (number-sequence (1+ index) (- (length outline) 1)))
          (let ((potential-next (nth j outline)))
            (when (and (not next-item) (<= (alist-get 'depth potential-next) depth))
              (setf (alist-get 'next current-item) j)
              (setq next-item potential-next))))
        
        (let* ((next-page (or (alist-get 'page next-item) num-pages))
               (next-top (or (alist-get 'top next-item) 1))
               ;; Range depends on whether or not the next section as on same page
               (range (if (= next-page page)
                          (list page 0 top 1 (if next-item next-top 1))
                        (cons (list page 0 top 1 1) (list next-page 0 0 1 next-top)))))
          (setf (alist-get 'next-page current-item) next-page
                (alist-get 'next-top current-item) next-top
                (alist-get 'range current-item) range))

        (setf (nth index outline) current-item)))

    (if (and outline (not full-p)
             (eq major-mode 'pdf-virtual-view-mode)
             (not (= 1 (org-ilm--pdf-page-normalized 1))))
        (let ((first-page (org-ilm--pdf-page-normalized 1))
              (last-page (org-ilm--pdf-page-normalized (pdf-info-number-of-pages))))
          (seq-filter
           (lambda (section)
             (<= first-page (alist-get 'page section) last-page))
           outline))
      outline)))

(defun org-ilm--pdf-page-normalized (&optional page)
  "If in virtual mode, maps virtual PAGE to document page, else return as is.
When not specified, PAGE is current page."
  (let ((page (or page (pdf-view-current-page))))
    (cond
     ((eq major-mode 'pdf-view-mode) page)
     ((eq major-mode 'pdf-virtual-view-mode)
      (nth 1 (pdf-virtual-document-page page)))
     (t (error "Not in a PDF buffer")))))

(defun org-ilm--pdf-active-region ()
  "If region select, return first region, if text select, return bounding box."
  (interactive)
  (if pdf-view--have-rectangle-region
      (car (pdf-view-active-region))
    (pcase-let* ((boxes (pdf-info-getselection
                         (pdf-view-current-page)
                         (car (pdf-view-active-region))))
                 (`(,min-l ,min-t ,max-r ,max-b) (car boxes)))
      
      (unless boxes
        (error "Selection does not contain any text"))
      
      (dolist (box (seq-subseq boxes 1))
        (cl-destructuring-bind (L T R B) box
          (setq min-l (min min-l L))
          (setq min-t (min min-t T))
          (setq max-r (max max-r R))
          (setq max-b (max max-b B))))
      
      (list min-l min-t max-r max-b))))

(defun org-ilm--pdf-region-normalized (&optional region virtual-page)
  "If in virtual mode, maps virtual REGION to document region, else return as is.
When not specified, REGION is active region."
  (let ((region (or region (org-ilm--pdf-active-region))))
    (cond
     ((eq major-mode 'pdf-view-mode) region)
     ((eq major-mode 'pdf-virtual-view-mode)
      (let* ((virtual-page (or virtual-page (pdf-view-current-page)))
             (page-region (nth 2 (pdf-virtual-document-page virtual-page))))
        (if page-region
            (pcase-let* ((`(,LE ,TO ,RI, BO) page-region)
                         (`(,le ,to ,ri ,bo) region)
                         (w (- RI LE))
                         (h (- BO TO)))
              (list (+ LE (* le w))
                    (+ TO (* to h))
                    (+ LE (* ri w))
                    (+ TO (* bo h))))
          ;; When page-region is nil, that means we are in a virtual page that
          ;; is not zoomed into a particular region, just shows the whole
          ;; page. In the case the virtual region is the same as the normal
          ;; document region.
          region)))
     (t (error "Not in a PDF buffer")))))

(defun org-ilm--pdf-region-denormalized (region &optional virtual-page)
  "Given a region with coords relative to full page, transform them to be
relative to virtual page.

If VIRTUAL-PAGE is omitted, use the current virtual page."
  (org-ilm--pdf-mode-assert)
  (if (eq major-mode 'pdf-view-mode)
      region

    (let* ((virtual-page (or virtual-page (pdf-view-current-page)))
           (page-region (nth 2 (pdf-virtual-document-page virtual-page))))
      (if page-region
          (pcase-let* ((`(,LE ,TO ,RI ,BO) page-region)
                       (`(,le ,to ,ri ,bo) region)
                       (w (- RI LE))
                       (h (- BO TO)))
            ;; Inverse of normalization:
            ;; full = LE + rel_virtual * w
            ;; rel_virtual = (full - LE) / w
            (list (/ (- le LE) w)
                  (/ (- to TO) h)
                  (/ (- ri LE) w)
                  (/ (- bo TO) h)))
        ;; No subregion (whole page shown)
        region))))

;;;;; Capture highlights

(defvar-local org-ilm-pdf-highlight-captures-p t)
(defvar-local org-ilm--pdf-captures nil)

(defun org-ilm--pdf-gather-captures ()
  "Return list of elements (id type spec) for all child elements of PDF element."
  (let* ((data (org-ilm--pdf-data))
         (id (nth 0 data))
         (headline (nth 4 data))
         (include-self (eq (org-ilm-type headline) 'card))
         (captures (org-ilm--element-children id 'headline include-self)))
    (seq-keep
     (lambda (headline)
       (when-let* ((id (org-element-property :ID headline))
                   (type (org-ilm-type headline))
                   (range (org-element-property :ILM_PDF headline))
                   (range (org-ilm--pdf-range-from-string range))
                   (spec (org-ilm--pdf-parse-spec range)))
         (list id type spec)))
     captures)))

(defun org-ilm--pdf-captures (&optional reparse-p)
  (org-ilm--pdf-mode-assert)
  (or (and (not reparse-p) (bound-and-true-p org-ilm--pdf-captures))
      (setq-local org-ilm--pdf-captures
                  (org-ilm--pdf-gather-captures))))

(defun org-ilm--pdf-page-captures (&optional page)
  "Return all specs in or part of PAGE."
  (setq page (or page (org-ilm--pdf-page-normalized)))
  (seq-filter
   (lambda (capture)
     (cl-destructuring-bind (&key begin-page end-page &allow-other-keys)
         (nth 2 capture)
       (or (and (not end-page) (= begin-page page))
           (and end-page (<= begin-page page end-page)))))
   (org-ilm--pdf-captures)))

(defun org-ilm--pdf-create-capture-context-menu (id)
  "Create a context menu for capture highlight ID."
  (let ((menu (make-sparse-keymap)))


    (define-key
     menu [ilm-open-collection]
     `(menu-item "Ilm: View in collection"
                 ,(lambda ()
                    (interactive)
                    (org-id-goto id))
                 :help "View this extract in the collection."))
    
    (define-key
     menu [ilm-open-attachment]
     `(menu-item "Ilm: Open attachment"
                 ,(lambda ()
                    (interactive)
                    (let ((path (org-ilm--org-with-point-at id
                                  (org-ilm--attachment-find))))
                      (find-file path)))
                 :help "Open the attachment of this extract."))
    
    menu))

(defun org-ilm--pdf-create-highlight-hotspots (captures size)
  "Hotspots specify interactible regions in the image generated by pdf-view.

See :map in info node `(elisp) Image Descriptors'

See `pdf-annot-create-hotspots', `pdf-annot-hotspot-function', and
buffer-local `pdf-view--hotspot-functions'."
  (seq-keep
   (lambda (capture)
     (when-let* ((id (nth 0 capture))
                 (type (nth 1 capture))
                 (spec (nth 2 capture))
                 ((eq (plist-get spec :type) 'area-page))
                 (region (org-ilm--pdf-region-denormalized
                          (plist-get spec :begin-area)))
                 (id-symb (intern (format "ilm-pdf-capture-%s" id)))
                 ;; Scale relative coords to pixel coords
                 (e (pdf-util-scale region size 'round)))

       ;; Bind the mouse click event for this ID to our handler
       (local-set-key
        (vector id-symb 'mouse-1)
        (lambda ()
          (interactive)
          (when-let ((entry (org-mem-entry-by-id id)))
            (message "%s: %s" (org-mem-entry-todo-state entry)
                     (org-mem-entry-title entry)))))

       (local-set-key
        (vector id-symb 'down-mouse-3)
        (lambda ()
          (interactive "@")
          (popup-menu (org-ilm--pdf-create-capture-context-menu id))))
       
       ;; Hotspot data ‘(AREA ID PLIST)’
       ;; See :map in (info "(elisp) Image Descriptors")
       (list
        `(rect . ((,(nth 0 e) . ,(nth 1 e))
                  . (,(nth 2 e) . ,(nth 3 e))))
        id-symb
        (list 'pointer 'hand
              'help-echo (format "Capture: %s" id)))))
   captures))

(defun org-ilm--pdf-info-renderpage-captures (page width captures &optional file-or-buffer)
  "Render PDF PAGE as image with CAPTURES highlighted."
  ;; To create the image we call `pdf-info-renderpage' at the end. It accepts a
  ;; list of commands that it goes through sequantilally and applies. The
  ;; important commands are setting the current style (:alpha :background
  ;; :foreground) and creating a highlight (:highlight-region).
  (let* ((this-id (car (org-ilm--attachment-data)))
         cmds
         (capture-cmds
          (lambda (captures color &optional fg-color)
            (apply #'append
                   cmds
                   (list :background color :foreground (or fg-color color))
                   (seq-keep
                    (lambda (capture)
                      (when (eq (plist-get (nth 2 capture) :type) 'area-page)
                        (list :highlight-region
                              (org-ilm--pdf-region-denormalized
                               (plist-get (nth 2 capture) :begin-area)))))
                    captures))))
         (extract-color (color-darken-name
                         (face-background 'org-ilm-face-extract) 10.))
         (cloze-color (color-darken-name
                         (face-background 'org-ilm-face-card) 10.))
         this-capture extracts clozes)

    (dolist (capture captures)
      (cond
       ((equal (car capture) this-id)
        (setq this-capture capture))
       ((eq (nth 1 capture) 'material)
        (push capture extracts))
       ((eq (nth 1 capture) 'card)
        (push capture clozes))))

    (setq cmds
          (append
           (list :alpha org-ilm-pdf-highlight-alpha)
           (funcall capture-cmds extracts extract-color)
           (funcall capture-cmds clozes cloze-color)
           (pcase (nth 1 this-capture)
             ('material
              (funcall capture-cmds (list this-capture) extract-color))
             ('card
              (append
               (when (and (org-ilm-reviewing-p)
                          (not (plist-get org-ilm--review-data :card-revealed)))
                 (list :alpha 1))
               (funcall capture-cmds (list this-capture) cloze-color "#000000"))))))

    (apply #'pdf-info-renderpage
           page width file-or-buffer cmds)))

;; Advice `pdf-view-create-page' to add highlights for our captures. The
;; function is responsible for generating an image of the PDF. During this
;; process, region rectangle and hotspots are created to generate the image,
;; which we intercept to add our capture highlights. Alternative was calling
;; `pdf-view-display-region' to create the regions but it gets overwritten as
;; soon as the image needs to be generated again.
(defun org-ilm--advice--pdf-view-create-page (page &optional window)
  (let* ((captures (when (and (org-ilm--attachment-data)
                              org-ilm-pdf-highlight-captures-p)
                     (org-ilm--pdf-page-captures)))
         
         ;; Replicate logic from pdf-view-display-region
         (size (pdf-view-desired-image-size page window))
         (width (car size))
         (hotspots (append
                    (org-ilm--pdf-create-highlight-hotspots captures size)
                    ;; Standard hotspots for eg PDF annotations
                    (pdf-view-apply-hotspot-functions window page size)))
         
         ;; Call the low-level renderer with highlight data
         (data (org-ilm--pdf-info-renderpage-captures
                page width captures)))

    ;; This is just a small wrapper around emacs' `create-image'.
    ;; The properties that are passed can be found in (C-x C-e):
    ;;      (info "(elisp) Image Descriptors")
    (pdf-view-create-image data
      :width width
      :rotation (or pdf-view--current-rotation 0)
      :map hotspots
      :pointer 'arrow)))

(defun org-ilm--ilm-hook-pdf ()
  (if org-ilm-global-minor-mode
      (advice-add #'pdf-view-create-page
                  :override #'org-ilm--advice--pdf-view-create-page)
    (advice-remove #'pdf-view-create-page
                   #'org-ilm--advice--pdf-view-create-page)))

(add-hook 'org-ilm-global-minor-mode-hook #'org-ilm--ilm-hook-pdf)
  

;;;;; Annotation highlight

;; Deprecated?
;; Previously I saved captures as annotations within the PDF file to let
;; pdf-view render them for me. I don't like this because:
;;   1. It doesn't reflect the current state of the captures, i.e. when I remove
;;      a capture or add one in another way.
;;   2. I wanted to highlight extracting/splitting entire pages in a different
;;      way to highlighting region captures.
;;   3. Annotation highlights are baked into the image, so extract will include
;;      the highlight as well

(defun org-ilm--pdf-add-square-annotation (page region &optional label dont-save-buffer)
  "Add a square highlight annotation that is stored within the PDF file."
  ;; Can only make annotations from within main file.
  (with-current-buffer (find-file-noselect (org-ilm--pdf-path))
    ;; Need active region for it to be square instead of text highlight
    (setq pdf-view--have-rectangle-region t)
    (setq pdf-view-active-region region)
    (pdf-view--push-mark)
    (pdf-annot-add-annotation
     'highlight
     (list region)
     (list
      (cons 'opacity 1.0)
      (cons 'label label)
      (cons 'color (face-background 'org-ilm-face-extract))
      ;; (cons 'contents "")
      )
     page)
    (unless dont-save-buffer
      (save-buffer))))

(defun org-ilm--pdf-annot-create-context-menu-advice (func &rest args)
  "Advice the function to add an additional menu item to open the
attachment from its highlight."
  (let* ((menu (apply func args))
         (annotation (car args))
         (extract-id (alist-get 'label annotation)))
    (define-key
     menu [ilm-open-collection]
     `(menu-item "Ilm: View in collection"
                 ,(lambda ()
                    (interactive)
                    (org-id-goto extract-id))
                 :help "View this extract in the collection."))
    (define-key
     menu [ilm-open-attachment]
     `(menu-item "Ilm: Open attachment"
                 ,(lambda ()
                    (interactive)
                    (let ((path (org-ilm--org-with-point-at
                                 extract-id
                                 (org-ilm--attachment-find))))
                      (find-file path)))
                 :help "Open the attachment of this extract."))
    menu))

;;;;; Virtual

;; PDF range. One of:
;; - Page:
;;   number
;; - Page range:
;;   (start . end)
;; - Area
;;   (page . (left top right bottom))
;; - Range with area
;;   ((start . area) . (end . area))
;;   ((start . area) . end)
;;   (start . (end . area))

(defun org-ilm--pdf-range-to-string (begin end)
  "Convert PDF page area from lisp to a string format."
  (format "(%s %s)"
          (prin1-to-string begin)
          (prin1-to-string end)))

(defun org-ilm--pdf-range-from-string (string)
  "Read STRING that represent a PDF page area to lisp."
  (let* ((begin-data (read-from-string string))
         (begin (car begin-data)))
    (if (= (length string) (cdr begin-data))
        begin
      (cons begin (car (read-from-string string (cdr begin-data)))))))

(defun org-ilm--pdf-area-p (area)
  "Test if  AREA is (page . (left top right bottom)) or ."
  (and (listp area)
       (integerp (car area))
       (listp (cdr area))
       (or (= 5 (length area))
           (and (listp (cadr area))
                (= (length (cadr area)) 4)))))

(defun org-ilm--pdf-parse-spec (range &optional file)
  "Parse RANGE as spec for viewing in virtual PDF buffers."
  (let ((first (or (car-safe range) range))
        (second (cdr-safe range))
        type begin-page begin-area end-page end-area)
    (cond
     ;; Page
     ((and (integerp first) (null second))
      (setq type 'page
            begin-page first))
     ;; Page range
     ;; (begin . end)
     ((and (integerp first) (integerp second))
      (setq type 'range-pages
            begin-page first
            end-page second))
     ;; Area
     ;; (page . (left top right bottom))
     ((org-ilm--pdf-area-p range)
      (setq type 'area-page
            begin-page first
            begin-area second))
     ;; Range with area
     ;; (start . (end . area))
     ((and (integerp first) (org-ilm--pdf-area-p second))
      (setq type 'range-area-pages
            begin-page first
            end-page (car second)
            end-area (cdr second)))
     ;; Range with area
     ;; ((start . area) . end)
     ((and (integerp second) (org-ilm--pdf-area-p first))
      (setq type 'range-area-pages
            begin-page (car first)
            begin-area (cdr first)
            end-page second))
     ;; Range with area
     ;; ((start . area) . (end . area))
     ((and (org-ilm--pdf-area-p first) (org-ilm--pdf-area-p second))
      (setq type 'range-area-pages
            begin-page (car first)
            begin-area (cdr first)
            end-page (car second)
            end-area (cdr second)))
     (t (error "Spec range invalid")))

    (list :file file
          :type type
          :begin-page begin-page
          :begin-area begin-area
          :end-page end-page
          :end-area end-area)))

(defun org-ilm--pdf-open-virtual (&optional no-region)
  "Open virtual page given by ILM_PDF spec, return buffer if found."
  (when-let* ((el-type (ignore-errors (org-ilm-type)))
              (pdf-range (org-entry-get nil "ILM_PDF"))
              (attachment (org-ilm--attachment-find-ancestor "pdf"))
              (spec (org-ilm--pdf-parse-spec
                     (org-ilm--pdf-range-from-string pdf-range)
                     attachment))
              (buffer-name (concat (org-id-get) ".pdf")))

    ;; For cards, the ILM_PDF property specifies the cloze, so it is not
    ;; desirable to zoom into just the cloze region. Instead infer from parent
    ;; what region should be, and `org-ilm--pdf-render-page-highlights' will
    ;; show the current cloze region with a black border. Further during review,
    ;; the alpha will be set to 1 to hide the cloze.
    (when (eq el-type 'card)
      (save-excursion
        (org-up-heading 1)
        (if-let ((parent-pdf-range (org-entry-get nil "ILM_PDF")))
            (setq spec (org-ilm--pdf-parse-spec
                        (org-ilm--pdf-range-from-string
                         parent-pdf-range)
                        attachment))
          (setq spec (org-ilm--pdf-parse-spec
                      (plist-get spec :begin-page)
                      attachment)))))

    (org-ilm--pdf-open-specs (list spec) buffer-name no-region)))

(defun org-ilm--pdf-open-specs (specs buffer-name &optional no-region)
  "Open a virtual pdf buffer with the given SPECS.
With NO-REGION non-nil, view entire page instead of zooming to specified region.

TODO Handle two column layout"
  ;; We use pop-to-buffer instead of with-current-buffer -> pop-to-buffer
  ;; because the same buffer can be edited on demand when this function is
  ;; called. If done the other way, with-current-buffer will deem the original
  ;; buffer damaged.
  ;; TODO Maybe there is a way to make virtual-edit-mode invisible so it appears
  ;; less janky.
  (pop-to-buffer (get-buffer-create buffer-name))
  (pdf-virtual-edit-mode)
  (erase-buffer)
  (insert ";; %VPDF 1.0\n\n")  
  (insert "(")
  (dolist (spec specs)
    (cl-destructuring-bind (&key file type begin-page begin-area end-page end-area) spec
      (insert "(\"" file "\"")
      (pcase type
        ('page
         (insert (number-to-string begin-page)))
        ('range-pages
         (insert (format "(%s . %s)" begin-page end-page)))
        ('area-page
         (if no-region
             (insert (format " %s " begin-page))
           (insert (format "(%s . %s)" begin-page begin-area))))
        ('range-area-pages
         ;; Begin
         (if (and begin-area (not no-region))
             (insert (format " (%s . %s) " begin-page begin-area))
           (insert (format " %s " begin-page)))
         ;; Pages in between begin and endn
         (when (> (- end-page begin-page) 1)
           (insert (format " (%s . %s) " (1+ begin-page) (1- end-page))))
         ;; End
         (if (and end-area (not no-region))
             (insert (format " (%s . %s) " end-page end-area))
           (insert (format " %s " end-page))))))
    (insert ")"))
  (insert ")\n")
  
  ;; (pdf-virtual-edit-mode)
  (pdf-virtual-view-mode)
  (current-buffer))

(defun org-ilm--pdf-open-page (pdf-path page-number buffer-name)
  "Open a virtual pdf buffer with a single page."
  (with-current-buffer (generate-new-buffer buffer-name)
    (insert ";; %VPDF 1.0\n\n")
    (org-ilm--debug nil page-number)
    (insert "((\"" pdf-path "\" " (number-to-string page-number) " ))\n")
    (pdf-virtual-view-mode)
    (pop-to-buffer (current-buffer))
    (current-buffer)))


;;;;; Convert

;; TODO off-load pages handling to here so that this function can be more usefull
(cl-defun org-ilm--pdf-convert-attachment-to-org (pdf-path pages org-id &key on-success on-error)
  "Convert attachment PDF to Md using Marker, then to Org mode using Pandoc."
  (convtools--convert-to-org-with-marker-pandoc
   :process-id org-id
   :input-path pdf-path
   :new-name org-id 
   :pdf-pages pages
   :on-success on-success
   :on-error on-error))

(defun org-ilm-pdf-convert ()
  "Convert PDF to another format within the same attachment.

See also `org-ilm-pdf-convert-org-respect-area'."
  (interactive)
  (org-ilm--pdf-mode-assert)
  (unless (org-ilm--attachment-data)
    (user-error "Not in an attachment buffer."))
  (org-ilm--pdf-convert-transient))

(defun org-ilm--pdf-convert-transient-format ()
  (when-let* ((args (or (transient-get-value) (transient-args transient-current-command)))
              (format (car (remove "main" args))))
    (intern format)))

(transient-define-prefix org-ilm--pdf-convert-transient ()
  :refresh-suffixes t
  :value '("image")
  
  ["PDF Convert"
   ("f" "Format" "%s"
    :class transient-switches
    :transient transient--do-call
    :allow-empty nil
    :always-read t
    :choices ("image" "org" "text")
    :argument-format "%s"
    :argument-regexp "\\(\\(image\\|org\\|text\\)\\)")
   (:info
    (lambda ()
      (propertize
       (pcase (org-ilm--pdf-convert-transient-format)
         ('org "Convert to Org file with Marker")
         ('text "Extract text to Org file")
         ('image "Convert this page to an image")
         (_ ""))
       'face 'transient-key-noop))
    :if org-ilm--pdf-convert-transient-format)

   ("m" "Main" "main" :transient transient--do-call)
   (:info (propertize "Use as main attachment of this element" 'face 'transient-key-noop))
   ]
  
  [
   ("RET" "Convert"
    (lambda ()
      (interactive)
      (let* ((args (transient-args transient-current-command))
             (main-p (member "main" args))
             (format (org-ilm--pdf-convert-transient-format)))
        (org-ilm--pdf-convert format main-p)))
    :inapt-if-not org-ilm--pdf-convert-transient-format)
   ]
  )

(defun org-ilm--pdf-convert (format &optional as-main-p)
  "Convert current PDF buffer into FORMAT."
  (cl-assert (member format '(org text image)))
  (pcase-let* ((pdf-buffer (current-buffer))
               (`(,org-id ,collection) (org-ilm--attachment-data))
               (headline (org-ilm--org-headline-element-from-id org-id))
               (num-pages (pdf-info-number-of-pages))
               ;; This will only return if in virtual page
               (`(,pdf-path ,_ ,region) (ignore-errors (pdf-virtual-document-page 1)))
               (pdf-path (or pdf-path buffer-file-name))
               (attach-dir (file-name-directory pdf-path))
               (out-path-format (expand-file-name (concat org-id ".%s") attach-dir))
               (on-success
                (lambda ()
                  (when as-main-p
                    (org-with-point-at headline
                      (org-entry-put nil "ILM_EXT" "org"))))))

    (pcase format
      ('text
       (with-temp-buffer
         (dolist (page (number-sequence 1 num-pages))
           (insert (pdf-info-gettext page '(0 0 1 1) nil pdf-buffer)))
         (write-region (point-min) (point-max) (format out-path-format "org")))
       (funcall on-success))
      ('image
       ;; Note: Will convert current page only
       (org-ilm--pdf-image-export org-id :dir attach-dir)
       (funcall on-success))
      ('org
       ;; Decide on whether to convert just the virtual pdf region or the entire
       ;; page.
       (if (and region (= 1 num-pages) org-ilm-pdf-convert-org-respect-area)
           (org-ilm--image-convert-attachment-to-org
            (org-ilm--pdf-image-export org-id :dir attach-dir)
            org-id
            :on-success
            (lambda (proc buf id) (funcall on-success)))
         (when (or (<= num-pages 3)
                   (yes-or-no-p (format "Convert %s pages to Org using Marker?" num-pages)))
           (org-ilm--pdf-convert-attachment-to-org
            pdf-path
            (if (= 1 num-pages)
                (1- (org-ilm--pdf-page-normalized))
              (cons (1- (org-ilm--pdf-page-normalized 1))
                    (1- (org-ilm--pdf-page-normalized num-pages))))
            org-id
            :on-success
            (lambda (proc buf id) (funcall on-success)))))))))

;;;;; Extract

(defun org-ilm--pdf-extract-prompt-for-output-type (extract-option)
  "Prompt user to select output type given the extract option."
  (let ((options (seq-filter
                  (lambda (option)
                    (member (cdr option)
                            (assoc extract-option org-ilm--pdf-extract-option-to-types)))
                  (org-ilm--invert-alist org-ilm--pdf-output-types))))
    (if (= 1 (length options))
        (cdr (car options))
      (cdr (assoc (completing-read "Extract as: " options nil t) options)))))

(defun org-ilm-pdf-extract (extent &optional output-type)
  "Extract PDF pages, sections, region, and text."
  (interactive
   (list
    (if (pdf-view-active-region-p)
        'region
      (let* ((options (org-ilm--invert-alist org-ilm--pdf-extract-options)))
        (cdr (assoc (completing-read "Extract: " options nil t) options))))
    current-prefix-arg))
  (cl-assert (or (eq major-mode 'pdf-view-mode) (eq major-mode 'pdf-virtual-view-mode)))
  (cl-assert (memq extent '(page outline region section)))
  
  ;; (when (and output-type (called-interactively-p))
  (unless output-type
    (setq output-type (org-ilm--pdf-extract-prompt-for-output-type extent)))

  (unless output-type (setq output-type 'virtual))
  
  (pcase extent
    ('page (org-ilm-pdf-page-extract output-type))
    ('outline (org-ilm-pdf-outline-extract))
    ('region (org-ilm-pdf-region-extract output-type))
    ('section (org-ilm-pdf-section-extract output-type))))

(defun org-ilm-pdf-page-extract (output-type)
  "Turn PDF page into an extract."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'page)))
  (pcase-let* ((`(,org-id ,attach-dir ,pdf-path ,collection ,headline)
                (org-ilm--pdf-data))
               (current-page (pdf-view-current-page))
               (current-page-real (org-ilm--pdf-page-normalized))
               (extract-org-id (org-id-new)))

    (pcase output-type
      ('virtual
       (let ((excerpt (org-ilm--generate-text-snippet
                       (pdf-info-gettext current-page '(0 0 1 1)))))
         (org-ilm--capture-extract
          :target org-id
          :title (format "Page %s%s" current-page-real
                         (if excerpt (concat ": " excerpt) ""))
          :props (list :ILM_PDF current-page-real))))
      ('text
       (let* ((text (pdf-info-gettext current-page '(0 0 1 1) nil)))
         (org-ilm--capture-extract
          :target org-id :content text :ext "org")))
      ('image
       (let* ((img-path (org-ilm--pdf-image-export extract-org-id)))
         (org-ilm--capture-extract
          :target org-id :file img-path
          :title (format "Page %s" current-page-real)
          :id extract-org-id :method 'mv :ext t)))
      ('org
       (org-ilm--capture-extract
        :target org-id
        :title (format "Page %s" current-page-real)
        :id extract-org-id
        :ext "org"
        :on-success
        (lambda (&rest _)
          (org-ilm--pdf-convert-attachment-to-org
           pdf-path
           (1- current-page-real)
           extract-org-id
           :on-success
           (lambda (proc buf id) (message "Conversion finished.")))))))))

;; TODO When extracting text, use add-variable-watcher to watch for changes in
;; pdf-view-active-region as it has no hooks. it allow buffer local and set only
;; (not let).
(defun org-ilm-pdf-region-extract (output-type &optional card-p)
  "Turn selected PDF region into an extract."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'region)))
  (cl-assert (pdf-view-active-region-p) nil "No active region")
  (when (and card-p (not (eq output-type 'virtual)))
    (error "Can only create virtual PDF card extracts"))
  
  (pcase-let* ((`(,org-id ,attach-dir ,pdf-path ,collection ,headline)
                (org-ilm--pdf-data))
               (extract-org-id (org-id-new))
               (current-page (pdf-view-current-page))
               (current-page-real (org-ilm--pdf-page-normalized))
               (region (org-ilm--pdf-active-region))
               (region-normalized (org-ilm--pdf-region-normalized region))
               (region-text (car (pdf-view-active-region-text)))
               (pdf-buffer (current-buffer))
               (title
                (format "Page %s region%s"
                        current-page-real
                        (if region-text
                            (concat ": " (org-ilm--generate-text-snippet region-text))
                          "")))
               (capture-data
                (list
                 :title title
                 :target org-id
                 :id extract-org-id
                 ;; Add PDF region so that it can be rendered in pdf page even
                 ;; when extracting as eg image
                 :props (list :ILM_PDF (cons current-page-real region-normalized))))
               (capture-on-success))

    (when (org-ilm--pdf-region-below-min-size-p current-page region-normalized)
      (user-error "Region smaller than minimum size. Try extracting as text or image."))

    (pcase output-type
      ('virtual)
      ('text
       (setf (plist-get capture-data :content) region-text
             (plist-get capture-data :ext) "org"))
      ('image
       (let ((file (org-ilm--pdf-image-export extract-org-id :region region)))
         (setf (plist-get capture-data :file) file
               (plist-get capture-data :method) 'mv
               (plist-get capture-data :ext) t)))
      ('org
       (setf (plist-get capture-data :ext) "org")
       (setq capture-on-success
             (lambda ()
               (org-ilm--image-convert-attachment-to-org
                ;; TODO this shouldnt be the normalized region i think
                (org-ilm--pdf-image-export
                 extract-org-id :region region-normalized :dir attach-dir)
                extract-org-id))))
      (_ (error "Unrecognized output type")))

    ;; Temporary disable `org-link-parameters' which is an overkill way to
    ;; deal with the following problem. When org-pdftools is used,
    ;; `org-pdftools-setup-link' is called, which adds a custom link in
    ;; `org-link-parameters' that detects an org capture in a pdf buffer, and
    ;; then creates a text underline highlight automatically. This is
    ;; undesired, so this turns it off. The backtrace is:
    ;;   org-pdftools-store-link()
    ;;   org-link--try-link-store-functions(nil)
    ;;   org-store-link(nil)
    (let ((org-link-parameters nil))
      (apply (if card-p #'org-ilm--capture-cloze #'org-ilm--capture-extract)
             :on-success
             (lambda (&rest _)
               (when capture-on-success
                 (funcall capture-on-success))
               (org-ilm--pdf-captures 'reparse)
               (pdf-view-deactivate-region)
               (pdf-view-redisplay)
               (when (eq major-mode 'pdf-virtual-view-mode)
                 (org-ilm-pdf-virtual-refresh)))
             capture-data))))

(defun org-ilm-pdf-section-extract (output-type)
  "Extract current section of outline."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'section)))

  (pcase-let* ((`(,org-id ,attach-dir ,pdf-path ,collection ,headline)
                (org-ilm--pdf-data))
               (extract-org-id (org-id-new))
               (pdf-buffer (current-buffer))
               ;; TODO pass orginial file if in virtual
               (outline (org-ilm--pdf-outline-get))
               (num-pages (pdf-info-number-of-pages))
               (current-page (org-ilm--pdf-page-normalized))
               (section))

    (unless outline (error "No outline found"))

    ;; Find the outline section of the current page. If page overlaps
    ;; multiple sections, prompt user to select.
    (let ((within-indices
           (seq-filter (lambda (i)
                         (let ((page-start (alist-get 'page (nth i outline)))
                               (page-end (if (= i (1- (length outline)))
                                             num-pages
                                           (alist-get 'page (nth (1+ i) outline)))))
                           (and (>= current-page page-start)
                                (<= current-page page-end))))
                       (number-sequence 0 (1- (length outline))))))
      (cond
       ((= 1 (length within-indices))
        (setq section (nth (car within-indices) outline)))
       ((> (length within-indices) 1)
        (let* ((choices (mapcar (lambda (i)
                                  (cons (alist-get 'title (nth i outline)) i))
                                within-indices))
               (choice (cdr (assoc (completing-read "Section: " choices nil t) choices))))
          (setq section (nth choice outline))))
       (t (error "Current page not within (known) section"))))

    ;; TODO When no title use page range
    (let ((title (concat "Section: " (alist-get 'title section)))
          (range (alist-get 'range section)))
      (pcase output-type
        ('virtual
         (org-ilm--capture-extract
          :target org-id
          :title title
          :props (list :ILM_PDF range)))
        ('text
         ;; TODO This currently extracts the actual PDF data lol
         (with-temp-buffer
           (if (= (length range) 2)
               (insert (pdf-info-gettext (car range) (cdr range) nil pdf-buffer) "\n")
             (let* ((range-begin (car range))
                    (range-end (cdr range))
                    (page-begin (car range-begin))
                    (page-end (car range-end)))
               (dolist (page (number-sequence page-begin page-end))
                 (insert (pdf-info-gettext
                          page
                          (cond
                           ((= page page-begin) (cdr range-begin))
                           ((= page page-end) (cdr range-end))
                           (t '(0 0 1 1)))
                          'word pdf-buffer)
                         "\n"))))

           (org-ilm--capture-extract
            :target org-id :content (buffer-string) :ext "org"
            :title title)))
        ('org
         (org-ilm--capture-extract
          :target org-id :id extract-org-id :ext "org"
          :title title
          :on-success
          (lambda (&rest _)
            (org-ilm--pdf-convert-attachment-to-org
             pdf-path
             (cons (1- (alist-get 'page section))
                   (1- (alist-get 'next-page section)))
             extract-org-id
             :on-success
             (lambda (proc buf id) (message "Conversion finished."))))))))))

;;;;; Split

(defun org-ilm-pdf-split ()
  "Split PDF outline items into a extracts."
  (interactive)
  (pcase-let* ((`(,org-id ,attach-dir ,pdf-path ,collection ,headline)
                (org-ilm--pdf-data))
               (outline (org-ilm--pdf-outline-get)))
    (unless outline (error "No outline found"))
    (setq x outline)
    (let ((depth (cdr
                  (org-ilm--select-alist
                   (seq-map
                    (lambda (section)
                      (cons
                       (format "%s%s"
                               (make-string (1- (alist-get 'depth section)) ?\s)
                               (alist-get 'title section))
                       (alist-get 'depth section)))
                    outline)
                   "Level on which to split: "
                   nil 'ordered)))
          (org-ilm-capture-show-menu nil))
      (dolist (section outline)
        (when (= depth (alist-get 'depth section))
          (org-ilm--capture-extract
           :target org-id
           :title (concat "Section: " (alist-get 'title section))
           :props (list :ILM_PDF (alist-get 'range section))))))))

;;;;; Cloze

(defun org-ilm-pdf-cloze ()
  (interactive)
  (org-ilm-pdf-region-extract 'virtual 'card-p))


;;;; Image attachment

(cl-defun org-ilm--image-convert-attachment-to-org (image-path org-id &key on-success on-error)
  "Convert an attachment image to Org with Marker."
  (let ((headline (org-ilm--org-headline-element-from-id org-id)))
    (convtools--convert-to-org-with-marker-pandoc
     :process-id org-id
     :input-path image-path
     :new-name org-id
     :on-success on-success
     :on-error on-error)))

(defun org-ilm-image-convert-to-org ()
  "Convert image attachment to an Org file."
  (interactive)
  (unless (eq major-mode 'image-mode)
    (user-error "Not in image buffer."))
  (unless (org-ilm--attachment-data)
    (user-error "Not in an attachment buffer."))

  (org-ilm--image-convert-attachment-to-org
   buffer-file-name
   (car (org-ilm--attachment-data))
   :on-success
   (lambda () 
     (when (yes-or-no-p "Conversion finished. Use as main attachment?")
       (org-entry-put nil "ILM_EXT" "org")))))

;;;; Attachments

(defvar-local org-ilm--data nil
  "Buffer-local ilm data stored in element attachment buffers.
This is used to keep track of changes in priority and scheduling.")

;; TODO org-attach-delete-all

(defun org-ilm-infer-id-from-attachment-path (path)
  "Attempt parsing the org-id from the attachment path, return (id . location)."
  (when path
    (let* ((parts (split-string (file-name-directory path) "/" t))
           (potential-id (pcase (file-name-nondirectory (org-attach-dir-from-id "abc"))
                           ("abc" (car (last parts)))
                           ("ab/c" (mapconcat #'identity (last parts 2) ""))
                           (_ nil)))
           (location (org-id-find potential-id)))
      (when location (cons potential-id location)))))

(defun org-ilm--attachment-data ()
  "Returns (org-id collection file) if current buffer is collection attachment file."
  (when-let* ((file-title (file-name-base
                           ;; Allow for non-file buffer: pdf virtual view
                           (or buffer-file-name (buffer-name))))
              (entry (org-mem-entry-by-id file-title))
              (src-file (org-mem-entry-file entry))
              (src-file (expand-file-name src-file)) ;; sep line, else err
              (collection (org-ilm--collection-file src-file)))
    ;; Exclude registries
    (unless (seq-some (lambda (r) (string= (expand-file-name r) src-file)) org-registry-registries) 
      (list file-title collection src-file))))

(defun org-ilm--attachment-extension ()
  "Return the extension of the attachment at point, assuming in collection."
  (or (org-entry-get nil "ILM_EXT" 'inherit) "org"))

(cl-defun org-ilm--attachment-path (&key not-exists-ok allowed-exts)
  "Return path to the attachment of element at point."
  (when (and allowed-exts (not (listp allowed-exts)))
    (error "ALLOWED-EXTS must be list of extensions"))
  (when-let* ((org-id (org-id-get))
              (ext (org-ilm--attachment-extension))
              (_ (or (not allowed-exts) (member ext allowed-exts)))
              (path (org-attach-expand (format "%s.%s" org-id ext))))
    (when (or not-exists-ok (file-exists-p path))
      path)))
  
(defun org-ilm--attachment-find (&optional ext org-id)
  "Return attachment file of element at point."
  (when-let* ((attach-dir (org-attach-dir))
              (org-id (or org-id (org-id-get)))
              (ext (or ext (org-ilm--attachment-extension)))
              (path-sans-ext (expand-file-name (concat org-id ".") attach-dir)))
    ;; `ext' gets inherited from parent, but element might be an org file. So
    ;; first check if the explicit ext file exists, if not check for org file.
    (cond
     ((let ((path (concat path-sans-ext ext)))
        (when (file-exists-p path) path)))
     ((let ((path (concat path-sans-ext "org")))
        (when (file-exists-p path) path))))))

(defun org-ilm--attachment-find-ancestor (type &optional headline)
  ""
  (let ((crumbs (cdr (org-mem-entry-crumbs (org-node-at-point))))
        attachment)
    (while (and crumbs (not attachment))
      (setq attachment (org-ilm--attachment-find type (nth 4 (pop crumbs)))))
    attachment))

(cl-defun org-ilm--attachment-open (&key pdf-no-region no-error)
  "Open the attachment of collection element at point, returns its buffer."
  (cond-let*
    ;; Check if there is an attachment org-id.ext where org-id is current
    ;; headline's id and ext is org by default or ILM_EXT property
    ([path (org-ilm--attachment-find)]
     (if-let ((buf (get-file-buffer path)))
         (switch-to-buffer buf)
       ;; If media type, open the media file with org-media-note.
       (org-ilm--media-open)
       ;; Open org file
       (run-hook-with-args 'org-attach-open-hook path)
       (find-file path)))
    ;; Check if headline represents a virtual view of a parent PDF element.
    ([virtual-pdf-buffer (org-ilm--pdf-open-virtual pdf-no-region)]
     virtual-pdf-buffer)
    ;; Check if attachment is in the process of being generated with a conversion
    ;; tool.
    ([org-id (org-id-get)]
     [conversion (convtools--conversion-by-id org-id)]
     (let ((message (pcase (plist-get conversion :state)
                      ;; TODO for success and error, provide option to
                      ;; delete headline and extract highlight in parent
                      ;; attachment.
                      ('success "Attachment finished conversion but not found.")
                      ('error "Attachment conversion failed.")
                      (_ "Attachment still being converted."))))
       (when (yes-or-no-p (concat message " View conversion buffer?"))
         (pop-to-buffer (plist-get conversion :buffer)))))
    ;; Check if there is a web link in the ROAM_REFS property and open website in
    ;; eww.
    ([web-refs (mochar-utils--org-mem-website-refs)]
     [web-ref (if (= 1 (length web-refs))
                  (car web-refs)
                (completing-read "Open: " web-refs nil t))]
     ;; We use window excursion so that we can return the eww buffer
     (save-window-excursion
       (eww-browse-url web-ref))
     (switch-to-buffer "*eww*"))
    (t (unless no-error (user-error "Attachment not found")))))
  
(defun org-ilm--attachment-open-by-id (id)
  (org-ilm--org-with-point-at id
    (org-ilm--attachment-open)))

(defun org-ilm--attachment-prepare-buffer ()
  "Prepare ilm attachment buffers."
  (when-let ((data (org-ilm--attachment-data)))
    (pcase-let* ((`(,id ,collection) data)
                 (element (org-ilm-element-from-id id))
                 (entry (org-mem-entry-by-id id))
                 (registry-entry (org-mem-entry-by-id
                                  (org-ilm-element-registry element))))

      ;; Prepare the buffer local data object which contains info about the
      ;; attachment as well as data used to update the priority.
      (setq-local
       org-ilm--data
       (list :id id
             :collection collection
             ;; TODO porbably remove and just use `org-ilm-element-from-id' to
             ;; get most recent
             :element element
             ;; :beta (org-ilm--priority-to-beta
             ;;        (org-ilm-element-prelative element))
             ;; Manual accumalating change in the priority
             :a 0
             :b 0
             ;; Data that is compiled to form change in priority
             :start (current-time)
             :extracts 0
             :cards 0
             :characters 0
             ))

      (pcase major-mode
        ('org-mode
         (org-ilm-recreate-overlays)
         (setf (plist-get org-ilm--data :size)
               (save-restriction
                 (widen)
                 (- (point-max) (point-min)))))
        )
      )))

(defun org-ilm--attachment-ensure-data-object ()
  "Ensure `org-ilm--data' is initialized properly.

Sometimes org-mode fails to load, which will lead to
`org-ilm--attachment-prepare-buffer' not initializing correctly. To deal
with that we use this function to make sure the object exists, and if
not, create it by calling the function again. If the object is still
missing, something else is wrong, so throw an error."
  (unless (bound-and-true-p org-ilm--data)
    (org-ilm--attachment-prepare-buffer)
    (unless (bound-and-true-p org-ilm--data)
      (error "Could not create attachment data `org-ilm--data'"))))

(defun org-ilm--attachment-priority-compile ()
  (org-ilm--attachment-ensure-data-object)
  (cl-destructuring-bind
      (&key id beta start a b cards extracts &allow-other-keys) org-ilm--data
    (let ((actions (+ cards extracts))
          (duration (float-time (time-subtract (current-time) start))))
      ;; Rewards extracts and cards
      (cl-incf b (org-ilm--priority-b-from-actions actions))
      
      ;; Only add duration penalty when reviewing
      ;; TODO Commented out because duration is not determinsitic. Any change to
      ;; priority needs to happen immediately, not queued up then applied.
      ;; (when (org-ilm-reviewing-id-p id)
      ;;   (cl-incf a (org-ilm--priority-a-from-duration duration)))
      
      (cons a b))))

(defun org-ilm--attachment-priority-update (action)
  (org-ilm--attachment-ensure-data-object)
  (pcase action
    ('extract
     (cl-incf (plist-get org-ilm--data :extracts)))
    ('card
     (cl-incf (plist-get org-ilm--data :cards)))
    (_ (error "Wrong value for ACTION")))
  (let ((update-params (org-ilm--attachment-priority-compile)))
    (org-ilm--org-with-point-at (plist-get org-ilm--data :id)
      (org-ilm--priority-update
       :beta (plist-get org-ilm--data :beta)
       :a (car update-params)
       :b (cdr update-params)))))

;;;;; Transient

(defun org-ilm-attachment-actions ()
  "Open menu with actions to be applied on current element attachment."
  (interactive)
  (if (eq (car (org-ilm--where-am-i)) 'attachment)
      (org-ilm--attachment-transient)
    (user-error "Not in an element attachment!")))

(transient-define-prefix org-ilm--attachment-transient ()
  ["Attachment"
   (:info*
    (lambda ()
      (propertize
       (org-ilm-element-title (org-ilm-element-from-context))
       'face 'italic)))
   ]
  
  [
   ("x" "Extract" org-ilm-extract
    :inapt-if
    (lambda ()
      (pcase major-mode
        ('org-mode (not (region-active-p)))
        ((or 'pdf-view-mode 'pdf-virtual-view-mode))
        (_ t))))
   ("c" "Cloze" org-ilm-cloze
    :inapt-if-not-mode org-mode)
   ("s" "Split" org-ilm-split
    :inapt-if-not-mode (org-mode pdf-view-mode pdf-virtual-view-mode))
   ]
  )


;;;; Transclusion

(defcustom org-ilm-transclusion-attachment-exts (append '("org") image-file-name-extensions)
  "List of extensions that are allowed to be transcluded from within the collection element.

See `org-ilm-attachment-transclude'."
  :type '(repeat string)
  :group 'org-ilm)

(defun org-ilm--transclusion-active-p ()
  (save-excursion
    (save-restriction
      (org-ilm--org-narrow-to-header)
      (text-property-search-forward 'org-transclusion-type))))

(defun org-ilm--transclusion-goto (file-path &optional action)
  ""
  (let* ((transclusion-text (format "#+transclude: [[file:%s]]" file-path))
         (current-level (org-current-level))
         (create (eq action 'create))
         (transclusion-pattern ; May start with whitespace
          (concat "^[ \t]*" (regexp-quote transclusion-text))))
    (save-restriction
      (org-ilm--org-narrow-to-header)
      ;; Need to remove active transclusion, otherwise pattern not found.
      (org-transclusion-remove-all)
      (goto-char (point-min))
      (if (re-search-forward transclusion-pattern nil t)
          (pcase action
            ('delete (if (org-next-line-empty-p)
                         (progn (delete-line) (delete-line))
                       (delete-line)))
            ('create (progn
                       (delete-line)
                       (split-line))))
        (when create
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))))
      (when create
        (insert (format "%s :level %s" transclusion-text (+ 1 current-level)))
        ;; Newline necessary, otherwise next line will be on same line
        (save-excursion (insert "\n"))))))

(defun org-ilm--transcludable-attachment-path ()
  (org-ilm--attachment-path :allowed-exts org-ilm-transclusion-attachment-exts))

(defun org-ilm-attachment-transclusion-delete ()
  "Delete transclusion text for the attachment of header at point."
  (interactive)
  (when-let ((attachment-path (org-ilm--transcludable-attachment-path)))
    (save-excursion
     (org-ilm--transclusion-goto attachment-path 'delete))))

(defun org-ilm-attachment-transclusion-create ()
  "Create and move point to transclusion for the attachment of header at point."
  (interactive)
  (when-let ((attachment-path (org-ilm--transcludable-attachment-path)))
    (org-ilm--transclusion-goto attachment-path 'create)))

(defun org-ilm-attachment-transclusion-transclude ()
  "Transclude the contents of the current heading's attachment."
  (interactive)
  (when (org-ilm--transcludable-attachment-path)
    (save-excursion
      (org-ilm-attachment-transclusion-create)
      (org-transclusion-add))))

(defun org-ilm-attachment-transclude (&optional dont-activate)
  "Transclude the contents of the current heading's attachment."
  (interactive "P")
  (unless (org-ilm--transcludable-attachment-path)
    (user-error "No transcludable attachment for element"))
  (if (org-ilm--transclusion-active-p)
      (org-ilm-attachment-transclusion-delete)
    (if dont-activate
        (org-ilm-attachment-transclusion-create)
      (org-ilm-attachment-transclusion-transclude))))

;;;; Target overlays

;; Targets refer to the anchor points used to highlight extracted or clozed
;; sections of text in org attachments. It looks like this:
;; <<{type}:beg:{id}>>bla bla<<{type}:end:{id}>>
;; Type can be 'card' or 'extr'.
;; They are called targets because that's what Org mode calls them. Targets are
;; part of the Org mode spec and are therefore parse by 'org-element'.

;; When an Org mode file is loaded that is recognized as an ilm attachment, the
;; buffer is parsed for target pairs. For each pair, three overlays are created:
;; one for each target element to hide it, and one that encapsulates the whole
;; target region to visually indicate it with a color.

(defvar org-ilm-target-value-regexp "\\(extr\\|card\\):\\(beg\\|end\\):\\([^>]+\\)"
  "Regexp to match values of targets enclosing extracts and clozes.")

(defvar org-ilm-target-regexp (format "<<%s>>" org-ilm-target-value-regexp)
  "Regexp to match targets enclosing extracts and clozes.")

(defvar org-ilm--targets-editable nil
  "Whether or not to allow editing/removing of target text.")

(defun org-ilm--target-parse-string (string &optional with-brackets)
  "Parse and return parts of target string as specified in `org-ilm-target-value-regexp'."
  (when-let* ((regexp (if with-brackets
                          org-ilm-target-regexp
                        org-ilm-target-value-regexp))
              (parts (s-match regexp string))
              (type (nth 1 parts))
              (pos (nth 2 parts))
              (id (nth 3 parts)))
    `(:string ,string :type ,type :pos ,pos :id ,id)))

(defun org-ilm--target-parse-element (element)
  "Parse and return parts of target element."
  (setq tmp-el element)
  (when-let* ((string (org-element-property :value element))
              (parts (org-ilm--target-parse-string string))
              (begin (org-element-property :begin element))
              (end (org-element-property :end element)))
    (let* (;; Parse target text to offset end position by spaces
           (target-text (buffer-substring-no-properties begin end))
           (trailing-spaces (length (progn
                                      (string-match " *\\'" target-text)
                                      (match-string 0 target-text))))
           (end-nowhite (- end trailing-spaces)))
      (setq parts (plist-put parts :begin begin))
      (setq parts (plist-put parts :end end-nowhite)))
    parts))

(defun org-ilm--target-parse-match ()
  "Parse and return target matched with `re-search-forward' and friends."
  (when-let ((target (org-ilm--target-parse-string (match-string-no-properties 0)))
             (begin (match-beginning 0))
             (end (match-end 0)))
    (setq target (plist-put target :begin begin))
    (setq target (plist-put target :end end))
    target))

(defun org-ilm--target-around-point (pos)
  "Returns start target or end target around point, depending if `POS' is 'begin or 'end."
  (cl-assert (memq pos '(begin end)))
  (let* ((find-begin (eq pos 'begin))
         (re-func (if find-begin #'re-search-backward #'re-search-forward))
         (pos-string (if find-begin "beg" "end")))
    (save-excursion
      (when (funcall re-func org-ilm-target-regexp nil t)
        (let ((target (org-ilm--target-parse-match)))
          (when (string-equal (plist-get target :pos) pos-string)
            target))))))

(defun org-ilm--targets-around-point ()
  "Returns begin and targets around point, or nil if not in highlight."
  (when-let ((target-begin (org-ilm--target-around-point 'begin))
             (target-end (org-ilm--target-around-point 'end)))
    (list target-begin target-end)))

(defun org-ilm-targets-remove-around-point ()
  "Removes targets around point if exists."
  (interactive)
  (when-let ((targets (org-ilm--targets-around-point)))
    (atomic-change-group
      (let ((org-ilm--targets-editable t))
        (delete-region (plist-get (nth 1 targets) :begin)
                       (plist-get (nth 1 targets) :end))
        (delete-region (plist-get (nth 0 targets) :begin)
                       (plist-get (nth 0 targets) :end)))
      (org-ilm-recreate-overlays))))

(defun org-ilm--target-ov-block-edit (ov after-change-p beg end &optional len-pre)
  ;; The hook functions are called both before and after each change. If the functions save the information they receive, and compare notes between calls, they can determine exactly what change has been made in the buffer text.
  ;; When called before a change, each function receives four arguments: the overlay, nil, and the beginning and end of the text range to be modified.
  ;; When called after a change, each function receives five arguments: the overlay, t, the beginning and end of the text range just modified, and the length of the pre-change text replaced by that range. (For an insertion, the pre-change length is zero; for a deletion, that length is the number of characters deleted, and the post-change beginning and end are equal.) 
  (unless (or after-change-p org-ilm--targets-editable)
    (user-error "Cannot modify this region")))

(defun org-ilm--overlay-edit-hook (ov after-change-p beg end &optional len-pre)
  )

(defun org-ilm--create-overlay (target-begin target-end &optional no-face)
  "Create an overlay to hide the target markers and highlight the target text."
  ;; Hide targets
  (dolist (target (list target-begin target-end))
    (let ((begin (plist-get target :begin))
          (end (plist-get target :end))
          (begin-p (string= "beg" (plist-get target :pos))))
      (let ((ov (make-overlay
                 begin
                 ;; If next character is newline, include it.  Otherwise hitting
                 ;; backspace on header will look like header is still on
                 ;; newline but is in fact after the target.
                 ;; (if (eq (char-after end) ?\n) (+ end 1) end)
                 (if (and (eq (char-after end) ?\n)
                          (or (= begin (point-min)) (eq (char-before begin) ?\n)))
                     (+ end 1)
                 end)
                 nil ;; buffer
                 ;; FRONT-ADVANCE: if non-nil, makes the marker for the front of
                 ;; the overlay advance when text is inserted there (which means
                 ;; the text *is not* included in the overlay).
                 t
                 ;; REAR-ADVANCE: if non-nil, makes the marker for the rear of
                 ;; the overlay advance when text is inserted there (which means
                 ;; the text *is* included in the overlay).
                 t)))
        (overlay-put ov 'org-ilm-target t)
        (overlay-put ov 'invisible t)
        ;; (overlay-put ov 'display "|")
        ;; (overlay-put ov 'cursor-intangible t)
        (overlay-put ov 'modification-hooks '(org-ilm--target-ov-block-edit))
        )
    ))
  
  ;; Highlight region
  (let* ((id (plist-get target-begin :id))
         (type (plist-get target-begin :type))
         (face (pcase type
                 ("extr" 'org-ilm-face-extract)
                 ("card" 'org-ilm-face-card)))
         (begin (plist-get target-begin :begin))
         (end (plist-get target-end :end))
         ;; (begin (1+ (plist-get target-begin :end)))
         ;; (end (plist-get target-end :begin))
         ov)
    (unless (= begin end)
      (setq ov (make-overlay begin end nil t t))
      (unless no-face
        (overlay-put ov 'face face))
      (overlay-put ov 'org-ilm-highlight t)
      (overlay-put ov 'org-ilm-type type)
      (overlay-put ov 'org-ilm-id id)
      (overlay-put ov 'modification-hooks '(org-ilm--overlay-edit-hook))
      (overlay-put ov 'help-echo (format "Highlight %s" id)))))

(defun org-ilm-remove-overlays (&optional point-min point-max)
  ""
  (interactive)
  (org-with-wide-buffer
   (remove-overlays point-min point-max 'org-ilm-highlight t)
   (remove-overlays point-min point-max 'org-ilm-target t)))

(defun org-ilm-recreate-overlays (&optional point-min point-max no-face)
  ""
  (interactive)
  (org-ilm-remove-overlays point-min point-max)
  (org-with-wide-buffer
   (goto-char (or point-min (point-min)))
   (let ((begin-targets (make-hash-table :test 'equal)))
     (while-let ((end (re-search-forward org-ilm-target-regexp point-max t))
                 (begin (match-beginning 0))
                 (string (match-string-no-properties 0))
                 (type (match-string-no-properties 1))
                 (pos (match-string-no-properties 2))
                 (id (match-string-no-properties 3))
                 (target (list :end end :begin begin :string string
                               :type type :pos pos :id id)))
       (when (member type '("extr" "card"))
         (pcase pos
           ("beg" (puthash id target begin-targets))
           ("end"
            (when-let ((begin-target (gethash id begin-targets)))
              (org-ilm--create-overlay begin-target target no-face)
              ;; TODO Do we need to remove it?
              (remhash id begin-targets)))))))))

(defun org-ilm--open-from-ov (ov)
  ""
  (when-let ((id (overlay-get ov 'org-ilm-id))
             (attachment (format "%s.org" id)))
    (when (or
           (file-exists-p attachment)
           (yes-or-no-p "Attachment does not exist. Open anyway?"))
        (find-file attachment))
    id))

;;;; Query

(defcustom org-ilm-queries
  `((Outstanding . org-ilm-query-outstanding)
    (All . org-ilm-query-all))
  "Alist mapping query name to a function that returns an org-ql query."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-ilm)

(defcustom org-ilm-custom-queries nil
  "Do not edit. Auto saved user added queries. See `org-ilm-queries'."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-ilm)

;;;;; Queries

;; Optimizations: https://github.com/alphapapa/org-ql/issues/88#issuecomment-570473621
;; + If any data is needed in action and query, use org-ql--value-at
;; + Prefer query over custom predicate
;; + Use regex preambles to quickly filter candidates

(defun org-ilm--compare-priority (first second)
  "Comparator of two headlines by sampled priority."
  (when-let ((priority-first (org-ilm-element-psample first))
             (priority-second (org-ilm-element-psample second)))
    (< priority-first priority-second)))

(defun org-ilm-query-collection (collection query)
  "Apply org-ql QUERY on COLLECTION, parse org-ilm data, and return the results."
  (unless (functionp query)
    (setq query (cdr (assoc query org-ilm-queries))))
  (let ((files (org-ilm--collection-files collection)))
    (org-ql-select files
      (funcall query)
      ;; TODO Pass as sexp so that org-ql can byte compile it
      :action #'org-ilm-element-at-point)))

(defun org-ilm-query-buffer (buffer query &optional narrow)
  "Apply org-ql QUERY on buffer, parse org-ilm data, and return the results."
  (org-ql-select buffer
    (funcall (cdr (assoc query org-ilm-queries)))
    :action #'org-ilm-element-at-point
    :narrow narrow))

(defun org-ilm--query-subjects (&optional collection)
  "Return list of subjects from COLLECTION.

TODO parse-headline pass arg to not sample priority to prevent recusrive subject search?"
  (let ((collection (or collection (plist-get org-ilm-queue :collection))))
    (cl-assert collection)
    (org-ql-select (org-ilm--collection-files collection)
      `(and (property "ID")
            ,(cons 'todo org-ilm-subject-states))
      :action #'org-ilm-element-at-point)))

(defun org-ilm-query-all ()
  "Query for org-ql to retrieve all elements."
  `(and
    (property "ID")
    (or ,(cons 'todo org-ilm-material-states)
        ,(cons 'todo org-ilm-card-states))))

(defun org-ilm-query-outstanding ()
  "Query for org-ql to retrieve the outstanding elements."
  (let ((today (ts-adjust 'minute (org-ilm-midnight-shift-minutes) (ts-now))))
    `(and
      (property "ID")
      (scheduled :to ,today)
      ,(cons 'todo (append org-ilm-material-states org-ilm-card-states)))))

(defun org-ilm--query-select ()
  "Prompt user for query to select from.
The queries are stored in `org-ilm-queries'."
  (org-ilm--select-alist org-ilm-queries "Query: "))


;;;; Queue

;; "Queue" is a bit of a misnomer, as it can be ordered in any way, and may
;; contain any element, outstanding or not, depending on the query it was used
;; to build it. However, on review start, it forms the review queue, so in that
;; sense it is a queue.

(defcustom org-ilm-queue-subject-nchar 6
  "Truncation size of subject names as displayed in the queue.
If available, the last alias in the ROAM_ALIASES property will be used."
  :type 'integer
  :group 'org-ilm)

(defvar-local org-ilm-queue nil
  "List of elements.")

(defvar org-ilm-queue-active-buffer nil
  "The active queue buffer. Reviewing will happen on this queue.")

(defvar org-ilm-queue-active-buffer-change-hook nil
  "Hook called when `org-ilm-queue-active-buffer' changes.")

;;;;; Queue object

;; Use a double dash "--" for the accessor because want to generalize
;; "org-ilm-queue-" functions to org-ilm-queue-buffer.
(cl-defstruct (org-ilm-queue (:conc-name org-ilm-queue--))
  "Queue of elements."
  name collection query type
  (ost (make-ost-tree))
  (elements (make-hash-table :test #'equal))
  key reversed)

(defun org-ilm-queue--element-value (queue element &optional no-key)
  "Return the value of ELEMENT by which it is sorted in QUEUE."
  (cond-let*
    ;; First check the node key in the ost: A custom value might have been set
    ;; for an element even though the queue sort is based on a key.
    ([_ (not no-key)]
     [node (ost-tree-node-by-id
            (org-ilm-queue--ost queue)
            (org-ilm-element-id element))]
     [key (ost-node-key node)]
     key)
    ([key (org-ilm-queue--key queue)]
     [getter (intern (concat "org-ilm-element-" key))]
     (cl-assert (functionp getter))
     (when-let ((value (funcall getter element)))
       (cl-typecase value
         (ts (ts-unix value))
         (t value))))))

(defun org-ilm-queue--count (queue)
  "Return the number of elements in QUEUE."
  (hash-table-count (org-ilm-queue--elements queue)))

(defun org-ilm-queue--empty-p (queue)
  "Return t if QUEUE is empty."
  (hash-table-empty-p (org-ilm-queue--elements queue)))

(defun org-ilm-queue--insert (queue element &optional value)
  "Insert ELEMENT in QUEUE."
  (let ((id (org-ilm-element-id element)))
    (puthash id element (org-ilm-queue--elements queue))
    (ost-tree-insert (org-ilm-queue--ost queue)
                     (or value
                         (org-ilm-queue--element-value queue element 'no-key))
                     id)))

;; TODO Some type of caching so that repeated selects doesnt search tree again
;; Should be implemented in `ost-tree-select'.
(cl-defun org-ilm-queue--select (queue index)
  "Return the element with the INDEX-th smallest key in QUEUE (0-based)."
  (when (org-ilm-queue--reversed queue)
    (setq index (- (org-ilm-queue--count queue) 1 index)))
  (let ((node (ost-select (org-ilm-queue--ost queue) index)))
    (gethash (ost-node-id node) (org-ilm-queue--elements queue))))

(defun org-ilm-queue--top (queue &optional n)
  "Return the first N elements in QUEUE."
  (unless (org-ilm-queue--empty-p queue)
    (mapcar
     (lambda (index)
       (org-ilm-queue--select queue index))
     (number-sequence 0 (or n (1- (org-ilm-queue--count queue)))))))

(defun org-ilm-queue--head (queue)
  "Return the first element in QUEUE."
  (org-ilm-queue--select queue 0))

(defun org-ilm-queue--contains-p (queue element)
  "Return ELEMENT if it is in QUEUE.
ELEMENT may be an `org-ilm-element' or its id."
  (let ((id (if (org-ilm-element-p element)
                (org-ilm-element-id element)
              element)))
    (gethash id (org-ilm-queue--elements queue))))

(defun org-ilm-queue--element (queue element-or-id)
  (if (org-ilm-element-p element-or-id)
      element-or-id
    (gethash element-or-id (org-ilm-queue--elements queue))))

(defun org-ilm-queue--remove (queue element)
  "Remove ELEMENT from QUEUE.
ELEMENT may be an `org-ilm-element' or its id."
  (let ((id (if (org-ilm-element-p element)
                (org-ilm-element-id element)
              element)))
    (ost-tree-remove (org-ilm-queue--ost org-ilm-queue) id)
    (remhash id (org-ilm-queue--elements org-ilm-queue))))

(defun org-ilm-queue--pop (queue)
  "Remove the top most element in QUEUE."
  (when (org-ilm-queue--empty-p queue)
    (error "Tried popping an empty queue \"%s\"" (org-ilm-queue--name queue)))
  (let ((head (org-ilm--queue-head)))
    (org-ilm--queue-remove (org-ilm-element-id head))))

(defun org-ilm-queue--move (queue element new-rank)
  "Move ELEMENT in QUEUE to NEW-RANK."
  (setq element (org-ilm-queue--element queue element))
  (cl-assert (org-ilm-element-p element))
  (ost-tree-move (org-ilm-queue--ost queue) (org-ilm-element-id id) new-rank))

(defun org-ilm-queue--move-many (queue new-ranks-alist)
  "Move many elements in QUEUE to a new rank.

NEW-RANKS-ALIST is an alist of (ID . NEW-RANK) pairs."
  (ost-tree-move-many (org-ilm-queue--ost queue) new-ranks-alist))


;;;;; Building a queue 

;; Queues are stored locally within each queue buffer.

(cl-defun org-ilm--queue-create (collection &key elements query name key reversed)
  "Create a new ilm queue object."
  (setq key (or key "prank"))
  (let ((queue (make-org-ilm-queue
                :name (or name
                          (when query (symbol-name query))
                          (symbol-name collection))
                :collection collection
                :query query
                :key key
                :reversed reversed)))
    (dolist (element elements)
      (org-ilm-queue--insert queue element))
    queue))

(defun org-ilm--queue-build (&optional collection query)
  "Build a queue and return it."
  (let* ((collection (or collection
                         (org-ilm--collection-from-context)
                         (car (org-ilm--select-collection))))
         (query (or query (car (org-ilm--query-select))))
         (elements (org-ilm-query-collection collection query)))
    (org-ilm--queue-create
     collection :elements elements :query query)))

(defun org-ilm-queue-build (&optional collection)
  (let* ((collection (or collection
                         (org-ilm--collection-from-context)
                         (car (org-ilm--select-collection))))
         (queues (cons (cons "Priority queue" "Full queue") org-ilm-queries))
         (choice (org-ilm--select-alist queues "Query: ")))
    (if (string= (car choice) "Priority queue")
        (org-ilm-pqueue-queue collection)
      (org-ilm--queue-build collection (car choice)))))

(defun org-ilm--queue-rebuild (&optional buffer)
  "Replace the queue object by a rebuild one.
If the queue has a query, run it again. Else re-parse elements."
  (org-ilm-with-queue-buffer buffer
    (setq org-ilm-queue
          (cond-let*
            ((eq (org-ilm-queue--type org-ilm-queue) 'pqueue)
             (org-ilm-pqueue-queue (org-ilm-queue--collection org-ilm-queue)))
            ([query (org-ilm-queue--query org-ilm-queue)]
             (org-ilm--queue-build
              (org-ilm-queue--collection org-ilm-queue) query))
            (t
             (let ((queue (copy-org-ilm-queue org-ilm-queue))
                   (elements (make-hash-table :test #'equal)))
               (dolist (id (hash-table-keys (org-ilm-queue--elements org-ilm-queue)))
                 (puthash id (or (org-ilm-element-from-id id) id) elements))
               (setf (org-ilm-queue--elements queue) elements)
               queue))))
    (current-buffer)))

(defun org-ilm--queue-sort (key reversed &optional buffer)
  (org-ilm-with-queue-buffer buffer
    (unless (string= (org-ilm-queue--key org-ilm-queue) key)
      (setf (org-ilm-queue--ost org-ilm-queue) (make-ost-tree)
            (org-ilm-queue--key org-ilm-queue) key)
      (dolist (element (hash-table-values (org-ilm-queue--elements org-ilm-queue)))
        (org-ilm-queue--insert org-ilm-queue element)))
    (setf (org-ilm-queue--reversed org-ilm-queue) reversed)
    org-ilm-queue))

(cl-defun org-ilm--queue-buffer-create (queue &key active-p switch-p)
  "Create a new queue buffer."
  (let ((buffer (generate-new-buffer
                 (format "*Ilm Queue (%s)*" (org-ilm-queue--name queue)))))
    (with-current-buffer buffer
      (setq-local org-ilm-queue queue)

      ;; Make sure that when the queue buffer is killed we update the active
      ;; buffer.
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (eq (current-buffer) org-ilm-queue-active-buffer)
                    (org-ilm-queue--set-active-buffer nil)))
                nil 'local)

      ;; Refresh when queue during changes that effect queue
      (add-hook 'org-ilm-review-next-hook
                #'org-ilm-queue-revert nil t)
      (add-hook 'org-ilm-review-quit-hook
                #'org-ilm-queue-revert nil t)
      (add-hook 'org-ilm-queue-active-buffer-change-hook
                #'org-ilm-queue-revert nil t)
      
      (org-ilm--queue-buffer-build :buffer buffer :switch-p switch-p)

      ;; This doesn't seem to activate when called in `org-ilm--queue-buffer-build'
      (eldoc-mode 1)
      
      (when active-p
        (org-ilm-queue--set-active-buffer buffer)))
    buffer))

(defun org-ilm--queue-buffer-p (buf)
  "Tests whether or not BUF is a queue buffer."
  (with-current-buffer buf
    (bound-and-true-p org-ilm-queue)))

(defun org-ilm--queue-buffers ()
  "Return all queue buffers."
  (seq-filter
   #'org-ilm--queue-buffer-p
   (buffer-list)))

(defun org-ilm-queue-buffers ()
  "View queue buffers in ibuffer."
  (interactive)
  ;; TODO use `org-ilm--queue-buffer-p'
  ;; cannot seem to figure out how!!!!!!!!!!!!!!!!!!!
  (ibuffer nil "*Ilm Queue Buffers*"
           '((name . "^\\*Ilm Queue"))))

(defun org-ilm-queue--set-active-buffer (buffer)
  (cl-assert (or (null buffer) (org-ilm--queue-buffer-p buffer)))
  (setq org-ilm-queue-active-buffer buffer)
  (run-hooks 'org-ilm-queue-active-buffer-change-hook)
  buffer)

(defun org-ilm--queue-select-active-buffer ()
  (when-let ((buffers (org-ilm--queue-buffers)))
    (org-ilm-queue--set-active-buffer
     (if (= (length buffers) 1)
         (car buffers)
       (completing-read "Set active queue buffer: " buffers nil t)))))

(defun org-ilm--queue-buffer-current ()
  "Return the current queue buffer.
This is either the current buffer if its a queue buffer, or the active
queue buffer."
  (or (and (org-ilm--queue-buffer-p (current-buffer)) (current-buffer))
      org-ilm-queue-active-buffer))  

(defmacro org-ilm-with-queue-buffer (buffer &rest body)
  "Run BODY with BUFFER set to a valid queue buffer."
  (declare (indent 1))
  `(let ((buf (or ,buffer (org-ilm--queue-buffer-current))))
     (unless (and buf (buffer-live-p buf))
       (error "No valid queue buffer found"))
     (with-current-buffer buf
       ,@body)))

(defun org-ilm-queue (new &optional dont-switch)
  "Build a queue and view it in Agenda-like buffer."
  (interactive "P")
  (if new
      (org-ilm--queue-buffer-create
       (org-ilm-queue-build) :switch-p (not dont-switch))
    (cond
     ;; If current buffer is queue buffer, switch to active queue buffer, or ask
     ;; to make this the active queue buffer.
     ((org-ilm--queue-buffer-p (current-buffer))
      (if org-ilm-queue-active-buffer
          (unless (eq (current-buffer) org-ilm-queue-active-buffer)
            (switch-to-buffer org-ilm-queue-active-buffer))
        (when (yes-or-no-p "Make current queue the active queue?")
          (org-ilm-queue--set-active-buffer (current-buffer)))))
     ;; If outside a queue buffer and there is an active queue buffer, switch to
     ;; it.
     (org-ilm-queue-active-buffer
      (switch-to-buffer org-ilm-queue-active-buffer))
     (t
      (if-let ((buffers (org-ilm--queue-buffers)))
          ;; If outside a queue buffer and there is no active queue buffer but there
          ;; are inactive ones, user can choose between switching to one of those,
          ;; making it active, or creating a new one, making it active.
          (let* ((candidates (mapcar #'buffer-name buffers))
                 (choice (completing-read "Queue: " candidates)))
            (if (string-empty-p choice)
                (org-ilm--queue-buffer-create
                 (org-ilm-queue-build)
                 :active-p t :switch-p (not dont-switch))
              (with-current-buffer (switch-to-buffer choice)
                (org-ilm-queue--set-active-buffer (current-buffer)))))
        ;; If outside a queue buffer and there are no active or inactive queue
        ;; buffers, make one and make it active.
        (org-ilm--queue-buffer-create
         (org-ilm-queue-build)
         :active-p t :switch-p (not dont-switch)))))))

;;;;; Queue operations

(cl-defun org-ilm--queue-insert (element &key buffer exists-ok)
  "Insert ELEMENT into the queue.
When EXISTS-OK, don't throw error if ELEMENT already in queue."
  (cl-assert (org-ilm-element-p element))
  (org-ilm-with-queue-buffer buffer
    (if (org-ilm-queue--contains-p org-ilm-queue element)
        (unless exists-ok
          (error "Element already in queue (%s)" (org-ilm-element-id element)))
      (org-ilm-queue--insert org-ilm-queue element)
      t)))

(cl-defun org-ilm--queue-count (&key buffer)
  "Return number of elements in the queue."
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--count org-ilm-queue)))

(cl-defun org-ilm--queue-empty-p (&key buffer)
  (= 0 (org-ilm--queue-count :buffer buffer)))

(cl-defun org-ilm--queue-select (index &key buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--select org-ilm-queue index)))

(cl-defun org-ilm--queue-top (&key n buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--top org-ilm-queue n)))

(cl-defun org-ilm--queue-head (&key buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--head org-ilm-queue)))

(cl-defun org-ilm--queue-elements (&key ordered buffer)
  "Return elements in the queue."
  (org-ilm-with-queue-buffer buffer
    (if ordered
        (org-ilm--queue-top :buffer buffer)
      (hash-table-values (org-ilm-queue--elements org-ilm-queue)))))

(cl-defun org-ilm--queue-remove (element-or-id &key buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--remove org-ilm-queue element-or-id)))

(cl-defun org-ilm--queue-pop (&key buffer)
  "Remove the top most element in the queue."
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--pop org-ilm-queue)))

;;;;; Dynamic building

;; TODO With prefix arg: transient with additional settings
;; - Mainly queue-specific priorities.
;; - Exclude cards.
;; Replace current arg with double arg
(defun org-ilm-queue-add-dwim (arg)
  "Add element at point to queue. With ARG, add to new queue.

If point on headline, add headline and descendants.
If point on subject, add all headlines of subject."
  (interactive "P")
  (when-let* ((headline (org-ilm--org-headline-at-point))
              (type (org-ilm-type headline))
              (collection (or (org-ilm--active-collection)
                              (org-ilm--select-collection)))
              (queue-buffer
               (let* ((queue-buffers (mapcar (lambda (b) (cons (buffer-name b) b))
                                             (org-ilm--queue-buffers)))
                      (queue (completing-read "Add to queue, or create new: "
                                              queue-buffers)))
                 (or (cdr (assoc queue queue-buffers))
                     (org-ilm--queue-buffer-create
                      (org-ilm--queue-create
                       collection
                       :name (when (string-empty-p queue)
                               (format "[%s] %s" (upcase (symbol-name type))
                                       (org-element-property :title headline))))))))
              (n-added 0))
    (if (eq type 'subject)
        (dolist (entry (org-ilm--collection-entries collection))
          (when-let* ((element (org-ilm-element-from-id (org-mem-entry-id entry)))
                      (subjects (car (org-ilm-element-subjects element)))
                      (ancestor-ids (mapcar #'car subjects)))
            (when (member (org-element-property :ID headline) ancestor-ids)
              (when (org-ilm--queue-insert element :buffer queue-buffer :exists-ok t)
                (cl-incf n-added)))))

      (save-excursion
        (org-back-to-heading t)
        (let ((end (save-excursion (org-end-of-subtree t)))
              el)
          (while (< (point) end)
            (when (setq el (org-ilm-element-at-point))
              (when (org-ilm--queue-insert el :buffer queue-buffer :exists-ok t)
                (cl-incf n-added))
              (outline-next-heading))))))

    (message "Added %s element(s) to the queue" n-added)
    
    (with-current-buffer (switch-to-buffer queue-buffer)
      (org-ilm-queue-revert))))


;;;; Queue view

(defvar-keymap org-ilm-queue-base-map
  :doc "Base map of ilm queue, regardless if empty or not."
  "n" (lambda ()
        (interactive)
        (next-line)
        (when (eobp) (previous-line)))
  "p" #'previous-line
  "b" #'backward-char
  "f" #'forward-char
  "q" #'quit-window
  "k" (lambda ()
        (interactive)
        (kill-buffer (current-buffer)))
  "g" #'org-ilm-queue-revert
  "G" #'org-ilm-queue-rebuild)
  
(defvar-keymap org-ilm-queue-map
  :doc "Keymap for the queue buffer."
  :parent org-ilm-queue-base-map
  "r" #'org-ilm-queue-review
  "e" #'org-ilm-element-actions
  "v" #'org-ilm-queue-sort
  "m" #'org-ilm-queue-object-mark
  "RET" #'org-ilm-queue-open-attachment
  "SPC" #'org-ilm-queue-open-element
  "P" #'org-ilm-queue-set-priority
  "N" #'org-ilm-queue-set-position
  "S" #'org-ilm-queue-set-schedule
  "D" #'org-ilm-queue-delete
  "M j" #'org-ilm-queue-mark-by-subject
  "M :" #'org-ilm-queue-mark-by-tag
  "M s" #'org-ilm-queue-mark-by-scheduled
  "M u" #'org-ilm-queue-mark-unmark-all
  "M ?" #'org-ilm-queue-mark-missing
  )

;; TODO Make this an alist (index . id) as index is used as queue object so that
;; we can do (vtable-update-object (vtable-current-table) index) to update the
;; table when marking rather than doing a whole revert.
(defvar-local org-ilm--queue-marked-objects nil
  "Org id of marked objects in queue.")

(defun org-ilm--queue-object-id (object)
  (if (org-ilm-element-p object)
      (org-ilm-element-id object)
    object))

(defun org-ilm--queue-mark-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--queue-object-id object)))
      (unless (member id org-ilm--queue-marked-objects)
        (push id org-ilm--queue-marked-objects)))))

(defun org-ilm--queue-unmark-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--queue-object-id object)))
      (setq org-ilm--queue-marked-objects
            (delete id org-ilm--queue-marked-objects)))))

(defun org-ilm--queue-mark-toggle-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--queue-object-id object)))
      (if (member id org-ilm--queue-marked-objects)
          (setq org-ilm--queue-marked-objects
                (delete id org-ilm--queue-marked-objects))
        (push id org-ilm--queue-marked-objects)))))

;; TODO This is ineffcient. Instead we should give vtable the selected-ed
;; objects directly.
(defun org-ilm--vtable-get-object (&optional index)
  "Return (index . queue object) at point or by index."
  ;; when-let because index can be nil when out of table bounds
  (when-let ((index (or index (vtable-current-object))))
    (cons index (org-ilm--queue-select index))))

(defun org-ilm--vtable-get-object-id ()
  (org-ilm--queue-object-id
   (cdr (org-ilm--vtable-get-object))))

(defun org-ilm-queue-open-attachment (object)
  "Open attachment of object at point."
  (interactive (list (cdr (org-ilm--vtable-get-object))))
  (org-ilm--attachment-open-by-id (org-ilm-element-id object)))

(defun org-ilm-queue-open-element (object)
  "Open attachment of object at point."
  (interactive (list (cdr (org-ilm--vtable-get-object))))
  (org-ilm--org-goto-id (org-ilm-element-id object)))

(defun org-ilm-queue-object-mark (object)
  "Mark the object at point."
  (interactive (list (cdr (org-ilm--vtable-get-object))))
  ;; Only toggle if called interactively
  ;; (when (called-interactively-p)
  (org-ilm--queue-mark-toggle-objects object)
  ;; TODO gives cache error no idea why
  ;; Maybe worked on?: https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-07/msg00802.html
  (vtable-update-object (vtable-current-table) (vtable-current-object))
  ;; (org-ilm-queue-revert)
  (when (called-interactively-p)
    (next-line)
    (when (eobp) (previous-line))))

(defun org-ilm-queue-mark-by-subject (subject-id)
  "Mark all elements in queue that are part of subject with id SUBJECT-ID."
  (interactive
   (list
    (org-mem-entry-id (org-ilm--subject-select-entry))))

  ;; Alternatively, we could have used
  ;; `org-ilm--subjects-get-with-descendant-subjects' to precompute the
  ;; descendancy, but this would require a list-to-list comparison eg with
  ;; `seq-some' per object, instead of just an `assoc'.
  (dolist (object (org-ilm--queue-elements))
    (when (org-ilm-element-p object)
      (let ((ancestor-ids (car (org-ilm-element-subjects object))))
        (when (member subject-id ancestor-ids)
          (org-ilm--queue-mark-objects object)))))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-by-tag (tag)
  "Mark all elements in queue that have tag TAG."
  (interactive
   (list
    (completing-read
     "Tag: "
     (org-ilm--collection-tags (org-ilm-queue--collection org-ilm-queue)))))
  (dolist (object (org-ilm--queue-elements))
    (when (and (org-ilm-element-p object)
               (member tag (org-ilm-element-tags object)))
      (org-ilm--queue-mark-objects object)))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-by-scheduled (days)
  "Mark all elements in queue that are scheduled in DAYS days.
DAYS can be specified as numeric prefix arg."
  (interactive "NNumber of days: ")
  (dolist (object (org-ilm--queue-elements))
    (when (org-ilm-element-p object)
      (when-let ((due (* -1 (org-ilm-element-schedrel object))))
        (when (<= (round due) days) 
          (org-ilm--queue-mark-objects object)))))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-missing ()
  "Mark all elements that are missing, i.e. do not have an ilm-element."
  (interactive)
  (dolist (object (org-ilm--queue-elements))
    (unless (org-ilm-element-p object)
      (org-ilm--queue-mark-objects object)))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-unmark-all ()
  "Unmark all marked elements."
  (interactive)
  (setq-local org-ilm--queue-marked-objects nil)
  (org-ilm-queue-revert))

(defun org-ilm-queue--set-header ()
  (setq header-line-format
        (concat
         (when (eq (current-buffer) org-ilm-queue-active-buffer)
           (if (org-ilm-reviewing-p)
               (propertize "[R] " 'face 'error)
             (propertize "[A] " 'face 'bold)))
         (org-ilm-queue--name org-ilm-queue)
         " ("
         (symbol-name (org-ilm-queue--collection org-ilm-queue))
         ")")))

(defun org-ilm-queue-revert (&optional rebuild)
  "Revert/refresh the queue buffer. With REBUILD, reparse elements."
  (interactive "P")
  (let ((was-empty (org-ilm--queue-empty-p)))
    (when rebuild
      (when (yes-or-no-p "Rebuild queue? This will parse the elements again. ")
        (org-ilm--queue-rebuild)))
    (if (or (and was-empty (not (org-ilm--queue-empty-p)))
            (org-ilm--queue-empty-p)
            (not (vtable-current-table)))
        (org-ilm--queue-buffer-build)
      (vtable-revert-command))
    (org-ilm-queue--set-header)))

(defun org-ilm-queue-rebuild ()
  "Rebuild the queue buffer."
  (interactive)
  (setq org-ilm--queue-marked-objects nil)
  (org-ilm-queue-revert 'rebuild))

(defun org-ilm--vtable-format-cell (string marked &optional missing)
  "Apply 'face property to cell STRING based in its MARKED and/or MISSING."
  (if (not (or marked missing))
      string
    (let ((face (list :inherit 'default :slant 'italic)))
      (when marked (setq face (plist-put face :underline t)))
      (when missing (setq face (plist-put face :foreground "#595959")))
      (propertize string 'face face))))
  
(cl-defun org-ilm--queue-make-vtable (&key keymap)
  "Build queue vtable.

A lot of formatting code from org-ql."
  (make-vtable
   :insert nil ; Return vtable object rather than insert at point
   :columns
   `((:name
      "Index"
      :max-width 4
      :align 'right
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,index ,missing) data)
                     (index-str (format "%4d" (1+ index))))
          (org-ilm--vtable-format-cell index-str marked missing))))
     (:name
      "Priority"
      :width 6
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,priority ,missing) data)
                     (rank (car priority))
                     (quantile (cdr priority)))
          (org-ilm--vtable-format-cell
           (if missing
               "NA"
             (propertize (format "%.2f" (* 100 quantile))
                         'face 'shadow))
           marked missing))))
     (:name
      "Type"
      :width 4
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,type ,missing) data))
          (org-ilm--vtable-format-cell
           (if missing
               "NA"
             (org-ql-view--add-todo-face
              (upcase (pcase type
                        ('material (car org-ilm-material-states))
                        ('card (car org-ilm-card-states))
                        (_ "????")))))
           marked missing))))
     ;; (:name
     ;;  "Cookie"
     ;;  :width 4
     ;;  :formatter
     ;;  (lambda (data)
     ;;    (pcase-let ((`(,marked ,p) data))
     ;;      (org-ilm--vtable-format-cell
     ;;       (org-ql-view--add-priority-face (format "[#%s]" p))
     ;;       marked))))
     (:name
      "Title"
      :min-width "50%"
      :max-width "55%"
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,title ,missing) data))
          (org-ilm--vtable-format-cell title marked missing))))
     (:name
      "Due"
      :max-width 8
      ;; :align 'right
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,due ,missing) data))
          (org-ilm--vtable-format-cell
           (cond
            (missing "NA")
            (due (org-add-props
                                   (org-ql-view--format-relative-date
                                    (round due))
                     nil 'face 'org-ql-view-due-date))
            (t ""))
           marked missing))))
     ;; (:name
     ;;  "Tags"
     ;;  :formatter
     ;;  (lambda (data)
     ;;    (pcase-let ((`(,marked ,tags) data))
     ;;      (if tags
     ;;          (org-ilm--vtable-format-cell
     ;;           (--> tags
     ;;                (s-join ":" it)
     ;;                (s-wrap it ":")
     ;;                (org-add-props it nil 'face 'org-tag))
     ;;           marked)
     ;;        ""))))
     (:name
      "Subjects"
      :max-width 15
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,subjects ,missing) data)
                    (names))
          (when subjects
            (setq names
                  (mapcar
                   (lambda (subject)
                     (let ((title (or
                                   (car (last (org-mem-entry-roam-aliases subject)))
                                   (org-mem-entry-title subject))))
                       (substring title 0 (min (length title)
                                               org-ilm-queue-subject-nchar))))
                   subjects)))
          (org-ilm--vtable-format-cell
           (cond
            (names (org-add-props (s-join "," names) nil 'face 'org-tag))
            (missing "NA")
            (t ""))
           marked missing)))))
   :objects-function
   (lambda ()
     (unless (org-ilm--queue-empty-p)
       (number-sequence 0 (1- (org-ilm--queue-count)))))
   :getter
   (lambda (row column vtable)
     (let* ((object (cdr (org-ilm--vtable-get-object row)))
            ;; See `org-ilm-pqueue-queue'. Missing elements are mapped back to id.
            (id (if (org-ilm-element-p object)
                    (org-ilm-element-id object)
                  object))
            (marked (member id org-ilm--queue-marked-objects)))
       (cond
        ((org-ilm-element-p object)
         (let ((subjects (org-ilm-element-subjects object))
               (priority (org-ilm-element-priority object)))
           (pcase (vtable-column vtable column)
             ("Index" (list marked row))
             ("Type" (list marked (org-ilm-element-type object)))
             ("Priority" (list marked priority))
             ;; ("Cookie" (list marked (org-ilm-element-pcookie object)))
             ("Title" (list marked (org-ilm-element-title object)))
             ("Due" (list marked (org-ilm-element-schedrel object)))
             ;; ("Tags" (list marked (org-ilm-element-tags object)))
             ("Subjects"
              (list marked
                    (mapcar #'org-mem-entry-by-id
                            ;; Only direct subjects
                            (last (car subjects) (cdr subjects))))))))
        ((stringp object)
         (pcase (vtable-column vtable column)
           ("Index" (list marked row t))
           ("Title" (list marked id t))
           (_ (list marked nil t))))
        (t (error "wrong object value in table row")))))
         
   :keymap (or keymap org-ilm-queue-map)
   :actions '("S" ignore)))

(defun org-ilm--queue-eldoc-show-info ()
  "Return info about the element at point in the queue buffer."
  (when-let* ((element (cdr (org-ilm--vtable-get-object))))
    (when (org-ilm-element-p element)
      (propertize (format "[#%s]" (org-ilm-element-pcookie element))
                  'face 'org-priority))))

(cl-defun org-ilm--queue-buffer-build (&key buffer switch-p keymap)
  "Build the contents of the queue buffer, and optionally switch to it."
  (org-ilm-with-queue-buffer buffer
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (if (org-ilm--queue-empty-p)
        (progn
          (insert (propertize "Queue empty..." 'face 'italic))
          (use-local-map (or keymap org-ilm-queue-base-map)))
      (vtable-insert (org-ilm--queue-make-vtable :keymap keymap)))
    (org-ilm-queue--set-header)
    (setq-local buffer-read-only t)
    (hl-line-mode 1)
    (eldoc-mode 1)
    (setq-local eldoc-documentation-function #'org-ilm--queue-eldoc-show-info)
    (goto-char (point-min))
    (when switch-p
      (switch-to-buffer buffer))))

(defvar org-ilm--queue-transient-buffer nil)

;;;;; Actions

(defun org-ilm-queue-review ()
  "Start reviewing current queue."
  (interactive)
  (org-ilm-review-start :queue-buffer (current-buffer)))

(defun org-ilm-queue-set-priority ()
  "Set the priority of the element at point, or bulk spread of marked elements."
  (interactive)
  (if org-ilm--queue-marked-objects
      (org-ilm-queue-spread-priority)
    (org-ilm-element-set-priority (org-ilm-element-from-context)))
  (org-ilm-queue-revert))

(defun org-ilm-queue-set-position ()
  (interactive)
  (if org-ilm--queue-marked-objects
      (org-ilm-queue-spread-position)
    (org-ilm--ost-move
     org-ilm-queue
     (org-ilm-element-id (org-ilm-element-from-context))
     (car (org-ilm--queue-select-read org-ilm-queue))))
  (org-ilm-queue-revert))

(defun org-ilm-queue-set-schedule ()
  (interactive)
  (let ((elements (org-ilm-queue--elements org-ilm-queue)))
    (if org-ilm--queue-marked-objects
        (let ((start-ts (ts-parse (org-read-date nil nil nil "Start date: ")))
              end-ts)
          (while (not end-ts)
            (let ((ts (ts-parse (org-read-date nil nil nil "End date: "))))
              (if (ts>= ts start-ts)
                  (setq end-ts ts)
                (message "End date must be after start date")
                (sleep-for 1))))
          (dolist (id org-ilm--queue-marked-objects)
            (org-ilm--org-with-point-at id
              (let* ((diff-days (plist-get
                                 (ts-human-duration
                                  (ts-difference end-ts start-ts))
                                 :days))
                     (rand-interval (random (1+ diff-days)))
                     (ts (ts-adjust 'day rand-interval start-ts))
                     (date (ts-format "%Y-%m-%d" ts)))
                (org-ilm-schedule (org-ilm-element-at-point) date)
                (puthash id (org-ilm-element-at-point) elements)))))

      ;; No marked elements, set schedule of element at point
      (org-ilm-element-set-schedule (org-ilm-element-from-context))
      (let ((element (org-ilm-element-from-context)))
        (puthash (org-ilm-element-id element) element elements)))
    (org-ilm-queue-revert)))

(defun org-ilm-queue-remove ()
  "Remove current or selected elements from this queue."
  (interactive)
  )

(defun org-ilm-queue-delete ()
  "Delete current or selected elements from the collection."
  (interactive)
  (let* ((queue-buffer (current-buffer))
         (collection (org-ilm-queue--collection org-ilm-queue))
         (ids (copy-sequence
               (or org-ilm--queue-marked-objects
                   (list (org-ilm--vtable-get-object-id)))))
         (ask-confirmation-p t)
         (confirm-p (lambda (message)
                      (if (not ask-confirmation-p)
                          t
                        (setq message (concat message " (y)es, (n)o, (!)all, (q)uit"))
                        (setq message (propertize message 'face 'bold))
                        (pcase (read-char message '("y" "n" "!" "q"))
                          (?y t)
                          (?n nil)
                          (?q (throw 'abort nil))
                          (?! (setq ask-confirmation-p nil)
                              t)))))
         (delete-id (lambda (id)
                      (org-ilm--queue-remove id :buffer queue-buffer)
                      (org-ilm-pqueue-remove id collection))))
    (catch 'abort
      (dolist (id ids)
        (cond
         ((ignore-errors (progn (org-id-goto id) t))
          (org-mark-subtree)
          (when (funcall confirm-p "Delete element?")
            (org-ilm-element-delete (org-ilm-element-at-point) 'warn-attach)
            (funcall delete-id id)))
         (t
          (switch-to-buffer queue-buffer)
          
          (when (funcall confirm-p "Element not found. Delete from priority queue?")
            (funcall delete-id id))))
        (when ask-confirmation-p
          (with-current-buffer queue-buffer
            (org-ilm-queue-revert)))))
    (switch-to-buffer queue-buffer)
    (org-ilm--queue-unmark-objects ids)
    (org-ilm-queue-revert)))

;;;;; Sort transient

(transient-define-prefix org-ilm--queue-sort-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (org-ilm-with-queue-buffer org-ilm--queue-transient-buffer
      (append
       (let ((key (org-ilm-queue--key org-ilm-queue)))
         (pcase key
           ("prank" '("--key=priority"))
           ("sched" '("--key=schedule"))))
       (when (org-ilm-queue--reversed org-ilm-queue)
         '("--reversed"))
       )))

  ["queue sort"
   ("k" "Key" "--key=" :choices ("priority" "schedule") :always-read t :transient t)
   ("r" "Reversed" "--reversed" :transient t)
   ("RET" "Sort"
    (lambda ()
      (interactive)
      (let* ((args (transient-args transient-current-command))
             (reversed (transient-arg-value "--reversed" args))
             (key (transient-arg-value "--key=" args)))
        (org-ilm-with-queue-buffer org-ilm--queue-transient-buffer
          (pcase key
            ("priority" (org-ilm--queue-sort "prank" reversed))
            ("schedule" (org-ilm--queue-sort "sched" reversed))
            (_ (user-error "Invalid key: %s" key)))
          (org-ilm-queue-revert))))
    :inapt-if-not
    (lambda ()
      (transient-arg-value "--key=" (transient-get-value))))
   ]
  )

(defun org-ilm-queue-sort ()
  (interactive)
  (setq org-ilm--queue-transient-buffer (current-buffer))
  (org-ilm--queue-sort-transient))

;;;;;; Priority and position spread transient

(defun org-ilm--queue-pspread-transient-format (extremum)
  (when-let* ((queue (or (transient-scope) (org-ilm-pqueue)))
              (args (transient-get-value))
              (rank (transient-arg-value (concat "--" extremum "=") args)))
    (propertize
     (org-ilm--ost-format-position
      queue (1- (string-to-number rank)))
     'face 'italic)))

(defun org-ilm--queue-pspread-transient-read (extremum)
  (cl-assert (member extremum '("a" "b")))
  (let* ((minimum-p (string= extremum "a"))
         (queue (transient-scope))
         (rank-this (car (if queue
                             (org-ilm--queue-select-read queue)
                           (org-ilm-pqueue-select-position))))
         (rank-other (transient-arg-value
                      (concat "--" (if minimum-p "max" "min") "=")
                      (transient-args transient-current-command)))
         (rank-other (when rank-other
                       (1- (string-to-number rank-other))))
         (n-marked (length org-ilm--queue-marked-objects)))
    (cond
     (rank-other
      (when (or (< (1+ (abs (- rank-other rank-this ))) n-marked)
                (funcall (if minimum-p #'<= #'>=) rank-other rank-this))
        (mochar-utils--transiet-set-target-value (if minimum-p "b" "a") nil)))
     (minimum-p
      (mochar-utils--transiet-set-target-value
       "b" (number-to-string (+ rank-this n-marked))))
     ((not minimum-p)
      (mochar-utils--transiet-set-target-value
       "a" (number-to-string (- rank-this n-marked)))))
    (number-to-string (1+ rank-this))))

(transient-define-prefix org-ilm--queue-pspread-transient (&optional queue)
  "This transient will set prioritiy if QUEUE is nil. Otherwise an
 org-ilm-queue can be passed so that the priority/order within that
 queue is changed."
  :refresh-suffixes t

  [:description
   (lambda ()
     (if (transient-scope) "Position spread" "Priority spread"))
   (:info
    (lambda ()
      (let ((n-materials 0)
            (n-cards 0))
        (dolist (id org-ilm--queue-marked-objects)
          (pcase (org-ilm-type (org-mem-entry-todo-state (org-mem-entry-by-id id)))
            ('material (cl-incf n-materials))
            ('card (cl-incf n-cards))))
        (propertize
         (format "%s element (%s materials, %s cards)"
                 (length org-ilm--queue-marked-objects)
                 n-materials n-cards)
         'face 'transient-value))))
   ("a" "Min" "--min=" :always-read t
    :transient transient--do-call
    :reader (lambda (&rest _) (org-ilm--queue-pspread-transient-read "a")))
   (:info
    (lambda () (or (org-ilm--queue-pspread-transient-format "min") ""))
    :if
    (lambda () (org-ilm--queue-pspread-transient-format "min")))
   ("b" "Max" "--max=" :always-read t
    :transient transient--do-call
    :reader (lambda (&rest _) (org-ilm--queue-pspread-transient-read "b")))
   (:info
    (lambda () (or (org-ilm--queue-pspread-transient-format "max") ""))
    :if
    (lambda () (org-ilm--queue-pspread-transient-format "max")))
   ("r" "Random" "--random")
   ]

  [
   ("RET" "Spread"
    (lambda ()
      (interactive)
      (let* ((queue (or (transient-scope) (org-ilm-pqueue)))
             (args (transient-args transient-current-command))
             (min-rank (1- (string-to-number (transient-arg-value "--min=" args))))
             (max-rank (1- (string-to-number (transient-arg-value "--max=" args))))
             (marked org-ilm--queue-marked-objects)
             (size (length marked))
             (random-p (transient-arg-value "--random" args))
             (order (number-sequence (1- size) 0 -1)))
        (when random-p
          (setq order (mochar-utils--list-shuffle order)))
        (org-ilm--ost-move-many
         queue
         (mapcar 
          (lambda (i)
            (let* ((id (nth i marked))
                   (j (nth i order))
                   (new-rank (round (+ min-rank
                                       (* (/ (float j) (max 1 (1- size)))
                                          (- max-rank min-rank))))))
              (cons id new-rank)))
          (number-sequence 0 (1- size))))
        (org-ilm-queue-revert)))
    :inapt-if-not
    (lambda ()
      (let ((args (transient-get-value)))
        (and (transient-arg-value "--min=" args)
             (transient-arg-value "--max=" args)))))
   ]

  (interactive)
  (transient-setup 'org-ilm--queue-pspread-transient nil nil :scope queue))

(defun org-ilm--queue-spread-position (&optional priority-queue-p)
  (unless org-ilm--queue-marked-objects
    (user-error "No elements marked!"))
  (setq org-ilm--queue-transient-buffer (current-buffer))
  (org-ilm--queue-pspread-transient
   (unless priority-queue-p org-ilm-queue)))

(defun org-ilm-queue-spread-position ()
  (interactive)
  (org-ilm--queue-spread-position))

(defun org-ilm-queue-spread-priority ()
  (interactive)
  (org-ilm--queue-spread-position 'priority-queue-p))


;;;; Queue select

;; Using a queue view to select a position in the queue

(defvar org-ilm--queue-select-timer nil)
(defvar org-ilm--queue-select-buffer nil)
(defvar-local org-ilm--queue-select-point 1)
(defvar-local org-ilm--queue-select-minibuffer-window nil)

(defvar-keymap org-ilm-queue-select-map
  :doc "Keymap of queue select buffer."
  "n" (lambda ()
        (interactive)
        (next-line)
        (when (eobp) (previous-line)))
  "p" #'previous-line
  "b" #'backward-char
  "f" #'forward-char
  "RET" (lambda ()
          (interactive)
          (org-ilm--queue-select-accept-minibuffer)))

(defun org-ilm--queue-select-update (rank)
  "Update preview buffer based on minibuffer INPUT."
  (when (and rank (buffer-live-p org-ilm--queue-select-buffer))
    (with-current-buffer org-ilm--queue-select-buffer
      (setq header-line-format
            (concat "Select position: "
                    (org-ilm--ost-format-position org-ilm-queue rank)))
      (with-selected-window (get-buffer-window)
        (goto-line (1+ rank))
        (hl-line-highlight)
        (recenter-top-bottom)))))

(defun org-ilm--queue-select-update-minibuffer ()
  (with-current-buffer org-ilm--queue-select-buffer
    (when (and org-ilm--queue-select-minibuffer-window
               (window-live-p org-ilm--queue-select-minibuffer-window))
      (when-let* ((element (cdr (org-ilm--vtable-get-object)))
                  (rank (org-ilm-queue--element-value org-ilm-queue element)))
        (with-selected-window org-ilm--queue-select-minibuffer-window
          (delete-minibuffer-contents)
          (insert (number-to-string (1+ rank))))))))

(defun org-ilm--queue-select-accept-minibuffer ()
  (with-current-buffer org-ilm--queue-select-buffer
    (when (and org-ilm--queue-select-minibuffer-window
               (window-live-p org-ilm--queue-select-minibuffer-window))
      (with-selected-window org-ilm--queue-select-minibuffer-window
        (exit-minibuffer)))))

(defun org-ilm--queue-select-read (queue &optional init-position)
  "Show only the preview buffer above the minibuffer during completion."
  (let* ((saved-config (current-window-configuration))
         (preview-buf (setq org-ilm--queue-select-buffer
                            (org-ilm--queue-buffer-create queue)))
         position)

    ;; Setup buffer
    (org-ilm--queue-buffer-build :buffer preview-buf :keymap org-ilm-queue-select-map)
    (with-current-buffer preview-buf
      (add-hook 'post-command-hook
                (lambda ()
                  (when (not (= (point) org-ilm--queue-select-point))
                    (setq org-ilm--queue-select-point (point))
                    (org-ilm--queue-select-update-minibuffer)))
                nil 'local))
    
    (unwind-protect
        (progn
          ;; Replace all windows with our preview buffer
          (delete-other-windows)
          (switch-to-buffer preview-buf)
          (org-ilm--queue-select-update
           (car (org-ilm--ost-parse-position queue (or init-position 0))))

          ;; now set up minibuffer hook
          (minibuffer-with-setup-hook
              (lambda ()
                (with-current-buffer preview-buf
                  (setq org-ilm--queue-select-minibuffer-window
                        (active-minibuffer-window)))
                (add-hook 'post-command-hook
                          (lambda ()
                            (when org-ilm--queue-select-timer
                              (cancel-timer org-ilm--queue-select-timer))
                            (setq org-ilm--queue-select-timer
                                  (run-with-idle-timer
                                   0.3 nil
                                   (lambda ()
                                     (when-let* ((input (string-to-number
                                                         (minibuffer-contents-no-properties)))
                                                 (input (if (= input 0) nil (1- input)))
                                                 (pos (org-ilm--ost-parse-position
                                                       queue input)))
                                       (setq position pos)
                                       (org-ilm--queue-select-update (car pos)))))))
                          nil t))
            (setq position (org-ilm--ost-read-position queue))))
      ;; restore old window setup
      (set-window-configuration saved-config)
      (kill-buffer preview-buf)
      (setq org-ilm--queue-select-buffer nil)
      )
    position))


;;;; Import

(transient-define-infix org-ilm--import-transient-collection ()
  :class 'transient-lisp-variable
  :variable 'org-ilm--active-collection
  :transient t
  :allow-empty nil
  :reader
  (lambda (prompt initial-input history)
    (car (org-ilm--select-collection))))

(transient-define-suffix org-ilm--import-transient-resource ()
  (interactive)
  (let ((hook (mochar-utils--add-hook-once
               'org-registry-register-hook
               (lambda (entry-id)
                 (let ((org-ilm--import-registry-data (list :id entry-id)))
                   (org-ilm--import-registry-transient))))))
    (let ((org-registry-types (list (assoc "resource" org-registry-types))))
      (call-interactively #'org-registry-register-dwim))
    (mochar-utils--add-hook-once
     'transient-post-exit-hook
     (lambda () (remove-hook 'org-registry-register-hook hook)))))

(transient-define-prefix org-ilm--import-transient ()
  :refresh-suffixes t
  ["Ilm import"
   ("C" "Collection" org-ilm--import-transient-collection)
   ("r" "Resource" org-ilm--import-resource-transient
    :inapt-if-nil org-ilm--active-collection)
   ("f" "File" org-ilm--import-file-transient
    :inapt-if-nil org-ilm--active-collection)
   ("m" "Media" org-ilm--import-media-transient
    :inapt-if-nil org-ilm--active-collection)
   ("g" "Registry" org-ilm--import-registry-transient
    :inapt-if-nil org-ilm--active-collection)
   ]
  )

(defun org-ilm-import ()
  "Import an item into your Ilm collection."
  (interactive)
  (org-ilm--import-transient))

;;;;; File

(defun org-ilm--import-file-transient-args ()
  (when transient-current-command
    (let* ((args (transient-args transient-current-command))
           (file (transient-arg-value "--file=" args))
           (method (transient-arg-value "--method=" args))
           (collection org-ilm--active-collection))
      (list :file file :method method :collection collection))))

(transient-define-infix org-ilm--import-file-transient-file ()
  :class 'transient-option
  :transient t
  :argument "--file="
  :allow-empty nil
  :prompt "File: "
  :reader
  (lambda (prompt initial-input history)
    (read-file-name prompt nil initial-input t)))

(transient-define-prefix org-ilm--import-file-transient ()
  :value
  (lambda ()
    (append
     '("--method=cp")
     (when buffer-file-name
       (list (concat "--file=" buffer-file-name)))))
  :refresh-suffixes t
  ["File import"
   ("f" "File" org-ilm--import-file-transient-file)
   ("m" "Method of attachment" "--method="
    :allow-empty nil
    :choices (mv cp ln lns) :prompt "Method of attachment: ")
   ("RET" "Import"
    (lambda ()
      (interactive)
      (let ((args (org-ilm--import-file-transient-args)))
        (org-ilm-import-file
         (plist-get args :file)
         (org-ilm--active-collection)
         (intern (plist-get args :method)))))
    :inapt-if-not
    (lambda ()
      (let ((args (org-ilm--import-file-transient-args)))
        (not
         (and (plist-get args :file)
              (plist-get args :method)
              (plist-get args :collection))))))
    ])

(defun org-ilm--import-select-method (&optional force-ask)
  "Ask user to choose whether to copy or move file when importing.

If `org-ilm-import-default-method' is set and `FORCE-ASK' is nil, return it."
  (if (and (not force-ask) org-ilm-import-default-method)
      org-ilm-import-default-method
    (let* ((choices '(("Copy" . cp) ("Move" . mv)))
           (choice (completing-read "Method: " choices nil t))
           (method (cdr (assoc choice choices))))
      method)))

(defun org-ilm-import-file (file collection method)
  "Import a file."
  (cl-assert (member method '(mv cp ln lns)))  
  (org-ilm--capture
   :type 'source :target collection
   :file file :method method :ext t))

;;;;; Media

(defun org-ilm--import-media-transient-args (&optional args)
  (setq args (or args (transient-args 'org-ilm--import-media-transient)))
  (list :source (transient-arg-value "--source=" args)
        :title (transient-arg-value "--title=" args)
        :collection org-ilm--active-collection))

(defun org-ilm--import-media-read-source ()
  (when-let* ((source (ffap-read-file-or-url "File or URL: " nil))
              (title source))
    (setq title (if (org-url-p source)
                    (convtools--ytdlp-title-from-url source)
                  (file-name-base source)))
    (mochar-utils--transiet-set-target-value "t" title)
    (list source title)))

(transient-define-infix org-ilm--import-media-transient-source ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--source="
  :allow-empty nil
  :always-read t
  :reader
  (lambda (prompt initial-input history)
    (car (org-ilm--import-media-read-source))))

(transient-define-prefix org-ilm--import-media-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (append
     (when-let ((source (org-ilm--import-media-read-source)))
       (list (concat "--source=" (car source))
             (concat "--title=" (cadr source))))))

  ["Media import"
   ("s" "Source" org-ilm--import-media-transient-source)
   ("t" "Title" "--title=" :prompt "Title: " :always-read t :transient transient--do-call)
   ("RET" "Import"
    (lambda ()
      (interactive)
      (cl-destructuring-bind (&key source title collection &allow-other-keys)
          (org-ilm--import-media-transient-args)
        (org-ilm--capture
         :type 'source :target collection
         :title title :content ""
         :props (list :ILM_MEDIA source))))
    :inapt-if-not
    (lambda ()
      (cl-destructuring-bind (&key source &allow-other-keys)
          (org-ilm--import-media-transient-args (transient-get-value))
        source)))])

;;;;; Resource

;; Generic object with citation that contains zero or more attachments. Think of:
;; - Website article / blog post
;; - Youtube video (media)
;; - Paper from arxiv link or doi (paper)
;; - Paper from local pdf file

(defvar org-ilm--import-resource-data nil)

(defun org-ilm--import-resource-process-source (&optional source)
  (unless source
    ;; TODO Support for path to pdf -> recover bibtex data (zotero-like)
    ;; (setq source (ffap-read-file-or-url "URL/path/DOI: " nil))
    (setq source (read-string "URL or ID: ")))

  (when (and source (not (string-empty-p source)))
    (let* ((data (org-ilm--citation-get-zotero-json source))
           (type "resource")
           (source-type (if (org-url-p source) 'url 'id))
           (title (or (alist-get 'title data)
                      (alist-get 'shortTitle data)
                      (when (eq source-type 'url)
                        (mochar-utils--get-page-title source))
                      source)))

      (pcase (alist-get 'itemType data)
        ((or "preprint" "conferencePaper" "document" "journalArticle" "manuscript")
         (setq type "paper"))
        ((or "videoRecording" "audioRecording")
         (setq type "media"))
        (_ (setq type "website")))
      
      (list :source source :source-type source-type
            :title title :type type :data data))))

(defun org-ilm--import-resource-transient-args (&optional args)
  (setq args (or args (transient-args 'org-ilm--import-resource-transient)))
  (cl-destructuring-bind (&key source-type bibtex id key title &allow-other-keys)
      org-ilm--import-resource-data
    
    (list :source (transient-arg-value "--source=" args)
          :source-type source-type
          :type (transient-arg-value "--type=" args)
          :bibtex bibtex
          :key key
          :id id
          :title title

          :paper-download (transient-arg-value "--paper-download" args)

          ;; HTML Download
          :html-download (transient-arg-value "--html-download" args)
          :html-simplify
          (cond
           ((transient-arg-value "--html-simplify-to-html" args)
            "html")
           ((transient-arg-value "--html-simplify-to-markdown" args)
            "markdown"))
          :html-orgify (transient-arg-value "--html-orgify" args)

          ;; Media download
          :media-download (transient-arg-value "--media-download" args)
          :media-template (transient-arg-value "--media-template=" args)
          :media-audio-only (transient-arg-value "--media-audio" args)
          :media-sub-langs (cdr (assoc "--media-subs=" args)))))

(transient-define-infix org-ilm--import-resource-transient-source ()
  :class 'transient-option
  :transient 'transient--do-call
  :key "s"
  :description "Source"
  :argument "--source="
  :always-read t
  :allow-empty nil
  :reader
  (lambda (&rest _)
    (let ((source-data (org-ilm--import-resource-process-source)))
      (setq org-ilm--import-resource-data source-data)
      (mochar-utils--transiet-set-target-value "t" (plist-get source-data :type))
      (plist-get source-data :source))))

(transient-define-infix org-ilm--import-resource-transient-key ()
  :class 'transient-option
  :transient 'transient--do-call
  :key "ck"
  :description
  (lambda ()
    (concat
     "Key"
     (when-let* ((key (plist-get org-ilm--import-resource-data :key))
                 (entry (or (org-mem-entry-by-roam-ref (concat "@" key))
                            (citar-get-entry key))))
       (propertize " DUPLICATE" 'face 'error))))
  :argument "--key="
  :always-read t
  :allow-empty nil
  :inapt-if-not
  (lambda () (plist-get org-ilm--import-resource-data :bibtex))
  :reader
  (lambda (&rest _)
    (let ((key (read-string "Key (empty to auto-generate): "))
          (bibtex (plist-get org-ilm--import-resource-data :bibtex)))
      (unless (and key (not (string-empty-p key)))
        (with-temp-buffer
          (insert (mochar-utils--format-bibtex-entry
                   bibtex
                   (plist-get org-ilm--import-resource-data :key)))
          (goto-char (point-min))
          (setq key (ignore-errors (bibtex-generate-autokey)))
          (unless (and key (not (string-empty-p key)))
            (setq key (upcase (substring (org-id-uuid) 0 8))))))
      (setf (plist-get org-ilm--import-resource-data :key) key
            (alist-get "=key=" bibtex nil nil #'string=) key)
      (mochar-utils--transiet-set-target-value "ck" key))))

(transient-define-prefix org-ilm--import-resource-transient (scope)
  :refresh-suffixes t
  :value
  (lambda ()
    (append
     '("--html-simplify-to-markdown" "--html-orgify"
       "--media-template=%(title)s.%(ext)s")
     (let ((id (or (plist-get org-ilm--import-resource-data :id) (org-id-new)))
           (source (plist-get org-ilm--import-resource-data :source)))
       (unless source
         (setq org-ilm--import-resource-data
               (org-ilm--import-resource-process-source))
         (setq source (plist-get org-ilm--import-resource-data :source)))
       (setf (plist-get org-ilm--import-resource-data :id) id)
       (list (concat "--source=" source)
             (concat "--key=" (plist-get org-ilm--import-resource-data :key))
             (concat "--type=" (plist-get org-ilm--import-resource-data :type))
             ;; Don't want media filename to be org-id as I rely on ILM_MEDIA
             ;; prop to point to media. The dedicated attachment is the org file
             ;; in media elements
             ;; (concat "--media-template=" id ".%(ext)s")
             ))))

  ["Import Resource"
   (org-ilm--import-resource-transient-source)
   (:info
    (lambda ()
      (let ((title (plist-get org-ilm--import-resource-data :title)))
          (propertize title 'face 'italic)))
    :if (lambda () (plist-get org-ilm--import-resource-data :title)))
   ("t" "Type" "--type=" :choices ("website" "media" "paper" "resource")
      :always-read t :allow-empty nil)
   ]

  ["HTML download"
   :hide
   (lambda ()
     (when-let ((args (org-ilm--import-resource-transient-args (transient-get-value))))
       (or (not (eq (plist-get args :source-type) 'url))
           (string= (plist-get args :type) "media"))))
   :setup-children
   (lambda (_)
     (convtools--transient-html-build t t))]

  ["Media download"
   :if
   (lambda ()
     (when-let ((args (org-ilm--import-resource-transient-args (transient-get-value))))
       (and (string= (plist-get args :type) "media")
            (eq (plist-get args :source-type) 'url))))
   :setup-children
   (lambda (_)
     (convtools--transient-media-build))
   ]

  ["Paper download"
   :if
   (lambda ()
     (when-let ((args (org-ilm--import-resource-transient-args (transient-get-value))))
       (string= (plist-get args :type) "paper")))
   ("pd" "Download" "--paper-download" :transient transient--do-call)
   ]

  ["Citation"
   ("cc" "Add citation"
    (lambda ()
      (interactive)
      ;; TODO We already save the zotero data in :data, write a function to
      ;; transform it into bibtex
      (let* ((args (org-ilm--import-resource-transient-args))
             (source (plist-get args :source)))
        (if-let* ((bibtex (org-ilm--citation-get-bibtex source 'as-alist))
                  (key (cdr (assoc "=key=" bibtex))))
            (progn
              (setf (plist-get org-ilm--import-resource-data :bibtex) bibtex
                    (plist-get org-ilm--import-resource-data :key) key)
              (mochar-utils--transiet-set-target-value "ck" key))
          (message "Bibtex could not be found"))))
    :transient transient--do-call)
   (org-ilm--import-resource-transient-key)
   ]

  ["Actions"
   ("RET" "Import"
    (lambda ()
      (interactive)
      (cl-destructuring-bind
          (&key source source-type type bibtex key title id
                paper-download
                html-download html-simplify html-orgify
                media-download media-template media-audio-only media-sub-langs)
          (org-ilm--import-resource-transient-args)

        (let ((transient-args (transient-args 'org-ilm--import-resource-transient)))
          (org-ilm--capture-capture
           'source
           :target (org-ilm--active-collection)
           :title title
           :id id
           :bibtex bibtex
           ;; Create an org attachment when downloading media for note
           ;; taking. HTML download is inactive when type is media, so no clash
           ;; with org file generated by it.
           :content (when media-download "")
           :props
           (list :ROAM_REFS (if key (concat source " @" key) source))
           :on-success
           (lambda (id attach-dir collection)
             (when (or html-download media-download paper-download)
               (make-directory attach-dir t)

               (when paper-download
                 (condition-case nil
                     (progn
                       (zotra-download-attachment
                        source nil
                        (expand-file-name (concat id ".pdf") attach-dir))
                       (mochar-utils--org-with-point-at id
                         (org-attach-sync)))
                   (error (message "Failed to download paper for %s" title))))

               (when html-download
                 (convtools--transient-html-run
                  source id attach-dir id transient-args))

               (when (or media-download media-sub-langs)
                 (convtools--transient-media-run
                  source attach-dir id transient-args
                  (lambda (id output-path)
                    (org-entry-put nil "ILM_MEDIA" (file-name-nondirectory output-path))
                    )
                  ))
               )))
            ))))
   ]

  (interactive "P")
  (transient-setup 'org-ilm--import-resource-transient nil nil :scope scope)
  (mochar-utils--add-hook-once
       'transient-post-exit-hook
       (lambda () (setq org-ilm--import-resource-data nil))))

;;;;; Registry

(defvar org-ilm--import-registry-data nil)

(defun org-ilm--import-registry-transient-args (&optional args)
  (setq args (or args (transient-args 'org-ilm--import-registry-transient)))
  (list :entry (org-mem-entry-by-id (transient-arg-value "--entry=" args))
        :attachment (transient-arg-value "--attachment=" args)
        :method (transient-arg-value "--method=" args)
        :media (transient-arg-value "--media=" args)
        :collection org-ilm--active-collection))

(defun org-ilm--import-registry-attachments (entry)
  "Returns the file paths either in the attachment directory or in the PATH property."
  (if-let* ((attach-dir (org-ilm--org-with-point-at
                            (org-mem-entry-id entry)
                          (org-attach-dir)))
            (files (org-attach-file-list attach-dir)))
      (cons attach-dir files)
    ;; No attachment directory. Check PATH property.
    (org-mem-entry-property "PATH" entry)))

(defun org-ilm--import-registry-read-attachment (entry)
  (when-let* ((attachment-data (org-ilm--import-registry-attachments entry)))
    (if (listp attachment-data)
        (let* ((attach-dir (car attachment-data))
               (attachments (cdr attachment-data))
               (attachment (completing-read "Attachment: " attachments)))
          (expand-file-name attachment attach-dir))
      attachment-data)))

(transient-define-infix org-ilm--import-registry-transient-entry ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--entry="
  :allow-empty nil
  :always-read t
  :reader
  (lambda (prompt initial-input history)
    (when-let* ((entry (org-registry--select-entry))
                (id (org-mem-entry-id entry)))
      (when-let ((attachment (org-ilm--import-registry-read-attachment entry)))
        (mochar-utils--transiet-set-target-value "a" attachment))
      id)))

(transient-define-infix org-ilm--import-registry-transient-attachment ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--attachment="
  :reader
  (lambda (prompt initial-input history)
    (let* ((entry (plist-get (org-ilm--import-registry-transient-args) :entry)))
      (org-ilm--import-registry-read-attachment entry))))

(transient-define-infix org-ilm--import-registry-transient-media ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--media="
  :reader
  (lambda (prompt initial-input history)
    (let* ((args (org-ilm--import-registry-transient-args))
           (sources (org-ilm--entry-media-sources (plist-get args :entry))))
      (completing-read "Media: " sources))))

(transient-define-prefix org-ilm--import-registry-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (append
     '("--method=cp")
     (let* ((entry-id (plist-get org-ilm--import-registry-data :id))
            (entry (or (org-mem-entry-by-id entry-id)
                       (org-registry--select-entry))))
       (list
        (concat "--entry=" (org-mem-entry-id entry))
        ;; (when-let ((attachment (org-ilm--import-registry-read-attachment entry)))
        ;;   (concat "--attachment=" attachment))
        ))))

  ["Registry import"
   ("e" "Entry" org-ilm--import-registry-transient-entry)
   (:info
    (lambda ()
      (when-let* ((args (org-ilm--import-registry-transient-args (transient-get-value)))
                  (entry (plist-get args :entry))
                  (type (org-mem-entry-property "TYPE" entry))
                  (title (org-mem-entry-title entry)))
        (propertize
         (format "%s (%s)" title type)
         'face 'transient-inapt-suffix)))
    :if
    (lambda ()
      (when-let* ((args (transient-get-value))
                  (args (org-ilm--import-registry-transient-args args)))
        (plist-get args :entry)))
    )
   ]

  ["Attachment"
   :hide
   (lambda ()
     (let* ((args (org-ilm--import-registry-transient-args (transient-get-value)))
            (entry (plist-get args :entry)))
       (not (and entry (org-ilm--import-registry-attachments entry)))))
   ("a" "Attachment" org-ilm--import-registry-transient-attachment)
   ("M" "Method of attachment" "--method="
    :allow-empty nil :transient transient--do-call
    :choices (mv cp ln lns) :prompt "Method of attachment: ")]

  ["Media"
   ("m" "Media" org-ilm--import-registry-transient-media)
   ]
  
  [
   [("RET" "Import"
     (lambda ()
       (interactive)
       (cl-destructuring-bind (&key collection entry attachment method media)
           (org-ilm--import-registry-transient-args)
         (org-ilm--capture
          :type 'source
          :target collection
          :file attachment
          :content (when (and (not attachment) media) "")
          :method (or (when method (intern method)) 'cp)
          :ext (when attachment t)
          :title (org-mem-entry-title entry)
          :props (append
                  (list :REGISTRY (org-mem-entry-id entry))
                  (when media (list :ILM_MEDIA media))))))
     :inapt-if
     (lambda ()
       (let ((args (org-ilm--import-registry-transient-args (transient-get-value))))
         (not 
          (and (plist-get args :entry)
               (plist-get args :collection))))))
    ]])

(defun org-ilm-import-registry (collection entry &optional attachment method)
  "Import a registry entry."
  (cl-assert (or (null method) (member method '(mv cp ln lns))))

  (org-ilm--capture
   :type 'source :target collection
   :file attachment :method (or method 'cp) :ext (when attachment t)
   :title (org-mem-entry-title entry)
   :props (list :REGISTRY (org-mem-entry-id entry))))


;;;; Math and stats

;; Functions to work with e.g. the beta distribution.
;; TODO Out of laziness this code was mostly generated by Claude. Seems ok from
;; some experiments but need to go through it properly.

(defun org-ilm--round-float (x decimals)
  "Round float X to DECIMALS decimals."
  (let ((factor (expt 10 decimals)))
    (/ (round (* factor x)) (float factor))))

(defun org-ilm--random-uniform ()
  "Generate a uniform random number between 0 and 1."
  (/ (random 1000000) 1000000.0))

(defun org-ilm--random-exponential (rate)
  "Generate an exponential random variable with given rate parameter."
  (- (/ (log (org-ilm--random-uniform)) rate)))

(defun org-ilm--random-gamma (shape)
  "Generate a gamma-distributed random variable with given shape parameter.
Uses Marsaglia and Tsang's method for shape >= 1, and Ahrens-Dieter for shape < 1."
  (cond
   ((< shape 1)
    ;; For shape < 1, use Ahrens-Dieter acceptance-rejection
    (let ((c (/ 1.0 shape))
          (d (- 1.0 shape)))
      (catch 'done
        (while t
          (let* ((u (org-ilm--random-uniform))
                 (v (org-ilm--random-uniform))
                 (x (expt u c))
                 (y (* v (expt x d))))
            (when (<= (+ x y) 1.0)
              (let ((z (- (log v) (* d (log x)))))
                (when (<= z (* shape (1- x)))
                  (throw 'done (* shape (expt (org-ilm--random-uniform) c)))))))))))
   
   ((= shape 1)
    ;; For shape = 1, gamma distribution is exponential
    (org-ilm--random-exponential 1.0))
   
   (t
    ;; For shape > 1, use Marsaglia and Tsang's method
    (let* ((d (- shape (/ 1.0 3.0)))
           (c (/ 1.0 (sqrt (* 9.0 d)))))
      (catch 'done
        (while t
          (let* ((x (org-ilm--random-normal))
                 (v (+ 1.0 (* c x))))
            (when (> v 0)
              (let* ((v (* v v v))
                     (u (org-ilm--random-uniform))
                     (x2 (* x x)))
                (when (or (< u (- 1.0 (* 0.0331 x2 x2)))
                          (< (log u) (+ (* 0.5 x2) (* d (- 1.0 v (log v))))))
                  (throw 'done (* d v))))))))))))

(defun org-ilm--random-normal ()
  "Generate a standard normal random variable using Box-Muller transform."
  (let ((u1 (org-ilm--random-uniform))
        (u2 (org-ilm--random-uniform)))
    (* (sqrt (* -2.0 (log u1)))
       (cos (* 2.0 pi u2)))))

(defun org-ilm--random-poisson (lambd)
  "Generate a Poission random variable using Knuth's algorithm."
  (let ((l (exp (- lambd)))
        (k 0)
        (p 1.0))
    (while (> p l)
      (setq k (1+ k))
      (setq p (* p (org-ilm--random-uniform))))
    (1- k)))

(defun org-ilm--random-beta (alpha beta &optional seed)
  "Generate a beta-distributed random variable with parameters ALPHA and BETA.
If SEED is provided, sets the random seed first for reproducible results."
  (when seed
    (unless (stringp seed)
      (error "Seed must be a string."))
    (random seed))
  
  (cond
   ;; Special cases for efficiency
   ((and (= alpha 1) (= beta 1))
    ;; Beta(1,1) is uniform
    (org-ilm--random-uniform))
   
   ((= alpha 1)
    ;; Beta(1,β) = 1 - U^(1/β)
    (- 1.0 (expt (org-ilm--random-uniform) (/ 1.0 beta))))
   
   ((= beta 1)
    ;; Beta(α,1) = U^(1/α)
    (expt (org-ilm--random-uniform) (/ 1.0 alpha)))
   
   (t
    ;; General case: use gamma method
    (let ((x (org-ilm--random-gamma alpha))
          (y (org-ilm--random-gamma beta)))
      (/ x (+ x y))))))

(defun org-ilm--beta-from-mean-sd (mean sd)
  "Compute Alpha and Beta for a Beta distribution given MEAN and standard deviation SD."
  (unless (and (> mean 0) (< mean 1))
    (error "Mean must be between 0 and 1"))
  (unless (> sd 0)
    (error "Standard deviation must be positive"))
  (let ((var (* sd sd)))  ;; convert SD to variance
    (let ((alpha (- (/ (* mean (- 1 mean)) var) mean)))
      (let ((beta (* alpha (/ (- 1 mean) mean))))
        (list alpha beta)))))

(defun org-ilm--beta-mode (beta)
  (let ((alpha (car beta))
        (beta (cdr beta)))
    (/ (- alpha 1) (+ alpha beta -2))))

;;;; Priority (cookie)

;; Old colde for when priority was based on headline priority cookie.

(defun org-ilm--get-priority (&optional headline-or-entry)
  "Return priority data of HEADLINE, or if nil, the one at point.

The return value is a cons with car the numerical value as set in the
cookie, and cdr a relative value between 0 and 1 based on the minimum
and maximum priority value."
  (unless headline-or-entry
    (setq headline-or-entry (org-ilm--org-headline-at-point)))
  
  (cl-assert (or (org-element-type-p headline-or-entry 'headline)
                 (org-mem-entry-p headline-or-entry)))
  
  (let ((min org-priority-highest)
        (max org-priority-lowest)
        pcookie prelative)

    (setq pcookie
          (if (org-mem-entry-p headline-or-entry)
              (when-let ((p (org-mem-entry-priority headline-or-entry)))
                (org-priority-to-value p))
            (org-element-property :priority headline-or-entry)))
    
    ;; If nothing set, set it to be the average value. In captures i found that
    ;; the returned value can be 0... so deal with that too.
    (when (or (null pcookie) (= pcookie 0))
      (setq pcookie (round (/ (+ min max) 2))))
    ;; Normalize to [0, 1]
    (setq prelative
          (org-ilm--round-float (/ (float (- pcookie min))
                                   (- max min))
                                2))
    (cons pcookie prelative)))

(defun org-ilm--priority-to-beta (priority &optional k)
  "Return (alpha . beta) for a Beta distribution with MODE in (0,1)
and concentration K >= 0 using the symmetric parametrisation.
MODE should be a number in [0,1], K >= 0."
  (cl-assert (<= 0 priority 1))
  (let* ((k (or k 10))
         (a (1+ (* k priority)))
         (b (1+ (* k (- 1 priority)))))
    (cons a b)))

(defun org-ilm--priority-combine-betas (&rest params-list)
  "Combine multiple Beta distributions."
  (cons
   (apply #'+ -1 (mapcar #'car params-list))
   (apply #'+ -1 (mapcar #'cdr params-list))))

(defun org-ilm--priority-spread-from-logbook (logbook)
  "Calculate the measure of spread K from logbook data.

For now, map through logistic k from 10 to 1000 based on number of reviews."
  (let* ((n (length logbook))
         (k-min 10)
         (k-max 1000)
         (n-max 10) ;; Number of reviews to max out k
         (kappa 5)) ;; Steepness curve
    (+ k-min
       (/ (- k-max k-min)
          (1+ (exp (- (* kappa (- (/ n n-max) 0.5)))))))))

(defun org-ilm--priority-adjusted-from-subjects (params subjects)
  "Average the priority out over the subject priorities."
  (let ((ancestors (nth 1 subjects)))
    (if (= 0 (length ancestors))
        params
      (apply #'org-ilm--priority-combine-betas params
             ;; Tighter variance for subjects
             (mapcar (lambda (p) (org-ilm--priority-to-beta p 10.))
                     (mapcar #'cdr ancestors))))))

(defun org-ilm--priority-beta-compile (priority subjects logbook)
  "Compile the finale beta parameters from priority value, subjects, and logbook history."
  (let* ((k (org-ilm--priority-spread-from-logbook logbook))
         (params (org-ilm--priority-to-beta priority)))
    (setq params (org-ilm--priority-adjusted-from-subjects params subjects))))

(defun org-ilm--priority-get-params (&optional headline)
  "Calculate the beta parameters of the heading at point."
  (let ((priority (cdr (org-ilm--get-priority headline)))
        (logbook (org-ilm--org-logbook-read headline))
        (subjects (org-ilm--subject-cache-gather headline)))
    (org-ilm--priority-beta-compile priority subjects logbook)))

(defun org-ilm--priority-sample (beta &optional seed)
  "Sample a priority value given the beta params and a seed."
  (org-ilm--random-beta (car beta) (cdr beta) seed))

(defun org-ilm--priority-sample (beta id &optional date)
  "Sample the priority from BETA, with ID and the date as seed."
  (let ((seed (format "%s%s"
                      (or date (org-ilm--current-date-utc))
                      id)))
    (org-ilm--random-beta (car beta) (cdr beta) seed)))

(cl-defun org-ilm--priority-update (&key a b beta)
  "Update org priority based on bayesian updating."
  (unless (or a b) (error "Either A or B or both must be provided"))
  (let* ((beta (if beta
                   (cons (car beta) (cdr beta)) ;; copy
                 (org-ilm--priority-to-beta (cdr (org-ilm--get-priority))))))
    (when a (setf (car beta) (+ a (car beta))))
    (when b (setf (cdr beta) (+ b (cdr beta))))

    (let* ((mode (org-ilm--beta-mode beta))
           (min org-priority-highest)
           (max org-priority-lowest)
           (priority (round (+ min (* mode (- max min))))))
      (org-ilm--org-priority-set priority))))

(defun org-ilm--priority-a-from-duration (duration)
  "S-curve map duration to a."
  (let ((a-min 0)
        (a-max 3)
        (duration (float duration))
        (duration-max 30)
        (kappa 10))
    (+ a-min
       (/ (- a-max a-min)
          (1+ (exp (* kappa (- (/ duration duration-max) 0.5))))))))

(defun org-ilm--priority-b-from-actions (count)
  "Map number of actions to b."
  (cl-assert (>= count 0 ))
  (min count 5))

;;;; Scheduling

(defun org-ilm--days-from-now (days)
  "Return ts object representing DAYS from now."
  (ts-adjust 'day -1 (ts-now)))

(defun org-ilm--initial-schedule-interval-from-priority (priority)
  "Calculate the initial schedule interval from the normalized priority value."
  (cl-assert (and (floatp priority) (<= 0 priority 1)))
  (let* ((rate (* 25 priority))
         (interval (+ (org-ilm--random-poisson rate) 1)))
    interval))

(defun org-ilm--initial-schedule-from-priority (priority &optional timestamp)
  "Calculate the initial schedule date from the normalized priority value."
  (setq timestamp (or timestamp (ts-now)))
  (cl-assert (ts-p timestamp))
  (let ((interval (org-ilm--initial-schedule-interval-from-priority priority)))
    (ts-adjust 'day interval timestamp)))

(cl-defun org-ilm--schedule (&key org-id timestamp interval-minutes interval-days ignore-time)
  (unless (setq org-id (or org-id (org-id-get)))
    (error "No headline with id given or at point."))
  (org-ilm--org-with-point-at org-id
    (org-ilm--org-schedule :timestamp timestamp
                           :interval-minutes interval-minutes
                           :interval-days interval-days
                           :ignore-time ignore-time)))

(defun org-ilm--set-schedule-from-priority ()
  "Set the schedule based on the priority."
  (when-let* ((id (org-id-get))
              (collection (org-ilm--collection-file))
              (priority (org-ilm-pqueue-priority
                         id :collection collection))
              (interval-days (org-ilm--initial-schedule-interval-from-priority
                              (cdr priority))))
    (org-ilm--schedule :interval-days interval-days)))

(defun org-ilm--update-from-priority-change (func &rest args)
  "Advice around `org-priority' to detect priority changes to update schedule."
  (apply func args)
  (org-ilm--set-schedule-from-priority))

(defun org-ilm--schedule-interval-calculate (priority scheduled last-interval)
  "Calculate the new schedule interval assuming review was done today."
  (if (null last-interval)
      (org-ilm--initial-schedule-interval-from-priority priority)
    (* last-interval 1.5)))

(defun org-ilm--element-schedule-interval-calculate (element)
  (let ((last-interval (org-ilm-element-last-interval element))
        (priority (cdr (org-ilm-element-priority element)))
        (scheduled (org-ilm-element-sched element)))
    (org-ilm--schedule-interval-calculate priority scheduled last-interval)))

;; TODO Turn this to function
;; (due (<= (/ (ts-diff scheduled (org-ilm--ts-today)) 86400) 0)))

;; TODO remove rely on element - just parse id
(defun org-ilm-schedule (element timestamp)
  "Set or update the schedule of the element at point."
  (interactive
   (list (org-ilm-element-at-point)
         (org-read-date 'with-time nil nil "Schedule: ")))
  (unless element (user-error "No ilm element at point"))
  (org-ilm--schedule :timestamp timestamp))

;;;; Subjects

;; TODO Probably still incredibly inefficient as we have to go up the hierarchy
;; everytime. A better option might be to use org-ql to find all headlines with
;; a SUBJECTS property and store mapping org-id -> subjects in a cache. Then use
;; org-mem's cached ancestry to do the lookup, without having to go up each
;; time.

(defun org-ilm--subjects-get-parent-subjects (&optional headline-thing all-ancestors)
  "Retrieve parent subjects of a headline, either in outline or through property.
When ALL-ANCESTORS, retrieve full ancestry recursively.

Eeach call of this function (recursive or not) only retrives _direct_
parents, which is defined differently for subjects and extracts/cards:
- Subject: First outline parent + _not_ inherited property links
- Others: First outline parent + property links of itself or inherited from _non-subject_ ancestors only"
  (let* ((headline (org-ilm--org-headline-from-thing headline-thing 'assert))
         (headline-id (org-element-property :ID headline))
         (headline-entry (org-mem-entry-by-id headline-id))
         (headline-ancestry (org-ilm--org-mem-ancestry-ids headline-entry))
         (headline-is-subj (member (org-element-property :todo-keyword headline) org-ilm-subject-states))
         (property-subjects-str "")
         outline-parent-subject subject-ids)

    ;; Check for ancestor subject headline in outline hierarchy. As we explore
    ;; up the hierarchy, store linked subjects of extracts.
    (cl-block nil
      (dolist (ancestor (org-mem-entry-crumbs headline-entry))
        (let* ((id (nth 4 ancestor))
               (is-self (string= id headline-id))
               (entry (org-mem-entry-by-id id))
               (state (when entry (org-mem-entry-todo-state entry)))
               (type (when state (org-ilm-type state))))
          (cond
           ;; Headline is self or incremental ancestor, store linked subjects
           ((or is-self
                (and (not headline-is-subj) ; Subject never inherit!
                     (eq type 'material)))
            (when-let ((prop (org-mem-entry-property "SUBJECTS+" entry)))
              (setq property-subjects-str
                    (concat property-subjects-str " " prop))))
           
           ;; Headline is subject, store as outline parent subject
           ((eq type 'subject)
            (cl-pushnew id subject-ids :test #'equal)
            (unless outline-parent-subject
              (setq outline-parent-subject id))
            (unless all-ancestors (cl-return)))))))

    ;; Process inherited SUBJECTS property.

    ;; This is tricky because linked subjects may themselves link to other
    ;; subjects, requiring this to be done recursively if we need all
    ;; subjects. Furthermore test for invalid linking, eg to a descendant or
    ;; circular links.
    (let ((link-match-pos 0)
          property-subject-ids property-subject-ancestors)
      (while-let ((_ (string-match org-link-any-re property-subjects-str link-match-pos))
                  (subject-string (match-string 1 property-subjects-str)))
        (setq link-match-pos (match-end 1))

        ;; First we gather valid subject-ids as well as their individual ancestries
        (when-let* ((org-id (org-ilm--org-id-from-string subject-string))
                    ;; Skip if linked to itself
                    (_ (not (string= org-id headline-id)))
                    ;; Skip if ancestor of headline
                    ;; TODO we do this later again as post-processing stop, so
                    ;; remove?
                    (_ (not (member org-id headline-ancestry)))
                    ;; Should have org id and therefore cached by org-mem
                    (entry (org-mem-entry-by-id org-id))
                    ;; Should be subject todo state
                    (_ (member (org-mem-entry-todo-state entry)
                               org-ilm-subject-states))
                    ;; Skip if is subject is descendant of headline - this
                    ;; will lead to circular DAG, causing infinite loops, and
                    ;; doesn't make sense anyway. Add a root string so
                    ;; subjects with no org-id parents don't return nil,
                    ;; terminating the when-let.
                    (ancestry (org-ilm--org-mem-ancestry-ids entry 'with-root-str))
                    (_ (not (member headline-id ancestry))))
          (cl-pushnew org-id property-subject-ids :test #'equal)
          (cl-pushnew ancestry property-subject-ancestors)

          ;; Recursively handle case where property derived subjects
          ;; themselves might link to other subjects in their properties.
          ;; Note, no need to do this for outline ancestors (previous step),
          ;; as properties are inherited.
          (when all-ancestors
            (dolist (parent-id (cdr (org-ilm--subjects-get-parent-subjects org-id all-ancestors)))
              (cl-pushnew parent-id property-subject-ids :test #'equal)
              (cl-pushnew (org-ilm--org-mem-ancestry-ids parent-id) property-subject-ancestors))))

        ;; Filter if subject-id is ancestor of any other subject-id
        (let ((ancestries (apply #'append property-subject-ancestors)))
          (dolist (subject-id property-subject-ids)
            (unless (member subject-id ancestries)
              (cl-pushnew subject-id subject-ids :test #'equal))))))

    (cons headline subject-ids)))

(defun org-ilm--subjects-get-with-descendant-subjects (subject)
  "Retrieve descendant subjects of a headline.
Descendants can be directly in outline or indirectly through property linking."
  (let ((id (plist-get subject :id)))
    (seq-filter
     (lambda (subj)
       (let ((ancestory (nth 2 (org-ilm--subject-cache-gather (plist-get subj :id)))))
         (member id ancestory)))
     (org-ilm--query-subjects))))

(defun org-ilm--subjects-get-with-descendant-subjects--deprecated (subject)
  "DEPRECATED now that we save full ancestor list in cache.

Retrieve descendant subjects of a headline.

Descendants can be directly in outline or indirectly through property linking.

This function does not have to be fast: Currently only called every now
and again by `org-ilm-queue-mark-by-subject'."
  (let ((all-subjects (mapcar (lambda (s)
                                (cons (plist-get s :id) s))
                              ;; Cached:
                              (org-ilm--query-subjects)))
        (queue (list (plist-get subject :id)))
        (subject-and-descendants (list (cons (plist-get subject :id) subject))))
    
    (org-ilm--debug-all
     all-subjects
     (lambda (s) (list (plist-get (cdr s) :title)
                       (car s)
                       (nth 3 (plist-get (cdr s) :subjects))))
     "-- Subject: %s (%s)\n       Parents: %s")

    ;; Queue will contain org-ids of SUBJECT and any of its descendants. For
    ;; each element in the queue, loop over subjects and when one has been found
    ;; that has as direct parent this queue element, it is a descendant and thus
    ;; added to the queue. When the loop for the current queue element is
    ;; finished, we found all its direct children and we thus remove it from the
    ;; queue. Repeat until exhausted.
    (while queue
      (org-ilm--debug "Queue: %s" queue)
      (let ((current (pop queue)))
        (org-ilm--debug "-- Queue current: %s" current)
        (dolist (subj all-subjects)
          (let ((subj-id (car subj))
                (subj-parents (nth 3 (plist-get (cdr subj) :subjects))))
            (org-ilm--debug "-- Subject %s: %s" subj-id subj-parents)
            (when (and (member current subj-parents)
                       (not (assoc subj-id subject-and-descendants)))
              (push subj subject-and-descendants)
              (push subj-id queue))))))

    subject-and-descendants))

;;;;; Functions

(defun org-ilm--subject-select-entry (&optional collection)
  (setq collection
        (or collection
            (org-ilm--collection-from-context)
            (org-ilm--active-collection)))
  (let ((choice
         (org-node-read-candidate
          "Subject: " nil
          (lambda (name entry)
            (when-let ((state (org-mem-todo-state entry))
                       (file (org-mem-entry-file entry)))
              (and
               (eq 'subject (org-ilm-type state))
               (org-ilm--collection-file file collection))))
          'require-match)))
    (gethash choice org-node--candidate<>entry)))

;;;;; Commands

(defun org-ilm-subject-dwim ()
  "Add subject if point in ilm element, otherwise create a new subject."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((at-heading-p (org-at-heading-p))
             (state (when at-heading-p (org-get-todo-state)))
             (ilm-type (org-ilm-type state)))
        (cond
         ((and ilm-type (not (eq ilm-type 'subject)))
          (call-interactively #'org-ilm-subject-add))
         ((and at-heading-p (null state))
          (call-interactively #'org-ilm-subject-into))
         (t (call-interactively #'org-ilm-subject-new))))
    (call-interactively #'org-ilm-subject-new)))

(defun org-ilm-subject-new (&optional select-collection-p)
  "Create a new subject."
  (interactive "P")
  (let* ((collection (org-ilm--collection-from-context))
         (collection (if (or select-collection-p (null collection))
                         (car (org-ilm--select-collection))
                       collection))
         (file (org-ilm--select-collection-file collection)))
    (cl-letf (((symbol-value 'org-capture-templates)
               (list
                (list
                 "s" "Subject" 'entry (list 'file file) "* SUBJ %?"
                 :hook (lambda () (org-node-nodeify-entry))))))
      (org-capture nil "s"))))

(defun org-ilm-subject-into ()
  "Convert headline at point to a subject."
  (interactive)
  (cond
   ((not (and (eq major-mode 'org-mode) (org-at-heading-p)))
    (user-error "Point not on a headline!"))
   ((org-get-todo-state)
    (user-error "Headline already has a TODO state!"))
   (t
    (org-todo (car org-ilm-subject-states))
    (org-node-nodeify-entry))))

(defun org-ilm-subject-add ()
  "Annotate headline at point with a subject.

TODO Skip if self or descendant."
  (interactive)
  (let* ((subject-entry (org-ilm--subject-select-entry))
         (subject-id (org-mem-entry-id subject-entry))
         (cur-subjects (org-entry-get nil "SUBJECTS" t)))
    (if (and cur-subjects
             (string-match subject-id cur-subjects))
        ;; TODO offer option to remove
        (message "Subject already added")
      (let* ((subject-desc (org-mem-entry-title subject-entry))
             (subject-link (org-link-make-string
                            (concat "id:" subject-id) subject-desc)))
        (org-entry-put nil "SUBJECTS+"
                       (concat cur-subjects " " subject-link))))))

;;;;; Subject cache

(defvar org-ilm-subject-cache (make-hash-table :test 'equal)
  "Map subject org-id -> ('(ancestor-subject-ids..) . num-direct-parents)

The direct parent ids are the last num-direct-parents ids in ancestor-subject-ids.

Gets reset after org-mem refreshes.")

(defun org-ilm-subject-cache-reset ()
  (setq org-ilm-subject-cache (make-hash-table :test 'equal)))

(defun org-ilm--subject-cache-gather (headline-or-id)
  "Gather recursively headline's priority and parent subject priorities.
Headline can be a subject or not."
  (let ((id (if (stringp headline-or-id)
                headline-or-id
              (org-element-property :ID headline-or-id))))
    ;; (org-ilm--debug "Gathering subjects for:" id)
    
    (or (gethash id org-ilm-subject-cache)
        (let* ((parents-data
                ;; Non-recursively, just direct parents in DAG
                (org-ilm--subjects-get-parent-subjects headline-or-id))
               (headline (car parents-data))
               (type (org-ilm-type headline))
               (is-subject (eq type 'subject))
               (parent-ids (cdr parents-data))
               (entry (org-mem-entry-by-id id))
               (ancestor-ids (copy-sequence parent-ids)))

          ;; Recursively call this function on all direct parent subjects to get
          ;; entire ancestory.
          (dolist (parent-id parent-ids)
            (let ((parent-data (org-ilm--subject-cache-gather parent-id)))
              (dolist (parent-ancestor (car parent-data))
                (cl-pushnew parent-ancestor ancestor-ids :test #'equal))))

          ;; Compile data in a list. If this headline is also a subject, add it
          ;; to the cache as well.
          (let ((data (when ancestor-ids (cons ancestor-ids (length parent-ids)))))
            (when is-subject
              (puthash id data org-ilm-subject-cache))
            ;; Return data
            data)))))

;;;; Review

;; TODO Pass headline id to clock in to during review
(defcustom org-ilm-review-clock-behavior nil
  "What to do with active clock during review.
Nil to do nothing, 'out to clock out on review, 'out-in to also clock back in."
  :type '(choice
          (const :tag "Nothing" nil)
          (const :tag "Clock out" 'out)
          (const :tag "Clock out and back in when done" 'out-in))
  :group 'org-ilm-review)

;;;;; Variables

;; Thought about making review stuff buffer local to queue buffer (as
;; `org-ilm-queue' itself is) but it might get messy. For example org only
;; allows for having one headline clocked in. In reality there should be only
;; one review session active anyway.

;; Besides storing the attachment buffer, this variable contains redundant data
;; as the current element should be the first element in org-ilm-queue. However
;; this redundancy is useful to make sure everything is still in sync.
(defvar org-ilm--review-data nil
  "Info of the current element being reviewed.")

(defvar org-ilm--review-interrupted-clock-marker nil
  "Stores the `org-clock-marker' of the interrupted clock when review started.")

(defvar org-ilm-review-next-hook nil
  "Hook run when new element has been setup for review.")

(defvar org-ilm-review-reveal-hook nil
  "Hook run when card should be revealed.")

(defvar org-ilm-review-quit-hook nil
  "Hook run when review session stopped.")

(defvar-keymap org-ilm-review-mode-map
  :doc "Keymap for `org-ilm-review-mode'."
  "<f5>" #'org-ilm-review-rate-easy
  "<f6>" #'org-ilm-review-rate-good
  "<f7>" #'org-ilm-review-rate-hard
  "<f8>" #'org-ilm-review-rate-again
  "<f9>" #'org-ilm-review-ensure-or-next)

;;;;; Review logic

;;;###autoload
(define-minor-mode org-ilm-review-mode
  "Minor mode during review."
  :group 'org-ilm
  :global t
  :interactive nil ;; shouldnt be a command
  (if org-ilm-review-mode
      (if (null org-ilm-queue-active-buffer)
          (progn
            (error "No active queue buffer")
            (org-ilm-review-mode -1))

        ;;; Minor mode on
        (with-current-buffer org-ilm-queue-active-buffer
          ;; Add kill hook on queue buffer
          (add-hook 'kill-buffer-hook
                    #'org-ilm--review-confirm-quit nil t))

        ;; Quit review when the active queue changes
        (add-hook 'org-ilm-queue-active-buffer-change-hook
                  #'org-ilm-review-quit)
        
        )
    
    ;;; Minor mode off
    ;; Remove kill buffer hooks
    (when (and org-ilm-queue-active-buffer
               (buffer-live-p org-ilm-queue-active-buffer))
      (with-current-buffer org-ilm-queue-active-buffer
        (remove-hook 'kill-buffer-hook
                     #'org-ilm--review-confirm-quit
                     t)))

    (remove-hook 'org-ilm-queue-active-buffer-change-hook
                 #'org-ilm-review-quit)
    ))

(defun org-ilm-reviewing-p ()
  "Return t when currently reviewing."
  org-ilm-review-mode)

(defun org-ilm-reviewing-id-p (id)
  "Return t when currently reviewing element with id ID."
  (when (org-ilm-reviewing-p)
    (equal id (plist-get org-ilm--review-data :id))))

(defun org-ilm--review-confirm-quit ()
  "Confirmation before killing the active attachment buffer or queue buffer
during review."
  (when (yes-or-no-p "Quit review?")
    (org-ilm-review-quit)))

(cl-defun org-ilm-review-start (&key queue-buffer)
  "Start a review session."
  (interactive)

  (when (org-ilm-reviewing-p)
    (pcase (read-char-choice "Already reviewing! (j)ump to element or (q)uit review? "
                             '("j" "q"))
      (?j (org-ilm-review-open-current))
      (?q (org-ilm-review-quit)))
    (cl-return-from org-ilm-review-start))

  (when queue-buffer
    (org-ilm-queue--set-active-buffer queue-buffer))

  (unless (or org-ilm-queue-active-buffer
              (org-ilm--queue-select-active-buffer))
    ;; TODO let user choose inactive one and make it active
    (user-error "No active queue buffer!"))

  ;; Make sure queue is not empty
  (with-current-buffer org-ilm-queue-active-buffer
    (when (org-ilm--queue-empty-p)
      (user-error "Queue is empty!")))

  (when (yes-or-no-p "Start reviewing?")

    ;; Store clocked-in task so that we can clock back in when done
    (when (member org-ilm-review-clock-behavior '(out out-in))
      (setq org-ilm--review-interrupted-clock-marker
            (when (org-clocking-p) (copy-marker org-clock-marker)))
      (org-clock-out nil 'fail-quietly))

    (org-ilm-review-mode 1)
    (org-ilm--review-next)))

(defun org-ilm-review-quit ()
  "Quit ilm review."
  (interactive)
  (org-ilm--review-cleanup-current-element)
  (org-ilm-review-mode -1)

  ;; Clock back in the active clock before review
  (when org-ilm--review-interrupted-clock-marker
    (when (and (eq org-ilm-review-clock-behavior 'out-in)
               (markerp org-ilm--review-interrupted-clock-marker))
      (with-current-buffer (marker-buffer org-ilm--review-interrupted-clock-marker)
        (save-excursion
          (goto-char org-ilm--review-interrupted-clock-marker)
          (org-clock-in))))
    (setq org-ilm--review-interrupted-clock-marker nil))
  
  (run-hooks 'org-ilm-review-quit-hook))

(defun org-ilm--review-ensure ()
  "Make sure that everything is ready before hitting next.
Return t if already ready."
  (let ((was-ready t)
        (element (plist-get org-ilm--review-data :element)))
    (when (org-ilm-element-card-p element)
      (unless (plist-get org-ilm--review-data :card-revealed)
        (setq was-ready nil)
        (org-ilm-review-reveal)))
    was-ready))

(defun org-ilm-review-ensure-or-next ()
  "If ready, go to next review, else get ready first."
  (interactive)
  (when (org-ilm--review-ensure)
    (org-ilm-review-next)))

(defun org-ilm-review-next (&optional rating)
  "Finish review of current element and go to the next one."
  (interactive)
  (unless (org-ilm-reviewing-p)
    (user-error "Not reviewing."))

  (org-ilm--review-ensure)

  ;; If card, make sure revealed and there is a rating
  (cl-assert (or (null rating) (member rating '(:good :easy :hard :again))))
  (let ((element (plist-get org-ilm--review-data :element)))
    (when (and (org-ilm-element-card-p element)
               (not (plist-get org-ilm--review-data :rating)))
      (setf (plist-get org-ilm--review-data :rating)
            (let ((ratings '(("Good" . :good)
                             ("Easy" . :easy)
                             ("Hard" . :hard)
                             ("Again" . :again))))
              (alist-get (completing-read "Rate:" ratings nil t)
                         ratings nil nil #'equal)))))
  
  (org-ilm--review-next))

(defun org-ilm-review-rate-good ()
  (interactive)
  (org-ilm-review-next :good))

(defun org-ilm-review-rate-easy ()
  (interactive)
  (org-ilm-review-next :easy))

(defun org-ilm-review-rate-hard ()
  (interactive)
  (org-ilm-review-next :hard))

(defun org-ilm-review-rate-again ()
  (interactive)
  (org-ilm-review-next :again))

(defvar org-ilm--review-update-schedule t)

(defun org-ilm--review-next ()
  (when org-ilm--review-data
    ;; Update priority and schedule
    (cl-destructuring-bind
        (&key buffer id element rating &allow-other-keys)
        org-ilm--review-data
      
      (when org-ilm--review-update-schedule
        (if (org-ilm-element-card-p element)
            (org-ilm--card-review rating id)
          (org-ilm--material-review id))))
    
    (let ((element (org-ilm--queue-pop)))
      
      ;; TODO Card might already be due, eg when rating again. So need to check the
      ;; new review time and add it back to the queue. Set a minimum position in
      ;; the queue so that the card is not reviewed too quickly again.
      )
    
    (org-ilm--review-cleanup-current-element))
  
  (if (org-ilm--queue-empty-p)
      (progn
        (message "Finished reviewing queue!")
        (org-ilm-review-quit)
        (switch-to-buffer org-ilm-queue-active-buffer))
    (org-ilm--review-setup-current-element)
    (run-hooks 'org-ilm-review-next-hook)
    (org-ilm--review-open-current-element)))

(defun org-ilm-review-open-current ()
  "Open the attachment of the element currently being reviewed."
  (interactive)
  (let ((buf (plist-get org-ilm--review-data :buffer)))
    (if (and org-ilm--review-data (buffer-live-p buf))
        (switch-to-buffer buf)
      ;; Setup current element again if that data somehow lost. Mainly for when the
      ;; buffer is killed, it needs to be setup again. The setup function doesn't
      ;; change the queue or anything, so its safe to call it again.
      (org-ilm--review-setup-current-element)
      (org-ilm--review-open-current-element))))

(defun org-ilm--review-setup-current-element ()
  "Setup the element about to be reviewed.

The main job is to prepare the variable `org-ilm--review-data', which
needs the attachment buffer."
  (cl-assert (not (org-ilm--queue-empty-p)))

  (let* ((element (org-ilm--queue-head))
         (id (org-ilm-element-id element))
         attachment-buffer)

    (org-ilm--org-with-point-at id
      (setq attachment-buffer
            ;; dont yet switch to the buffer, just return it so we can do some
            ;; processing first.
            (save-window-excursion
              (org-ilm--attachment-open :no-error t))))

    ;; Prepare the attachment buffer if exists
    (when attachment-buffer
      (with-current-buffer attachment-buffer

        ;; Ask to end the review when killing the buffer
        (add-hook 'kill-buffer-hook
                  #'org-ilm--review-confirm-quit
                  nil t)

        ;; We update the buffer-local `org-ilm--data' (see
        ;; `org-ilm--attachment-prepare-buffer') with a start time. This is used
        ;; to calculate the new priority.
        (org-ilm--attachment-ensure-data-object)
        (setf (plist-get org-ilm--data :start) (current-time))

        ;; TODO Run hook here so that individual components can run their own
        ;; preparation without doing it all here.

        ;; If card, hide clozes
        (when (org-ilm-element-card-p element)
          (org-ilm--card-hide-clozes)

          ;; PDF clozes
          (pdf-view-redisplay)
          (when (org-ilm--pdf-mode-p)
            (add-hook 'org-ilm-review-reveal-hook
                      (lambda ()
                        ;; TODO Need a stupid wait here, fix
                        (run-at-time .1 nil 'pdf-view-redisplay))
                      nil 'local)))
          ))

    (setq org-ilm--review-data
          (list :element element
                :id (org-ilm-element-id element)
                :buffer attachment-buffer))))

(defun org-ilm-review-reveal ()
  "Reveal the cloze contents of the current element."
  (interactive)
  (run-hooks 'org-ilm-review-reveal-hook)
  (setf (plist-get org-ilm--review-data :card-revealed) t
        header-line-format (org-ilm--review-header-build)))

(defun org-ilm--review-open-current-element ()
  "Open and prepare the attachment buffer of the element being reviewed."
  (let ((buffer (plist-get org-ilm--review-data :buffer)))
    (if buffer
        (with-current-buffer (switch-to-buffer buffer)
          (setq header-line-format
                (org-ilm--review-header-build)))
      ;; No attachment, simply go to the element in the collection
      (org-ilm--org-goto-id (plist-get org-ilm--review-data :id))
      (message "No attachment found for element"))))

(defun org-ilm--review-cleanup-current-element ()
  "Clean up the element being reviewed, in preparation for the next element."
  (when-let ((buffer (plist-get org-ilm--review-data :buffer))
             (element (plist-get org-ilm--review-data :element)))
    (with-current-buffer buffer
      (when (org-ilm-element-card-p element)
        ;; TODO Clean up card overlays
        ;; (org-srs-item-cloze-remove-overlays (point-min) (point-max))
        )
      (setq header-line-format nil)
      (remove-hook 'kill-buffer-hook #'org-ilm--review-confirm-quit t))
    ;; I know it doesn't make sense to clean the buffer then kill it, but better
    ;; have the cleaning up code ready in case i want an option later to not
    ;; kill the buffer
    (kill-buffer buffer))
  (setq org-ilm--review-data nil))

;;;;; Buffer header 

(defun org-ilm--review-header-make-button (title func)
  (propertize
   (substitute-command-keys
    (format 
     "\\<org-ilm-review-mode-map>[%s `\\[%s]']"
     title (symbol-name func)))
   'mouse-face 'highlight
   'local-map (let ((map (make-sparse-keymap)))
                (define-key map [header-line mouse-1] func)
                map)))

(defun org-ilm--review-header-build ()
  "Build the header string of the attachment currently being reviewed."
  (let* ((element (plist-get org-ilm--review-data :element))
         (card-p (org-ilm-element-card-p element))
         (card-revealed (plist-get org-ilm--review-data :card-revealed)))
    (concat
     (propertize "Ilm Review" 'face '(:weight bold :height 1.0))
     "   "
     (funcall
      #'concat
      (unless card-p
         (org-ilm--review-header-make-button
          "Next" 'org-ilm-review-ensure-or-next))
      (when (and card-p (not card-revealed))
        (org-ilm--review-header-make-button
         "Reveal answer" 'org-ilm-review-ensure-or-next))
      (when (and card-p card-revealed)
        (concat

         (org-ilm--review-header-make-button
          "Easy" 'org-ilm-review-rate-easy)
         " "
         (org-ilm--review-header-make-button
          "Good" 'org-ilm-review-rate-good)
         " "
         (org-ilm--review-header-make-button
          "Hard" 'org-ilm-review-rate-hard)
         " "
         (org-ilm--review-header-make-button
          "Again" 'org-ilm-review-rate-again))
        ))

     )))

;;;;; Actions

(defun org-ilm-review ()
  (interactive)
  (if (org-ilm-reviewing-p)
      (org-ilm-review-actions)
    (org-ilm-review-start)))

(defun org-ilm-review-postpone ()
  (interactive)
  (cl-assert (org-ilm-reviewing-p))
  (let (date)
    (while (ts<= (ts-parse (setq date (org-read-date nil nil nil "Postpone: ")))
                 (org-ilm--ts-today))
      (message "Minimum postpone date should be tomorrow")
      (sleep-for 1.))
    (org-ilm--org-with-point-at (plist-get org-ilm--review-data :id)
      (org-ilm-schedule (org-ilm-element-at-point) date)
      (let ((org-ilm--review-update-schedule nil))
        (org-ilm--review-next)))))

(defun org-ilm-review-open-collection ()
  (interactive)
  (cl-assert (org-ilm-reviewing-p))
  (org-ilm--org-goto-id (plist-get org-ilm--review-data :id)))

(defun org-ilm-review-open-attachment ()
  (interactive)
  (cl-assert (org-ilm-reviewing-p))
  (if-let ((buf (plist-get org-ilm--review-data :buffer)))
      (switch-to-buffer buf)
    (user-error "Element has no buffer!")))

;;;;; Transient

(transient-define-prefix org-ilm--review-transient ()
  :refresh-suffixes t
  ["Review"
   (:info*
   (lambda ()
     (propertize
      (org-ilm-element-title (plist-get org-ilm--review-data :element))
      'face '(:slant italic))))]

  [
   ["Actions"
    ("n" "Next" org-ilm-review-next)
    ("p" "Postpone" org-ilm-review-postpone)
    ("q" "Quit" org-ilm-review-quit)]
   ["Element"
    ("e" "Element..." org-ilm-element-actions)
    ("c" "Open collection" org-ilm-review-open-collection)
    ("a" "Open attachment" org-ilm-review-open-attachment)]
   ]
  )

(defun org-ilm-review-actions ()
  (interactive)
  (if (org-ilm-reviewing-p)
      (org-ilm--review-transient)
    (user-error "Not reviewing!")))


;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here

