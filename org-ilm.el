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
(require 'org-srs)
(require 'org-transclusion)
(require 'cl-lib)
(require 'dash)
(require 'ts)
(require 'vtable)
(require 'eldoc)
(require 'transient)

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

(defcustom org-ilm-incr-states '("INCR")
  "TODO state of elements to be processed incrementally."
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
  "o" #'org-ilm-open-dwim
  "x" #'org-ilm-extract-dwim
  "z" #'org-ilm-cloze
  "c" #'org-ilm-cloze-toggle-this
  "t" #'org-ilm-attachment-transclude
  "j" #'org-ilm-subject-dwim
  "r" #'org-ilm-review
  "q" #'org-ilm-queue
  "+" #'org-ilm-queue-add-dwim)

(defvar org-ilm-target-value-regexp "\\(extract\\|card\\):\\(begin\\|end\\):\\([^>]+\\)"
  "Regexp to match values of targets enclosing extracts and clozes.")

(defvar org-ilm-target-regexp (format "<<%s>>" org-ilm-target-value-regexp)
  "Regexp to match targets enclosing extracts and clozes.")

(defvar org-ilm--targets-editable nil
  "Whether or not to allow editing/removing of target text.")

;;;; Minor mode

;; TODO This hook is called not just after file save but on a timer as well. Do we want the timer to reset cache?
(defun org-ilm--org-mem-hook (&rest _)
  (org-ilm-priority-subject-cache-reset))

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
        (add-hook 'after-change-major-mode-hook #'org-ilm--attachment-prepare-buffer)
        (add-hook 'org-mode-hook #'org-ilm--attachment-prepare-buffer)
        (add-hook 'org-mem-post-full-scan-functions
                  #'org-ilm--org-mem-hook)
        (add-hook 'org-mem-post-targeted-scan-functions
                  #'org-ilm--org-mem-hook)
        (define-key pdf-view-mode-map (kbd "A") org-ilm-pdf-map)
        (advice-add 'pdf-annot-create-context-menu
                    :around #'org-ilm--pdf-annot-create-context-menu-advice)
        )
    ;; Disable
    (add-hook 'after-change-major-mode-hook #'org-ilm--attachment-prepare-buffer)
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
  (condition-case err
      (org-ilm--attachment-open)
    (error
     (when (yes-or-no-p "No attachment found. Create new Org attachment? ")
       (let* ((attach-dir (org-attach-dir-get-create))
              (file-path (expand-file-name (concat (org-id-get) ".org") attach-dir)))
         (find-file file-path)
       )))))
    ;; (error (user-error "%s" (error-message-string err)))))

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
     ('collection (org-ilm-open-attachment))
     (_ (org-ilm-open-collection)))))

(defun org-ilm-cloze-toggle-this ()
  "Toggle cloze at point, without creating the card."
  (interactive)
  (if (org-ilm--srs-in-cloze-p)
      (org-srs-item-uncloze-dwim)
    (org-srs-item-cloze-dwim)))

(defun org-ilm-extract-dwim ()
  "Extract region depending on file."
  (interactive)
  (if (eq 'attachment (car (org-ilm--where-am-i)))
    (cond
     ((eq major-mode 'org-mode)
      (org-ilm-org-extract))
     ((or (eq major-mode 'pdf-view-mode)
          (eq major-mode 'pdf-virtual-view-mode))
      (call-interactively #'org-ilm-pdf-extract)))
    (message "Extracts can only be made from within an attachment")))
  
;;;; Utilities

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

(defun org-ilm--ts-today ()
  (let ((now (ts-now)))
    (make-ts :year (ts-year now) :month (ts-month now) :day (ts-day now))))

(defun org-ilm--select-alist (alist &optional prompt formatter)
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
                              (if (symbolp (car thing))
                                  (symbol-name (car thing))
                                (car thing))
                              'face '(:weight bold))
                             (propertize
                              (if (symbolp second)
                                  (symbol-name second)
                                second)
                              'face '(:slant italic))))
                   thing)))
              alist))
            (choice (completing-read (or prompt "Select: ") choices nil t))
            (item (cdr (assoc choice choices))))
       item))))

(defun org-ilm--current-date-utc ()
  "Current date in UTC as string."
  (format-time-string "%Y-%m-%d" (current-time) t))

(defun org-ilm--buffer-text-process (&optional region-begin region-end keep-clozes remove-footnotes)
  "Prepare buffer text for new extract or card element."
  (let* ((text (buffer-substring-no-properties (or region-begin (point-min))
                                               (or region-end (point-max))))
         ;; Remove ilm targets
         (text (s-replace-regexp org-ilm-target-regexp "" text))
         (buffer (current-buffer)))
    (when remove-footnotes
      (setq text (s-replace-regexp org-footnote-re "" text)))
    (with-temp-buffer
      (insert text)
      
      ;; Remove clozes by looping until none left
      (unless keep-clozes
        (while (let ((clz (org-srs-item-cloze-collect))) clz)
          (let* ((cloze (car (org-srs-item-cloze-collect)))
                 (region-begin (nth 1 cloze))
                 (region-end   (nth 2 cloze))
                 (inner (substring-no-properties (nth 3 cloze))))
            (goto-char region-begin)
            (delete-region region-begin region-end)
            (insert inner))))

      ;; Copy over footnotes 
      (unless remove-footnotes
        (let ((footnotes '()))
          (goto-char (point-min))
          (while (re-search-forward org-footnote-re nil t)
            (when-let* ((fn (match-string 0))
                        (label (match-string 1))
                        (def (with-current-buffer buffer
                               (org-footnote-get-definition label))))
              (push (concat fn " " (nth 3 def)) footnotes)))
          (goto-char (point-max))
          (insert "\n\n")
          (dolist (fn (delete-dups (nreverse footnotes)))
            (insert fn "\n"))))
      
      (buffer-string))))

(defun org-ilm--generate-text-snippet (text)
  ""
  (let* ((text (replace-regexp-in-string "\n" " " text))
         (text (org-link-display-format text)) ;; Remove org links
         (text (string-trim text))) ;; Trim whitespace
    (substring text 0 (min 50 (length text)))))

(defun org-ilm--org-narrow-to-header ()
  "Narrow to headline and content up to next heading or end of buffer."
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))

(defun org-ilm--buffer-file-name ()
  "Like `buffer-file-name' but support indirect buffers."
  (or buffer-file-name (buffer-file-name (buffer-base-buffer))))

(defun org-ilm--where-am-i ()
  "Returns one of ('collection collection), ('attachment (org-id collection)),
('queue collection org-id), nil."
  ;; For collection, we return file as well, useful in a directory based collection.
  (if-let ((collection (org-ilm--collection-file (org-ilm--buffer-file-name))))
      (cons 'collection collection)
    (if-let ((attachment (org-ilm--attachment-data)))
        (cons 'attachment attachment)
      (when (bound-and-true-p org-ilm-queue)
        (let* ((el (org-ilm--vtable-get-object))
               (id (when el (org-ilm-element-id el))))
          (list 'queue (plist-get org-ilm-queue :collection) id))))))

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
    ((null ,thing)
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

(defun org-ilm--interval-to-schedule-string (interval)
  "Turn INTERVAL (days) into a date string used in in org headline SCHEDULED."
  (org-format-time-string
   (org-time-stamp-format)
   (ts-unix (ts-adjust 'day interval (ts-now)))))

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

;;;; Collection

(defcustom org-ilm-collections '((ilm . "~/ilm/"))
  "Alist mapping collection name to path to its org file or directory."
  :type '(alist :key-type symbol
                :value-type (choice (file :tag "File")
                                    (directory :tag "Directory")))
  :group 'org-ilm)

(defvar org-ilm--active-collection nil
  "The current collection that is active.")

(defun org-ilm--active-collection ()
  (or org-ilm--active-collection
      (let ((collection (org-ilm--select-collection)))
        (setq org-ilm--active-collection (car collection)))))

(defun org-ilm--collection-from-context ()
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
      ('collection (cadr location))
      ('attachment (caaddr location))
      ('queue (cadr location)))))

(defun org-ilm--collection-file (&optional file collection)
  "Return collection of which file belongs to.
A collection symbol COLLECTION can be passed to test if file belongs to
 that collection."
  (when-let ((file (or file buffer-file-name))
             (path (expand-file-name file))
             (test (lambda (f place)
                     (if (f-directory-p place)
                         (and
                          (file-in-directory-p f place)
                          (not (file-in-directory-p f org-attach-id-dir)))
                       (file-equal-p f place)))))
    (if collection
        (when-let ((col (pcase (type-of collection)
                          ('symbol
                           (assoc collection org-ilm-collections))
                          ('string
                           (seq-find
                            (lambda (c) (file-equal-p collection (cdr c)))
                            org-ilm-collections))
                          ('cons collection))))
          (when (funcall test path (cdr col)) col))
      (seq-find (lambda (c) (funcall test path (cdr c))) org-ilm-collections))))

(defun org-ilm--select-collection ()
  "Prompt user for collection to select from.
The collections are stored in `org-ilm-collections'."
  (org-ilm--select-alist org-ilm-collections "Collection: "))

(defun org-ilm--collection-files (collection)
  "Return all files that belong to COLLECTION."
  (let ((file-or-dir (alist-get collection org-ilm-collections)))
    (cond
     ((f-file-p file-or-dir)
      (list (expand-file-name file-or-dir)))
     ((f-dir-p file-or-dir)
      (f-glob "*.org" file-or-dir))
     (t (error "No files in collection %s" collection)))))

(defun org-ilm--select-collection-file (collection)
  "Propmt user to select a file from COLLECTION."
  (cl-assert (assoc collection org-ilm-collections))
  (let ((files (org-ilm--collection-files collection)))
    (cond
     ((= 1 (length files))
      (car files))
     ((> (length files) 1)
      (completing-read "Collection file: " files nil 'require-match)))))

;;;; Types

;; There are three headline types: Subjects, Incrs, and Cards.

(defun org-ilm-type (headline-or-state)
  "Return ilm type from an org headline or its todo state.

See `org-ilm-card-states', `org-ilm-incr-states', and `org-ilm-subject-states'."
  (when-let ((state (cond
                     ((org-element-type-p headline-or-state 'headline)
                      (org-element-property :todo-keyword headline-or-state))
                     ((stringp headline-or-state)
                      headline-or-state))))
    (cond
     ((member state org-ilm-card-states) 'card)
     ((member state org-ilm-incr-states) 'incr)
     ((member state org-ilm-subject-states) 'subj))))

;;;; Element

(cl-defstruct org-ilm-element
  "A piece of knowledge."
  id state level pcookie rawval title tags sched schedrel
  type subjects prelative psample pbeta)

(defun org-ilm-element-at-point ()
  "Parse org-ilm data of headline at point.

Was thinking of org-ql--value-at this whole function, but this is
wasteful if headline does not match query."
  (let* ((headline (org-ilm--org-headline-at-point))
         ;; `headline' looses the priority property which is set manually in
         ;; `org-ilm--org-headline-at-point', after I access the :todo-keyword
         ;; property below. I think it is because it is a deferred value, which
         ;; might cause org to overwrite the custom set :priority value after
         ;; resolving it when it is accessed. In any case, it is essentialy to
         ;; use the priority before accessing the other
         ;; properties. Alternatively we can just resolve the deferred
         ;; properties by accessing them all in
         ;; `org-ilm--org-headline-at-point'.
         (priority (org-ilm--get-priority headline))
         (id (org-element-property :ID headline))
         (todo-keyword (org-element-property :todo-keyword headline))
         (type (org-ilm-type todo-keyword))
         (is-card (eq type 'card))
         (logbook (unless is-card (org-ilm--logbook-read headline)))
         (subjects (org-ilm--priority-subject-gather headline))
         (beta (org-ilm--priority-beta-compile (cdr priority) subjects logbook))
         (psample (org-ilm--priority-sample beta id))
         (scheduled (if is-card
                        ;; TODO Move this out to srs section
                        (org-ql--value-at (point) #'org-ilm--srs-earliest-due-timestamp)
                      (when-let ((s (org-element-property :scheduled headline)))
                        (ts-parse-org-element s))))
         (now (ts-now)))
    
    (when type ; non-nil only when org-ilm type
      (make-org-ilm-element
       ;; Headline element properties
       :id id
       :state todo-keyword
       :level (org-element-property :level headline)
       :pcookie (car priority)
       :rawval (org-element-property :raw-value headline)
       :tags (org-element-property :tags headline)
       :title (org-no-properties ; Remove text properties from title
               (org-element-interpret-data (org-element-property :title headline)))

       ;; Ilm stuff
       :sched scheduled
       :schedrel (when scheduled ; convert from sec to days
                   (/ (ts-diff now scheduled) 86400))
       :type type
       ;; cdr to get rid of headline priority in the car - redundant
       :subjects (cdr subjects)
       :prelative (cdr priority)
       :pbeta beta
       :psample psample))))

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
         (org-ilm-element-at-point))))))

(defun org-ilm-element-is-card (element)
  (cl-assert (org-ilm-element-p element))
  (eq 'card (org-ilm-element-type element)))

(defun org-ilm-element-last-review (element)
  (cl-assert (org-ilm-element-p element))
  (org-ilm--org-with-point-at (org-ilm-element-id element)
    (pcase (org-ilm-element-type element)
      ('incr (plist-get (car (org-ilm--logbook-read)) :timestamp-end))
      ('card (org-ilm--srs-last-review-timestamp)))))

(defun org-ilm-element-last-interval (element)
  "Last interval in days."
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
    (org-ilm--org-with-point-at
        org-id
      (save-restriction
        (org-ilm--org-narrow-to-header)
        (if if-matches-query
            (car (org-ilm-query-buffer
                  (current-buffer)
                  if-matches-query
                  t))
          (org-ilm-element-at-point))))))

;;;;; Logbook

(defun org-ilm--logbook-parse (logbook)
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
    
(defun org-ilm--logbook-read (&optional headline)
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
            (org-ilm--logbook-parse logbook)))))))

;;;;; Actions

(defun org-ilm-element-set-schedule (element)
  "Set the schedule of an ilm element."
  (interactive
   (list (or org-ilm--element-transient-element (org-ilm-element-from-context))))
  (org-ilm--org-with-point-at (org-ilm-element-id element)
    (call-interactively #'org-ilm-schedule)))

(defun org-ilm-element-set-priority (element)
  "Set the priority of an ilm element."
  (interactive
   (list (or org-ilm--element-transient-element (org-ilm-element-from-context))))
  (org-ilm--org-with-point-at (org-ilm-element-id element)
    (let ((min org-priority-highest)
          (max org-priority-lowest)
          priority)
      (while (null priority)
        (let ((number (read-number (format "Priority (%s-%s): " min max) org-priority-default)))
          (when (<= min number max)
            (setq priority number))))
      (org-ilm--org-priority-set priority))))

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
                 (pcookie (org-ilm-element-pcookie element)))
       (propertize (format "#%s" pcookie) 'face 'transient-value))))
  :transient 'transient--do-call
  (interactive)
  (call-interactively #'org-ilm-element-set-priority)
  (setq org-ilm--element-transient-element (org-ilm-element-from-context)))

(transient-define-prefix org-ilm--element-transient ()
  :refresh-suffixes t
  [:description
   (lambda () (org-ilm-element-title org-ilm--element-transient-element))
   (org-ilm--element-transient-schedule)
   (org-ilm--element-transient-priority)
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


;;;; Capture

(defun org-ilm--capture (type target data &optional on-success on-abort)
  "Make an org capture to make a new source heading, extract, or card.

TYPE should be one of 'extract 'card, or 'source.

TARGET should be the org-capture target. If it is a string, it is
interpreted as the org ID of a heading, which will be the
target. Finally, if it is a symbol, it is interpreted as a collection
name, and the user will be prompted to select a file from the collection
to be used as the target.

DATA is a plist that contains info about the capture entry.

The callback ON-SUCCESS is called when capture is saved.

The callback ON-ABORT is called when capture is cancelled."
  (cl-assert (member type '(extract card source)))
  
  (let* ((state (car (if (eq type 'card) org-ilm-card-states org-ilm-incr-states)))
         (title (plist-get data :title))
         (id (or (plist-get data :id) (org-id-new)))
         (ext (plist-get data :ext))
         (content (plist-get data :content))
         (props (plist-get data :props))
         (file (plist-get data :file))
         (method (or (plist-get data :method) 'cp)) ;; org-attach-method
         (priority (plist-get data :priority))
         (template (plist-get data :template))
         attach-dir ; Will be set in :hook, and passed to on-success
         (before-finalize
          (lambda ()
            ;; If this is a source header where the attachments will
            ;; live, we need to set the DIR property, otherwise for
            ;; some reason org-attach on children doesn't detect that
            ;; there is a parent attachment header, even with a non-nil
            ;; `org-attach-use-inheritance'.
            (when (eq type 'source)
              (org-entry-put
               nil "DIR"
               (abbreviate-file-name (org-attach-dir-get-create))))
            
            (when file
              ;; Attach the file. Turn of auto tagging if not import source.
              (let ((org-attach-auto-tag (if (eq type 'source) org-attach-auto-tag nil)))
                (org-attach-attach file nil method)

                ;; Make sure the file name is the org-id
                (rename-file
                 (expand-file-name (file-name-nondirectory file) attach-dir)
                 (expand-file-name (concat id "." (file-name-extension file)) attach-dir)
                 'ok-if-already-exists)))
            
                ))
         (after-finalize
          (lambda ()
            ;; Deal with success and aborted capture. This can be detected in
            ;; after-finalize hook with the `org-note-abort' flag set to t in
            ;; `org-capture-kill'.
            (if org-note-abort
                (when on-abort
                  (funcall on-abort))
              ;; More often than not org-node doens't register a newly created
              ;; node so I need to rerun the scan. Org-mem is fast, but this is
              ;; still very wasteful.
              ;; TODO Figure out how to prevent doing a full rescan
              (org-mem-reset)
              (when on-success
                (funcall on-success attach-dir))))))

    ;; See `org-attach-method'
    (cl-assert (member method '(mv cp ln lns)))

    ;; Set the target.
    (setq target
          (cond
           ((stringp target)
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
            (list 'file (org-ilm--select-collection-file target)))
           (t target)))

    ;; Generate title from content if no title provided
    (when (and (not title) content)
      (setq title (org-ilm--generate-text-snippet content)))

    ;; Set extension from file when set to t
    (when (eq ext t)
      (if file
          (setq ext (file-name-extension file))
        (error "Cannot infer extension when no file provided (:ext=t)")))

    ;; Save content in a temporary file if no file provided
    (when (and (not file) content)
      (setq file (expand-file-name
                  (format "%s.%s" id (or ext "org"))
                  temporary-file-directory)
            method 'cp)

      ;; TODO set MUSTBENEW to t?
      (write-region content nil file))

    (unless template
      (setq template
            (format "* %s%s%s %s"
                    state
                    (if priority (format " [#%s]" priority) "")
                    (if title (concat " " title) "")
                    "%?")))

    (cl-letf (((symbol-value 'org-capture-templates)
               (list
                (list
                 "i" "Import" 'entry target template
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
                   (org-node-nodeify-entry)

                   ;; Attachment extension if specified
                   (when ext
                     (org-entry-put nil "ILM_EXT" ext))

                   ;; Additional properties
                   (when props
                     (cl-loop for (p v) on props by #'cddr
                              do (org-entry-put nil (if (stringp p) p (substring (symbol-name p) 1)) v)))

                   ;; Scheduling. We do not add a schedule for cards, as that
                   ;; info is parsed with
                   ;; `org-ilm--srs-earliest-due-timestamp'.
                   (unless (eq type 'card)
                     ;; Set initial schedule data based on priortiy
                     (org-ilm--set-schedule-from-priority)

                     ;; Add advice around priority change to automatically
                     ;; update schedule, but remove advice as soon as capture
                     ;; is finished.
                     (advice-add 'org-priority
                                 :around #'org-ilm--update-from-priority-change)
                     (add-hook 'kill-buffer-hook
                               (lambda ()
                                 (advice-remove 'org-priority
                                                #'org-ilm--update-from-priority-change))
                               nil t))

                   ;; For cards, need to transclude the contents in order for
                   ;; org-srs to detect the clozes.
                   ;; TODO `org-transclusion-add' super slow!!
                   (when (eq type 'card)
                     ;; TODO this can be a nice macro
                     ;; `org-ilm-with-attachment-transcluded'
                     (save-excursion
                       (org-ilm--transclusion-goto file 'create)
                       (org-transclusion-add)
                       (org-srs-item-new 'ilm-cloze)
                       (org-ilm--transclusion-goto file 'delete))))
                 :before-finalize before-finalize
                 :after-finalize after-finalize))))
      (org-capture nil "i"))))

;;;; Org attachment

(defun org-ilm-org-extract ()
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
           (region-text (org-ilm--buffer-text-process region-begin region-end))
           (extract-org-id (org-id-new)))

      (org-ilm--capture
       'extract
       file-org-id
       (list :id extract-org-id :content region-text)
       (lambda (&rest _) ;; on-success

         ;; Wrap region with targets.
         (with-current-buffer file-buf
           (save-excursion
             (let* ((is-begin-line (or (= region-begin 0)
                                       (eq (char-before region-begin) ?\n)))
                    (target-template (format "<<extract:%s:%s>>" "%s" extract-org-id))
                    (target-begin (concat
                                   (format target-template "begin")
                                   ;; Region starts at beginning of line: insert
                                   ;; on new line above
                                   (when is-begin-line "\n")))
                    (target-end (format target-template "end")))
               ;; Insert target before region
               (goto-char region-begin)
               (insert target-begin)

               ;; Insert target after region, adjusting for start target length
               (goto-char (+ region-end (length target-begin)))
               (insert target-end)))
           (save-buffer)
           (org-ilm-recreate-overlays))

         (org-ilm--attachment-priority-update 'extract))))))

(defun org-ilm-cloze ()
  "Create a cloze card.

A cloze is made automatically of the element at point or active
region. If instead a grouped set of clozes must be made, you can first
call `org-ilm-cloze-toggle-this' a couple times before calling this
command."
  (interactive)

  (let* ((file-buf (current-buffer))
         (card-org-id (org-id-new))
         (file-org-id (file-name-sans-extension
                       (file-name-nondirectory
                        buffer-file-name)))
         (clozes (org-srs-item-cloze-collect))
         buffer-text snippet auto-clozed)

    ;; When no clozes have been made, make cloze of element at point or active
    ;; region. We set the flag auto-clozed to t, so that when capture is
    ;; aborted, we can toggle it back.
    (unless clozes
      (org-srs-item-cloze-dwim)
      (setq clozes (org-srs-item-cloze-collect))
      (setq auto-clozed t))
    (cl-assert clozes)

    ;; The buffer text we export should be cleaned of targets but not of clozes.
    ;; The text we use to make a snippet should not contain clozes as well.
    ;; TODO Inefficient, snippet should be cleaned afterwards
    (setq buffer-text (org-ilm--buffer-text-process nil nil t)
          snippet (org-ilm--generate-text-snippet (org-ilm--buffer-text-process)))

    (org-ilm--capture
     'card
     file-org-id
     (list :id card-org-id :content buffer-text :title snippet)
     (lambda (&rest _)
       ;; Success callback. Go through each cloze in the source file and replace
       ;; it with our target tags. Render them by recreating overlays.
       (with-current-buffer file-buf
         (save-excursion
           ;; TODO similar functionality `org-ilm--buffer-text-process', extract
           ;; to own function.
           (while (let ((clz (org-srs-item-cloze-collect))) clz)
             (let* ((cloze (car (org-srs-item-cloze-collect)))
                    (region-begin (nth 1 cloze))
                    (region-end   (nth 2 cloze))
                    (inner (substring-no-properties (nth 3 cloze)))
                    (target-template (format "<<card:%s:%s>>" "%s" card-org-id))
                    (target-begin (format target-template "begin"))
                    (target-end (format target-template "end")))
               (goto-char region-begin)
               (delete-region region-begin region-end)
               (insert target-begin inner target-end))))
         (save-buffer)
         (org-ilm-recreate-overlays)

         (org-ilm--attachment-priority-update 'card)))
     (lambda ()
       ;; Abort callback. Undo cloze if made automatically by this function.
       (when auto-clozed
         (with-current-buffer file-buf
           (org-srs-item-uncloze-dwim)))))))


;;;; PDF attachment

;; TODO keymap for pdf. Prefix A(ttachment) or I(lm)
;; I default bound to popup info buffer but i dont find that useful
;; But A better because in image mode, A also unused.

;; Nice functions/macros:
;; pdf-util-track-mouse-dragging
;; pdf-view-display-region
;; pdf-virtual-document-page: normalized virtual page + range -> actual

(defvar-keymap org-ilm-pdf-map
  :doc "Keymap for PDF attachments."
  "d" #'org-ilm-pdf-open-full-document
  "x" #'org-ilm-pdf-extract
  "c" #'org-ilm-pdf-convert
  "n" #'org-ilm-pdf-toggle-narrow)

(defcustom org-ilm-pdf-minimum-virtual-page-size '(1 . 1)
  "The minimum size of the area to create a virtual PDF extract."
  :type '(cons (symbol :tag "Width")
               (symbol :tag "Height"))
  :group 'org-ilm)

(defcustom org-ilm-pdf-convert-org-respect-area t
  "When virtual view is an area of a single page, convert just that area.

This is done by converting the area to an image first. Note that this will likely effect the quality of the conversion output, probably for the worse if it contains complex objects other than text."
  :type 'boolean
  :group 'org-ilm)

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

;;;;; Utilities

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

(defmacro org-ilm--pdf-with-point-on-collection-headline (of-document &rest body)
  "Place point on the headline belonging to this PDF attachment.
When OF-DOCUMENT non-nil, jump to headline which has the orginal document as attachment."
  `(let* ((virtual-page (pdf-view-current-page))
          (document (pdf-virtual-document-page virtual-page))
          (pdf-path (nth 0 document))
          (page (nth 1 document))
          (region (nth 2 document)))
     ;; TODO use org-ilm--attachment-data
     (when-let* ((org-id (file-name-base (if ,of-document pdf-path (buffer-name))))
                 (headline (org-ilm--org-headline-element-from-id org-id)))
       (org-with-point-at headline
         ,@body))))

(defun org-ilm--pdf-insert-section-as-extract (section level &optional data)
  (let ((range (alist-get 'range section))
        (depth (alist-get 'depth section))
        (title (concat "Section: "
                       (or (plist-get data :title) (alist-get 'title section)))))
    (org-ilm--pdf-insert-range-as-extract range title (+ level depth) data)))

(defun org-ilm--pdf-insert-range-as-extract (range title level &optional data)
  (let* ((priority (or (plist-get data :priority) .5))
         (interval (or (plist-get data :interval)
                       (org-ilm--schedule-interval-from-priority priority)))
         (org-id (or (plist-get data :id) (org-id-new)))
         (schedule-str (org-ilm--interval-to-schedule-string interval)))
    (insert (make-string level ?*) " INCR " title "\n")
    (insert "SCHEDULED: " schedule-str "\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: %s\n" org-id))
    (insert (format ":PDF_RANGE: %s\n" range))
    (insert ":END:\n")))

(cl-defun org-ilm--pdf-image-export (filename &key region page dir)
  "Export current PDF buffer as an image.

REGION: Note that if in virtual view with region, already exports that region."
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

(cl-defun org-ilm--pdf-path (&key directory-p)
  "Infer the path of the PDF file, regardless if in virtual or not.
When DIRECTORY-P, return directory of the file."
  (let ((path (expand-file-name
               (if (eq major-mode 'pdf-virtual-view-mode)
                   (car (pdf-virtual-document-page 1))
                 buffer-file-name))))
    (if directory-p
        (file-name-directory path)
      path)))

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

;;;;; Data
;; Like outline and stuff

(defun org-ilm--pdf-outline-get (&optional file-or-buffer)
  "Parse outline of the PDF in FILE-OR-BUFFER or current buffer.
Same as `pdf-info-outline' but adds in each section info about the next section at the same or shallower depth."
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
        ;; (unless (and next-item (= (length outline) (1+ index)))
          ;; (setf (alist-get 'next current-item) (1+ index))
          ;; (setq next-item (nth (1+ index) outline)))
        
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
    outline))

(defun org-ilm--pdf-page-normalized (&optional page)
  "If in virtual mode, maps virtual PAGE to document page, else return as is.
When not specified, PAGE is current page."
  (let ((page (or page (pdf-view-current-page))))
    (cond
     ((eq major-mode 'pdf-view-mode) page)
     ((eq major-mode 'pdf-virtual-view-mode)
      (nth 1 (pdf-virtual-document-page page)))
     (t (error "Not in a PDF buffer")))))

(defun org-ilm--pdf-region-normalized (&optional region virtual-page)
  "If in virtual mode, maps virtual REGION to document region, else return as is.
When not specified, REGION is active region."
  (let ((region (or region (car (pdf-view-active-region)))))
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

;;;;; Annotation highlight

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

(defun org-ilm--pdf-parse-spec (spec)
  "Parse SPEC for viewing in virtual PDF buffers."
  (org-ilm--debug "Spec:" spec)
  (let ((file (car spec))
        (first (cadr spec))
        (second (cddr spec))
        type begin-page begin-area end-page end-area)
    (cond
     ;; Page range
     ;; (begin . end)
     ((and (integerp first) (integerp second))
      (setq type 'range-pages
            begin-page first
            end-page second))
     ;; Area
     ;; (page . (left top right bottom))
     ((org-ilm--pdf-area-p (cdr spec))
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
     (t (error "Spec invalid")))

    (list :file file
          :type type
          :begin-page begin-page
          :begin-area begin-area
          :end-page end-page
          :end-area end-area)))

(defun org-ilm--pdf-open-ranges (specs buffer-name &optional no-region)
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
    (cl-destructuring-bind (&key file type begin-page begin-area end-page end-area)
        (org-ilm--pdf-parse-spec spec)
      (insert "(\"" file "\"")
      (pcase type
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

(defun org-ilm--pdf-open-virtual (pdf-path buffer-name)
  "Open PDF in virtual view mode.
The main purpose is to toggle into a widened (this func) and narrowed
view (`org-ilm--pdf-open-ranges')."
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert ";; %VPDF 1.0\n\n")  
    (insert "((" pdf-path "))")
    (pdf-virtual-view-mode)
    (pop-to-buffer (current-buffer))
    (current-buffer)))

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

(defun org-ilm-pdf-convert (output-type)
  "Convert PDF to another format within the same attachment.

See also `org-ilm-pdf-convert-org-respect-area'."
  (interactive
   (list
    (let ((options (seq-filter
                    (lambda (option) (member (cdr option) '(text org image)))
                    (org-ilm--invert-alist org-ilm--pdf-output-types))))
      (cdr (assoc (completing-read "Convert to: " options nil t) options)))))
  (unless (or (eq major-mode 'pdf-view-mode) (eq major-mode 'pdf-virtual-view-mode))
    (user-error "Not in PDF buffer."))
  (unless (org-ilm--attachment-data)
    (user-error "Not in an attachment buffer."))

  (let* ((pdf-buffer (current-buffer))
         (org-id (file-name-base (buffer-name)))
         (headline (org-ilm--org-headline-element-from-id org-id))
         (num-pages (pdf-info-number-of-pages))
         (document-page-1 (pdf-virtual-document-page 1))
         (pdf-path (expand-file-name (car document-page-1)))
         (region (nth 2 document-page-1))
         (attach-dir (file-name-directory pdf-path))
         (out-path-format (expand-file-name (concat org-id ".%s") attach-dir))
         (on-success
          (lambda ()
            (when (yes-or-no-p "Conversion finished. Use as main attachment?")
              (org-with-point-at headline
                (org-entry-put nil "ILM_EXT" "org"))))))

    (pcase output-type
      ('text
       (with-temp-buffer
         (dolist (page (number-sequence 1 num-pages))
           (insert (pdf-info-gettext page '(0 0 1 1) nil pdf-buffer)))
         (write-region (point-min) (point-max) (format out-path-format "org")))
       (funcall on-success))
      ('org
       ;; Decide on whether to convert just the virtual pdf region or the entire
       ;; page.
       (if (and (eq major-mode 'pdf-virtual-view-mode)
                (= 1 num-pages) region
                org-ilm-pdf-convert-org-respect-area)
           (org-ilm--image-convert-attachment-to-org
            (org-ilm--pdf-image-export org-id :dir attach-dir)
            org-id
            :on-success
            (lambda (proc buf id) (funcall on-success)))
         (org-ilm--pdf-convert-attachment-to-org
          pdf-path
          (if (= 1 num-pages)
              (1- (nth 1 (pdf-virtual-document-page 1)))
            (cons (1- (nth 1 (pdf-virtual-document-page 1)))
                  (1- (nth 1 (pdf-virtual-document-page num-pages)))))
          org-id
          :on-success
          (lambda (proc buf id) (funcall on-success)))))
      ('image
       (unless (= 1 num-pages)
         (user-error "Can only convert a single-paged document to an image."))
       (org-ilm--pdf-image-export org-id :dir attach-dir)
       (funcall on-success)))))

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

(defun org-ilm--pdf-extract-prepare ()
  "Groundwork to do an extract."
  (let ((location (org-ilm--where-am-i))
        org-id attach-dir attachment buffer collection headline)

    ;; Handle both when current buffer is PDF or headine in
    ;; collection. Furthermore we need the headline level to indent the outline
    ;; headlines appropriately.
    (cond
     ((eq (car location) 'attachment)
      (cl-assert (or (eq major-mode 'pdf-view-mode)
                     (eq major-mode 'pdf-virtual-view-mode)))
      (setq org-id (nth 0 (cdr location))
            collection (nth 1 (cdr location)))
      (setq attach-dir
            (pcase major-mode
              ('pdf-view-mode
               (file-name-directory buffer-file-name))
              ('pdf-virtual-view-mode
               (org-ilm--org-attach-dir-from-id org-id))
              (_ (error "This attachment is not a PDF")))))
     ((eq (car location) 'collection)
      (setq org-id (org-id-get)
            collection (cdr location)
            attach-dir (org-attach-dir)))
     (t (error "Not in attachment or on collection element.")))
    (setq attachment (concat org-id ".pdf"))
    (if-let ((buf (get-buffer attachment)))
        (setq buffer buf)
      (if (file-exists-p attachment)
          (setq buffer (find-file-noselect attachment))
        (error "PDF file not found (%s)" attachment)))
    (setq headline (org-ilm--org-headline-element-from-id org-id))
    (unless headline
      (error "Collection element not found"))
    (list org-id attach-dir attachment buffer collection headline
          (org-element-property :level headline)
          (org-ilm--interval-to-schedule-string
           (org-ilm--schedule-interval-from-priority .5)))))

(defun org-ilm-pdf-page-extract (output-type)
  "Turn PDF page into an extract."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'page)))
  (cl-destructuring-bind (org-id attach-dir attachment buffer collection headline level schedule-str)
      (org-ilm--pdf-extract-prepare)

    (switch-to-buffer buffer)
    (let ((current-page (pdf-view-current-page))
          (current-page-real (org-ilm--pdf-page-normalized))
          (pdf-path (org-ilm--pdf-path))
          (extract-org-id (org-id-new)))

      (pcase output-type
        ('virtual
         (let ((excerpt (org-ilm--generate-text-snippet
                         (pdf-info-gettext current-page '(0 0 1 1)))))
           (with-temp-buffer
             (org-ilm--pdf-insert-range-as-extract
              current-page-real
              (concat "Page "
                      (number-to-string current-page-real)
                      (when excerpt (concat ": " excerpt))
                      " %?")
              (1+ level))
             (let* ((org-capture-templates
                     `(("c" "Capture" entry (id ,org-id) ,(buffer-string)))))
               (org-capture nil "c")))))
        ('text
         (let* ((text (pdf-info-gettext current-page '(0 0 1 1) nil)))
           (org-ilm--capture 
            'extract
            org-id
            (list :content text :ext "org"))))
        ('image
         (let* ((img-path (org-ilm--pdf-image-export extract-org-id)))
           (org-ilm--capture
            'extract
            org-id
            (list :file img-path
                  :title (format "Page %s" current-page-real)
                  :id extract-org-id
                  :method 'mv
                  :ext t))))
        ('org
         (org-ilm--capture
          'extract
          org-id
          (list 
           :title (format "Page %s" current-page-real)
           :id extract-org-id
           :ext "org")
          (lambda (&rest _)
            (org-ilm--pdf-convert-attachment-to-org
             pdf-path
             (1- current-page-real)
             extract-org-id
             :on-success
             (lambda (proc buf id) (message "Conversion finished."))))))))))

(defun org-ilm-pdf-region-extract (output-type)
  "Turn selected PDF region into an extract.

TODO When extracting text, use add-variable-watcher to watch for changes
in pdf-view-active-region as it has no hooks. it allow buffer local and
set only (not let)."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'region)))
  (unless (pdf-view-active-region-p)
    (user-error "No active region."))
  
  (cl-destructuring-bind (org-id attach-dir attachment buffer collection headline level schedule-str)
      (org-ilm--pdf-extract-prepare)

    ;; Orginally used `with-current-buffer' but throws an error on virtual page
    ;; buffers when calling `pdf-view-current-page' (something to do with the
    ;; `save-buffer' or `set-buffer' call in `with-current-buffer'). Since it
    ;; works fine when virtual page buffer is active, just do that.
    (switch-to-buffer buffer)
    (let* ((current-page-real (org-ilm--pdf-page-normalized))
           (current-page (pdf-view-current-page))
           (region (org-ilm--pdf-region-normalized))
           (region-text (car (pdf-view-active-region-text)))
           (pdf-buffer (current-buffer))
           (title
            (concat
             "Page " (number-to-string current-page-real)
             " region"
             (when region-text (concat ": " (org-ilm--generate-text-snippet region-text))) ))
           (extract-org-id (org-id-new))
           capture-data capture-on-success)

      ;; TODO Would be nice to prompt for other output-type and repeat the
      ;; function from here. Can do so with a while loop around the pcase
      ;; below. Could then be generalized to macro so I can reuse it in all
      ;; extract functions.
      (when (org-ilm--pdf-region-below-min-size-p current-page region)
        (user-error "Region smaller than minimum size. Try extracting as text or image."))

      (pcase output-type
        ('virtual
         (let (org-headline-content)
           (with-temp-buffer
             (org-ilm--pdf-insert-range-as-extract
              (cons current-page-real region)
              (concat title "%?")
              (1+ level)
              (list :id extract-org-id))
             (setq capture-data (list :template (buffer-string))))))
        ('text
         (setq capture-data
               (list :id extract-org-id
                     :title title
                     :content region-text
                     :ext "org")))
        ('image
         (setq capture-data
               (list :file (org-ilm--pdf-image-export extract-org-id :region region)
                     :method 'mv
                     :title title
                     :id extract-org-id
                     :ext t)))
        ('org
         (setq capture-data
               (list :title title :ext "org" :id extract-org-id))
         (setq capture-on-success
               (lambda ()
                 (org-ilm--image-convert-attachment-to-org
                  ;; TODO this shouldnt be the normalized region i think
                  (org-ilm--pdf-image-export
                   extract-org-id :region region :dir attach-dir)
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
        (org-ilm--capture
         'extract
         org-id
         capture-data
         (lambda (&rest _)
           (when capture-on-success
             (funcall capture-on-success))
           (org-ilm--pdf-add-square-annotation
            current-page-real region extract-org-id)
           (when (eq major-mode 'pdf-virtual-view-mode)
             (org-ilm-pdf-virtual-refresh))))))))

(defun org-ilm-pdf-outline-extract ()
  "Turn PDF outline items into a extracts.

It's a bit of a black sheep compared to other extract options because we
make a bunch of headers."
  (interactive)
  (cl-destructuring-bind (org-id attach-dir attachment buffer collection headline level schedule-str)
      (org-ilm--pdf-extract-prepare)

    (let ((outline (org-ilm--pdf-outline-get buffer))
          org-outline)
      (unless outline (error "No outline found"))
      
      (with-temp-buffer
        (dolist (section outline)
          (org-ilm--pdf-insert-section-as-extract section level))
        (setq org-outline (buffer-string)))

      (let* ((org-capture-templates
              `(("c" "Capture" entry (id ,org-id) ,org-outline))))
        (org-capture nil "c")))))

(defun org-ilm-pdf-section-extract (output-type)
  "Extract current section of outline."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'section)))
  (cl-destructuring-bind (org-id attach-dir attachment buffer collection headline level schedule-str)
      (org-ilm--pdf-extract-prepare)
    (let (section)
      (switch-to-buffer buffer)

      ;; Get section
      (let ((outline (org-ilm--pdf-outline-get buffer)) ; TODO pass orginial file if in virtual
            (num-pages (pdf-info-number-of-pages))
            (current-page (org-ilm--pdf-page-normalized))
            outline-index org-heading-str)
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
            (setq outline-index (car within-indices)))
           ((> (length within-indices) 1)
            (let* ((choices (mapcar (lambda (i)
                                      (cons (alist-get 'title (nth i outline)) i))
                                    within-indices))
                   (choice (cdr (assoc (completing-read "Section: " choices nil t) choices))))
              (setq outline-index choice)))
           (t (error "Current page not within (known) section"))))
        
        (setq section (nth outline-index outline)))

      (pcase output-type
        ('virtual
         (with-temp-buffer
           (org-ilm--pdf-insert-section-as-extract section level)

           (let* ((org-capture-templates
                   `(("c" "Capture" entry (id ,org-id) ,(buffer-string)))))
             (org-capture nil "c"))))
        ('text
         ;; TODO This currently extracts the actual PDF data lol
         (with-temp-buffer
           (let ((range (alist-get 'range section)))
             (if (= (length range) 2)
                 (insert (pdf-info-gettext (car range) (cdr range) nil buffer) "\n")
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
                            'word buffer)
                           "\n")))))

           (org-ilm--capture
            'extract
            org-id
            (list :content (buffer-string)
                  :title (concat "Section: " (alist-get 'title section))
                  :ext "org"))))
        ('org
         (let ((pdf-path (org-ilm--pdf-path))
               (extract-org-id (org-id-new)))
           (org-ilm--capture
            'extract
            org-id
            (list 
             :title (concat "Section: " (alist-get 'title section))
             :id extract-org-id
             :ext "org")
            (lambda (&rest _)
              (org-ilm--pdf-convert-attachment-to-org
               pdf-path
               (cons (1- (alist-get 'page section))
                     (1- (alist-get 'next-page section)))
               extract-org-id
               :on-success
               (lambda (proc buf id) (message "Conversion finished.")))))))))))

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
  "Returns (org-id collection) if current buffer is collection attachment file."
  (when-let* ((file-title (file-name-base
                           ;; Allow for non-file buffer: pdf virtual view
                           (or buffer-file-name (buffer-name))))
              (entry (org-mem-entry-by-id file-title))
              (src-file (org-mem-entry-file entry))
              (src-file (expand-file-name src-file)) ;; sep line, else err
              (collection (org-ilm--collection-file src-file)))
    ;; Exclude registries
    (unless (seq-some (lambda (r) (string= (expand-file-name r) src-file)) org-registry-registries) 
      (list file-title collection))))

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
  (cond
   ;; Check if there is an attachment org-id.ext where org-id is current
   ;; headline's id and ext is org by default or ILM_EXT property
   ((when-let ((path (org-ilm--attachment-find)))
      (progn
        (run-hook-with-args 'org-attach-open-hook path)
        (find-file path))))
   ;; Check if headline represents a virtual view of a parent PDF element by
   ;; looking at the PDF_RANGE property.
   ((when-let* ((pdf-range (org-entry-get nil "PDF_RANGE"))
                ;; Returns 0 if not a number
                (pdf-page-maybe (string-to-number pdf-range))
                (attachment (org-ilm--attachment-find-ancestor "pdf"))
                (buffer-name (concat (org-id-get) ".pdf")))
      (if (not (= pdf-page-maybe 0))
          (org-ilm--pdf-open-page attachment pdf-page-maybe buffer-name)
        (org-ilm--pdf-open-ranges
         (list (cons attachment (org-ilm--pdf-range-from-string pdf-range)))
         buffer-name
         pdf-no-region))))
   ;; Check if attachment is in the process of being generated with a conversion
   ;; tool.
   ((when-let* ((org-id (org-id-get))
                (conversion (seq-find
                             (lambda (conversion)
                               (string= (plist-get conversion :id) org-id))
                             (convtools--conversions))))
      (let ((message (pcase (plist-get conversion :state)
                       ;; TODO for success and error, provide option to
                       ;; delete headline and extract highlight in parent
                       ;; attachment.
                       ('success "Attachment finished conversion but not found.")
                       ('error "Attachment conversion failed.")
                       (_ "Attachment still being converted."))))
        (when (yes-or-no-p (concat message " View conversion buffer?"))
          (pop-to-buffer (plist-get conversion :buffer))))))
   ;; Check if there is a web link in the ROAM_REFS property and open website in
   ;; eww.
   ((when-let* ((web-refs (mochar-utils--org-mem-website-refs))
                (web-ref (if (= 1 (length web-refs))
                             (car web-refs)
                           (completing-read "Open: " web-refs nil t))))
      ;; We use window excursion so that we can return the eww buffer
      (save-window-excursion
        (eww-browse-url web-ref))
      (switch-to-buffer "*eww*")))
   (t (unless no-error (user-error "Attachment not found")))))
  
(defun org-ilm--attachment-open-by-id (id)
  (org-ilm--org-with-point-at id
    (org-ilm--attachment-open)))

(defun org-ilm--attachment-prepare-buffer ()
  "Prepare ilm attachment buffers."
  (when-let ((data (org-ilm--attachment-data)))
    (pcase-let* ((`(,id ,collection) data)
                 (element (org-ilm--org-with-point-at id
                            (org-ilm-element-at-point))))

      ;; Prepare the buffer local data object which contains info about the
      ;; attachment as well as data used to update the priority.
      (setq-local
       org-ilm--data
       (list :id id
             :collection collection
             ;; TODO porbably remove and just use `org-ilm-element-from-id' to
             ;; get most recent
             :element element
             :beta (org-ilm--priority-to-beta
                    (org-ilm-element-prelative element))
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
        ))))

(defun org-ilm--attachment-ensure-data-object ()
  "Ensure `org-ilm--object' is initialized properly.

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

;;;; Targets

;; Targets refer to the anchor points used to highlight extracted or clozed
;; sections of text in org attachments. It looks like this:
;; <<{type}:begin:{id}>>bla bla<<{type}:end:{id}>>
;; Type can be 'card' or 'extract'.
;; They are called targets because that's what Org mode calls them. Targets are
;; part of the Org mode spec and are therefore parse by 'org-element'.

;; When an Org mode file is loaded that is recognized as an ilm attachment, the
;; buffer is parsed for target pairs. For each pair, three overlays are created:
;; one for each target element to hide it, and one that encapsulates the whole
;; target region to visually indicate it with a color.

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
         (pos-string (if find-begin "begin" "end")))
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

;;;; Overlays

(defun org-ilm--ov-block-edit (ov after beg end &optional len)
  (unless (or after org-ilm--targets-editable)
    (user-error "Cannot modify this region")))

(defun org-ilm--create-overlay (target-begin target-end &optional no-face)
  "Create an overlay to hide the target markers and highlight the target text."
  ;; Hide targets
  (dolist (target (list target-begin target-end))
    (let ((begin (plist-get target :begin))
          (end (plist-get target :end)))
      (let ((ov (make-overlay
                 begin
                 ;; If next character is newline, include it.  Otherwise hitting
                 ;; backspace on header will look like header is still on
                 ;; newline but is in fact after the target.
                 (if (eq (char-after end) ?\n) (+ end 1) end)
                 nil t t)))
        (overlay-put ov 'org-ilm-target t)
        (overlay-put ov 'invisible t)
        (overlay-put ov 'modification-hooks '(org-ilm--ov-block-edit))
        )
    ))
  
  ;; Highlight region
  (let* ((id (plist-get target-begin :id))
         (type (plist-get target-begin :type))
         (face (pcase type
                 ("extract" 'org-ilm-face-extract)
                 ("card" 'org-ilm-face-card)))
         (ov (make-overlay
             (plist-get target-begin :begin)
             (plist-get target-end :end))))
    (unless no-face
      (overlay-put ov 'face face))
    (overlay-put ov 'org-ilm-highlight t)
    (overlay-put ov 'org-ilm-type type)
    (overlay-put ov 'org-ilm-id id)
    (overlay-put ov 'help-echo (format "Highlight %s" id))))

(defun org-ilm-remove-overlays ()
  ""
  (interactive)
  (org-with-wide-buffer
   (remove-overlays (point-min) (point-max) 'org-ilm-highlight t)
   (remove-overlays (point-min) (point-max) 'org-ilm-target t)))

;; TODO Accept begin and end position so that new overlays can be processed by
;; narrowing to just that region, without having to redo the whole file.
(defun org-ilm-recreate-overlays (&optional no-face)
  ""
  (interactive)
  (org-ilm-remove-overlays)
  (org-with-wide-buffer
   (let ((begin-targets (make-hash-table :test 'equal)))
     (org-element-map (org-element-parse-buffer) 'target
       (lambda (target-element)
         (when-let* ((target (org-ilm--target-parse-element target-element))
                     (target-id (plist-get target :id)))
           (when (member (plist-get target :type) '("extract" "card"))
             (pcase (plist-get target :pos)
               ("begin" (puthash target-id target begin-targets))
               ("end"
                (when-let ((begin-target (gethash target-id begin-targets)))
                 (progn
                   (org-ilm--create-overlay begin-target target no-face)
                   ;; TODO Do we need to remove it?
                   (remhash target-id begin-targets))))))))))))

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

(defun org-ilm--ql-card-due ()
  "Check if org-srs earliest card due today, optimized for org-ql query."
  (when-let ((due (org-ql--value-at (point) #'org-ilm--srs-earliest-due-timestamp)))
    (ts<= due (ts-now))))

(defun org-ilm-query-collection (collection query)
  "Apply org-ql QUERY on COLLECTION, parse org-ilm data, and return the results."
  (let ((files (org-ilm--collection-files collection)))
    (org-ql-select files
      (funcall (cdr (assoc query org-ilm-queries)))
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
      (cons 'todo org-ilm-subject-states)
      :action #'org-ilm-element-at-point)))

(defun org-ilm-query-all ()
  "Query for org-ql to retrieve all elements."
  `(or ,(cons 'todo org-ilm-incr-states) ,(cons 'todo org-ilm-card-states)))

(defun org-ilm-query-outstanding ()
  "Query for org-ql to retrieve the outstanding elements."
  `(or
    (and ,(cons 'todo org-ilm-incr-states) (scheduled :to today))
    (and ,(cons 'todo org-ilm-card-states) (org-ilm--ql-card-due))))

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

;;;;; Building a queue buffer

;; Queues are stored locally within each queue buffer.

(defun org-ilm--queue-build (&optional collection query)
  "Build a queue and return it."
  (let* ((collection (or collection
                         (org-ilm--collection-from-context)
                         (car (org-ilm--select-collection))))
         (query (or query (car (org-ilm--query-select))))
         (elements (org-ilm-query-collection collection query))
         (element-map (make-hash-table :test #'equal))
         (ost (make-ost-tree)))
    (dolist (element elements)
      (puthash (org-ilm-element-id element) element element-map)
      (ost-tree-insert ost (org-ilm-element-psample element)
                       (org-ilm-element-id element)))
    (list
     :elements element-map
     :ost ost
     :collection collection
     :query query)))

;; TODO I think this should be removed
(cl-defun org-ilm-queue-build (&key buffer select-collection)
  "Built the queue and store it in a queue buffer, which is returned."
  (interactive "P")

  (org-ilm-with-queue-buffer buffer
    (let ((collection (if select-collection
                          (car (org-ilm--select-collection))
                        (or
                         (plist-get org-ilm-queue :collection)
                         (car (org-ilm--select-collection)))))
          (query (or (plist-get org-ilm-queue :query) (car (org-ilm--query-select)))))
      (setq org-ilm-queue (org-ilm--queue-build collection query))
      (current-buffer))))

(cl-defun org-ilm--queue-buffer-create (queue &key active-p switch-p)
  "Create a new queue buffer."
  (let ((buffer (generate-new-buffer
                 (format "*Ilm Queue (%s|%s)*"
                         (plist-get queue :collection)
                         (plist-get queue :query)))))
    (with-current-buffer buffer
      (setq-local org-ilm-queue queue)

      ;; Make sure that when the queue buffer is killed we update the active
      ;; buffer.
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (when (eq (current-buffer) org-ilm-queue-active-buffer)
                    (org-ilm-queue--set-active-buffer nil)))
                nil 'local)

      ;; Refresh when queue popped during review
      (add-hook 'org-ilm-review-next-hook
                #'org-ilm-queue-revert nil t)
      (add-hook 'org-ilm-review-quit-hook
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
  (run-hooks 'org-ilm-queue-active-buffer-change-hook))

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

(defun org-ilm-queue (new)
  "Build a queue and view it in Agenda-like buffer."
  (interactive "P")
  (if new
      (org-ilm--queue-buffer-create
       (org-ilm--queue-build) :switch-p t)
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
                 (org-ilm--queue-build)
                 :active-p t :switch-p t)
              (with-current-buffer (switch-to-buffer choice)
                (org-ilm-queue--set-active-buffer (current-buffer)))))
        ;; If outside a queue buffer and there are no active or inactive queue
        ;; buffers, make one and make it active.
        (org-ilm--queue-buffer-create
         (org-ilm--queue-build)
         :active-p t :switch-p t))))))

;;;;; Queue operations

(cl-defun org-ilm-queue-insert (element &key buffer exists-ok)
  "Insert ELEMENT into the queue.
When EXISTS-OK, don't throw error if ELEMENT already in queue."
  (cl-assert (org-ilm-element-p element))
  (org-ilm-with-queue-buffer buffer
    (let* ((priority (org-ilm-element-psample element))
           (id (org-ilm-element-id element))
           (exists (gethash id (plist-get org-ilm-queue :elements))))
      (if exists
          (unless exists-ok
            (error "Element already in queue (%s)" (org-ilm-element-id element)))
        (ost-tree-insert (plist-get org-ilm-queue :ost) priority id)
        (puthash id element (plist-get org-ilm-queue :elements))
        t))))

(cl-defun org-ilm-queue-count (&key buffer)
  "Return number of elements in the queue."
  (org-ilm-with-queue-buffer buffer
    (hash-table-count (plist-get org-ilm-queue :elements))))

(cl-defun org-ilm-queue-empty-p (&key buffer)
  (org-ilm-with-queue-buffer buffer
    (= 0 (org-ilm-queue-count))))

;; TODO Some type of caching so that repeated selects doesnt search tree again
;; Should be implemented in `ost-tree-select'.
(cl-defun org-ilm-queue-select (index &key buffer)
  (org-ilm-with-queue-buffer buffer
    (let ((node (ost-select (plist-get org-ilm-queue :ost) index)))
      (gethash (ost-node-id node) (plist-get org-ilm-queue :elements)))))

(cl-defun org-ilm-queue-top (&key n buffer)
  (org-ilm-with-queue-buffer buffer
    (unless (org-ilm-queue-empty-p)
      (mapcar
       #'org-ilm-queue-select
       (number-sequence 0 (or n (1- (org-ilm-queue-count))))))))

(cl-defun org-ilm-queue-head (&key buffer)
  (org-ilm-queue-select 0 :buffer buffer))

(cl-defun org-ilm-queue-elements (&key ordered buffer)
  "Return elements in the queue."
  (org-ilm-with-queue-buffer buffer
    (if ordered
        (error "Todo")
      (hash-table-values (plist-get org-ilm-queue :elements)))))

(cl-defun org-ilm-queue-remove (id &key buffer)
  (org-ilm-with-queue-buffer buffer
    (if-let ((element (gethash id (plist-get org-ilm-queue :elements))))
        (progn
          (ost-tree-remove (plist-get org-ilm-queue :ost) id)
          (remhash id (plist-get org-ilm-queue :elements))
          element)
      (error "Element not in queue %s" id))))

(cl-defun org-ilm-queue-pop (&key buffer)
  "Remove the top most element in the queue."
  (org-ilm-with-queue-buffer buffer
    (when (org-ilm-queue-empty-p)
      (error "Can't pop an empty queue"))
    (let ((top (org-ilm-queue-head)))
      (org-ilm-queue-remove (org-ilm-element-id top)))))

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
              (type (org-ilm-type headline)))
    (pcase type
      ('subj
       ;; TODO
       )

      ('incr
       (save-excursion
         (org-back-to-heading t)
         (let ((n-added 0)
               (end (save-excursion (org-end-of-subtree t)))
               el)
           (while (< (point) end)
             (when (setq el (org-ilm-element-at-point))
               (when (org-ilm-queue-insert el :exists-ok t)
                 (cl-incf n-added))
               (outline-next-heading)))
           (message "Added %s element(s) to the queue" n-added)))))))

;;;;; Queue view

(defvar org-ilm-queue-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")
                (lambda ()
                  (interactive)
                  (next-line)
                  (when (eobp) (previous-line))))
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "b") #'backward-char)
    (define-key map (kbd "f") #'forward-char)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "k")
                (lambda ()
                  (interactive)
                  (kill-buffer (current-buffer))))
    (define-key map (kbd "r") #'org-ilm-review-start)
    (define-key map (kbd "m") #'org-ilm-queue-object-mark)
    (define-key map (kbd "g") #'org-ilm-queue-revert)
    (define-key map (kbd "RET") #'org-ilm-queue-open-attachment)
    (define-key map (kbd "SPC") #'org-ilm-queue-open-element)
    (define-key map (kbd "M j") #'org-ilm-queue-mark-by-subject)
    (define-key map (kbd "M :") #'org-ilm-queue-mark-by-tag)
    (define-key map (kbd "M s") #'org-ilm-queue-mark-by-scheduled)
    (define-key map (kbd "M u") #'org-ilm-queue-mark-unmark-all)
    ;; TODO r: Review start command
    ;; TODO G: query again - undo manual changes
    ;; TODO C-u G: like G but also select collection
    ;; TODO B: bulk commands
    map)
  "Keymap for the queue buffer.")

(defvar-local org-ilm--queue-marked-objects nil
  "Org id of marked objects in queue.")

;; TODO This is ineffcient. Instead we should give vtable the selected-ed
;; objects directly.
(defun org-ilm--vtable-get-object (&optional index)
  "Return queue object at point or by index."
  ;; when-let because index can be nil when out of table bounds
  (when-let ((index (or index (vtable-current-object))))
    (org-ilm-queue-select index)))

(defun org-ilm-queue-open-attachment (object)
  "Open attachment of object at point."
  (interactive (list (org-ilm--vtable-get-object)))
  (org-ilm--attachment-open-by-id (org-ilm-element-id object)))

(defun org-ilm-queue-open-element (object)
  "Open attachment of object at point."
  (interactive (list (org-ilm--vtable-get-object)))
  (org-ilm--org-goto-id (org-ilm-element-id object)))

(defun org-ilm-queue-object-mark (object)
  "Mark the object at point."
  (interactive (list (org-ilm--vtable-get-object)))
  (let ((id (org-ilm-element-id object)))
    (if (member id org-ilm--queue-marked-objects)
        ;; Only toggle if called interactively
        (when (called-interactively-p)
          ;; not sure why but inconsistent behavior when not setq, even though
          ;; cl-delete is meant to remove destructively.
          (setq org-ilm--queue-marked-objects
                (cl-delete id org-ilm--queue-marked-objects :test #'equal)))
      (cl-pushnew id org-ilm--queue-marked-objects :test #'equal))
    ;; TODO gives cache error no idea why
    ;; Maybe worked on?: https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-07/msg00802.html
    ;; (vtable-update-object (vtable-current-table) object)
    (org-ilm-queue-revert))
  (when (called-interactively-p)
    (next-line)
    (when (eobp) (previous-line))))

(defun org-ilm-queue-mark-by-subject (subject-id)
  "Mark all elements in queue that are part of subject with id SUBJECT-ID."
  (interactive
   (list
    (org-mem-entry-id (org-ilm--subject-select))))

  ;; Alternatively, we could have used
  ;; `org-ilm--subjects-get-with-descendant-subjects' to precompute the
  ;; descendancy, but this would require a list-to-list comparison eg with
  ;; `seq-some' per object, instead of just an `assoc'.
  (dolist (object (org-ilm-queue-elements))
    (let ((ancestor-ids (mapcar #'car (car (org-ilm-element-subjects object)))))
      (when (member subject-id ancestor-ids)
        (org-ilm-queue-object-mark object)))))

(defun org-ilm-queue-mark-by-tag (tag)
  "Mark all elements in queue that have tag TAG."
  (interactive
   (list
    (completing-read
     "Tag: "
     (with-current-buffer (find-file-noselect (cdr (plist-get org-ilm-queue :collection)))
       (org-get-buffer-tags)))))
  (dolist (object (org-ilm-queue-elements))
    (when (member tag (org-ilm-element-tags object))
      (org-ilm-queue-object-mark object))))

(defun org-ilm-queue-mark-by-scheduled (days)
  "Mark all elements in queue that are scheduled in DAYS days.
DAYS can be specified as numeric prefix arg."
  (interactive "N")
  (dolist (object (org-ilm-queue-elements))
    (when-let ((due (* -1 (org-ilm-element-schedrel object))))
      (when (<= (round due) days) 
        (org-ilm-queue-object-mark object)))))

(defun org-ilm-queue-mark-unmark-all ()
  "Unmark all marked elements."
  (interactive)
  (setq-local org-ilm--queue-marked-objects nil)
  (org-ilm-queue-revert))

(defun org-ilm-queue--set-header ()
  (setq header-line-format
        (concat
         (symbol-name (plist-get org-ilm-queue :query))
         " ("
         (symbol-name (plist-get org-ilm-queue :collection))
         ")")))

(defun org-ilm-queue-revert (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(4))   ;; C-u
    (when (yes-or-no-p "Query again?")
      (org-ilm-queue-build)))
   ((equal arg '(16))  ;; C-u C-u
    (org-ilm-queue-build 'select-collection)))
  (vtable-revert-command)
  (org-ilm-queue--set-header))

(defun org-ilm--vtable-format-marked (string marked)
  (if marked
      (propertize string 'face '(:inherit default :slant italic :underline t))
    string))
  
(defun org-ilm--queue-make-vtable ()
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
        (pcase-let* ((`(,marked ,index) data)
                     ;; (index-str (format "%d" index)))
                     (index-str (format "%4d" (1+ index))))
          (org-ilm--vtable-format-marked index-str marked))))
     (:name
      "Priority"
      :width 6
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,psample ,prelative ,pcookie) data))
          (when psample
            (org-ilm--vtable-format-marked
             (propertize (format "%.2f" (* 100 psample))
                         'face 'shadow)
             marked)))))
     (:name
      "Type"
      :width 4
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,type) data))
          (org-ilm--vtable-format-marked
           (org-ql-view--add-todo-face (upcase (symbol-name type)))
           marked))))
     ;; (:name
     ;;  "Cookie"
     ;;  :width 4
     ;;  :formatter
     ;;  (lambda (data)
     ;;    (pcase-let ((`(,marked ,p) data))
     ;;      (org-ilm--vtable-format-marked
     ;;       (org-ql-view--add-priority-face (format "[#%s]" p))
     ;;       marked))))
     (:name
      "Title"
      :min-width "50%"
      :max-width "55%"
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,title) data))
          (org-ilm--vtable-format-marked title marked))))
     (:name
      "Due"
      :max-width 8
      ;; :align 'right
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,due) data))
          (let ((due-str (if due
                             (org-add-props
                                 (org-ql-view--format-relative-date
                                  (round due))
                                 nil 'face 'org-ql-view-due-date)
                           "")))
            (org-ilm--vtable-format-marked due-str marked)))))
     ;; (:name
     ;;  "Tags"
     ;;  :formatter
     ;;  (lambda (data)
     ;;    (pcase-let ((`(,marked ,tags) data))
     ;;      (if tags
     ;;          (org-ilm--vtable-format-marked
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
        (pcase-let ((`(,marked ,subjects) data))
          (if subjects
              (let ((names
                     (mapcar
                      (lambda (subject)
                        (let ((title (or
                                      (car (last (org-mem-entry-roam-aliases subject)))
                                      (org-mem-entry-title subject))))
                          (substring title 0 (min (length title)
                                                  org-ilm-queue-subject-nchar))))
                      subjects)))
                (org-ilm--vtable-format-marked
                 (org-add-props (s-join "," names) nil 'face 'org-tag)
                 marked))
                "")))))
   :objects-function
   (lambda ()
     (unless (org-ilm-queue-empty-p)
       (number-sequence 0 (1- (org-ilm-queue-count)))))
   :getter
   (lambda (row column vtable)
     (let* ((object (org-ilm--vtable-get-object row))
            (id (org-ilm-element-id object))
            (marked (member id org-ilm--queue-marked-objects))
            (subjects (org-ilm-element-subjects object)))
       (pcase (vtable-column vtable column)
         ("Index" (list marked row))
         ("Type" (list marked (org-ilm-element-type object)))
         ("Priority" (list marked
                           (org-ilm-element-psample object)
                           (org-ilm-element-prelative object)
                           (org-ilm-element-pcookie object)))
         ;; ("Cookie" (list marked (org-ilm-element-pcookie object)))
         ("Title" (list marked (org-ilm-element-title object)))
         ("Due" (list marked (org-ilm-element-schedrel object)))
         ;; ("Tags" (list marked (org-ilm-element-tags object)))
         ("Subjects"
          (list marked
                (when (nth 0 subjects)
                  (mapcar (lambda (s) (org-mem-entry-by-id (car s)))
                          (last (nth 0 subjects) (nth 1 subjects)))))))))
   :keymap org-ilm-queue-map))

(defun org-ilm--queue-eldoc-show-info ()
  "Return info about the element at point in the queue buffer."
  (when-let* ((element (org-ilm--vtable-get-object)))
    (propertize (format "[#%s]" (org-ilm-element-pcookie element))
                'face 'org-priority)))

(cl-defun org-ilm--queue-buffer-build (&key buffer switch-p)
  "Build the contents of the queue buffer, and optionally switch to it."
  (org-ilm-with-queue-buffer buffer
    (when (org-ilm-queue-empty-p)
      (org-ilm-queue-build)
      (when (org-ilm-queue-empty-p)
        (user-error "Queue is empty!")))
    
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (vtable-insert (org-ilm--queue-make-vtable))
    (org-ilm-queue--set-header)
    (setq-local buffer-read-only t)
    (hl-line-mode 1)
    (eldoc-mode 1)
    (setq-local eldoc-documentation-function #'org-ilm--queue-eldoc-show-info)
    (goto-char (point-min))
    (when switch-p
      (switch-to-buffer buffer))))


;;;; Import

(transient-define-infix org-ilm--import-transient-collection ()
  :class 'transient-lisp-variable
  :variable 'org-ilm--active-collection
  :transient t
  :allow-empty nil
  :reader
  (lambda (prompt initial-input history)
    (car (org-ilm--select-collection))))

(transient-define-prefix org-ilm--import-transient ()
  :refresh-suffixes t
  ["Ilm import"
   ("c" "Collection" org-ilm--import-transient-collection)
   ]
  ["Type"
   ("f" "File" org-ilm--import-file-transient
    :inapt-if-nil org-ilm--active-collection)
   ("w" "Website" org-ilm--import-website-transient
    :inapt-if-nil org-ilm--active-collection)
   ("r" "Registry" org-ilm--import-registry-transient
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
         (assoc org-ilm--active-collection org-ilm-collections)
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
   'source
   (car collection)
   (list :file file :method method :ext t)))

;;;;; Website

(defun org-ilm--import-website-transient-args (&optional args)
  (when transient-current-command
    (let ((args (or args (transient-args transient-current-command))))
      (list :url (transient-arg-value "--url=" args)
            :simplify (cond
                       ((transient-arg-value "--simplify-to-html" args)
                        "html")
                       ((transient-arg-value "--simplify-to-markdown" args)
                        "markdown"))
            :title (transient-arg-value "--title=" args)
            :download (transient-arg-value "--download" args)
            :orgify (transient-arg-value "--orgify" args)
            :collection org-ilm--active-collection))))

(transient-define-infix org-ilm--import-website-transient-url ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--url="
  :allow-empty nil
  :always-read t
  :prompt "URL: "
  :reader
  (lambda (prompt initial-input history)
    (let ((url (read-string prompt initial-input history)))
      (unless (string-empty-p url)
        (let* ((args (org-ilm--import-website-transient-args))
               (title (plist-get args :title))
               (title-object (transient-suffix-object 'org-ilm--import-website-transient-title)))
          (when (or (null title) (string-empty-p title))
            (oset title-object value (mochar-utils--get-page-title url))
            (transient-set)
            )))
      url)))

(transient-define-infix org-ilm--import-website-transient-title ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--title="
  :allow-empty nil
  :always-read t
  :prompt "Title (empty to auto-generate): "
  :reader
  (lambda (prompt initial-input history)
    (let ((title (read-string prompt initial-input history)))
      (if (string-empty-p title)
          (mochar-utils--get-page-title
           (plist-get (org-ilm--import-website-transient-args) :url))
        title))))

(transient-define-argument org-ilm--import-website-transient-simplify ()
  :class 'transient-switches
  :transient 'transient--do-call
  :description "Simplify to HTML or Markdown"
  :argument-format "--simplify-to-%s"
  :argument-regexp "\\(--simplify-to-\\(html\\|markdown\\)\\)"
  :choices '("html" "markdown"))

(transient-define-prefix org-ilm--import-website-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (let (url title)
      (setq url (thing-at-point 'url))
      (unless url
        (when (eq major-mode 'eww-mode)
          (setq url (eww-current-url))
          (setq title (plist-get eww-data :title))))
      (when (and (not title) url)
        (setq title (mochar-utils--get-page-title url)))
      (append
       '("--simplify-to-markdown" "--orgify")
       (when url
         (list (concat "--url=" url)))
       (when title
         (list (concat "--title=" title))))))
  
  ["Website import"
   [("u" "URL" org-ilm--import-website-transient-url)
    ("t" "Title" org-ilm--import-website-transient-title)
    ("d" "Download" "--download"
     :summary "Download HTML file with Monolith"
     :transient transient--do-call)]]
  
  ["Download options"
    :hide
    (lambda ()
      (not (plist-get (org-ilm--import-website-transient-args) :download)))
    [("s" "Simplify" org-ilm--import-website-transient-simplify)
     ("o" "Org conversion" "--orgify"
      :summary "Convert to Org mode with Pandoc"
      :transient transient--do-call)]]
   
   [
    [("RET" "Import"
     (lambda ()
       (interactive)
       (let* ((args (org-ilm--import-website-transient-args
                     (transient-args 'org-ilm--import-website-transient))))
         (apply #'org-ilm-import-website
                (assoc (plist-get args :collection) org-ilm-collections)
                (plist-get args :url)
                args)
         ))
     :inapt-if
     (lambda ()
       (let ((args (org-ilm--import-website-transient-args)))
         (not
          (and (plist-get args :url)
               (plist-get args :title)
               (plist-get args :collection))))))
    ]])

(cl-defun org-ilm-import-website (collection url &key title download simplify orgify &allow-other-keys)
  "Import a website."
  (cl-assert (or (null simplify) (member simplify '("html" "markdown"))))
  
  (let* ((org-id (org-id-new))
         (output-dir (org-attach-dir-from-id org-id)))

    (org-ilm--capture
     'source
     (car collection)
     (list :id org-id :title title :props (list :ROAM_REFS url))
     (lambda (attach-dir)
       (let ((monolith-args
              (list :input-path url
                    :output-path (expand-file-name
                                  (concat org-id ".html")
                                  attach-dir))))
         (cond
          ((null download) nil)
          (orgify
           (apply
            (if simplify
                #'convtools--convert-to-org-with-monolith-defuddle-pandoc
              #'convtools--convert-to-org-with-monolith-pandoc)
            (list
             :process-id org-id
             :monolith-args monolith-args
             :defuddle-args (list :output-format simplify)
             :on-success
             (lambda (&rest _)
               (message "Finished import: %s" url)))))
          (simplify
           (apply
            #'convtools--convert-with-monolith-defuddle
            (list
             :process-id org-id
             :monolith-args monolith-args
             :defuddle-args (list :output-format simplify)
             :on-success
             (lambda (&rest _)
               (message "Finished import: %s" url)))))
          (t ; Download, dont simplify or orgify
             (apply
              #'convtools--convert-with-monolith
              :process-id org-id
              monolith-args))))
       ))))


;;;;; Registry

(defun org-ilm--import-registry-transient-args (&optional args)
  (when transient-current-command
    (let ((args (or args (transient-args transient-current-command))))
      (list :entry (transient-arg-value "--entry=" args)
            :attachment (transient-arg-value "--attachment=" args)
            :method (transient-arg-value "--method=" args)
            :collection org-ilm--active-collection))))

(defun org-ilm--import-registry-transient-attachments (&optional entry-id)
  "Returns the file paths either in the attachment directory or in the PATH property."
  (unless entry-id
    (setq entry-id (plist-get (org-ilm--import-registry-transient-args) :entry)))
  (if-let* ((attach-dir (org-ilm--org-with-point-at entry-id (org-attach-dir)))
              (files (org-attach-file-list attach-dir)))
      (mapcar (lambda (f) (expand-file-name f attach-dir)) files)

    ;; No attachment directory. Check PATH property.
    (when-let* ((entry (org-mem-entry-by-id entry-id))
                (file (org-mem-entry-property "PATH" entry)))
      (list file))))

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
      
      (when-let ((attachments (org-ilm--import-registry-transient-attachments id))
                 (attachment-object (transient-suffix-object 'org-ilm--import-registry-transient-attachment)))
        (oset attachment-object value
              (completing-read "Attachment: " attachments)))
      
      id)))

(transient-define-infix org-ilm--import-registry-transient-attachment ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--attachment="
  :allow-empty nil
  :always-read t
  :reader
  (lambda (prompt initial-input history)
    (let* ((files (org-ilm--import-registry-transient-attachments)))
      (completing-read "Attachment: " files))))

(transient-define-prefix org-ilm--import-registry-transient ()
  :value (lambda ()
           '("--method=cp"))
  :refresh-suffixes t

  ["Registry import"
   ("e" "Entry" org-ilm--import-registry-transient-entry)
   (:info (lambda ()
            (let* ((args (org-ilm--import-registry-transient-args))
                   (entry (org-mem-entry-by-id (plist-get args :entry)))
                   (type (org-mem-entry-property "TYPE" entry))
                   (title (org-mem-entry-title entry)))
              (propertize
               (format "%s (%s)" title type)
               'face 'transient-inapt-suffix)))
          :if (lambda ()
                (plist-get (org-ilm--import-registry-transient-args) :entry)))]

  ["Attachment"
   :hide (lambda () (not (org-ilm--import-registry-transient-attachments)))
   ("a" "Attachment" org-ilm--import-registry-transient-attachment)
   ("m" "Method of attachment" "--method="
    :allow-empty nil :transient transient--do-call
    :choices (mv cp ln lns) :prompt "Method of attachment: ")]
  
  [
   [("RET" "Import"
     (lambda ()
       (interactive)
       (cl-destructuring-bind (&key collection entry attachment method)
           (org-ilm--import-registry-transient-args)
         (org-ilm-import-registry collection entry attachment (when method (intern method)))))
     :inapt-if
     (lambda ()
       (let ((args (org-ilm--import-registry-transient-args)))
         (not 
          (and (plist-get args :entry)
               (plist-get args :collection))))))
    ]])

(defun org-ilm-import-registry (collection entry-id &optional attachment method)
  "Import a registry entry."
  (cl-assert (or (null method) (member method '(mv cp ln lns))))

  (let ((entry (org-mem-entry-by-id entry-id)))
    (org-ilm--capture
     'source
     collection
     (list :file attachment :method (or method 'cp) :ext (when attachment t)
           :title (org-mem-entry-title entry)
           :props (list ;;:ROAM_REFS (format "[[registry:%s]]" entry-id)
                        :REGISTRY entry-id)
           ))))


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

(defun org-ilm--random-poisson (lambda)
  "Generate a Poission random variable using Knuth's algorithm."
  (let ((l (exp (- lambda)))
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


;;;; Priority

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
        (logbook (org-ilm--logbook-read headline))
        (subjects (org-ilm--priority-subject-gather headline)))
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

;;;;; Subject priorities

;; I don't know if we want to add anything to this but we can always just extend
;; the value list.
;; TODO Add sum of ancestor priorities?
(defvar org-ilm-priority-subject-cache (make-hash-table :test 'equal)
  "Map subject org-id -> (priority '((ancestor-subject-id . priority)) num-direct-parents)

The direct parent ids are the last num-direct-parents ids in ancestor-subject-ids.

Gets reset after org-mem refreshes.")

(defun org-ilm-priority-subject-cache-reset ()
  (setq org-ilm-priority-subject-cache (make-hash-table :test 'equal)))

(defun org-ilm--priority-subject-gather (headline-or-id)
  "Gather recursively headline's priority and parent subject priorities.
Headline can be a subject or not."
  (let ((id (if (stringp headline-or-id)
                headline-or-id
              (org-element-property :ID headline-or-id))))
    ;; (org-ilm--debug "Gathering subjects for:" id)
    
    (or (gethash id org-ilm-priority-subject-cache)
        (let* ((parents-data
                ;; Non-recursively, just direct parents in DAG
                (org-ilm--subjects-get-parent-subjects headline-or-id))
               (headline (car parents-data))
               (type (org-ilm-type headline))
               (is-subject (eq type 'subj))
               (parent-ids (cdr parents-data))
               (entry (org-mem-entry-by-id id))
               ;; Using entry is much faster, there is no need to parse the headline
               (priority (cdr (org-ilm--get-priority entry)))
               (ancestor-data
                (mapcar
                 (lambda (parent-id)
                   (cons parent-id
                         (cdr (org-ilm--get-priority
                               (org-mem-entry-by-id parent-id)))))
                 parent-ids)))

          ;; Recursively call this function on all direct parent subjects to get
          ;; their priority data.
          (dolist (parent-id parent-ids)
            (let ((parent-data (org-ilm--priority-subject-gather parent-id)))
              ;; Add parent's ancestors to list of ancestors
              (dolist (parent-ancestor (nth 1 parent-data))
                (unless (assoc parent-ancestor ancestor-data)
                  (push parent-ancestor ancestor-data)))))

          ;; Compile data in a list. If this headline is also a subject, add it
          ;; to the cache as well.
          (let ((priority-data (list priority ancestor-data (length parent-ids))))
            (when is-subject
              ;; (org-ilm--debug "Adding to hash:" id)
              (puthash id priority-data org-ilm-priority-subject-cache))
            ;; Return data
            priority-data)))))

;;;; Scheduling

(defun org-ilm--days-from-now (days)
  "Return ts object representing DAYS from now."
  (ts-adjust 'day -1 (ts-now)))

(defun org-ilm--schedule-interval-from-priority (priority)
  "Calculate a schedule interval from the normalized priority value."
  (cl-assert (<= 0 priority 1))
  (let* ((priority-bounded (min (max (- 1 priority) 0.05) 0.95))
         (rate (* 25 (- 1 priority-bounded)))
         (interval (+ (org-ilm--random-poisson rate) 1)))
    interval))

(defun org-ilm--set-schedule-from-priority ()
  "Set the schedule based on the priority."
  (when-let ((interval (org-ilm--schedule-interval-from-priority (cdr (org-ilm--get-priority)))))
    (org-schedule nil (format "+%sd" interval))))

(defun org-ilm--update-from-priority-change (func &rest args)
  "Advice around `org-priority' to detect priority changes to update schedule."
  (let ((old-priority (org-ilm--get-priority)))
    (apply func args)
    (let ((new-priority (org-ilm--get-priority)))
      (org-ilm--set-schedule-from-priority))))

;; TODO Better scheduling algorithm:
;; Early review, etc.
(defun org-ilm--schedule-interval-calculate (priority scheduled last-interval)
  "Calculate the new schedule interval assuming review was done today."
  (if (null last-interval)
      (org-ilm--schedule-interval-from-priority priority)
    (* last-interval 1.5)))

(defun org-ilm--element-schedule-interval-calculate (element)
  (let ((last-interval (org-ilm-element-last-interval element))
        (priority (org-ilm-element-prelative element))
        (scheduled (org-ilm-element-sched element)))
    (org-ilm--schedule-interval-calculate priority scheduled last-interval)))

;; TODO Turn this to function
;; (due (<= (/ (ts-diff scheduled (org-ilm--ts-today)) 86400) 0)))

(defun org-ilm--schedule-update ()
  "Update the scheduled date of an incr element at point after review.

This will update the schedule date regardless of whether the element is
due or not."
  (let* ((element (org-ilm-element-at-point))
         (scheduled (org-ilm-element-sched element)))
    (cl-assert scheduled)
    (cl-assert (eq (org-ilm-element-type element) 'incr))
    (let* ((interval (org-ilm--element-schedule-interval-calculate element))
           (timestamp (ts-adjust 'day interval (org-ilm--ts-today))))
      (cl-assert (> interval 0))
      (org-schedule nil (ts-format "%Y-%m-%d" timestamp)))))

(defun org-ilm-schedule (element date)
  "Set or update the schedule of the element at point."
  (interactive
   (list (org-ilm-element-at-point)
         (org-read-date nil nil nil "Schedule: ")))
  (unless element (user-error "No ilm element at point"))

  ;; Only read the date
  (pcase (org-ilm-element-type element)
    ('incr (org-schedule nil date))
    ;; Convert to ISO8601
    ('card (org-ilm--srs-set-timestamp (concat date "T09:00:00Z")))))
    


;;;; SRS

;; TODO https://github.com/bohonghuang/org-srs/issues/30#issuecomment-2829455202

(defun org-ilm--srs-in-cloze-p ()
  "Is point on a cloze?

Just a hack by looking at code of `org-srs-item-cloze-item-at-point'."
  (if (org-srs-item-cloze-bounds) t nil))

(defun org-ilm--srs-earliest-due-timestamp ()
  "Returns the earliest due timestamp of this headline's cards."
  (when-let* ((items (org-srs-query-region #'always (org-entry-beginning-position) (org-entry-end-position)))
         (due-timestamps
          (save-excursion
            (mapcar (lambda (item) (apply #'org-srs-item-due-timestamp item)) items))))
    (ts-parse (apply #'org-srs-timestamp-min due-timestamps))))

(defun org-ilm--srs-review-this ()
  "Start an org-srs review session of just the heading at point.

Achieves this by narrowing to subtree and calling
`org-srs-review-start'. In the collection, cards are always the leafs of
the hierarchy, so we don't have to worry about another srs heading being
below it.

TODO replace with org-srs-item-review as suggested by org-srs creator:
https://github.com/bohonghuang/org-srs/issues/20#issuecomment-2816991976"
  (org-narrow-to-subtree)
  ;; When buffer narrows, will only review items within narrowed region.
  (org-srs-review-start))

;; TODO This returns the first item only.
(defun org-ilm--srs-headline-item ()
  "Return item of the org-srs card of headline at point."
  (save-excursion
    (org-back-to-heading)
    (org-srs-log-beginning-of-drawer)
    (forward-line)
    (org-srs-item-at-point)))

;; TODO This returns the first item only.
(defun org-ilm--srs-headline-item-type ()
  "Return the item type of the cards of this headline."
  ;; Returns (type-info id)
  ;; type-info can be: (cloze) (card front) (card back)
  (car (car (org-ilm--srs-headline-item))))

;; TODO This returns the first item only.
(defun org-ilm--srs-review-rate (rating)
  "Rate SRS item without being in review.

https://github.com/bohonghuang/org-srs/issues/27#issuecomment-2830949169"
  (org-srs-property-let ((org-srs-review-cache-p nil))
    (apply #'org-srs-review-rate rating (org-ilm--srs-headline-item))))

;; TODO This works on first item only (if item is nil)
(defun org-ilm--srs-set-timestamp (time-or-duration &optional item)
  "Set time, or add duration to org-srs ITEM. If nil, current heading item.

Usage: (apply #'org-ilm--srs-set-timestamp (org-srs-timestamp-now) '(1 :day))

Duration specification: (NUM :UNIT). For units see `org-srs-time-units'.

From: `org-srs-review-postpone'"
  (unless item (setq item (org-ilm--srs-headline-item)))
  (save-excursion
    (org-srs-item-with-current item
      (setf (org-srs-item-due-timestamp) (cl-etypecase time-or-duration
                                           (org-srs-timestamp time-or-duration)
                                           (list (apply #'org-srs-timestamp+
                                                        (org-srs-timestamp-max
                                                         (org-srs-item-due-timestamp)
                                                         (org-srs-timestamp-now))
                                                        time-or-duration)))))))
;; TODO Only works for first item
(defun org-ilm--srs-table ()
  "Return the table rows of the first srs item of the current heading."
  (save-excursion
    (save-restriction
      (org-ilm--org-narrow-to-header)
      (re-search-forward org-srs-item-regexp)
      (forward-line)
      (org-srs-table-lines))))

(defun org-ilm--srs-last-review-timestamp ()
  ;; TODO Only works for first item
  (when-let* ((table (org-ilm--srs-table))
              (last (pop table)))
    (when (= 1 (length last))
      (setq last (pop table)))
    (when last
      (ts-parse (alist-get 'timestamp last)))))

;;;;; Custom org-srs types

(cl-defmethod org-srs-item-review ((type (eql 'ilm-cloze)) &rest args)
  "Exactly as the default 'cloze type, but assumes no header."
  (cl-loop with visibility = (org-srs-item-cloze-visibility) and cloze-id-set = args
           initially (org-srs-item-cloze-remove-overlays (point-min) (point-max))
           ;; for cloze in (progn (org-srs-item-narrow) (org-srs-item-cloze-collect))
           for cloze in (org-srs-item-cloze-collect)
           for (id . (start end text hint)) = cloze
           if (or (null cloze-id-set) (member id cloze-id-set))
           collect (cons (org-srs-item-cloze-put-overlay
                          start end
                          (org-srs-item-cloze-current hint))
                         cloze)
           into hidden-clozes
           else
           do (cl-ecase visibility
                ((nil) (org-srs-item-cloze-put-overlay start end (org-srs-item-cloze-hidden)))
                ((t) (org-srs-item-cloze-put-overlay start end text)))
           finally
           (cl-loop with centeredp = (org-srs-item-cloze-centered-in-review-p)
                    for (overlay _id start _end text _hint) in hidden-clozes
                    do (cl-assert (overlayp overlay))
                    unless (> (length hidden-clozes) 1)
                    do (goto-char start)
                    and when (cl-etypecase centeredp
                               (boolean centeredp)
                               (function (funcall centeredp)))
                    do (recenter) (org-srs-item-cloze-recenter-horizontally)
                    do (org-srs-item-add-hook-once
                        'org-srs-item-after-confirm-hook
                        (apply-partially #'\(setf\ org-srs-item-cloze-overlay-text\) (org-srs-item-cloze-answer text) overlay)))
           (org-srs-item-add-hook-once
            'org-srs-review-continue-hook
            (apply-partially #'org-srs-item-cloze-remove-overlays (point-min) (point-max))
            50)
           (apply (org-srs-item-confirm) type args)))

;;;;; Temporary advice

;; https://github.com/bohonghuang/org-srs/issues/45

(defun org-ilm--org-srs-item-goto-override (&rest args)
  (let* ((marker (apply #'org-srs-item-marker args))
         (buffer (marker-buffer marker)))
    ;; (cl-assert (eq (window-buffer) (current-buffer)))
    (unless (eq buffer (current-buffer))
      (switch-to-buffer buffer nil t)
      ;; (cl-assert (eq (window-buffer) buffer))
      )
    (cl-assert (eq (current-buffer) buffer))
    (goto-char marker)))
(advice-add 'org-srs-item-goto
            :override #'org-ilm--org-srs-item-goto-override)

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
                     (eq type 'incr)))
            (when-let ((prop (org-mem-entry-property "SUBJECTS+" entry)))
              (setq property-subjects-str
                    (concat property-subjects-str " " prop))))
           
           ;; Headline is subject, store as outline parent subject
           ((eq type 'subj)
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
       (let ((ancestory (nth 2 (org-ilm--priority-subject-gather (plist-get subj :id)))))
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
            (and
             (eq 'subj (org-ilm-type (org-mem-todo-state entry)))
             (org-ilm--collection-file (org-mem-entry-file entry) collection)))
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
         ((and ilm-type (not (eq ilm-type 'subj)))
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

;;;; Review

(defcustom org-ilm-review-clock-back-in t
  "What to do with active clock after review.
Nil to do nothing (stay clocked out), t to clock back in, or 'continue
to clock back in and continue the previous clock as if never clocked
out."
  :type '(choice
          (const :tag "No (stay clocked out)" nil)
          (const :tag "Yes (clock back in)" t)
          (const :tag "Yes and continue last" continue))
  :group 'org-ilm)

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

(defvar org-ilm-review-quit-hook nil
  "Hook run when review session stopped.")

(defvar-keymap org-ilm-review-mode-map
  :doc "Keymap for `org-ilm-review-mode'."
  "<f5>" #'org-ilm-review-rate-easy
  "<f6>" #'org-ilm-review-rate-good
  "<f7>" #'org-ilm-review-rate-hard
  "<f8>" #'org-ilm-review-rate-again
  "<f9>" #'org-ilm-review-next)

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

  (unless org-ilm-queue-active-buffer
    ;; TODO let user choose inactive one and make it active
    (user-error "No active queue buffer!"))

  ;; Make sure queue is not empty
  (with-current-buffer org-ilm-queue-active-buffer
    (when (org-ilm-queue-empty-p)
      (org-ilm-queue-build)
      (when (org-ilm-queue-empty-p)    
        (user-error "Queue is empty!"))))

  (when (yes-or-no-p "Start reviewing?")

    ;; Store clocked-in task so that we can clock back in when done
    (setq org-ilm--review-interrupted-clock-marker
          (when (org-clocking-p) (copy-marker org-clock-marker)))

    (org-ilm-review-mode 1)
    (org-ilm--review-next)))

(defun org-ilm-review-quit ()
  "Quit ilm review."
  (interactive)
  (org-ilm--review-cleanup-current-element)
  (org-ilm-review-mode -1)

  ;; Clock back in the active clock before review
  (when org-ilm--review-interrupted-clock-marker
    (when (and org-ilm-review-clock-back-in
               (markerp org-ilm--review-interrupted-clock-marker))
      (with-current-buffer (marker-buffer org-ilm--review-interrupted-clock-marker)
        (save-excursion
          (goto-char org-ilm--review-interrupted-clock-marker)
          (pcase org-ilm-review-clock-back-in
            ('continue (org-ilm--clock-in-continue-last))
            ;; Note: this actually matches on t. not a mistake (shouldnt be "_")
            (t (org-clock-in))))))
    (setq org-ilm--review-interrupted-clock-marker nil))
  
  (run-hooks 'org-ilm-review-quit-hook))

(defun org-ilm-review-next (&optional rating)
  "Finish review of current element and go to the next one."
  (interactive)
  (unless (org-ilm-reviewing-p)
    (user-error "Not reviewing."))

  ;; Make sure there is a rating when element is a card
  (cl-assert (or (null rating) (member rating '(:good :easy :hard :again))))
  (when (and
         (plist-get org-ilm--review-data :card-type)
         (not (plist-get org-ilm--review-data :rating)))
    (setf (plist-get org-ilm--review-data :rating)
          (or rating
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
        (&key buffer id element rating card-type &allow-other-keys)
        org-ilm--review-data
      
      (when org-ilm--review-update-schedule
        (org-ilm--org-with-point-at id
          (if card-type
              (org-ilm--srs-review-rate rating)
            (org-ilm--schedule-update)))))
    
    (let ((element (org-ilm-queue-pop)))
      
      ;; TODO Card might already be due, eg when rating again. So need to check the
      ;; new review time and add it back to the queue. Set a minimum position in
      ;; the queue so that the card is not reviewed too quickly again.
      )
    
    (org-ilm--review-cleanup-current-element))
  
  (if (org-ilm-queue-empty-p)
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

The main job is to prepare the variable
`org-ilm--review-data', which needs the card-type stored
in the collection and the attachment buffer.
TODO Store card type in org-ilm-element? Reason against it is for cards with
back and front, which are different types, so depends on what is due. But that's
a whole other problem, since we can only deal with one card (type?) now."
  (cl-assert (not (org-ilm-queue-empty-p)))

  (let* ((element (org-ilm-queue-head))
         (id (org-ilm-element-id element))
         card-type attachment-buffer)

    (org-ilm--org-with-point-at id
      (when (org-ilm-element-is-card element)
        (setq card-type
              (org-ilm--srs-headline-item-type)))
      
      (setq attachment-buffer
            ;; dont yet switch to the buffer, just return it so we can do some
            ;; processing first.
            (save-window-excursion
              (org-ilm--attachment-open :no-error t)))

      ;; TODO Causes weird folding problems in collection file
      (org-clock-in))

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
        (setf (plist-get org-ilm--data :start) (current-time))))

    (setq org-ilm--review-data
          (list :element element
                :id (org-ilm-element-id element)
                :buffer attachment-buffer
                :card-type card-type))))

(defun org-ilm--review-open-current-element ()
  "Open and prepare the attachment buffer of the element being reviewed."
  (let ((buffer (plist-get org-ilm--review-data :buffer))
        (card-type (plist-get org-ilm--review-data :card-type)))
    (if buffer
      (with-current-buffer (switch-to-buffer buffer)
        (setq header-line-format
              (org-ilm--review-header-build))

        ;; Prepare org-srs card overlays. First tried doing it before poping to
        ;; buffer, but `org-srs-item-review' immediately prompts user to type any
        ;; key to reveal the answer, so no time to switch to buffer.
        (when card-type
          (add-hook
           'org-srs-item-after-confirm-hook
           (lambda ()
             (setq org-ilm--review-data
                   (plist-put org-ilm--review-data
                              :card-revealed t))
             (setq header-line-format
                   (org-ilm--review-header-build)))
           nil t)
          (org-srs-item-review card-type)))
      ;; No attachment, simply go to the element in the collection
      (org-ilm--org-goto-id (plist-get org-ilm--review-data :id))
      (message "No attachment found for element"))))

(defun org-ilm--review-cleanup-current-element ()
  "Clean up the element being reviewed, in preparation for the next element."
  (when-let ((buffer (plist-get org-ilm--review-data :buffer)))
    (with-current-buffer buffer
      (when (plist-get org-ilm--review-data :card-type)
        (org-srs-item-cloze-remove-overlays (point-min) (point-max)))
      (setq header-line-format nil)
      (remove-hook 'kill-buffer-hook #'org-ilm--review-confirm-quit t))
    ;; I know it doesn't make sense to clean the buffer then kill it, but better
    ;; have the cleaning up code ready in case i want an option later to not
    ;; kill the buffer
    (kill-buffer buffer))
  
  ;; Clock out. Strictly speaking not necessary: clocking into next task will
  ;; clock out this one. But good to be explicit.
  (org-clock-out nil 'fail-quietly)
  
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
         (card-type (plist-get org-ilm--review-data :card-type))
         (card-revealed (plist-get org-ilm--review-data :card-revealed)))
    (concat
     (propertize "Ilm Review" 'face '(:weight bold :height 1.0))
     "   "
     (funcall
      #'concat
      (unless card-type
         (org-ilm--review-header-make-button
          "Next" 'org-ilm-review-next))
      (when (and card-type (not card-revealed))
        "Continue with any key")
      (when (and card-type card-revealed)
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

