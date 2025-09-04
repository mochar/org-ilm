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
(require 'org-srs)
(require 'org-transclusion)
(require 'cl-lib)
(require 'dash)
(require 'ts)

;;;; Customization
(defgroup org-ilm nil
  "Incremental learning mode."
  :group 'org
  :prefix "org-ilm-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom org-ilm-collections-alist '((ilm . "~/org/ilm.org"))
  "Alist mapping collection name to path to its org file."
  :type '(alist :key-type symbol :value-type file)
  :group 'org-ilm)

(defcustom org-ilm-queries-alist `((Outstanding . ,org-ilm--query-outstanding))
  "Alist mapping query name to org-ql query.")

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

(defcustom org-ilm-schedule-max-days 60
  "Maximum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-schedule-min-days 1
  "Minimum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-incr-state "INCR"
  "TODO state of elements to be processed incrementally."
  :type 'string
  :group 'org-ilm)

(defcustom org-ilm-card-state "CARD"
  "TODO state of flash cards."
  :type 'string
  :group 'org-ilm)

(defcustom org-ilm-subject-states '("SUBJ")
  "TODO state of subjects."
  :type '(repeat string)
  :group 'org-ilm)

(defcustom org-ilm-card-default-location 'collection
  "Whether to store the card content in the `collection' or as an `attachment'.

When set to `attachment', org-transclusion will be used to transclude the content in the collection during review."
  :type '(choice (const :tag "Collection" 'collection)
                 (const :tag "Attachment" 'attachment))
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

(defvar org-ilm-queue nil
  "List of org headings that form the queue.")

(defvar org-ilm-map (make-sparse-keymap)
  "Keymap for `org-ilm-global-mode'.")

(defvar org-ilm-target-value-regexp "\\(extract\\|card\\):\\(begin\\|end\\):\\([^>]+\\)"
  "Regexp to match values of targets enclosing extracts and clozes.")

(defvar org-ilm-target-regexp (format "<<%s>>" org-ilm-target-value-regexp)
  "Regexp to match targets enclosing extracts and clozes.")

(defvar org-ilm--targets-editable nil
  "Whether or not to allow editing/removing of target text.")

;;;; Minor mode

(defun org-ilm--org-mem-hook (&rest _)
  (org-ilm-priority-subject-cache-reset))

;;;###autoload
(define-minor-mode org-ilm-global-mode
  "Prepare some hooks and advices some functions."
  :init-value nil
  :global t
  :lighter nil ;; String to display in mode line
  :group 'org-ilm
  :keymap org-ilm-map
  (if org-ilm-global-mode
      ;; Enable
      (progn
        (add-hook 'org-mode-hook #'org-ilm-prepare-buffer)
        (add-hook 'org-mem-post-full-scan-functions
                  #'org-ilm--org-mem-hook)
        (add-hook 'org-mem-post-targeted-scan-functions
                  #'org-ilm--org-mem-hook)
        )
    ;; Disable
    (remove-hook 'org-mode-hook #'org-ilm-prepare-buffer)
    (remove-hook 'org-mem-post-full-scan-functions
              #'org-ilm--org-mem-hook)
    (remove-hook 'org-mem-post-targeted-scan-functions
              #'org-ilm--org-mem-hook)
    ))

;;;; Commands
;; TODO move commands to appriopriate headings

(defun org-ilm-import-website (url)
  ""
  (interactive "sURL: "))

(defun org-ilm-extract ()
  "Extract text in region.

Will become an attachment Org file that is the child heading of current entry."
  (interactive)
  (unless (use-region-p) (user-error "No region active."))
  (let* ((file-buf (current-buffer))
         (file-path buffer-file-name)
         (file-name (file-name-sans-extension
                     (file-name-nondirectory
                      file-path)))
         (attach-org-id (car (org-ilm--attachment-data)))
         (attach-org-id-marker (org-id-find attach-org-id t))
         (file-org-id file-name)
         (file-org-id-marker (org-id-find file-org-id t)))
    (unless attach-org-id-marker
      (error "Current file is not an attachment, or failed to parse org ID."))
    (unless file-org-id-marker
      (error "Current file name not org ID, or failed to find it."))
    
    (let* ((region-begin (region-beginning))
           (region-end (region-end))
           (region-text (org-ilm--buffer-text-clean region-begin region-end))
           (snippet (org-ilm--generate-text-snippet region-text))
           (extract-org-id (org-id-new))
           (extract-tmp-path (expand-file-name
                              (format "%s.org" extract-org-id)
                              temporary-file-directory)))

      ;; Save region content into tmp file and move it as attachment to main
      ;; heading.
      (write-region region-text nil extract-tmp-path)
      (org-ilm--capture
       'extract
       `(id ,file-org-id)
       extract-org-id
       extract-tmp-path
       snippet
       (lambda ()
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
           (org-ilm-recreate-overlays)))))))

(defun org-ilm-prepare-buffer ()
  "Recreate overlays if current buffer is an attachment Org file."
  (interactive)
  (when (org-ilm--attachment-data)
    (org-ilm-recreate-overlays)))

(defun org-ilm-open ()
  "Open element at point."
  (interactive)
  (if-let* ((attach-dir (org-attach-dir))
            (org-id (org-id-get))
            (path (expand-file-name (format "%s.org" org-id) attach-dir)))
      (progn
        (run-hook-with-args 'org-attach-open-hook path)
        (org-open-file path t)) ;; last param: in-emacs
      (error "No attachment directory exist, no Org id at current heading, or no attachment file associated with Org id.")))

(defun org-ilm-open-highlight ()
  "Open element associated with highlight at point."
  (interactive)
  (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'org-ilm-id)) (overlays-at (point)))))
    (pcase (length ovs)
      (0 nil)
      (1 (org-ilm--open-from-ov (nth 0 ovs)))
      (t
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
      (progn
        (find-file (car org-id-loc))
        (goto-char (cdr org-id-loc)))
    (let ((collection (org-ilm--select-collection)))
      (find-file (cdr collection)))))

(defun org-ilm-open-dwim ()
  "Open element of highlight or within collection."
  (interactive)
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
     ('collection (org-ilm-open))
     ('attachment
      (unless (org-ilm-open-highlight)
        (org-ilm-open-collection)))
     (t (org-ilm-open-collection)))))

(defun org-ilm-set-schedule-from-priority ()
  "Set schedule date based on priority."
  (interactive)
  (org-ilm--set-schedule-from-priority))

(defun org-ilm-cloze-toggle-this ()
  "Toggle cloze at point, without creating the card."
  (interactive)
  (if (org-ilm--srs-in-cloze-p)
      (org-srs-item-uncloze-dwim)
    (org-srs-item-cloze-dwim)))
  
;;;; Functions

;;;;; Utilities

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

(defun org-ilm--select-alist (alist &optional prompt formatter)
  "Prompt user for element in alist to select from.
If empty return nil, and if only one, return it."
  (pcase (length alist)
    (0 nil)
    (1 (nth 0 alist))
    (t (let* ((choices (mapcar
                        (lambda (thing)
                          (cons
                           (if formatter
                               (funcall formatter thing)
                             (format "%s %s"
                                     (propertize
                                      (if (symbolp (car thing))
                                          (symbol-name (car thing))
                                        (car thing))
                                      'face '(:weight bold))
                                     (propertize (nth 1 thing) 'face '(:slant italic))))
                           thing))
                        alist))
              (choice (completing-read (or prompt "Select: ") choices nil t))
              (item (cdr (assoc choice choices))))
         item))))

(defun org-ilm--select-collection ()
  "Prompt user for collection to select from.
The collections are stored in `org-ilm-collections-alist'."
  (org-ilm--select-alist org-ilm-collections-alist "Collection: "))

(defun org-ilm--select-query ()
  "Prompt user for query to select from.
The queries are stored in `org-ilm-queries-alist'."
  (org-ilm--select-alist org-ilm-queries-alist "Query: "))

(defun org-ilm-get-subjects (&optional headline)
  "Returns subjects of headline or parses "
  (if (and headline (plist-member headline :subjects))
      (plist-get headline :subjects)
    (org-ilm--priority-subject-gather headline)))

(defun org-ilm--capture (type target org-id tmp-file-path &optional title on-success on-abort)
  "Make an org capture to make a new source heading, extract, or card.

The callback ON-SUCCESS is called when capture is saved.
The callback ON-ABORT is called when capture is cancelled."
  (let* ((state (if (eq type 'card) org-ilm-card-state org-ilm-incr-state))
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
            
            ;; Attach the file. We always use 'mv because we are
            ;; importing the file from the tmp dir.
            (org-attach-attach tmp-file-path nil 'mv)))
         (after-finalize
          (lambda ()
            ;; Deal with success and aborted capture. This can be detected in
            ;; after-finalize hook with the `org-note-abort' flag set to t in
            ;; `org-capture-kill'.
            (if org-note-abort
                (when on-abort
                  (funcall on-abort))
              (when on-success
                (funcall on-success)))))
         (org-capture-templates
          `(("i" "Import"
             entry ,target
             ,(format "* %s [#5] %s %s" state title "%?")
             ;; :unnarrowed ; TODO for extracts might be nice?
             :hook (lambda ()
                     (org-entry-put nil "ID" ,org-id)

                     ;; If this is a source header where the attachments will
                     ;; live, we need to set the DIR property, otherwise for
                     ;; some reason org-attach on children doesn't detect that
                     ;; there is a parent attachment header, even with a non-nil
                     ;; `org-attach-use-inheritance'.
                     ;; (when (eq ',type 'source)
                       ;; (org-entry-put
                        ;; nil "DIR"
                        ;; (abbreviate-file-name (org-attach-dir-get-create))))

                     ;; Schedule the header. We do not add a schedule for cards,
                     ;; as that info is parsed with
                     ;; `org-ilm--srs-earliest-due-timestamp'.
                     (unless (eq ',type 'card)
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
                     (when (eq ',type 'card)
                       (save-excursion
                         (org-ilm--transclusion-goto ,tmp-file-path 'create)
                         (org-transclusion-add)
                         (org-srs-item-new 'cloze)
                         (org-ilm--transclusion-goto ,tmp-file-path 'delete))))
             :before-finalize ,before-finalize
             :after-finalize ,after-finalize))))
    (org-capture nil "i")))

(defun org-ilm--current-date-utc ()
  "Current date in UTC as string."
  (format-time-string "%Y-%m-%d" (current-time) t))

(defun org-ilm--buffer-text-clean (&optional region-begin region-end keep-clozes)
  "Extract buffer text without clozes and extract and card targets."
  (let ((text (s-replace-regexp
               org-ilm-target-regexp ""
               (buffer-substring-no-properties (or region-begin (point-min))
                                               (or region-end (point-max))))))
    ;; Remove clozes by looping until none left
    (if keep-clozes
        text
      (with-temp-buffer
        (insert text)
        (while (let ((clz (org-srs-item-cloze-collect))) clz)
          (let* ((cloze (car (org-srs-item-cloze-collect)))
                 (region-begin (nth 1 cloze))
                 (region-end   (nth 2 cloze))
                 (inner (substring-no-properties (nth 3 cloze))))
            (goto-char region-begin)
            (delete-region region-begin region-end)
            (insert inner)))
        (buffer-string)))))

(defun org-ilm--generate-text-snippet (text)
  ""
  (let* ((text-clean (replace-regexp-in-string "\n" " " text))
         (snippet (substring text-clean 0 (min 50 (length text-clean)))))
    snippet))

(defun org-ilm--org-narrow-to-header ()
  ""
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1) (narrow-to-region (point-min) (point))))

(defun org-ilm--where-am-i ()
  "Returns one of ('collection collection), ('attachment (org-id collection)), nil."
  ;; TODO
  (let ((collections (mapcar
                      (lambda (c) (expand-file-name (cdr c)))
                      org-ilm-collections-alist)))
    (if (member buffer-file-name collections)
        (cons 'collection buffer-file-name)
      (when-let* ((_ buffer-file-name)
                  (file-title (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name)))
                  (_ (org-uuidgen-p file-title))
                  (src-file (org-id-find-id-file file-title)))
        (when (member (expand-file-name src-file) collections)
          (cons 'attachment (list file-title src-file)))))))

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

(defun org-ilm--org-headline-at-point ()
  "Return headline at point.

TODO org-ql uses following snippet in query action, is it more efficient?
(org-element-headline-parser (line-end-position))"
  (let ((element (org-element-at-point)))
    (when (eq (car element) 'headline)
      element)))

(defun org-ilm--org-headline-element-from-id (org-id)
  "Return headline element from org id.

Since we need to visit location anyway, prefer not to use org-mem."
  (when-let ((marker (org-id-find org-id 'marker)))
    (save-excursion
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (org-ilm--org-headline-at-point)))))

(defun org-ilm--org-id-from-string (string)
  "Interpret STRING as org-id, link with org-id, or else nil."
  (cond
   ((org-uuidgen-p string) string)
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
                        (org-node-by-id entry-or-id)
                      entry-or-id)))
    (let ((ancestry
           (delq nil ;; Delete nils returned when no org-id
                 (mapcar (lambda (x)
                           (car (last x)))
                         ;; cdr to skip itself
                         (cdr (org-mem-entry-crumbs entry))))))
      (if with-root-str (cons "ROOT" ancestry) ancestry))))

;;;;; Elements

(defun org-ilm--logbook-parse (logbook)
  "Parses the Org contents of a logbook drawer entry."
  (let ((contents (org-element-contents logbook)))
    (mapcar
     ;; If more data is needed, see:
     ;; https://orgmode.org/worg/dev/org-element-api.html
     ;; See: Timestamp, Clock
     (lambda (clock)
       (let ((timestamp (org-element-property :value clock)))
         `(:duration-minutes
           ,(org-duration-to-minutes (org-element-property :duration clock))
           :status ,(org-element-property :status clock)
           :day-end ,(org-element-property :day-end timestamp)
           :day-start ,(org-element-property :day-start timestamp)
           :month-end ,(org-element-property :month-end timestamp)
           :month-start ,(org-element-property :month-start timestamp)
           :year-end ,(org-element-property :year-end timestamp)
           :year-start ,(org-element-property :year-start timestamp))))
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

(defun org-ilm-logbook-get (&optional headline)
  "Returns logbook or parses it if not available."
  ;; Might be nil, but if its a member, it has been parsed before, so don't
  ;; parse again.
  (if (and headline (plist-member headline :logbook))
      (plist-get headline :logbook)
    (org-ilm--logbook-read headline)))



;;;;; Attachments
(defun org-ilm-infer-id-from-attachment-path (path)
  "Attempt parsing the org-id from the attachment path, return (id . location)."
  (when path
    (let* ((parts (split-string (file-name-directory path) "/" t))
           (potential-id (pcase (file-name-nondirectory (org-attach-dir-from-id "abc"))
                           ("abc" (car (last parts)))
                           ("ab/c" (mapconcat #'identity (last parts 2) ""))
                           (t nil)))
           (location (org-id-find potential-id)))
      (when location (cons potential-id location)))))

(defun org-ilm--attachment-data ()
  "Returns (org-id collection-path) if current buffer is collection attachment file."
  (when-let ((location (org-ilm--where-am-i)))
    (when (eq 'attachment (car location))
      (cdr location))))

(defun org-ilm--attachment-path (&optional if-exists)
  "Return path to the attachment of heading at point."
  (when-let* ((org-id (org-id-get))
              (path (org-attach-expand (format "%s.org" org-id))))
    (when (or (not if-exists) (file-exists-p path))
      path)))

;;;;; Transclusion

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
            ('delete (delete-line))
            ('create (progn
                       (delete-line)
                       (split-line))))
        (when create
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))))
      (when create
        (insert (format "%s :level %s" transclusion-text (+ 1 current-level)))))))

(defun org-ilm-attachment-transclusion-delete ()
  "Delete transclusion text for the attachment of header at point."
  (interactive)
  (when-let ((attachment-path (org-ilm--attachment-path t)))
    (org-ilm--transclusion-goto attachment-path 'delete)))

(defun org-ilm-attachment-transclusion-create ()
  "Create and move point to transclusion for the attachment of header at point."
  (interactive)
  (when-let ((attachment-path (org-ilm--attachment-path t)))
    (org-ilm--transclusion-goto attachment-path 'create)))

(defun org-ilm-attachment-transclusion-transclude ()
  "Transclude the contents of the current heading's attachment."
  (interactive)
  (save-excursion
    (org-ilm-attachment-transclusion-create)
    (org-transclusion-add)))


;;;;; Targets

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
    


;;;;; Overlays

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

;;;;; Query

;; Optimizations: https://github.com/alphapapa/org-ql/issues/88#issuecomment-570473621
;; + If any data is needed in action and query, use org-ql--value-at
;; + Prefer query over custom predicate
;; + Use regex preambles to quickly filter candidates

(defun org-ilm-parse-headline ()
  "Parse org-ilm data of headline at point.

Was thinking of org-ql--value-at this whole function, but this is
wasteful if headline does not match query."
  ;; Don't use (org-element-headline-parser (line-end-position)) as org-ql does, it will fail to return parents correctly.
  (let* ((headline (org-ilm--org-headline-at-point))
         (todo-keyword (org-element-property :todo-keyword headline))
         (type (cond
                 ((string= todo-keyword org-ilm-card-state) 'card)
                 ((string= todo-keyword org-ilm-incr-state) 'incr)
                 ((member todo-keyword org-ilm-subject-states) 'subj)))
         (is-card (eq type 'card))
         (scheduled (if is-card
                        (org-ql--value-at (point) #'org-ilm--srs-earliest-due-timestamp)
                      (ts-parse-org-element (org-element-property :scheduled headline))))
         (now (ts-now)))

    (list
     ;; Basic headline element properties
     :todo todo-keyword
     :id (org-element-property :ID headline)
     :level (org-element-property :level headline)
     :priority (org-ilm--get-priority headline)
     :raw-value (org-element-property :raw-value headline)
     :tags (org-element-property :tags headline)
     :title (org-no-properties ; Remove text properties from title
             (org-element-interpret-data (org-element-property :title headline)))

     ;; Our stuff
     :scheduled scheduled
     :scheduled-relative (when scheduled ; convert from sec to days
                           (/ (ts-diff now scheduled) 86400))
     :type type
     :logbook (unless is-card (org-ilm--logbook-read headline))
     :subjects (org-ilm--priority-subject-gather headline)
     :priority-sample (org-ilm--sample-priority-from-headline headline))))

(defun org-ilm--compare-priority (first second)
  "Comparator of two headlines by sampled priority."
  (when-let ((priority-first (plist-get first :priority-sample))
             (priority-second (plist-get second :priority-sample)))
    (< priority-first priority-second)))

(defun org-ilm--ql-card-due ()
  "Check if org-srs earliest card due today, optimized for org-ql query."
  (when-let ((due (org-ql--value-at (point) #'org-ilm--srs-earliest-due-timestamp)))
    (ts<= due (ts-now))))

(defvar org-ilm--query-outstanding
  `(or
    (and
     (todo ,org-ilm-incr-state)
     (scheduled :to today))
    (and
     (todo ,org-ilm-card-state)
     (org-ilm--ql-card-due)))
  "Query for org-ql to retrieve the outstanding elements.")

(defun org-ilm-query-queue (collection query)
  "Apply org-ql QUERY on COLLECTION, parse org-ilm data, and return the results."
  (let ((entries (org-ql-select (cdr collection)
                   (cdr query)
                   :action #'org-ilm-parse-headline
                   :sort #'org-ilm--compare-priority)))
    entries))

(defun org-ilm--query-subjects (&optional collection)
  "Return list of subjects from COLLECTION.

TODO parse-headline pass arg to not sample priority to prevent recusrive subject search?"
  (let ((collection (or collection (plist-get org-ilm-queue :collection))))
    (cl-assert collection)
    (org-ql-select (cdr collection)
      (cons 'todo org-ilm-subject-states)
      :action #'org-ilm-parse-headline)))


;;;;; Queue

(defvar org-ilm-queue-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "b") #'backward-char)
    (define-key map (kbd "f") #'forward-char)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "k")
                (lambda ()
                  (interactive)
                  (kill-buffer (current-buffer))))
    (define-key map (kbd "m") #'org-ilm-queue-object-mark)
    (define-key map (kbd "g") #'org-ilm-queue-revert)
    (define-key map (kbd "M s") #'org-ilm-queue-mark-by-subject)
    ;; TODO r: Review start command
    ;; TODO G: query again - undo manual changes
    ;; TODO C-u G: like G but also select collection
    ;; TODO RET/space: replicate open like in agenda
    ;; TODO M :: mark by tag
    ;; TODO M s: mark by number of days due
    ;; TODO M j: mark by subject
    ;; TODO B: bulk commands
    map)
  "Keymap for the queue buffer.")

(defvar org-ilm--queue-marked-objects nil
  "Org id of marked objects in queue.")

(defun org-ilm-queue-object-mark (object)
  "Mark the object at point."
  (interactive (list (vtable-current-object)))
  (let* ((id (plist-get object :id)))
    (if (member id org-ilm--queue-marked-objects)
        ;; not sure why but inconsistent behavior when not setq, even though
        ;; cl-delete is meant to remove destructively.
        (setq org-ilm--queue-marked-objects (cl-delete id org-ilm--queue-marked-objects  :test #'equal))
      (cl-pushnew id org-ilm--queue-marked-objects  :test #'equal))
    ;; TODO gives cache error no idea why
    ;; Maybe worked on?: https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-07/msg00802.html
    ;; (vtable-update-object (vtable-current-table) object)
    (org-ilm-queue-revert)))

(defun org-ilm-queue-mark-by-subject (subject)
  "Mark all elements in queue that are part of SUBJECT."
  (interactive
   (list
    (org-ilm--select-alist
     (org-ilm--query-subjects)
     "Subject: " ; Prompt
     (lambda (subject) ; Formatter
       (mapconcat
        (lambda (crumb) (nth 3 crumb))
        (cdr (reverse (org-mem-entry-crumbs (org-node-by-id (plist-get subject :id)))))
        " > ")))))

  (let* ((subject-and-descendants (org-ilm--subjects-get-with-descendant-subjects subject))
         (desc-ids (mapcar #'car subject-and-descendants)))
    (dolist (object (plist-get org-ilm-queue :queue))
      ;; TODO which is faster, check desc in subjs or other way around
      (when (seq-some
             (lambda (s) (member s desc-ids))
             (nth 3 (plist-get object :subjects)))
        (org-ilm-queue-object-mark object)))))

(defun org-ilm-queue-revert ()
  (interactive)
  (vtable-revert-command)
  (setq header-line-format (symbol-name (car (plist-get org-ilm-queue :query)))))
  
(defun org-ilm--queue-make-vtable ()
  "Build queue vtable.

A lot of formatting code from org-ql."
  (make-vtable
   :insert nil ; Return vtable object rather than insert at point
   :columns `((:name
               "Marked"
               :width 3
               :formatter
               (lambda (m)
                 (if m
                     (propertize " > " 'face 'error)
                     "   ")))
              (:name "Priority"
               :width 6
               :formatter
               (lambda (p)
                 (if p
                     (propertize (format "%.2f" (* 100 p)) 'face 'shadow)
                   "")))
              (:name
               "Type"
               :width 5
               :formatter
               (lambda (type)
                 (org-ql-view--add-todo-face (upcase (symbol-name type)))))
              (:name
               "Cookie"
               :width 4
               :formatter
               (lambda (p)
                 (org-ql-view--add-priority-face (format "[#%s]" p))))
              (:name
               "Title"
               :formatter
               (lambda (title-data)
                 (let* ((title (nth 0 title-data))
                        (scheduled-relative (nth 1 title-data))
                        (due-string
                         (if scheduled-relative
                             (org-add-props
                                 (org-ql-view--format-relative-date (round scheduled-relative))
                                 nil 'face 'org-ql-view-due-date)
                           "")))
                   (concat title " " due-string))))
              (:name
               "Tags"
               :align 'right
               :formatter (lambda (tags)
                            (if tags
                              (--> tags
                                   (s-join ":" it)
                                   (s-wrap it ":")
                                   (org-add-props it nil 'face 'org-tag))
                              "")))
              )
   :objects-function (lambda () (plist-get org-ilm-queue :queue))
   :getter (lambda (object column vtable)
             (pcase (vtable-column vtable column)
               ("Marked" (member (plist-get object :id) org-ilm--queue-marked-objects))
               ("Type" (plist-get object :type))
               ("Priority" (plist-get object :priority-sample))
               ("Cookie" (plist-get object :priority))
               ("Title" (list (plist-get object :title) (plist-get object :scheduled-relative)))
               ("Tags" (plist-get object :tags))))
   :keymap org-ilm-queue-map))

(defun org-ilm--display-queue ()
  "Open the active queue."
  (let ((buf (get-buffer-create "*ilm queue*")))
    (with-current-buffer buf
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (vtable-insert (org-ilm--queue-make-vtable))
      (setq header-line-format (symbol-name (car (plist-get org-ilm-queue :query))))
      (setq-local buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(defun org-ilm-queue (collection query)
  "View queue in Agenda-like buffer."
  (interactive (list
                (org-ilm--select-collection)
                (org-ilm--select-query)))
  (setq org-ilm-queue
        (list
         :queue (org-ilm-query-queue collection query)
         :collection collection
         :query query))
  (org-ilm--display-queue))


;;;;; Import

(defun org-ilm--select-import-method (&optional force-ask)
  "Ask user to choose whether to copy or move file when importing.

If `org-ilm-import-default-method' is set and `FORCE-ASK' is nil, return it."
  (if (and (not force-ask) org-ilm-import-default-method)
      org-ilm-import-default-method
    (let* ((choices '(("Copy" . cp) ("Move" . mv)))
           (choice (completing-read "Method: " choices nil t))
           (method (cdr (assoc choice choices))))
      method)))

(defun org-ilm-import-org-file (file collection method)
  "Import an Org file."
  (interactive
   (list
    (read-file-name "Import Org file: ")
    (org-ilm--select-collection)
    (org-ilm--select-import-method)))

  (let* ((org-id (org-id-new))
         (file-tmp-path (expand-file-name
                         (format "%s.org" org-id)
                         temporary-file-directory)))
    
    ;; Move or copy the file to tmp directory so we can rename the file with the
    ;; org ID before attaching it.
    (pcase method
      ('mv (rename-file file file-tmp-path))
      ('cp (copy-file file file-tmp-path))
      (t (error "Parameter METHOD must be one of 'mv or 'cp.")))

    (org-ilm--capture 'source `(file ,(cdr collection)) org-id file-tmp-path)))

;;;;; Stats
;; Functions to work with e.g. the beta distribution.
;; Random generation code from Claude.

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


;;;;; Priority

(defun org-ilm--validated-priority (priority)
  "Returns numeric value of priority if valid value, else nil."
  (cond
   ((stringp priority)
    (when (member priority '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (string-to-number priority)))
   ((numberp priority)
    (when (member priority '(1 2 3 4 5 6 7 8 9))
      priority))))

(defun org-ilm--get-priority (&optional headline)
  "Return priority value of HEADLINE, or if nil, the one at point.

If no priority is set, return default value of 5."
  (let ((priority (if headline
                      (org-element-property :priority headline)
                    (org-entry-get nil "PRIORITY"))))
    ;; When HEADLINE is parsed with org-element, buffer local
    ;; `org-priority-lowest' and highest not respected, returns a high number
    ;; instead that can be converted back to our priority.
    ;; TODO its a char, use (char-to-string) 
    (if (and (numberp priority) (> priority 48))
        (- priority 48)
      (or
       (org-ilm--validated-priority priority)
       5))))

(defun org-ilm--beta-from-priority (priority)
  "Beta parameters from priority value."
  (let* ((a priority)
         (b (+ 1 (- 9 a))))
    (list a b)))

(defun org-ilm--beta-combine (&rest params-list)
  "Combine multiple Beta distributions."
  (list
   (apply #'+ -1 (mapcar (lambda (x) (nth 0 x)) params-list))
   (apply #'+ -1 (mapcar (lambda (x) (nth 1 x)) params-list))))

(defun org-ilm--priority-adjusted-from-logbook (params logbook)
  "Calculate the variance adjusted beta parameters from logbook data.

For now, decrease variance by proportionally scaling a and b by some
factor that increases with the number of reviews."
  (let* ((reviews (length logbook))
         (a (* (nth 0 params) (+ 1 reviews)))
         (b (* (nth 1 params) (+ 1 reviews))))
    (list a b)))

(defun org-ilm--priority-adjusted-from-subjects (params subjects)
  "Average the priority out over the subject priorities."
  (let ((subject-count (nth 2 subjects))
        (subject-sum (nth 1 subjects)))
    (if (= 0 subject-count)
        params
      (let* ((subjects-avg-priority (/ (float subject-sum) subject-count))
             (subjects-params (mapcar
                               (lambda (p) (* p subject-count))
                               (org-ilm--beta-from-priority subjects-avg-priority))))
        (org-ilm--beta-combine params subjects-params)))))

(defun org-ilm--priority-beta-compile (priority subjects logbook)
  "Compile the finale beta parameters from priority value, subjects, and logbook history."
  (let ((params (org-ilm--beta-from-priority priority)))
    (setq params (org-ilm--priority-adjusted-from-logbook params logbook))
    (setq params (org-ilm--priority-adjusted-from-subjects params subjects))))

(defun org-ilm--priority-get-params (&optional headline)
  "Calculate the beta parameters of the heading at point."
  (let ((priority (org-ilm--get-priority headline))
        (logbook (org-ilm-logbook-get headline))
        (subjects (org-ilm-get-subjects headline)))
    (org-ilm--priority-beta-compile priority subjects logbook)))

(defun org-ilm--sample-priority (beta-params &optional seed)
  "Sample a priority value given the beta params."
  (interactive (list (org-ilm--priority-get-params)))
  (org-ilm--random-beta (nth 0 beta-params)
                        (nth 1 beta-params)
                        seed))

(defun org-ilm--sample-priority-from-headline (headline)
  "This still assumes there is a headline at point. Used in `org-ilm--ql-headline-action'."
  (when-let* ((beta (org-ilm--priority-get-params headline))
              (seed (format "%s%s"
                            (org-ilm--current-date-utc)
                            (org-element-property :ID headline)))
              (sample (org-ilm--sample-priority beta seed)))
    sample))

;;;;;; Subject priorities

(defvar org-ilm-priority-subject-cache (make-hash-table :test 'equal)
  "Map subject org-id -> (priority priority-sum priority-count parent-subject-ids)

Gets reset after org-mem refreshes.")

(defun org-ilm-priority-subject-cache-reset ()
  (setq org-ilm-priority-subject-cache (make-hash-table :test 'equal)))

(defun org-ilm--priority-subject-gather (headline-or-id)
  "Gather recursively headline's priority and parent subject priorities.
Headline can be a subject or not."
  (let ((id (if (stringp headline-or-id)
                headline-or-id
              (org-element-property :ID headline-or-id))))
    (org-ilm--debug "Gathering subjects for:" id)
    (or (gethash id org-ilm-priority-subject-cache)
        (let* ((parents-data
                (org-ilm--subjects-get-parent-subjects headline-or-id))
               (headline (car parents-data))
               (is-subject (member
                            (org-element-property :todo-keyword headline)
                            org-ilm-subject-states))
               (parent-ids (cdr parents-data))
               (priority (org-ilm--get-priority headline))
               (priority-sum 0)
               (priority-count 0))

          ;; Recursively call this function on all direct parent subjects to get
          ;; their priority data.
          (dolist (parent-id parent-ids)
            (let ((parent-priority-data (org-ilm--priority-subject-gather parent-id)))
              (setq priority-count
                    (+ priority-count (nth 2 parent-priority-data) 1))
              ;; Sum is sum of parent + parent itself
              (setq priority-sum
                    (+ priority-sum
                       (nth 1 parent-priority-data)
                       (nth 0 parent-priority-data)))))

          ;; Compile data in a list. If this headline is also a subject, add it
          ;; to the cache as well.
          (let ((priority-data (list priority priority-sum priority-count parent-ids)))
            (when is-subject
              (puthash id priority-data org-ilm-priority-subject-cache))
            ;; Return data
            priority-data)))))


;;;;; Scheduling

(defun org-ilm--schedule-interval-from-priority (priority)
  "Calculate a schedule interval from priority value."
  (setq priority (org-ilm--validated-priority priority))
  (unless priority
    (error "Priority value invalid."))
  (let* ((priority-normalized (/ (- (- 10 priority) 1) 8.0))
         (priority-bounded (min (max priority-normalized 0.05) 0.95))
         (rate (* 25 (- 1 priority-bounded)))
         (interval (+ (org-ilm--random-poisson rate) 1)))
    interval))

(defun org-ilm--set-schedule-from-priority ()
  "Set the schedule based on the priority."
  (when-let ((interval (org-ilm--schedule-interval-from-priority (org-ilm--get-priority))))
    (org-schedule nil (format "+%sd" interval))))

(defun org-ilm--update-from-priority-change (func &rest args)
  "Advice around `org-priority' to detect priority changes to update schedule."
  (let ((old-priority (org-ilm--get-priority)))
    (apply func args)
    (let ((new-priority (org-ilm--get-priority)))
      (org-ilm--set-schedule-from-priority))))

;;;;; SRS
;; TODO https://github.com/bohonghuang/org-srs/issues/30#issuecomment-2829455202
;; TODO https://github.com/bohonghuang/org-srs/issues/27#issuecomment-2830949169
;; TODO https://github.com/bohonghuang/org-srs/issues/22#issuecomment-2817035409

(defun org-ilm-cloze ()
  "Create a cloze card.

A cloze is made automatically of the element at point or active
region. If instead a grouped set of clozes must be made, you can first
call `org-ilm-cloze-toggle-this' a couple times before calling this
command."
  (interactive)

  (let* ((file-buf (current-buffer))
         (card-org-id (org-id-new))
         (card-tmp-path (expand-file-name
                         (format "%s.org" card-org-id)
                         temporary-file-directory))
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
    (setq buffer-text (org-ilm--buffer-text-clean nil nil t)
          snippet (org-ilm--generate-text-snippet (org-ilm--buffer-text-clean)))

    (write-region buffer-text nil card-tmp-path)

    (org-ilm--capture
     'card
     `(id ,file-org-id)
     card-org-id
     card-tmp-path
     snippet
     (lambda ()
       ;; Success callback. Go through each cloze in the source file and replace
       ;; it with our target tags. Render them by recreating overlays.
       (with-current-buffer file-buf
         (save-excursion
           ;; process clozes until none left TODO similar functionality
           ;; `org-ilm--buffer-text-clean', extract to own function
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
         (org-ilm-recreate-overlays)))
     (lambda ()
       ;; Abort callback. Undo cloze if made automatically by this function.
       (when auto-clozed
         (with-current-buffer file-buf
           (org-srs-item-uncloze-dwim))))
     )))

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


;;;;; Subjects

;; TODO org-ilm--subjects-gather should first org-ql query for all SUBJ then gather priorties. this is because org-ql caches them. to get the cache-key use code below:
;; (cl-letf (((symbol-function 'org-ql--select-cached)
;;            (lambda (&rest args)
;;              (setq org-ilm--subj-query-cache-key args)
;;              (message "%s" args))))
;;   (org-ilm--collect-queue-entries (car org-ilm-collections-alist)))

(defun org-ilm--subjects-get-parent-subjects (&optional headline-thing check-links-recursively include-indirect-ancestors)
  "Retrieve parent subjects of a headline.

Processing is done to remove indirect or redundant parents, so that the returned parents form the direct parents of the DAG.

When CHECK-LINKS-RECURSIVELY, repeat process for all linked subjects in SUBJECTS property.
When INCLUDE-INDIRECT-ANCESTORS, include also non-parent ancestors."
  (let* ((headline (org-ilm--org-headline-from-thing headline-thing 'assert))
         (headline-id (org-element-property :ID headline))
         (headline-entry (org-node-by-id headline-id))
         (headline-ancestry (org-ilm--org-mem-ancestry-ids headline-entry))
         subject-ids)

    (org-ilm--debug "Ancestory of:" headline-id)

    ;; Check for ancestor subject headline in outline hierarchy.
    ;; TODO skip if `headline-ancestry' is nil?
    (unless
        ;; We don't need to check for ancestors if top-level headline.
        (= 1 (org-element-property :level headline))
      (org-element-lineage-map
          headline
          (lambda (element)
            (when (member
                   (org-element-property :todo-keyword element) org-ilm-subject-states)
              ;; TODO creating id might mess up with parsing and cache and
              ;; whatever as we're updating the buffer during parsing
              (let ((org-id (org-id-get element 'create)))
                (cl-pushnew org-id subject-ids :test #'equal)
                ;; Return non-nil in case we dont want all ancestors
                org-id)))
        '(headline) nil (not include-indirect-ancestors)))

    (org-ilm--debug "-- Outline ancestors:" subject-ids)
    
    ;; Check for inherited SUBJECTS property.
    
    ;; This is tricky because parent subjects might link to other subjects in
    ;; their property, requiring this to be done recursively if we need all
    ;; subjects. Furthermore test for invalid linking, eg to a descendant or
    ;; circular links.

    ;; Note: do this after getting outline ancestors as outline does not have
    ;; aforementioned problems, and properties are inherited.
    (when-let ((property-subjects-str (org-entry-get headline "SUBJECTS" 'inherit)))
      (let (property-subject-ids property-subject-ancestors)

        ;; First we gather valid subject-ids as well as their individual ancestries
        (dolist (subject-string (string-split property-subjects-str))
          (when-let* ((org-id (org-ilm--org-id-from-string subject-string))
                      ;; Skip if linked to itself
                      (_ (not (string= org-id headline-id)))
                      ;; Skip if ancestor of headline
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
            (when check-links-recursively
              (dolist (parent-id (cdr (org-ilm--subjects-get-parent-subjects org-id 'recursive include-indirect-ancestors)))
                (cl-pushnew parent-id property-subject-ids :test #'equal)
                (cl-pushnew (org-ilm--org-mem-ancestry-ids parent-id) property-subject-ancestors)))))

        ;; Filter if subject-id is ancestor of any other subject-id
        ;; TODO don't do this if `include-indirect-ancestors' non-nil right??
        (let ((ancestries (apply #'append property-subject-ancestors)))
          (dolist (subject-id property-subject-ids)
            (unless (member subject-id ancestries)
              (cl-pushnew subject-id subject-ids :test #'equal))))))

    (org-ilm--debug "-- All ancestors:" subject-ids)
    
    (cons headline subject-ids)))

(defun org-ilm--subjects-get-with-descendant-subjects (subject)
  "Retrieve descendant subjects of a headline.

Descendants can be directly in outline or indirectly through property linking.

This function does not have to be fast: Currently only called every now
and again by `org-ilm-queue-mark-by-subject'.

TODO can perhaps be sped up by utilizing `org-ilm-priority-subject-cache'?
Contains full subject hierarchy + is cached."
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

(defun org-ilm-subject-add ()
  "Annotate headline at point with a subject.

TODO Skip if self or descendant."
  (interactive)
  (let* ((subject-entry (org-ilm--node-read-candidate-state-only org-ilm-subject-states))
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


;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here

