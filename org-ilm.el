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

(defcustom org-ilm-queries-alist
  `((Outstanding . org-ilm-query-outstanding))
  "Alist mapping query name to a function that returns an org-ql query."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-ilm)

(defcustom org-ilm-custom-queries-alist nil
  "Do not edit. Auto saved user added queries. See `org-ilm-queries-alist'."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-ilm)

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

(defvar-keymap org-ilm-map
  :doc "Keymap for `org-ilm-global-mode'."
  "o" #'org-ilm-open-dwim
  "x" #'org-ilm-extract-dwim
  "z" #'org-ilm-cloze
  "c" #'org-ilm-cloze-toggle-this
  "j" #'org-ilm-subject-add
  "q" #'org-ilm-queue)

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
  ;; :keymap org-ilm-map
  (if org-ilm-global-mode
      ;; Enable
      (progn
        (add-hook 'org-mode-hook #'org-ilm-prepare-buffer)
        (add-hook 'org-mem-post-full-scan-functions
                  #'org-ilm--org-mem-hook)
        (add-hook 'org-mem-post-targeted-scan-functions
                  #'org-ilm--org-mem-hook)
        (define-key pdf-view-mode-map (kbd "A") org-ilm-pdf-map)
        (advice-add 'pdf-annot-create-context-menu
                    :around #'org-ilm--pdf-annot-create-context-menu-advice)
        )
    ;; Disable
    (remove-hook 'org-mode-hook #'org-ilm-prepare-buffer)
    (remove-hook 'org-mem-post-full-scan-functions
              #'org-ilm--org-mem-hook)
    (remove-hook 'org-mem-post-targeted-scan-functions
                 #'org-ilm--org-mem-hook)
    (define-key pdf-view-mode-map (kbd "A") nil)
    (advice-remove 'pdf-annot-create-context-menu
                   #'org-ilm--pdf-annot-create-context-menu-advice)
    ))

;;;; Commands
;; TODO move commands to appriopriate headings

(defun org-ilm-import-website (url)
  ""
  (interactive "sURL: "))

(defun org-ilm-prepare-buffer ()
  "Recreate overlays if current buffer is an attachment Org file."
  (interactive)
  (when (org-ilm--attachment-data)
    (org-ilm-recreate-overlays)))

(defun org-ilm-open-attachment ()
  "Open attachment of item at point."
  (interactive)
  (org-ilm--attachment-open))

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
     ('collection (org-ilm-open-attachment))
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

(defun org-ilm--select-alist (alist &optional prompt formatter)
  "Prompt user for element in alist to select from.
If empty return nil, and if only one, return it."
  (pcase (length alist)
    (0 nil)
    (1 (nth 0 alist))
    (t (let* ((choices (mapcar
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
                                       (propertize second 'face '(:slant italic))))
                             thing)))
                        alist))
              (choice (completing-read (or prompt "Select: ") choices nil t))
              (item (cdr (assoc choice choices))))
         item))))

(defun org-ilm--select-query ()
  "Prompt user for query to select from.
The queries are stored in `org-ilm-queries-alist'."
  (org-ilm--select-alist org-ilm-queries-alist "Query: "))

(defun org-ilm-get-subjects (&optional headline)
  "Returns subjects of headline or parses "
  (if (and headline (plist-member headline :subjects))
      (plist-get headline :subjects)
    (org-ilm--priority-subject-gather headline)))

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

(defun org-ilm--collection-file (&optional file collection)
  "Return collection of which file belongs to.
A collection symbol COLLECTION can be passed to test if file belongs to
 that collection."
  (when-let ((file (or file buffer-file-name))
             (path (expand-file-name file))
             (test (lambda (f place)
                     (if (f-directory-p place)
                         (file-in-directory-p f place)
                       (file-equal-p f place)))))
    (if collection
        (when-let ((col (pcase (type-of collection)
                          ('symbol
                           (assoc collection org-ilm-collections-alist))
                          ('string
                           (seq-find
                            (lambda (c) (file-equal-p collection (cdr c)))
                            org-ilm-collections-alist))
                          ('cons collection))))
          (when (funcall test path (cdr col)) col))
      (seq-find (lambda (c) (funcall test path (cdr c))) org-ilm-collections-alist))))

(defun org-ilm--where-am-i ()
  "Returns one of ('collection collection), ('attachment (org-id collection)), nil."
  (if-let ((collection (org-ilm--collection-file buffer-file-name)))
      (cons 'collection collection)
    (when-let* ((file-title (file-name-base
                             ;; Allow for non-file buffer: pdf virtual view
                             (or buffer-file-name (buffer-name))))
                (_ (org-ilm--org-id-p file-title))
                (src-file (car (org-id-find file-title))))
      (when-let (collection (org-ilm--collection-file (expand-file-name src-file)))
        (list 'attachment file-title collection)))))

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
  "Return headline at point.

TODO org-ql uses following snippet in query action, is it more efficient?
(org-element-headline-parser (line-end-position))"
  (let ((element (org-element-at-point)))
    (when (eq (car element) 'headline)
      element)))

(defmacro org-ilm--org-with-point-at (thing &rest body)
  "THING for now should be an org-id.

Note: Previously used org-id-find but it put point above
headline. org-mem takes me there directly.

Alternatively org-id-goto could be used, but does not seem to respect
save-excursion."
  `(when-let* ((org-id ,thing)
               (entry (org-mem-entry-by-id org-id))
               (file (org-mem-entry-file-truename entry))
               (buf (or (find-buffer-visiting file)
                        (find-file-noselect file))))
     (with-current-buffer buf
       ;; Jump to correct position
       (goto-char (org-find-property "ID" org-id))
       ,@body)))

(defun org-ilm--org-headline-element-from-id (org-id)
  "Return headline element from org id."
  (save-excursion
    (org-ilm--org-with-point-at
     org-id
     (org-ilm--org-headline-at-point))))

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
                        (org-node-by-id entry-or-id)
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

;;;; Elements

(defun org-ilm--select-collection ()
  "Prompt user for collection to select from.
The collections are stored in `org-ilm-collections-alist'."
  (org-ilm--select-alist org-ilm-collections-alist "Collection: "))


;;;;; Logbook
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



;;;; Capture

(defun org-ilm--capture (type target-or-id data &optional on-success on-abort)
  "Make an org capture to make a new source heading, extract, or card.

The callback ON-SUCCESS is called when capture is saved.
The callback ON-ABORT is called when capture is cancelled."
  (cl-assert (member type '(extract card source)))
  (let* ((state (if (eq type 'card) org-ilm-card-state org-ilm-incr-state))
         (target (if (stringp target-or-id) `(id ,target-or-id) target-or-id))
         (title (plist-get data :title))
         (id (or (plist-get data :id) (org-id-new)))
         (attachment-ext (plist-get data :ext))
         (content (plist-get data :content))
         (file (plist-get data :file))
         (priority (plist-get data :priority))
         (template (plist-get data :template))
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
            (when file
              (let ((org-attach-auto-tag (if (eq type 'source) org-attach-auto-tag nil)))
                (org-attach-attach file nil 'mv)))))
         (after-finalize
          (lambda ()
            ;; Deal with success and aborted capture. This can be detected in
            ;; after-finalize hook with the `org-note-abort' flag set to t in
            ;; `org-capture-kill'.
            (if org-note-abort
                (when on-abort
                  (funcall on-abort))
              (when on-success
                (funcall on-success))))))

    ;; Generate title from content if no title provided
    (when (and (not title) content)
      (setq title (org-ilm--generate-text-snippet content)))

    ;; Set extension from file when set to t
    (when (eq attachment-ext t)
      (if file
          (setq attachment-ext (file-name-extension file))
        (error "Cannot infer extension when no file provided (:ext=t)")))

    ;; Save content in a temporary file if no file provided
    ;; (unless (or content file)
      ;; (error "Cannot capture without content or file."))
    (when (and (not file) content)
      (setq file (expand-file-name
                  (format "%s.%s" id (or attachment-ext "org"))
                  temporary-file-directory))

      ;; TODO set MUSTBENEW to t?
      (write-region content nil file))

    (unless template
      (setq template
            (format "* %s%s %s %s"
                    state
                    (if priority (format " [#%s]" priority) "")
                    (or title "")
                    "%?")))

    (let ((org-capture-templates
           `(("i" "Import"
              entry ,target
              ,template
              :hook (lambda ()
                      ;; Regardless of type, every headline will have an id.
                      (org-entry-put nil "ID" ,id)

                      ;; Attachment extension if specified
                      (when ,attachment-ext
                        (org-entry-put nil "ILM_EXT" ,attachment-ext))

                      ;; Scheduling. We do not add a schedule for cards, as that
                      ;; info is parsed with
                      ;; `org-ilm--srs-earliest-due-timestamp'.
                      (unless (eq ',type 'card)
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
                      (when (eq ',type 'card)
                        ;; TODO this can be a nice macro
                        ;; `org-ilm-with-attachment-transcluded'
                        (save-excursion
                          (org-ilm--transclusion-goto ,file 'create)
                          (org-transclusion-add)
                          (org-srs-item-new 'cloze)
                          (org-ilm--transclusion-goto ,file 'delete))))
              :before-finalize ,before-finalize
              :after-finalize ,after-finalize))))
      (org-capture nil "i"))))

;;;; Conversion

;;;;; To org with Pandoc

(cl-defun org-ilm--convert-to-org-with-pandoc (file-path format &key output-dir keep-buffer-alive on-success on-error)
  "Convert a file to Org mode format using Pandoc."
  (let* ((file-path (expand-file-name file-path))
         (output-dir (expand-file-name (or output-dir (file-name-directory file-path))))
         ;; Make sure we are in output dir so that media files references correct
         (default-directory output-dir)
         (process-name (format "org-ilm-pandoc (%s)" file-path))
         (process-buffer (get-buffer-create (concat "*" process-name "*"))))
    (org-ilm--debug "Pandoc conversion:\n--File: %s" file-path)
    (make-process
     :name process-name
     :buffer process-buffer
     :command
     (append
        (list
         "pandoc"
         "--from" format
         "--to" "org"
         "--wrap=preserve"
         ;; Relative to output-dir, or absolute path. Since we set
         ;; default-directory to output path and want to store media in same
         ;; folder, set to "."
         "--extract-media" "."
         "--verbose"
         file-path
         "-o" (concat (file-name-sans-extension file-path) ".org")))
     :sentinel
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (if (= (process-exit-status proc) 0)
             (funcall on-success proc process-buffer)
           (funcall on-error proc process-buffer))
         (unless keep-buffer-alive (kill-buffer process-buffer)))))))
     

;;;;; PDF with Marker
;; Marker conversion from PDF: https://github.com/datalab-to/marker

;; Much better Org formatting when converting from markdown
(cl-defun org-ilm--convert-pdf-with-marker (pdf-path format &key output-dir pages disable-image-extraction keep-buffer-alive move-content-out new-name to-org on-success on-error)
  "Convert a PDF document using Marker.

OUTPUT-DIR is the output directory. If not specified, will be the same
directory as PDF-PATH. Note that Marker stores the output in another dir
within this directory, named after the file. With MOVE-CONTENT-OUT set
to non-nil, the directory contents will be moved up to be in OUTPUT-DIR.

NEW-NAME if non-nil can be a string to rename the output file. Marker
does not have an option for this so it is done here.
"
  (unless (and org-ilm-pdf-marker-path (file-executable-p org-ilm-pdf-marker-path))
    (user-error "Marker executable not available. See org-ilm-pdf-marker-path."))
  (unless (member format '("markdown" "json" "html" "chunks"))
    (error "FORMAT must be one of [markdown|json|html|chunks]"))
  (unless (or (not to-org) (member format '("markdown" "html")))
    (error "When TO-ORG, FORMAT must be one of [markdown|html]"))
  (let* ((pdf-path (expand-file-name pdf-path))
         (pdf-base-name (file-name-base pdf-path))
         ;; for rename-file, NEWNAME recognized as dir if ends in slash
         (output-dir (if output-dir
                         (concat (expand-file-name output-dir) "/")
                       (file-name-directory pdf-path)))
         (output-dir-dir (file-name-concat output-dir (file-name-base pdf-path)))
         (process-name (format "org-ilm-marker (%s)" pdf-path))
         (process-buffer (get-buffer-create (concat "*" process-name "*"))))
    (org-ilm--debug "Marker conversion:\n--Pages: %s" pages)
    (make-process
     :name process-name
     :buffer process-buffer
     :command
     (append
      (list
       org-ilm-pdf-marker-path
       pdf-path
       "--output_format" format
       "--output_dir" output-dir)
      (when pages
        (list
         "--page_range"
         (cond
          ((stringp pages) pages)
          ((integerp pages) (number-to-string pages))
          ;; TODO allow "0,5-10,20" as list of integers and cons
          ((and (consp pages) (integerp (car pages)) (integerp (cdr pages)))
           (format "%s-%s" (car pages) (cdr pages)))
          (t (error "Argument PAGES not correctly specified.")))))
      ;; "--disable_tqdm"
      ;; Extract images from the document. Default is True.
      (when disable-image-extraction '("--disable_image_extraction"))
      ;; Whether to flatten the PDF structure. 
      ;; "--flatten_pdf BOOLEAN"
      (list
       "--detection_batch_size" "1"
       "--ocr_error_batch_size" "1" ; slowest
       "--layout_batch_size" "1"
       "--recognition_batch_size" "1"
       "--equation_batch_size" "1"
       "--table_rec_batch_size" "1"))
     :sentinel
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (if (= (process-exit-status proc) 0)
             (progn
               ;; Renaming the files is done by replacing the pdf base file name
               ;; in each otuput folder file name, but only if the file name
               ;; starts with it. This is to hit less false positives if the
               ;; base file name is small, which won't be the case i think since
               ;; org-ilm works with org-ids all the time.

               ;; TODO Works ok but I think a better approach would be to create
               ;; a symlink with the new name and point marker to that. Then
               ;; delete symlink afterwards.
               (when (or move-content-out new-name)
                 ;; Appearently that regex is needed otherwise returns "." and
                 ;; ".." as files lol
                 (dolist (file (directory-files output-dir-dir 'abs-file-name directory-files-no-dot-files-regexp))
                   (let ((file-base-name (file-name-base file))
                         (file-ext (file-name-extension file)))
                     (rename-file
                      file
                      (file-name-concat
                       (if move-content-out output-dir output-dir-dir)
                       (when new-name
                         (concat
                          (replace-regexp-in-string
                           ;; Only replace name if filename starts with it
                           (concat "^" pdf-base-name)
                           new-name
                           file-base-name)
                          "." file-ext)))
                      'ok-if-exists)))
                 (when move-content-out
                   (delete-directory output-dir-dir)))
               (funcall on-success proc process-buffer))
           (funcall on-error proc process-buffer))
         (unless keep-buffer-alive (kill-buffer process-buffer))))
     )))


;;;; Org attachment
;; Org mode attachments

(defun org-ilm-org-extract ()
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
           (extract-org-id (org-id-new)))

      ;; Save region content into tmp file and move it as attachment to main
      ;; heading.
      (org-ilm--capture
       'extract
       file-org-id
       (list :id extract-org-id :content region-text)
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

(defconst org-ilm--pdf-output-types
  '((virtual . "Virtual view")
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
    (region . (virtual text image))
    (section . (virtual text org))))

(defcustom org-ilm-pdf-marker-path (executable-find "marker_single")
  "Path to the marker_single executable."
  :type 'file
  :group 'org-ilm)


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
     (org-ilm--attachment-open region)
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
  (let* ((priority (or (plist-get data :priority) 5))
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

(defun org-ilm--pdf-image-export (filename &optional region page)
  ""
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
                      temporary-file-directory))
      (write-region (point-min) (point-max) img-path))
    (kill-buffer img-buffer)
    img-path))

(cl-defun org-ilm--pdf-path (&keys directory-p)
  "Infer the path of the PDF file, regardless if in virtual or not.
When DIRECTORY-P, return directory of the file."
  (let ((path (expand-file-name
               (if (eq major-mode 'pdf-virtual-view-mode)
                   (car (pdf-virtual-document-page 1))
                 buffer-file-name))))
    (if directory-p
        (file-name-directory path)
      path)))

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
        (pcase-let* ((`(,LE ,TO, RI, BO) page-region)
                     (`(,le ,to ,ri ,bo) region)
                     (w (- RI LE))
                     (h (- BO TO)))
          (list (+ LE (* le w))
                (+ TO (* to h))
                (+ LE (* ri w))
                (+ TO (* bo h))))))
     (t (error "Not in a PDF buffer")))))

;;;;; Annotation highlight

(defun org-ilm--pdf-add-square-annotation (region &optional label dont-save-buffer)
  "Add a square highlight annotation that is stored within the PDF file."
  ;; Need active region for it to be square instead of text highlight
  (setq pdf-view--have-rectangle-region t)
  (setq pdf-view-active-region region)
  (pdf-view--push-mark)
  (pdf-annot-add-markup-annotation
   (list region)
   'highlight
   (face-background 'org-ilm-face-extract)
   `(
     (opacity . 1.0)
     (label . ,label)
     ;; (contents . "")
     ))
  (unless dont-save-buffer
    (save-buffer)))

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

(defun org-ilm--pdf-convert-to-org (pdf-path pages org-id headline on-success)
  "Convert attachment PDF to Md using Marker, then to Org mode using Pandoc."
  (let ((attach-dir (file-name-directory pdf-path)))
    (org-ilm--convert-pdf-with-marker
     pdf-path
     "markdown"
     :new-name org-id
     :pages pages
     :keep-buffer-alive t
     :move-content-out t
     :to-org t
     :on-success
     (lambda (proc buf)
       (kill-buffer buf)
       (org-ilm--convert-to-org-with-pandoc
        (file-name-concat attach-dir (concat org-id ".md"))
        "markdown"
        :keep-buffer-alive t
        :on-success
        (lambda (proc buf)
          (kill-buffer buf)
          (save-excursion
            ;; Fails with org-id, must be headline
            (org-with-point-at headline
              (org-attach-sync)
              (funcall on-success))))
        :on-error
        (lambda (proc buf)
          (switch-to-buffer buf))))
     :on-error
     (lambda (proc buf)
       (switch-to-buffer buf)))))

(defun org-ilm-pdf-convert (output-type)
  "Convert PDF to another format within the same attachment."
  (interactive
   (list
    (let ((options (seq-filter
                    (lambda (option) (member (cdr option) '(text org)))
                    (org-ilm--invert-alist org-ilm--pdf-output-types))))
      (cdr (assoc (completing-read "Convert to: " options nil t) options)))))

  (let* ((pdf-buffer (current-buffer))
         (org-id (file-name-base (buffer-name)))
         (headline (org-ilm--org-headline-element-from-id org-id))
         (tmp-path (expand-file-name
                    (concat org-id ".org")
                    temporary-file-directory))
         (num-pages (pdf-info-number-of-pages))
         (pdf-path (expand-file-name (car (pdf-virtual-document-page 1))))
         (attach-dir (file-name-directory pdf-path))
         ;; Not a lot of code reuse..
         (on-success (lambda () 
                       (when (yes-or-no-p "Conversion finished. Use as main attachment?")
                         (org-entry-put nil "ILM_EXT" "org")))))
    (pcase output-type
      ('text
       (with-temp-buffer
         (dolist (page (number-sequence 1 num-pages))
           (insert (pdf-info-gettext page '(0 0 1 1) nil pdf-buffer)))
         (write-region (point-min) (point-max) tmp-path)
         (save-excursion
           (org-with-point-at headline
             (let ((org-attach-auto-tag nil))
               (org-attach-attach tmp-path nil 'mv))
             (funcall on-success)))))
      ('org
       (org-ilm--pdf-convert-to-org
        pdf-path
        (if (= 1 num-pages)
            (1- (nth 1 (pdf-virtual-document-page 1)))
          (cons (1- (nth 1 (pdf-virtual-document-page 1)))
                (1- (nth 1 (pdf-virtual-document-page num-pages)))))
        org-id headline on-success)
       ))))

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
        org-id attachment buffer collection headline)

    ;; Handle both when current buffer is PDF or headine in
    ;; collection. Furthermore we need the headline level to indent the outline
    ;; headlines appropriately.
    (cond
     ((eq (car location) 'attachment)
      (setq org-id (nth 0 (cdr location))
            collection (nth 1 (cdr location))))
     ((eq (car location) 'collection)
      (setq org-id (org-id-get)
            collection (cdr location)))
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
    (list org-id attachment buffer collection headline
          (org-element-property :level headline)
          (org-ilm--interval-to-schedule-string
           (org-ilm--schedule-interval-from-priority 5)))))

(defun org-ilm-pdf-page-extract (output-type)
  "Turn PDF page into an extract."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'page)))
  (cl-destructuring-bind (org-id attachment buffer collection headline level schedule-str)
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
                  :ext t))))
        ('org
         (org-ilm--capture
          'extract
          org-id
          (list 
           :title (format "Page %s" current-page-real)
           :id extract-org-id
           :ext "org")
          (lambda ()
            (org-ilm--pdf-convert-to-org
             pdf-path
             (1- current-page-real)
             extract-org-id
             (org-ilm--org-headline-element-from-id extract-org-id)
             (lambda () (message "Conversion finished."))))))
            )
         )))

(defun org-ilm-pdf-region-extract (output-type)
  "Turn selected PDF region into an extract.

TODO When extracting text, use add-variable-watcher to watch for changes
in pdf-view-active-region as it has no hooks. it allow buffer local and
set only (not let)."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'region)))
  (unless (pdf-view-active-region-p)
    (user-error "No active region."))
  
  (cl-destructuring-bind (org-id attachment buffer collection headline level schedule-str)
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
           capture-data)

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
               (list :file (org-ilm--pdf-image-export extract-org-id region)
                     :title title
                     :id extract-org-id
                     :ext t)))
        (t (error "Unrecognized output type")))

      (org-ilm--capture
       'extract
       org-id
       capture-data
       (lambda ()
         (with-current-buffer pdf-buffer
           (org-ilm--pdf-add-square-annotation
            region extract-org-id)))))))

(defun org-ilm-pdf-outline-extract ()
  "Turn PDF outline items into a extracts.

It's a bit of a black sheep compared to other extract options because we
make a bunch of headers."
  (interactive)
  (cl-destructuring-bind (org-id attachment buffer collection headline level schedule-str)
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
  (cl-destructuring-bind (org-id attachment buffer collection headline level schedule-str)
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
            (lambda ()
              (org-ilm--pdf-convert-to-org
               pdf-path
               (cons (1- (alist-get 'page section))
                     (1- (alist-get 'next-page section)))
               extract-org-id
               (org-ilm--org-headline-element-from-id extract-org-id)
               (lambda () (message "Conversion finished.")))))))
        )
      )))



;;;; Attachments

;; org-attach-delete-all

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
  "Returns (org-id collection) if current buffer is collection attachment file."
  (when-let ((location (org-ilm--where-am-i)))
    (when (eq 'attachment (car location))
      (cdr location))))

(defun org-ilm--attachment-path (&optional if-exists)
  "Return path to the attachment of heading at point."
  (when-let* ((org-id (org-id-get))
              (path (org-attach-expand (format "%s.org" org-id))))
    (when (or (not if-exists) (file-exists-p path))
      path)))

(defun org-ilm--attachment-extension ()
  "Return the extension of the attachment at point, assuming in collection."
  (or (org-entry-get nil "ILM_EXT" 'inherit) "org"))
  
(defun org-ilm--attachment-find (&optional type org-id)
  "Return attachment file of the headline."
  (when-let* ((attach-dir (org-attach-dir))
              (org-id (or org-id (org-id-get)))
              (type (or type (org-ilm--attachment-extension)))
              (path (expand-file-name (concat org-id "." type) attach-dir)))
      (when (file-exists-p path) path)))

(defun org-ilm--attachment-find-ancestor (type &optional headline)
  ""
  (let ((crumbs (cdr (org-mem-entry-crumbs (org-node-at-point))))
        attachment)
    (while (and crumbs (not attachment))
      (setq attachment (org-ilm--attachment-find type (nth 4 (pop crumbs)))))
    attachment))

(defun org-ilm--attachment-open (&optional pdf-no-region)
  "Open the attachemnt of collection element at point."
  (if-let* ((path (org-ilm--attachment-find)))
      (progn
        (run-hook-with-args 'org-attach-open-hook path)
        (org-open-file path 'in-emacs))
    (if-let* ((pdf-range (org-entry-get nil "PDF_RANGE"))
              ;; Returns 0 if not a number
              (pdf-page-maybe (string-to-number pdf-range))
              (attachment (org-ilm--attachment-find-ancestor "pdf"))
              (buffer-name (concat (org-id-get) ".pdf")))
        (if (not (= pdf-page-maybe 0))
            (org-ilm--pdf-open-page attachment pdf-page-maybe buffer-name)
          (org-ilm--pdf-open-ranges
           (list (cons attachment (org-ilm--pdf-range-from-string pdf-range)))
           buffer-name
           pdf-no-region))
      (error "Attachment not found"))))


;;;; Transclusion

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


;;;; Targets

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
     ;; cdr to get rid of headline priority in the car - redundant
     :subjects (cdr (org-ilm--priority-subject-gather headline))
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

(defun org-ilm-query-queue (collection query)
  "Apply org-ql QUERY on COLLECTION, parse org-ilm data, and return the results."
  (let ((entries (org-ql-select (cdr collection)
                   (funcall (cdr query))
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

(defun org-ilm-query-outstanding ()
  "Query for org-ql to retrieve the outstanding elements."
  `(or
    (and (todo ,org-ilm-incr-state) (scheduled :to today))
    (and (todo ,org-ilm-card-state) (org-ilm--ql-card-due))))


;;;; Queue


(defcustom org-ilm-queue-subject-nchar 6
  "Truncation size of subject names as displayed in the queue.
If available, the last alias in the ROAM_ALIASES property will be used."
  :type 'integer
  :group 'org-ilm)

;;;;; Building

;; TODO Finish after rewriting subject cache
(defun org-ilm-queue-add-dwim (arg)
  "Add element at point to queue. With ARG, empty queue before adding.

If point on headline, add headline and descendants.
If point on subject, add all headlines of subject."
  (interactive "P")
  (when-let ((headline (org-ilm--org-headline-at-point)))
    (let ((state (org-element-property :todo-keyword headline)))
      (cond
       ((member state org-ilm-subject-states)
        
        )))))

(defun org-ilm-queue-refresh (&optional select-collection)
  "Call the query again."
  (interactive "P")
  (let ((collection (if select-collection
                        (org-ilm--select-collection)
                      (or
                       (plist-get org-ilm-queue :collection)
                       (org-ilm--select-collection))))
        (query (or (plist-get org-ilm-queue :query) (org-ilm--select-query))))
    (setq org-ilm-queue
          (list
           :queue (org-ilm-query-queue collection query)
           :collection collection
           :query query))))

;;;;; Commands       

(defun org-ilm-queue (reset)
  "View queue in Agenda-like buffer."
  (interactive "P")
  (if (and org-ilm-queue (not reset))
      (org-ilm--queue-display)
    (org-ilm-queue-refresh)
    (org-ilm--queue-display)))


;;;;; Within queue 
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
    (define-key map (kbd "m") #'org-ilm-queue-object-mark)
    (define-key map (kbd "g") #'org-ilm-queue-revert)
    (define-key map (kbd "RET") #'org-ilm-queue-open-attachment)
    (define-key map (kbd "SPC") #'org-ilm-queue-open-element)
    (define-key map (kbd "M j") #'org-ilm-queue-mark-by-subject)
    (define-key map (kbd "M :") #'org-ilm-queue-mark-by-tag)
    (define-key map (kbd "M s") #'org-ilm-queue-mark-by-scheduled)
    ;; TODO r: Review start command
    ;; TODO G: query again - undo manual changes
    ;; TODO C-u G: like G but also select collection
    ;; TODO B: bulk commands
    map)
  "Keymap for the queue buffer.")

(defvar org-ilm--queue-marked-objects nil
  "Org id of marked objects in queue.")

(defun org-ilm--vtable-get-object (&optional index)
  "Return queue object at point or by index."
  (let ((index (or index (vtable-current-object))))
    (nth index (plist-get org-ilm-queue :queue))))

(defun org-ilm-queue-open-attachment (object)
  "Open attachment of object at point."
  (interactive (list (org-ilm--vtable-get-object)))
  (let ((loc (org-id-find (plist-get object :id) 'marker)))
    (with-current-buffer (marker-buffer loc)
      (goto-char loc)
      (org-ilm-open-attachment))))

(defun org-ilm-queue-open-element (object)
  "Open attachment of object at point."
  (interactive (list (org-ilm--vtable-get-object)))
  (let ((loc (org-id-find (plist-get object :id) 'marker)))
    (pop-to-buffer (marker-buffer loc))
    (goto-char loc)))

(defun org-ilm-queue-object-mark (object)
  "Mark the object at point."
  (interactive (list (org-ilm--vtable-get-object)))
  (let* ((id (plist-get object :id)))
    (if (member id org-ilm--queue-marked-objects)
        ;; Only toggle if called interactively
        (when (called-interactively-p)
          ;; not sure why but inconsistent behavior when not setq, even though
          ;; cl-delete is meant to remove destructively.
          (setq org-ilm--queue-marked-objects (cl-delete id org-ilm--queue-marked-objects  :test #'equal)))
      (cl-pushnew id org-ilm--queue-marked-objects  :test #'equal))
    ;; TODO gives cache error no idea why
    ;; Maybe worked on?: https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-07/msg00802.html
    ;; (vtable-update-object (vtable-current-table) object)
    (org-ilm-queue-revert))
  (when (called-interactively-p)
    (next-line)
    (when (eobp) (previous-line))))

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

  ;; Alternatively, we could have used
  ;; `org-ilm--subjects-get-with-descendant-subjects' to precompute the
  ;; descendancy, but this would require a list-to-list comparison eg with
  ;; `seq-some' per object, instead of just an `assoc'.
  (dolist (object (plist-get org-ilm-queue :queue))
    (let ((ancestor-ids (mapcar #'car (car (plist-get object :subjects)))))
      (when (member (plist-get subject :id) ancestor-ids)
        (org-ilm-queue-object-mark object)))))

(defun org-ilm-queue-mark-by-tag (tag)
  "Mark all elements in queue that have tag TAG."
  (interactive
   (list
    (completing-read
     "Tag: "
     (with-current-buffer (find-file-noselect (cdr (plist-get org-ilm-queue :collection)))
       (org-get-buffer-tags)))))
  (dolist (object (plist-get org-ilm-queue :queue))
    (when (member tag (plist-get object :tags))
      (org-ilm-queue-object-mark object))))

(defun org-ilm-queue-mark-by-scheduled (days)
  "Mark all elements in queue that are scheduled in DAYS days.
DAYS can be specified as numeric prefix arg."
  (interactive "N")
  (dolist (object (plist-get org-ilm-queue :queue))
    (when-let ((due (* -1 (plist-get object :scheduled-relative))))
      (when (<= (round due) days) 
        (org-ilm-queue-object-mark object)))))

(defun org-ilm-queue--set-header ()
  (setq header-line-format
        (concat
         (symbol-name (car (plist-get org-ilm-queue :query)))
         " ("
         (symbol-name (car (plist-get org-ilm-queue :collection)))
         ")")))

(defun org-ilm-queue-revert (&optional select-collection)
  (interactive "P")
  (org-ilm-queue-refresh select-collection)
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
                     (index-str (format "%4d" index)))
          (org-ilm--vtable-format-marked index-str marked))))
     (:name
      "Priority"
      :width 6
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,p) data))
          (if p
              (org-ilm--vtable-format-marked
               (propertize (format "%.2f" (* 100 p)) 'face 'shadow)
               marked)))))
     (:name
      "Type"
      :width 5
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
      ;; :max-width "50%"
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
     (let ((queue (plist-get org-ilm-queue :queue)))
       (unless (= 0 (length queue))
         (number-sequence 0 (- (length queue) 1)))))
   :getter
   (lambda (row column vtable)
     (let* ((object (org-ilm--vtable-get-object row))
            (id (plist-get object :id))
            (marked (member id org-ilm--queue-marked-objects))
            (subjects (plist-get object :subjects)))
       (pcase (vtable-column vtable column)
         ("Index" (list marked row))
         ("Type" (list marked (plist-get object :type)))
         ("Priority" (list marked (plist-get object :priority-sample)))
         ;; ("Cookie" (list marked (plist-get object :priority)))
         ("Title" (list marked (plist-get object :title)))
         ("Due" (list marked (plist-get object :scheduled-relative)))
         ;; ("Tags" (list marked (plist-get object :tags)))
         ("Subjects"
          (list marked
                (when (nth 0 subjects)
                  (mapcar (lambda (s) (org-mem-entry-by-id (car s)))
                          (last (nth 0 subjects) (nth 1 subjects)))))))))
   :keymap org-ilm-queue-map))

(defun org-ilm--queue-display ()
  "Open the active queue."
  (let ((buf (get-buffer-create "*ilm queue*")))
    (with-current-buffer buf
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (vtable-insert (org-ilm--queue-make-vtable))
      (org-ilm-queue--set-header)
      (setq-local buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buf)))


;;;; Import

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

    (org-ilm--capture
     'source
     `(file ,(cdr collection))
     (list :id org-id :file file-tmp-path))))

;;;; Stats
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


;;;; Priority

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

(defun org-ilm--beta-from-priority (priority &optional scale)
  "Beta parameters from priority value.
SCALE influence variance."
  (let* ((scale (or scale 1.))
         (a priority)
         (b (+ 1 (- 9 a))))
    (list (* scale a) (* scale b))))

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
  (let ((ancestors (nth 1 subjects)))
    (org-ilm--debug nil ancestors)
    (if (= 0 (length ancestors))
        params
      (apply #'org-ilm--beta-combine params
             ;; Tighter variance for subjects
             (mapcar (lambda (p) (org-ilm--beta-from-priority p 5.))
                     (mapcar #'cdr ancestors))))))

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
    (org-ilm--debug "Gathering subjects for:" id)
    (or (gethash id org-ilm-priority-subject-cache)
        (let* ((parents-data
                ;; Non-recursively, just direct parents in DAG
                (org-ilm--subjects-get-parent-subjects headline-or-id))
               (headline (car parents-data))
               (is-subject (member
                            (org-element-property :todo-keyword headline)
                            org-ilm-subject-states))
               (parent-ids (cdr parents-data))
               (priority (org-ilm--get-priority headline))
               (ancestor-data
                (mapcar
                 (lambda (parent-id)
                   (cons parent-id
                         (org-ilm--get-priority
                          (org-ilm--org-headline-element-from-id parent-id))))
                 parent-ids)))

          (org-ilm--debug nil ancestor-data)
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

;;;; SRS
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

    (org-ilm--capture
     'card
     file-org-id
     (list :id card-org-id :content buffer-text :title snippet)
     (lambda ()
       ;; Success callback. Go through each cloze in the source file and replace
       ;; it with our target tags. Render them by recreating overlays.
       (with-current-buffer file-buf
         (save-excursion
           ;; TODO similar functionality `org-ilm--buffer-text-clean', extract
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
         (org-ilm-recreate-overlays)))
     (lambda ()
       ;; Abort callback. Undo cloze if made automatically by this function.
       (when auto-clozed
         (with-current-buffer file-buf
           (org-srs-item-uncloze-dwim)))))))

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
         (headline-entry (org-node-by-id headline-id))
         (headline-ancestry (org-ilm--org-mem-ancestry-ids headline-entry))
         (headline-is-subj (member (org-element-property :todo-keyword headline) org-ilm-subject-states))
         (property-subjects-str "")
         outline-parent-subject subject-ids)

    (org-ilm--debug "Ancestory of:" headline-id)

    ;; Check for ancestor subject headline in outline hierarchy. As we explore
    ;; up the hierarchy, store linked subjects of extracts.
    (org-element-lineage-map
        headline
        (lambda (element)
          (let ((is-self (string= (org-element-property :ID element) headline-id))
                (state (org-element-property :todo-keyword element)))
            (cond
             ;; Headline is self or incremental ancestor, store linked subjects
             ((or is-self
                  (and (not headline-is-subj) ; Subject never inherit!
                       (string= state org-ilm-incr-state)))
              (when-let ((prop (org-entry-get element "SUBJECTS")))
                (setq property-subjects-str
                      (concat property-subjects-str " " prop))
                ;; Return nil so we don't terminate map
                nil))
             
             ;; Headline is subject, store as outline parent subject
             ((member state org-ilm-subject-states)
              ;; We could create id with second arg of org-id-get, but this
              ;; might mess up with parsing and cache and whatever as we're
              ;; updating the buffer during parsing
              (let ((org-id (org-id-get element)))
                (cl-pushnew org-id subject-ids :test #'equal)
                (unless outline-parent-subject
                  (setq outline-parent-subject org-id))
                ;; Return non-nil in case we dont want all ancestors. This
                ;; means we only gather linked subjects of extracts up to
                ;; first subject.
                org-id)))))
      '(headline) 'with-self (not all-ancestors))

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

    (org-ilm--debug "-- All ancestors:" subject-ids)
    
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

