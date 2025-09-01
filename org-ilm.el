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
  "Default TODO state of elements to be processed incrementally."
  :type 'string
  :group 'org-ilm)

(defcustom org-ilm-card-state "CARD"
  "Default TODO state of flash cards."
  :type 'string
  :group 'org-ilm)

(defcustom org-ilm-card-default-location 'collection
  "Whether to store the card content in the `collection' or as an `attachment'.

When set to `attachment', org-transclusion will be used to transclude the content in the collection during review."
  :type '(choice (const :tag "Collection" 'collection)
                 (const :tag "Attachment" 'attachment))
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

(defvar org-ilm-target-regexp "<<\\(extract\\|card\\):\\([^>]+\\)>>"
  "Regexp to match targets enclosing extracts and clozes.")

;;;; Minor mode

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
        )
    ;; Disable
    (remove-hook 'org-mode-hook #'org-ilm-prepare-buffer)
    ))

;;;; Commands
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
         (attach-org-id (car (org-ilm-infer-id-from-attachment-path file-path)))
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
             (let ((target-text (format "<<extract:%s>>" extract-org-id))
                   (is-begin-line (or (= region-begin 0) (eq (char-before) ?\n))))
               ;; Insert target before region
               (goto-char region-begin)
               (if is-begin-line
                   ;; Region starts at beginning of line: insert on new line above
                   (insert (format "%s\n" target-text))
                 ;; Otherwise, insert before region inline
                 (insert target-text))

               ;; Insert target after region, adjusting for start target length
               (goto-char (+ region-end (length target-text)))
               (insert target-text)))
           (save-buffer)
           (org-ilm-recreate-overlays)))))))

(defun org-ilm-prepare-buffer ()
  "Recreate overlays if current buffer is an attachment Org file."
  (interactive)
  (when (org-ilm-infer-id)
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
  (if-let ((org-id-loc (org-ilm-infer-id)))
      (progn
        (find-file (car (cdr org-id-loc)))
        (goto-char (cdr (cdr org-id-loc))))
    (let ((collection (org-ilm--select-collection)))
      (find-file (cdr collection)))))

(defun org-ilm-open-dwim ()
  "Open element of highlight or within collection."
  (interactive)
  (let ((collection-files (mapcar
                           (lambda (c) (file-truename (cdr c)))
                           org-ilm-collections-alist)))
    (cond
     ((member (file-truename buffer-file-name) collection-files)
      (org-ilm-open))
     ((org-ilm-infer-id)
      (unless (org-ilm-open-highlight)
        (org-ilm-open-collection))))))

(defun org-ilm-queue (collection)
  "View queue in Agenda-like buffer."
  (interactive (list (org-ilm--select-collection)))
  (setq org-ilm-queue (org-ilm--collect-queue-entries collection))
  (org-ilm--display-queue))

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
(defun org-ilm--select-collection ()
  "Prompt user for collection to select from.

The collections are stored in `org-ilm-collections-alist'. If empty
return nil, and if only one, return it."
  (pcase (length org-ilm-collections-alist)
    (0 nil)
    (1 (nth 0 org-ilm-collections-alist))
    (t (let* ((choices (mapcar
                        (lambda (collection)
                          (cons
                           (format "%s %s"
                                   (propertize
                                    (symbol-name (car collection))
                                    'face '(:weight bold))
                                   (propertize (cdr collection) 'face '(:slant italic)))
                           collection))
                        org-ilm-collections-alist))
              (choice (completing-read "Collection: " choices nil t))
              (collection (cdr (assoc choice choices))))
         collection))))

(defun org-ilm--parse-logbook (logbook)
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
    
(defun org-ilm--read-logbook (&optional headline)
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
            (org-ilm--parse-logbook logbook)))))))

(defun org-ilm--get-logbook (&optional headline)
  "Returns logbook or parses it if not available."
  (if-let ((logbook (org-element-property :logbook headline)))
      logbook
    (org-ilm--read-logbook headline)))

(defun org-ilm--capture (type target org-id file-path &optional title on-success on-abort)
  "Make an org capture to make a new source heading, extract, or card.

The callback ON-SUCCESS is called when capture is saved, by passing it to before-finalize hook, which is set to nil in `org-capture-kill'.

The callback ON-ABORT is called when capture is cancelled, which can be
detected in after-finalize hook with the `org-note-abort' flag set to t
in `org-capture-kill'.  TODO Therefore ON-SUCCESS can probably be called
in after-finalize as well, but I figured this out after implementing
ON-SUCCESS."
  (let* ((state (if (eq type 'card) org-ilm-card-state org-ilm-incr-state))
         (after-finalize (lambda ()
                           (when org-note-abort (funcall on-abort))))
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
                     (when (eq ',type 'source)
                       (org-entry-put
                        nil "DIR"
                        (abbreviate-file-name (org-attach-dir-get-create))))
                     
                     ;; Attach the file. We always use 'mv because we are
                     ;; importing the file from the tmp dir.
                     (org-attach-attach ,file-path nil 'mv)

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
                                 nil t)
                       ))
             :before-finalize ,on-success
             :after-finalize ,after-finalize))))
    (org-capture nil "i")))

(defun org-ilm--current-date-utc ()
  "Current date in UTC as string."
  (format-time-string "%Y-%m-%d" (current-time) t))

(defun org-ilm--buffer-text-clean (&optional region-begin region-end)
  "Extract buffer text without clozes and extract and card targets."
  (let ((text (s-replace-regexp
               org-ilm-target-regexp ""
               (buffer-substring-no-properties (or region-begin (point-min))
                                               (or region-end (point-max))))))
    ;; Remove clozes by looping until none left
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
      (buffer-string))))

(defun org-ilm--generate-text-snippet (text)
  ""
  (let* ((text-clean (replace-regexp-in-string "\n" " " text))
         (snippet (substring text-clean 0 (min 50 (length text-clean)))))
    snippet))

(defun org-ilm--where-am-i ()
  "Returns one of 'collection, 'attachment, nil."
  ;; TODO
  )


;;;;; Attachments
(defun org-ilm-infer-id ()
  "Attempt parsing the org-id of current attachment file."
  (org-ilm-infer-id-from-attachment-path buffer-file-name))
  
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

(defun org-ilm--attachment-path (&optional if-exists)
  "Return path to the attachment of heading at point."
  (when-let* ((org-id (org-id-get))
              (path (org-attach-expand (format "%s.org" org-id))))
    (when (or (not if-exists) (file-exists-p path))
      path)))

;;;;;; Transclusion
(defun org-ilm--org-narrow-to-header ()
  ""
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1) (narrow-to-region (point-min) (point))))

(defun org-ilm--attachment-transclusion-create-text (&optional path level)
  ""
  (when-let ((path (or path (org-ilm--attachment-path t)))
             (level (or level (org-current-level))))
    (format "#+transclude: [[file:%s]] :level %s" path (+ 1 level))))

(defun org-ilm-attachment-transclusion-create (&optional transclude)
  "Create and move point to transclusion for the attachment of header at point."
  (interactive)
  (when-let* ((transclusion-text (org-ilm--attachment-transclusion-create-text))
              (transclusion-pattern ; May start with whitespace
               (concat "^[ \t]*" (regexp-quote transclusion-text))))
    (save-restriction
      (org-ilm--org-narrow-to-header)
      ;; Need to remove active transclusion, otherwise pattern not found.
      (org-transclusion-remove-all)
      (goto-char (point-min))
      (if (re-search-forward transclusion-pattern nil t)
          ;; Transclusion text exists, go to beginning of the line
          (beginning-of-line)
        ;; Not found, append at end of the header
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert transclusion-text "\n")
        (beginning-of-line)))
    (when transclude
      (org-transclusion-add))))

(defun org-ilm-attachment-transclusion-transclude ()
  "Transclude the contents of the current heading's attachment."
  (interactive)
  (save-excursion
    (org-ilm-attachment-transclusion-create t)))


;;;;; Extracts

(defun org-ilm--ov-block-edit (ov after beg end &optional len)
  (unless after
    (user-error "Cannot modify this region")))

(defun org-ilm--add-target-end-nowhite-property (target)
  "Add :end-nowhite property which excludes spaces at the end of target string."
  (let* ((begin (org-element-property :begin target))
         (end (org-element-property :end target))
         (target-text (buffer-substring-no-properties begin end))
         (trailing-spaces (length (progn
                                    (string-match " *\\'" target-text)
                                    (match-string 0 target-text))))
         (end-nowhite (- end trailing-spaces)))
    (org-element-put-property target :end-nowhite end-nowhite)))

(defun org-ilm--create-overlay (target-begin target-end &optional no-face)
  ""
  (setq target-begin (org-ilm--add-target-end-nowhite-property target-begin))
  (setq target-end (org-ilm--add-target-end-nowhite-property target-end))
  
  ;; Hide targets
  (dolist (target (list target-begin target-end))
    (let ((begin (org-element-property :begin target))
          (end (org-element-property :end-nowhite target)))
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
  (let* ((parts (split-string (org-element-property :value target-begin) ":"))
         (type (nth 0 parts))
         (id (nth 1 parts))
         (face (pcase type
                 ("extract" 'org-ilm-face-extract)
                 ("card" 'org-ilm-face-card)))
         (ov (make-overlay
             (org-element-property :begin target-begin)
             (org-element-property :end-nowhite target-end))))
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
   (let ((targets (make-hash-table :test 'equal)))
     (org-element-map (org-element-parse-buffer) 'target
       (lambda (target)
         (let* ((value (org-element-property :value target))
                (parts (split-string value ":"))
                (type (nth 0 parts))
                (id (nth 1 parts)))
           (when (and
                  id
                  (member type '("extract" "card")))
             (if-let ((prev-target (gethash value targets)))
                 (progn
                   (org-ilm--create-overlay prev-target target no-face)
                   (remhash value targets))
               (puthash value target targets)))))))))

(defun org-ilm--open-from-ov (ov)
  ""
  (when-let ((id (overlay-get ov 'org-ilm-id)))
    (find-file (format "%s.org" id))
    id))

;;;;; Queue view

(defun org-ilm--ql-headline-action ()
  "Add some custom properties to headline when org-ql parses buffer."
  (let* (;; This part is same as `element-with-markers' action of org-ql.
         (headline (org-ql--add-markers
                    (org-element-headline-parser (line-end-position))))
         (is-card (string-equal (org-element-property :todo-keyword headline)
                                org-ilm-card-state)))
    (if is-card
        ;; If card, set due time of earliest item as scheduled property
        ;; This is a hack to show due time in days in the agenda view.
        (when-let* ((due-timestamp-str (org-ilm--srs-earliest-due-timestamp))
                    (due-timestamp (parse-iso8601-time-string due-timestamp-str))
                    (due-org-timestamp (org-timestamp-from-time due-timestamp)))
          (setq headline (org-element-put-property headline :scheduled due-org-timestamp)))
      ;; If not card, attach logbook as property
      (let ((logbook (unless is-card (org-ilm--read-logbook headline))))
        (setq headline (plist-put headline :logbook logbook))))

    ;; Sample priority value
    (let ((sample (org-ilm--sample-priority-from-headline headline)))
      (setq headline (plist-put headline :priority-sample sample)))
    headline))

(defun org-ilm--compare-priority (first second)
  "Comparator of two headlines by sampled priority."
  (when-let ((priority-first (org-element-property :priority-sample first))
             (priority-second (org-element-property :priority-sample second)))
    (< priority-first priority-second)))

(defun org-ilm--collect-queue-entries (collection)
  "Return entries (headings) that form the outstanding queue.

Ideally we want to use the (scheduled :to today) predicate instead of
post-query filtering like we do here, since we do add the :scheduled
property to card headlines with `org-ilm--ql-headline-action'. However
because org-ql applies this action after the query filter, this does not
work."
  (with-current-buffer (find-file-noselect (cdr collection))
    (let ((entries (org-ql-select (current-buffer)
                     `(or
                       (and
                        (todo ,org-ilm-incr-state)
                        (scheduled :to today))
                       (todo ,org-ilm-card-state))
                     :action #'org-ilm--ql-headline-action
                     :sort #'org-ilm--compare-priority)))
      (seq-filter
       (lambda (entry)
         (let ((is-card (string-equal
                         (org-element-property :todo-keyword entry)
                         org-ilm-card-state)))
           (if is-card
               (when-let* ((scheduled (org-element-property :scheduled entry))
                           (scheduled-str (org-timestamp-translate scheduled)))
                 (<= (org-timestamp-to-now scheduled-str) 0))
             t)))
       entries))))

(defun org-ilm--format-heading-element (element)
  "Add priority to heading format on top of what org-ql adds."
  (let* ((string (org-ql-view--format-element element))
         (priority (* 100 (plist-get element :priority-sample)))
         (prefix (propertize (format "%.2f" priority) 'face 'shadow))
         (new-string (concat "   " prefix " " (string-trim-left string))))
    ;; Need to add back properties for agenda to work properly
    (org-add-props new-string (text-properties-at 0 string))))

(defun org-ilm--display-queue ()
  "Open an Org-ql view of elements in `org-ilm-queue'."
  (let ((strings (-map #'org-ilm--format-heading-element org-ilm-queue)))
    (org-ql-view--display :buffer "*Ilm Queue*" :header "Ilm queue" :strings strings)))


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
      (setq p (* p (random-uniform))))
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

(defun org-ilm--adjusted-priority-from-logbook (a b logbook)
  "Calculate the variance adjusted beta parameters from logbook data.

For now, decrease variance by proportionally scaling a and b by some
factor that increases with the number of reviews."
  (let* ((reviews (length logbook))
         (a (* a (+ 1 reviews)))
         (b (* b (+ 1 reviews))))
    (list a b)))

(defun org-ilm--beta-from-priority-and-logbook (priority logbook)
  "Beta parameters from priority value, adjusted by logbook history."
  (let ((params (org-ilm--beta-from-priority priority)))
    (org-ilm--adjusted-priority-from-logbook (nth 0 params)
                                             (nth 1 params)
                                             logbook)))

(defun org-ilm--get-priority-params (&optional headline)
  "Calculate the beta parameters of the heading at point."
  (let ((priority (org-ilm--get-priority headline))
        (logbook (org-ilm--get-logbook headline)))
    (org-ilm--beta-from-priority-and-logbook priority logbook)))

(defun org-ilm--sample-priority (beta-params &optional seed)
  "Sample a priority value given the beta params."
  (interactive (list (org-ilm--get-priority-params)))
  (org-ilm--random-beta (nth 0 beta-params)
                        (nth 1 beta-params)
                        seed))

(defun org-ilm--sample-priority-from-headline (headline)
  "This still assumes there is a headline at point. Used in `org-ilm--ql-headline-action'."
  (when-let* ((beta (org-ilm--get-priority-params headline))
              (seed (format "%s%s"
                            (org-ilm--current-date-utc)
                            (org-element-property :ID headline)))
              (sample (org-ilm--sample-priority beta seed)))
    sample))

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
    
    (setq buffer-text (org-ilm--buffer-text-clean)
          snippet (org-ilm--generate-text-snippet buffer-text))

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
                    (target-text (format "<<card:%s>>" card-org-id)))
               (goto-char region-begin)
               (delete-region region-begin region-end)
               (insert target-text inner target-text))))
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
    (apply #'org-srs-timestamp-min due-timestamps)))

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


;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here

