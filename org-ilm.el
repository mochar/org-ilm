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

;;;; Variables

(defface org-ilm-face-extract
  '((t :background "yellow"))
  "Face used to highlight extracts.")

(defvar org-ilm-queue nil
  "List of org headings that form the queue.")

;;;; Commands
(defun org-ilm-import-website (url)
  ""
  (interactive "sURL: "))

(defun org-ilm-extract ()
  "Extract text in region to attachment Org file that is the child heading of current entry."
  (interactive)
  (unless (use-region-p) (user-error "No region active."))
  (let* ((file-path buffer-file-name)
         (file-name (file-name-sans-extension
                     (file-name-nondirectory
                      file-path)))
         (attach-org-id (org-ilm-infer-id-from-attachment-path file-path))
         (attach-org-id-marker (org-id-find attach-org-id t))
         (file-org-id file-name)
         (file-org-id-marker (org-id-find file-org-id t)))
    (unless attach-org-id-marker
      (error "Current file is not an attachment, or failed to parse org ID."))
    (unless file-org-id-marker
      (error "Current file name not org ID, or failed to find it."))
    
    (let* ((region-begin (region-beginning))
           (region-end (region-end))
           (region-text (buffer-substring-no-properties region-begin region-end))
           (region-text-clean (replace-regexp-in-string "\n" " " region-text))
           (snippet (substring region-text-clean 0 (min 50 (length region-text-clean))))
           (extract-org-id (org-id-new))
           (extract-tmp-path (expand-file-name
                              (format "%s.org" extract-org-id)
                              temporary-file-directory)))

      
      ;; Save region content into tmp file and move it as attachment to main
      ;; heading.
      (write-region region-text nil extract-tmp-path)
      (org-with-point-at attach-org-id-marker
        (org-attach-attach extract-tmp-path nil 'mv)
        (save-buffer))

      ;; Create child heading.
      (org-with-point-at file-org-id-marker
        (org-insert-heading-respect-content)
        (org-do-demote) ;; make it a child
        (insert snippet)
        (org-set-property "ID" extract-org-id)
        (save-buffer))
      
      ;; Wrap region with targets.
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
      (org-ilm-recreate-overlays))))

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

(defun org-ilm-queue (collection)
  "View queue in Agenda-like buffer."
  (interactive (list (org-ilm--select-collection)))
  (setq org-ilm-queue (org-ilm--collect-queue-entries collection))
  (org-ilm--display-queue))


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

;;;;; Attachments
(defun org-ilm-infer-id ()
  "Attempt parsing the org-id of current attachment file."
  (org-ilm-infer-id-from-attachment-path buffer-file-name))
  
(defun org-ilm-infer-id-from-attachment-path (path)
  "Attempt parsing the org-id from the attachment path."
  (when path
    (let ((parts (split-string (file-name-directory path) "/" t)))
      (pcase (file-name-nondirectory (org-attach-dir-from-id "abc"))
        ("abc" (car (last parts)))
        ("ab/c" (mapconcat #'identity (last parts 2) ""))
        (t nil)))))

;;;;; Extracts

(defun org-ilm--ov-block-edit (ov after beg end &optional len)
  (unless after
    (user-error "Cannot modify this region")))

(defun org-ilm--create-overlay (target-begin target-end)
  ""
  (message "Creating ilm overlay")
  
  ;; Hide targets
  (dolist (target (list target-begin target-end))
    (let ((begin (org-element-property :begin target))
          (end (org-element-property :end target)))
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
         (ov (make-overlay
             (org-element-property :begin target-begin)
             (org-element-property :end target-end))))
    (overlay-put ov 'face 'org-ilm-face-extract)
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
(defun org-ilm-recreate-overlays ()
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
           (message "Target: %s %s" type id)
           (when (and
                  id
                  (member type '("extract" "cloze")))
             (if-let ((prev-target (gethash value targets)))
                 (org-ilm--create-overlay prev-target target)
               (puthash value target targets)))))))))

(defun org-ilm--open-from-ov (ov)
  ""
  (when-let ((id (overlay-get ov 'org-ilm-id)))
    (find-file (format "%s.org" id))))

;;;;; Queue view

(defun org-ilm--collect-queue-entries (collection)
  "Return entries (headings) that form the outstanding queue."
  ;; TODO This now returns all headings, later should take into account outstanding.
  (with-current-buffer (find-file-noselect (cdr collection))
    (org-ql-select (current-buffer) nil :action 'element-with-markers)))

(defun org-ilm--display-queue ()
  "Open an Org-ql view of elements in `org-ilm-queue'."
  (let ((strings (-map #'org-ql-view--format-element org-ilm-queue)))
    (org-ql-view--display :buffer "*Ilm Queue*" :header "Ilm queue" :strings strings)))


;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here

