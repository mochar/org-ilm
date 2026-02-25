;;; org-ilm-attachment.el --- Attachments -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'transient)

(require 'org-ilm-registry)
(require 'org-ilm-core)

;;;; Variables

(defvar org-ilm-attachment-after-setup-hook nil
  "Hook run when finished setting up attachment buffer.

Functions are passed the current buffer as argument.")

;;;; Functions

;; TODO I dont actually use this
(defun org-ilm--attachment-dir ()
  "Attachment directory of element at point.
This just returns the inherited DIR property, but only if the property
holding headline is an ilm element."
  (org-element-lineage-map
      (org-element-at-point nil)
      (lambda (el)
        (and (org-ilm--element-by-id (org-element-property :ID el))
             (org-element-property :DIR el)))
    '(headline org-data) ;; TODO exlcude org-data?
    'with-self 'first-match))

(defvar-local org-ilm--data nil
  "Buffer-local ilm data stored in element attachment buffers.")

(defun org-ilm--attachment-element-id ()
  (file-name-base
   ;; Allow for non-file buffer: pdf virtual view
   (or buffer-file-name (buffer-name))))

(defun org-ilm--attachment-element-entry ()
  (-some-> (org-ilm--attachment-element-id)
    org-mem-entry-by-id))

(defun org-ilm--attachment-element ()
  (-some-> (org-ilm--attachment-element-id)
    org-ilm--element-by-id))

(defun org-ilm--attachment-data ()
  "Returns (org-id collection file type) if current buffer is collection attachment file."
  (when-let* ((entry (org-ilm--attachment-element-entry))
              (id (org-mem-entry-id entry))
              (element (org-ilm--element-from-entry entry))
              (src-file (org-mem-entry-file entry))
              (src-file (expand-file-name src-file)) ;; sep line, else err
              (collection (org-ilm-element--collection element))
              (type (cond
                     ((org-ilm--pdf-mode-p) 'pdf)
                     ((oref element media) 'media)
                     ((eq major-mode 'org-mode) 'org)
                     ((eq major-mode 'image-mode) 'image)
                     (t major-mode))))
    ;; Exclude registries
    (when (and (member (oref element type) '(material card))
               (not (string= src-file (org-ilm--collection-registry-path collection))))
      (list id collection src-file type))))

(defun org-ilm--attachment-extension ()
  "Return the extension of the attachment at point, assuming in collection."
  (or (org-entry-get nil org-ilm-property-ext 'inherit) "org"))

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
    ;; First check if attachment is in the process of being generated with a
    ;; conversion tool.
    ([org-id (org-id-get)]
     [conversion (org-ilm-convert--conversion-by-id org-id)]
     [state (plist-get conversion :state)]
     [_ (eq state 'busy)]
     (let ((message (pcase state
                      ;; TODO for success and error, provide option to
                      ;; delete headline and extract highlight in parent
                      ;; attachment.
                      ('success "Attachment finished conversion but not found.")
                      ('error "Attachment conversion failed.")
                      (_ "Attachment still being converted."))))
       (when (yes-or-no-p (concat message " View conversion buffer?"))
         (pop-to-buffer (plist-get conversion :buffer)))))
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
    ;; Check if is media but missing an org file
    ([media (org-ilm--media-open)]
     (let ((f (expand-file-name (concat (org-id-get) ".org")
                                        (org-attach-dir))))
       (make-empty-file f)
       (find-file f)))
    ;; ;; Check if there is a web link in the ROAM_REFS property and open website in
    ;; ;; eww.
    ;; ([web-refs (org-ilm--org-mem-website-refs)]
    ;;  [web-ref (if (= 1 (length web-refs))
    ;;               (car web-refs)
    ;;             (completing-read "Open: " web-refs nil t))]
    ;;  ;; We use window excursion so that we can return the eww buffer
    ;;  (save-window-excursion
    ;;    (eww-browse-url web-ref))
    ;;  (switch-to-buffer "*eww*"))
    (t (unless no-error (user-error "Attachment not found")))))
  
(defun org-ilm--attachment-open-by-id (id)
  (org-ilm--org-with-point-at id
    (org-with-wide-buffer
     (org-ilm--attachment-open))))

(cl-defgeneric org-ilm--attachment-setup ()
  (message ""))

(defun org-ilm--attachment-prepare-buffer ()
  "Prepare ilm attachment buffers."
  (unless (bound-and-true-p org-ilm--data) 
    (when-let ((data (org-ilm--attachment-data)))
      (pcase-let* ((`(,id ,collection ,collection-file ,attach-type) data)
                   (element (org-ilm--element-by-id id))
                   (entry (org-mem-entry-by-id id)))

        ;; Prepare the buffer local data object which contains info about the
        ;; attachment as well as data used to update the priority.
        (setq-local
         org-ilm--data
         (list :id id
               :type (oref element type)
               :collection collection
               :attach-type attach-type
               ))

        (org-ilm--attachment-setup)
        (run-hook-with-args 'org-ilm-attachment-after-setup-hook (current-buffer))))))

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


;;;;; Navigation

(defun org-ilm--attachment-navigate (org-nav-cmd &rest args)
  (if-let ((attachment (org-ilm--attachment-data))
           (buffer (current-buffer)))
      (org-ilm--org-with-point-at (car attachment)
        (org-with-wide-buffer
         (let ((point (point)))
           (apply org-nav-cmd args)
           (when (= point (point))
             (user-error "Nothing there")))
         (condition-case err
             (progn
               (org-ilm--attachment-open)
               (when (called-interactively-p)
                 (kill-buffer buffer))
               attachment)
           (error
            (user-error "%s" (error-message-string err))))))
    (user-error "Not in an ilm attachment!")))

(defun org-ilm-attachment-navigate-forward ()
  (interactive)
  (org-ilm--attachment-navigate #'org-forward-heading-same-level 1 t))

(defun org-ilm-attachment-navigate-backward ()
  (interactive)
  (org-ilm--attachment-navigate #'org-backward-heading-same-level 1 t))

(defun org-ilm-attachment-navigate-previous ()
  (interactive)
  (org-ilm--attachment-navigate #'org-previous-visible-heading 1))

(defun org-ilm-attachment-navigate-next ()
  (interactive)
  (org-ilm--attachment-navigate #'org-next-visible-heading 1))

(defun org-ilm-attachment-navigate-up ()
  (interactive)
  (let ((orig-attachment (org-ilm--attachment-navigate #'org-up-heading 1 t)))
    (goto-char (point-min))
    (re-search-forward (car orig-attachment) nil t)))

;;;;; Transient

(defun org-ilm-attachment-actions ()
  "Open menu with actions to be applied on current element attachment."
  (interactive)
  (if (eq (car (org-ilm--where-am-i)) 'attachment)
      (if-let ((element (org-ilm--element-from-context)))
          (org-ilm--attachment-transient element)
        (user-error "Element not found"))
    (user-error "Not in an element attachment!")))

(transient-define-prefix org-ilm--attachment-transient (scope)
  ["Attachment"
   (:info*
    (lambda ()
      (propertize
       (org-ilm-element--title (transient-scope))
       'face 'italic)))
   ]
  
  [
   ["Actions"
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
   ["Media"
    :if (lambda () (org-ilm-element--media (transient-scope)))
    ("ms" "Insert subtitles"
     (lambda ()
       (interactive)
       (org-ilm--media-insert-subtitles)))
    ("mc" "Insert chapters"
     (lambda ()
       (interactive)
       (org-ilm--media-insert-chapters)))
    ]
   ["Navigation"
    ("f" "Forward" org-ilm-attachment-navigate-forward)
    ("b" "Backward" org-ilm-attachment-navigate-backward)
    ("n" "Next" org-ilm-attachment-navigate-next)
    ("p" "Previous" org-ilm-attachment-navigate-previous)
    ("u" "Up" org-ilm-attachment-navigate-up)
    ]
   ]

  (interactive "P")
  (transient-setup 'org-ilm--attachment-transient nil nil :scope scope))

;;; Footer

(provide 'org-ilm-attachment)

;;; org-ilm-attachment.el ends here
