;;; org-ilm-image.el --- Image attachments -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-attachment)
(require 'org-ilm-convert)

;;;; Variables

(defvar-keymap org-ilm-image-map
  :doc "Keymap for image attachments."
  "c o" #'org-ilm-image-convert-to-org
  "c p" #'org-ilm-image-convert-to-pdf)

;;;; Functions

(cl-defun org-ilm--image-convert-attachment-to-org (image-path org-id &key on-success on-error)
  "Convert an attachment image to Org with Marker."
  (let ((headline (org-ilm--org-headline-element-from-id org-id)))
    (org-ilm-convert--convert-to-org-with-marker-pandoc
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
   (lambda (&rest _) 
     (when (yes-or-no-p "Conversion finished. Use as main attachment?")
       (org-entry-put nil org-ilm-property-ext "org")))))

(defun org-ilm-image-convert-to-pdf ()
  "Convert image attachment to a PDF file."
  (interactive)
  (unless (eq major-mode 'image-mode)
    (user-error "Not in image buffer."))
  (pcase-let* ((`(,id ,collection ,file) (org-ilm--attachment-data))
               (from-file buffer-file-name)
               (to-file (concat (file-name-sans-extension from-file) ".pdf" ))
               (cmd (format "convert %s %s" from-file to-file)))
    (unless id (user-error "Not in an attachment buffer."))
    (let ((status (call-process-shell-command cmd)))
      (if (zerop status)
          (when (yes-or-no-p "Conversion finished. Use as main attachment?")
            (org-ilm--org-with-point-at id
              (org-entry-put nil org-ilm-property-ext "pdf")))
        (user-error "Conversion failed with code %d" status)))))

;;; Footer

(provide 'org-ilm-image)

;;; org-ilm-image.el ends here
