;;; org-ilm-transclusion.el --- Transclusion -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-transclusion)

(require 'org-ilm-utils)
(require 'org-ilm-attachment)

;;;; Variables

(defcustom org-ilm-transclusion-attachment-exts (append '("org") image-file-name-extensions)
  "List of extensions that are allowed to be transcluded from within the collection element.

See `org-ilm-attachment-transclude'."
  :type '(repeat string)
  :group 'org-ilm)

;;;; Functions

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

;;; Footer

(provide 'org-ilm-transclusion)

;;; org-ilm-transclusion.el ends here
