;;; org-ilm-overview.el --- Collections view -*- lexical-binding: t; -*-

;;; Commentary:

;; Overview buffer.

;;; Code:

;;;; Requirements

(require 'magit-section)
(require 'org-ilm-collection)

;;;; Variables

(defconst org-ilm-overview-buffer-name "*Ilm Overview*")

;;;; Sections

;; Magit sections.

(defclass org-ilm-overview-collections-section (magit-section)
  ())

(defvar-keymap org-ilm-overview-collection-section-map
  :doc "Keymap for `collection' sections."
  "a" #'consult-buffer
  )

(defclass org-ilm-overview-collection-section (magit-section)
  ((keymap :initform 'org-ilm-overview-collection-section-map)
   (heading-highlight-face :initform 'magit-diff-file-heading-highlight)
   (heading-selection-face :initform 'magit-diff-file-heading-selection)
   ))

(defvar-keymap org-ilm-overview-queue-section-map
  :doc "Keymap for `queue' sections."
  )

(defclass org-ilm-overview-queue-section (magit-section)
  ((keymap :initform 'org-ilm-overview-queue-section-map)))

;;;; View

(defun org-ilm--overview-insert-collections ()
  (let* ((collections (org-ilm--collections))
         (align (if collections
                    (1+ (apply #'max (mapcar (lambda (c) (string-width (symbol-name (car c)))) collections)))
                  1)))
    (magit-insert-section (org-ilm-overview-collections-section)
      (magit-insert-heading "Collections")
      
      (dolist (collection collections)
        (let ((name (symbol-name (car collection)))
              (path (org-ilm--collection-property- collection :path)))
          (magit-insert-section (org-ilm-overview-collection-section collection t)
            (magit-insert-heading
              (propertize name 'font-lock-face 'magit-section-secondary-heading)
              (make-string (- align (length name)) ?\s)
              (propertize (abbreviate-file-name path) 'font-lock-face 'shadow))

            (magit-insert-section (org-ilm-overview-queue-section)
              (insert "Priority queue" ?\n))

            (magit-insert-section (org-ilm-overview-queue-section)
              (insert "Outstanding queue" ?\n))

            ))))))

(defun org-ilm--overview-revert (&optional arg noconfirm)
  (setq-local buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min))
  (magit-insert-section (magit-root-section)
    (org-ilm--overview-insert-collections))
  (magit-section-show-level-2)
  (setq-local buffer-read-only t)
  (goto-char (point-min)))

;;;; Mode

(defvar-keymap org-ilm-overview-mode-map
  :doc "Keymap for `org-ilm-overview-mode'."
  :parent magit-section-mode-map
  "r" #'org-ilm-review
  )

(define-derived-mode org-ilm-overview-mode magit-section-mode "Ilm Overview"
  ""
  :interactive nil
  :group 'org-ilm
  (setq-local revert-buffer-function #'org-ilm--overview-revert))

;;;; Commands

(defun org-ilm-overview ()
  "Open ilm overview buffer."
  (interactive)
  (let ((buf (get-buffer org-ilm-overview-buffer-name)))
    (unless buf
      (with-current-buffer (setq buf (generate-new-buffer org-ilm-overview-buffer-name))
        (org-ilm-overview-mode)
        (org-ilm--overview-revert)))
    (switch-to-buffer buf)))

;;; Footer

(provide 'org-ilm-overview)

;;; org-ilm-overview.el ends here
