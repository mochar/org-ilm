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

(defvar-keymap org-ilm-overview-collection-section-map
  :doc "Keymap for `collection' sections."
  "RET" (lambda (&optional priority-queue)
          (interactive "P")
          (let ((collection (car (oref (magit-section-at) value))))
            (if priority-queue
                (org-ilm--bqueue-open-pqueue collection)
              (org-ilm--bqueue-open-outstanding collection))))
  )

(defclass org-ilm-overview-collection-section (magit-section)
  ((keymap :initform 'org-ilm-overview-collection-section-map)
   (heading-highlight-face :initform 'magit-diff-file-heading-highlight)
   (heading-selection-face :initform 'magit-diff-file-heading-selection)
   ))

(defvar-keymap org-ilm-overview-queue-section-map
  :doc "Keymap for `queue' sections."
  "RET" (lambda ()
          (interactive)
          (pcase (oref (magit-section-at) value)
                          (`(:priority . ,collection)
                           (org-ilm--bqueue-open-pqueue collection))
                          (`(:outstanding . ,collection)
                           (org-ilm--bqueue-open-outstanding collection))
                          (`(:custom . ,queue)
                           (org-ilm--bqueue-open-custom
                            (map-elt (cdr queue) :collection)
                            (car queue)))))
  )

(defclass org-ilm-overview-queue-section (magit-section)
  ((keymap :initform 'org-ilm-overview-queue-section-map)))

;;;; View

(defun org-ilm--overview-insert-collections ()
  (let* ((collections (org-ilm--collections))
         (path->collections (seq-group-by
                             (lambda (c) (org-ilm--collection-property- c :path))
                             collections)))
    (dolist (path-collections path->collections)
      (let* ((path (car path-collections))
             (collections (cdr path-collections)))
        (magit-insert-section (magit-section)
          (magit-insert-heading
            (propertize (abbreviate-file-name path) 'font-lock-face 'magit-section-heading)
            (propertize (format "\s(%s)" (length collections)) 'font-lock-face t))
          
          (dolist (collection collections)
            (let ((name (car collection)))
              (magit-insert-section (org-ilm-overview-collection-section collection t)
                (magit-insert-heading
                  (propertize (symbol-name name) 'font-lock-face 'magit-diff-file-heading)
                  )

                (magit-insert-section (org-ilm-overview-queue-section (cons :priority name))
                  (insert "Priority queue" ?\n))

                (magit-insert-section (org-ilm-overview-queue-section (cons :outstanding name))
                  (insert "Outstanding queue" ?\n))

                (dolist (queue (map-elt org-ilm-custom-queues name))
                  (magit-insert-section (org-ilm-overview-queue-section (cons :custom queue))
                    (insert (map-elt (cdr queue) :name) ?\n)))

                ))))
        
        (insert ?\n)
        ))))

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
