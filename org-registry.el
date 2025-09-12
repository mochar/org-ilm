;;; org-registry.el --- Registry in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: html, org

;;; Commentary:

;; This package needs some commentary...

;;; Code:

;;;; Requirements
(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'org-latex-preview)
(require 'cl-lib)
(require 'dash)

;;;; Customization

(defgroup org-registry nil
  "Org registry"
  :group 'org
  :prefix "org-registry-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-registry"))

(defcustom org-registry-registries '("~/org/registry.org")
  "List of Org file paths to be used as registries."
  :type '(repeat file)
  :group 'org-registry)

;;;; Variables

;;;; Minor mode

(define-minor-mode org-registry-global-minor-mode
  "Prepare some hooks and advices some functions."
  :init-value nil
  :global t
  :lighter nil ;; String to display in mode line
  :group 'org-registry
  (if org-registry-global-minor-mode
      ;; Enable
      (progn
        (advice-add 'delete-overlay
                    :before #'org-registry--delete-overlay-advice)
        )
    ;; Disable
    (advice-remove 'delete-overlay #'org-registry--delete-overlay-advice)))

;;;; Link previews

(defun org-registry--link-preview (ov id link)
  ;; This function must return a non-nil value to indicate success.
  (let ((entry (org-mem-entry-by-id id)))
    (org-with-point-at (org-element-begin link)
      (pcase (org-mem-entry-property "TYPE" entry)
        ("latex" (org-registry--link-preview-latex entry ov link))
        ("file" (org-registry--link-preview-file entry ov link))))))

(defun org-registry--link-follow (id prefix-arg)
  (org-node-goto-id id))

(org-link-set-parameters
 "registry"
 :follow #'org-registry--link-follow
 :preview #'org-registry--link-preview)


;;;;; Latex preview

(defun org-registry--link-preview-latex (entry ov link)
  "Place a latex overlay on the link.

The way this is implemented is by using `org-latex-preview-place' which
 actually creates a new overlay. Therefore we need some way to detect
 when the link preview overlay is deleted, so that latex overlay can
 also be deleted. The function called when a preview is turned off is
 `org-link-preview-clear' which calls `delete-overlay'. Unfortunately
 the 'modification-hooks overlay property does not detect overlay
 deletions. So my best solution at the moment is to advice
 `delete-overlay', which is done in the global minor mode."
  (when-let ((latex (org-mem-entry-property "LATEX" entry)))
    (overlay-put ov 'org-registry-latex t)
    (org-latex-preview-place
     org-latex-preview-process-default
     (list
      (list
       (overlay-start ov)
       (overlay-end ov)
       latex)))
    t))

(defun org-registry--delete-overlay-advice (&rest args)
  (let ((ov (car args)))
    (when (overlay-get ov 'org-registry-latex)
    (dolist (o (overlays-in (overlay-start ov) (overlay-end ov)))
      (when (eq (overlay-get o 'org-overlay-type)
                'org-latex-overlay)
        (delete-overlay o)
      )
    )
  )))

;;;;; File preview
(defun org-registry--link-preview-file (entry ov link)
  (org-link-preview-file
   ov (expand-file-name
       (org-mem-entry-property "PATH" entry)
       "~/")
   link))


;;;; Footer

(provide 'org-registry)

;;; org-registry.el ends here


