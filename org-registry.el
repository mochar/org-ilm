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
(require 'org-ql)
(require 'org-node)
(require 'org-transclusion)
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

(defcustom org-registry-types nil
  "Alist mapping registry type name to plist properties."
  :type '(alist :tag "Registry type parameters"
		:value-type plist)
  :group 'org-registry)

(defface org-registry-link-face
  '((t :inherit org-link))
  "Face used by registry link types."
  :group 'org-registry)

;;;; Minor mode

(defun org-registry--delete-overlay-advice (&rest args)
  "Advice to detect if our org-link face was deleted."
  (let ((ov (car args)))
    (when-let* ((type (overlay-get ov 'org-registry-type))
                (data (cdr (assoc type org-registry-types)))
                (teardown-func (plist-get data :teardown)))
      (funcall teardown-func ov))))

(define-minor-mode org-registry-global-minor-mode
  "Prepare some hooks and advices some functions."
  :init-value nil
  :global t
  :lighter nil ;; String to display in mode line
  :group 'org-registry
  (if org-registry-global-minor-mode
      ;; Enable
      (progn
        ;; Detect deletion, call type :teardown ov
        (advice-add 'delete-overlay
                    :before #'org-registry--delete-overlay-advice)
        
        (dolist (type-data org-registry-types)
          (when-let ((f (plist-get (cdr type-data) :setup)))
            (funcall f))))
    ;; Disable
    (dolist (type-data org-registry-types)
        (when-let ((f (plist-get (cdr type-data) :cleanup)))
          (funcall f)))
    (advice-remove 'delete-overlay
                   #'org-registry--delete-overlay-advice)))

;;;; Types

(defun org-registry-set-type (type &rest parameters)
  "Set registry TYPE properties to PARAMETERS.
PARAMETERS should be keyword value pairs. See `org-registry-types'."
  (let ((data (assoc type org-registry-types)))
    (if data
        (setcdr data (org-combine-plists (cdr data) parameters))
      (push (cons type parameters) org-registry-types))))

(defun org-registry--link-preview (ov id link)
  ;; This function must return a non-nil value to indicate success.
  (let* ((entry (org-mem-entry-by-id id))
         (type (org-mem-entry-property "TYPE" entry)))
    (when-let ((data (cdr (assoc type org-registry-types))))
      (overlay-put ov 'org-registry-type type)
      (org-with-point-at (org-element-begin link)
        (funcall (plist-get data :preview) entry ov link)))))

(defun org-registry--link-follow (id prefix-arg)
  (org-node-goto-id id))

(defun org-registry--link-face (id)
  (if-let ((entry (org-mem-entry-by-id id))
             (type (org-mem-entry-property "TYPE" entry))
             (data (cdr (assoc type org-registry-types)))
             (face (plist-get data :face)))
      face
    'org-registry-link-face))

(org-link-set-parameters
 "registry"
 :follow #'org-registry--link-follow
 :preview #'org-registry--link-preview
 :face #'org-registry--link-face)


;;;;; Latex type

(defun org-registry--type-latex-preview (entry ov link)
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

(defun org-registry--type-latex-teardown (ov)
  "When latex registry link ov gets deleted, remove the latex ov inside it."
  (dolist (o (overlays-in (overlay-start ov) (overlay-end ov)))
    (when (eq (overlay-get o 'org-overlay-type)
              'org-latex-overlay)
      (delete-overlay o))))

(org-registry-set-type
 "latex"
 :preview #'org-registry--type-latex-preview
 :teardown #'org-registry--type-latex-teardown)


;;;;; File type

(defun org-registry--type-file-preview (entry ov link)
  (org-link-preview-file
   ov (expand-file-name
       (org-mem-entry-property "PATH" entry)
       "~/")
   link))

(org-registry-set-type
 "file" :preview #'org-registry--type-file-preview)

;;;;; Org type

(defface org-registry-org-header-face
  '((t :slant italic))
  "Face used to display the title above the transcluded content.")

(defun org-registry--type-org-preview (entry ov link)
  "Transclude Org mode content with org-transclusion."
  (let ((begin (overlay-start ov))
        (end (overlay-end ov)))
    (goto-char end)
    (insert (format "\n#+transclude: [[id:%s]] :only-contents" (org-mem-entry-id entry)))
    (org-transclusion-add)
    (overlay-put ov 'display (org-mem-entry-title entry))
    ;; TODO fgiure out how to not inherit org-link properties
    (overlay-put ov 'face 'org-registry-org-header-face)
    t))

(defun org-registry--type-org-teardown (ov)
  (save-excursion
    (goto-char (overlay-end ov))
    ;; TODO might be better to keep checking until non-emtpy line
    (forward-line)
    (cond
     ((org-transclusion-within-transclusion-p)
        (progn
          (org-transclusion-remove)
          (delete-line)))
     ((org-transclusion-check-add)
      (delete-line))
     (t
      (message "Cannot find transclusion to clean up")))))

(org-registry-set-type
 "org"
 :preview #'org-registry--type-org-preview
 :teardown #'org-registry--type-org-teardown)

;;;; Footer

(provide 'org-registry)

;;; org-registry.el ends here


