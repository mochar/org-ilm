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
(require 'map)
(require 'dash)
(require 'rx)
(require 'ts)
(require 'vtable)
(require 'eldoc)
(require 'cond-let)
(require 'embark)

(require 'ost)
(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-element)
(require 'org-ilm-collection)
(require 'org-ilm-attachment)
(require 'org-ilm-capture)
(require 'org-ilm-card)
(require 'org-ilm-concept)
(require 'org-ilm-context)
(require 'org-ilm-image)
(require 'org-ilm-import)
(require 'org-ilm-log)
(require 'org-ilm-material)
(require 'org-ilm-media)
(require 'org-ilm-org)
(require 'org-ilm-queue)
(require 'org-ilm-pdf)
(require 'org-ilm-pqueue)
(require 'org-ilm-priority)
(require 'org-ilm-query)
(require 'org-ilm-bqueue-view)
(require 'org-ilm-bqueue)
(require 'org-ilm-schedule)
(require 'org-ilm-transclusion)
(require 'org-ilm-review)
(require 'org-ilm-registry)
(require 'org-ilm-convert)

;;;; Variables

(defface org-ilm-face-extract
  '((t :background "yellow"))
  "Face used to highlight extracts.")

(defface org-ilm-face-card
  '((t :background "pink"))
  "Face used to highlight clozes.")

(defvar-keymap org-ilm-map
  :doc "Keymap for `org-ilm-global-minor-mode'."
  "i" #'org-ilm-import
  "e" #'org-ilm-element-menu
  "a" #'org-ilm-attachment-actions
  "p" #'org-ilm-element-set-priority
  "s" #'org-ilm-element-set-schedule
  "o" #'org-ilm-open-dwim
  "x" #'org-ilm-extract
  "z" #'org-ilm-cloze
  ;; "c" #'org-ilm-cloze-toggle
  "c" #'org-ilm-clozeify
  "t" #'org-ilm-attachment-transclude
  "." #'org-ilm-point
  ";" org-ilm-context-map
  "n" #'org-ilm-concept-set
  "g" org-ilm-registry-prefix-map
  "k" #'org-ilm-element-delete
  "r" #'org-ilm-review
  "q" #'org-ilm-queue
  "+" #'org-ilm-queue-add-dwim
  "b" #'org-ilm-bibliography)

;;;; Minor mode

;; TODO This hook is called not just after file save but on a timer as well. Do
;; we want the timer to reset cache?
(defun org-ilm--org-mem-hook (parse-results)
  (org-ilm-concept-cache-reset))

;;;###autoload
(define-minor-mode org-ilm-global-minor-mode
  "Prepare some hooks and advices some functions."
  :init-value nil
  :global t
  :lighter nil ;; String to display in mode line
  :group 'org-ilm
  ;; :keymap org-ilm-map
  (if org-ilm-global-minor-mode
      ;; Enable
      (progn
        (add-hook 'after-change-major-mode-hook
                  #'org-ilm--attachment-prepare-buffer)
        (add-hook 'org-mode-hook
                  #'org-ilm--collection-prepare-buffer)
        (add-hook 'org-mem-post-full-scan-functions
                  #'org-ilm--org-mem-hook)
        (add-hook 'org-mem-post-targeted-scan-functions
                  #'org-ilm--org-mem-hook)
        (add-hook 'kill-emacs-hook
                  #'org-ilm--pqueue-write-all)
        
        (define-key image-mode-map (kbd "A") org-ilm-image-map)
        )
    ;; Disable
    (remove-hook 'after-change-major-mode-hook
                 #'org-ilm--attachment-prepare-buffer)
    (remove-hook 'org-mode-hook
                 #'org-ilm--collection-prepare-buffer)
    (remove-hook 'org-mem-post-full-scan-functions
              #'org-ilm--org-mem-hook)
    (remove-hook 'org-mem-post-targeted-scan-functions
                 #'org-ilm--org-mem-hook)
    (remove-hook 'kill-emacs-hook
                 #'org-ilm--pqueue-write-all)
    (org-ilm--pqueue-write-all)
    (define-key image-mode-map (kbd "A") nil)
    ))

;;;; Commands

(defun org-ilm-open-attachment ()
  "Open attachment of ilm element at point."
  (interactive)
  (unless (org-ilm--attachment-open :no-error t)
    ;; Since we are opening an attachment, the element is already made, meaning
    ;; it or a parent should have the DIR property set.
    (pcase (read-char-choice "No attachments. (n)ew, (r)egistry, (s)elect: " '("n" "r" "s"))
      (?n 
       (find-file (expand-file-name (concat id ".org") (org-attach-dir-get-create))))
      (?s
       (if-let* ((attach-dir (org-attach-dir))
                 (attachments (org-attach-file-list attach-dir))
                 (attachment (completing-read "Attachment to dedicate as element's: "
                                              attachments nil t))
                 (attachment (expand-file-name attachment attach-dir))
                 (attachment-new (expand-file-name
                                  (format "%s.%s" (org-id-get)
                                          (file-name-extension attachment))
                                  (file-name-directory attachment))))
           (progn
             (rename-file attachment attachment-new)
             (find-file attachment-new))
         (user-error "No attachment found")))
      (?r
       (if-let* ((registry-id (org-entry-get nil "REGISTRY")))
           (user-error "Not implemented!") ;; TODO
         (user-error "No registry entry"))))))

(defun org-ilm-open-highlight ()
  "Open element associated with highlight at point."
  (interactive)
  (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'org-ilm-id)) (overlays-at (point)))))
    (pcase (length ovs)
      (0 nil)
      (1 (org-ilm--open-from-ov (nth 0 ovs)))
      (_
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
      (org-ilm--org-goto-id (car data))
    (let ((collection (org-ilm--select-collection)))
      (find-file (cdr collection)))))

(defun org-ilm-open-dwim ()
  "Open element of highlight or within collection."
  (interactive)
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
     ('attachment
      (unless (org-ilm-open-highlight)
        (org-ilm-open-collection)))
     ('collection
      (if (org-ilm--element-at-point)
          (org-ilm-open-attachment)
        (user-error "No ilm element at point!")))
     ('registry
      ;; TODO Allow for multiple ilm elements for same registry
      ;; Eg: blog and accompanying video, manuscript and published paper, etc
      (if-let ((entry (seq-find
                       (lambda (entry)
                         (and (org-ilm--element-type entry)
                              (string= (org-id-get)
                                       (org-mem-entry-property "REGISTRY" entry))))
                       (hash-table-values org-mem--id<>entry))))
          (org-node-goto-id (org-mem-entry-id entry))
        (user-error "No ilm element derived from this registry entry!")))
     (_ (org-ilm-open-collection)))))

(defun org-ilm-extract ()
  "Extract region depending on file."
  (interactive)
  (org-ilm--extract))

(defun org-ilm-cloze ()
  "Create a cloze card"
  (interactive)
  (org-ilm--cloze))

(defun org-ilm-split ()
  "Split document by section."
  (interactive)
  (if (eq 'attachment (car (org-ilm--where-am-i)))
      (cond
       ((eq major-mode 'org-mode)
        (call-interactively #'org-ilm-org-split))
       ((org-ilm--pdf-mode-p)
        (call-interactively #'org-ilm-pdf-split)))
    (user-error "Splitting can only be done from within an attachment")))

(defun org-ilm-point (&optional arg)
  "Mark location as reading point."
  (interactive "P")
  (org-ilm--point))

  
;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here
