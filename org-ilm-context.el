;;; org-ilm-context.el --- Context -*- lexical-binding: t; -*-

;;; Commentary:

;; Context can be added to current element and children by adding arbitrary text
;; to <org-id>.context.org file. This buffer will dynamically view the context
;; org file of the currently opened element attachment.

;;; Code:

;;;; Requirements

(require 'ol) ; org-link
(require 'org-attach)
(require 'pdf-view)

(require 'org-ilm-element)
(require 'org-ilm-pdf)
(require 'org-ilm-element)

;;;; Variables

(defconst org-ilm--context-name "*Ilm Context*")

(defvar-keymap org-ilm-context-map
  :doc "Keymap for the context frame"
  "f" #'org-ilm-context-frame
  "n" #'org-ilm-context-new
  "a" #'org-ilm-context-add)

;;;; Functions

(defvar org-ilm--context-frame nil)

(defun org-ilm--context-default-buffer ()
  "Buffer shown when no context available."
  (let ((buffer (get-buffer org-ilm--context-name)))
    (unless (buffer-live-p buffer)
      (with-current-buffer (setq buffer (get-buffer-create org-ilm--context-name))
        (insert (propertize "No context found" 'face 'Info-quoted))
        (read-only-mode 1)))
    buffer))

(defun org-ilm--context-frame ()
  "Return the context frame, making one if not exists."
  (if (frame-live-p org-ilm--context-frame)
      org-ilm--context-frame
    (let ((frame (make-frame `((name . ,org-ilm--context-name)
                               ;; (minibuffer . nil)
                               (unsplittable . t)
                               (width . 50)
                               (height . 20)
                               (tab-bar-lines . nil)))))
      (with-selected-window (frame-root-window frame)
        (switch-to-buffer (org-ilm--context-default-buffer)))
      (setq org-ilm--context-frame frame))))

(defun org-ilm--context-save-buffer ()
  (with-selected-window (frame-root-window (org-ilm--context-frame))
    (when buffer-file-name (save-buffer))))

(defun org-ilm--context-frame-p (frame)
  (string= (frame-parameter frame 'name) org-ilm--context-name))

(defun org-ilm--context-frame-on-delete (frame)
  (when (org-ilm--context-frame-p frame)
    (org-ilm-context-mode -1)))

(define-minor-mode org-ilm-context-mode
  "Minor mode that opens context file in the context buffer of last opened attachment."
  :init-value nil
  :global t
  :lighter nil
  :interactive nil
  :group 'org-ilm
  (if org-ilm-context-mode
      (progn
        (add-hook 'window-buffer-change-functions
                  #'org-ilm--context-detect)
        (add-hook 'delete-frame-functions
                  #'org-ilm--context-frame-on-delete))
    (org-ilm--context-save-buffer)
    (remove-hook 'window-buffer-change-functions
                 #'org-ilm--context-detect)
    (remove-hook 'delete-frame-functions
                 #'org-ilm--context-frame-on-delete)
    (kill-buffer (org-ilm--context-default-buffer))))

(defun org-ilm--context-detect (frame)
  "Detects if last opened buffer in FRAME is an ilm attachment and updates
the context frame."
  (unless (org-ilm--context-frame-p frame)
    (with-current-buffer (window-buffer (frame-root-window frame))
      (let ((context-path (org-ilm--context-get-path)))
        (with-selected-window (frame-root-window (org-ilm--context-frame))
          (when buffer-file-name (save-buffer))
          (if context-path
              (find-alternate-file context-path)
            (switch-to-buffer (org-ilm--context-default-buffer))))))))

(defun org-ilm--context-get-path ()
  "Return the path to the context file of the current selected attachment."
  (let ((location (org-ilm--where-am-i)))
    (when (eq (car location) 'attachment)
      (when-let* ((attach-dir (if (org-ilm--pdf-mode-p)
                                  (nth 1 (org-ilm--pdf-data))
                                (file-name-directory (buffer-file-name))))
                  (id (nth 1 location))
                  (entry (org-mem-entry-by-id id))
                  (crumbs (org-mem-entry-crumbs entry)))
        (catch 'path
          (dolist (crumb crumbs)
            (when-let* ((id (nth 4 crumb))
                        (path (expand-file-name (concat id ".context.org") attach-dir)))
              (when (file-exists-p path)
                (throw 'path path)))))))))

;;;;; Commands

(defun org-ilm-context-frame ()
  "Open a frame that will show the context of the last viewed element attachment."
  (interactive)
  (let ((frame (org-ilm--context-frame)))
    (org-ilm-context-mode 1)
    (org-ilm--context-detect (selected-frame))
    (raise-frame org-ilm--context-frame)))

(defun org-ilm-context-new ()
  "Create a new context file for the element at point."
  (interactive)
  (if-let* ((element (org-ilm--element-from-context))
            (id (org-ilm-element--id element))
            (attach-dir (org-ilm--element-with-point-at element
                          (expand-file-name (org-attach-dir-get-create))))
            (entry (org-mem-entry-by-id id))
            (ancestory (seq-keep
                        (lambda (crumb)
                          (when-let* ((id (nth 4 crumb))
                                      (e (org-mem-entry-by-id id))
                                      (_ (member (org-ilm--element-type e)
                                                 '(material card)))
                                      (title (org-ilm--org-mem-title-full e)))
                            (cons title e)))
                        (reverse (org-mem-entry-crumbs entry))))
            (choice (completing-read "Element: " ancestory nil t))
            (context-entry (cdr (assoc choice ancestory)))
            (context-id (org-mem-entry-id context-entry)))
      (find-file (expand-file-name (concat context-id ".context.org") attach-dir))
    (user-error "No element at point")))


(defun org-ilm-context-add ()
  "Add active region to context."
  (interactive)
  (if-let ((path (org-ilm--context-get-path)))
      (let (text)
        (cond
         ((eq major-mode 'org-mode)
          (unless (region-active-p) (user-error "Region not active"))
          (setq text (buffer-substring-no-properties (region-beginning) (region-end))))
         ((org-ilm--pdf-mode-p)
          (unless (pdf-view-active-region-p) (user-error "Region not active"))
          (let* ((data (org-ilm--pdf-data))
                 (id (nth 0 data))
                 (attach-dir (nth 1 data))
                 (filename-base (concat id "_" (substring (org-id-new) 0 8)))
                 (out-file-base (expand-file-name filename-base attach-dir))
                 (out-file (org-ilm--pdf-image-export
                            out-file-base :region (car (pdf-view-active-region t)))))
            (setq text (concat "file:" (file-name-nondirectory out-file))))))
        
        (when text
          (with-current-buffer (find-file-noselect path)
            (save-excursion
              (goto-char (point-max))
              (unless (bolp) (newline))
              (insert text))
            (org-link-preview nil (point-min) (point-max))
            (save-buffer))))
    (user-error "No context file")))

;;; Footer

(provide 'org-ilm-context)

;;; org-ilm-context.el ends here
