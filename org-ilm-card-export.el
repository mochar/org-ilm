;;; org-ilm-card-export.el --- Export card contents -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1

;;; Commentary:

;; Export functionality.

;;; Code:

(require 'org)
(require 'cl-lib)

(defvar-local org-ilm-card-export-window-configuration nil
  "Saved window configuration during the export process.")

(defvar org-ilm-card-export-prepare-finalize-hook nil
  "Hook that is run before the finalization starts.")

(defvar org-ilm-card-export-after-finalize-hook nil
  "Hook that is run after finalization.")

(defvar org-ilm-card-export-buffer-name "*Ilm Card Export*")

(defvar org-ilm-card-export-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-ilm-card-export-finalize)
    (define-key map "\C-c\C-k" #'org-ilm-card-export-kill)
    map)
  "Keymap for `org-ilm-card-export-mode', a minor mode.")

;;;###autoload
(define-minor-mode org-ilm-card-export-mode
  "Minor mode for special key bindings in an export buffer."
  :group 'org-ilm
  (cl-assert (eq major-mode 'org-mode))
  (if org-ilm-card-export-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-ilm-card-export-mode-map>Org-ilm card export buffer.  Finish `\\[org-ilm-card-export-finalize]', abort `\\[org-ilm-card-export-kill]'."))
    (setq-local header-line-format nil)))

(defun org-ilm-card-export-finish ()
  "Finalize and clean up the export process."
  (cl-assert org-ilm-card-export-mode)
  (let ((after-hooks (and (local-variable-p 'org-ilm-card-export-after-finalize-hook)
                          org-ilm-card-export-after-finalize-hook)))
    (kill-buffer)
    (set-window-configuration (cl-shiftf org-ilm-card-export-window-configuration nil))
    (mapc #'funcall (seq-filter #'functionp after-hooks))))

(defun org-ilm-card-export-finalize ()
  "Finalize the Org-ilm card export process."
  (interactive)
  (cl-assert org-ilm-card-export-mode)
  (run-hooks 'org-ilm-card-export-prepare-finalize-hook)
  (org-ilm-card-export-finish))

(defun org-ilm-card-export-kill ()
  "Abort the current Org-ilm card export process."
  (interactive)
  (cl-assert org-ilm-card-export-mode)
  (org-ilm-card-export-finish))

(defun org-ilm-card-export-cloze (&optional on-prepare on-after)
  "Export a cloze card."
  (let* ((source-buffer (current-buffer))
         (orig-point (point))
         (orig-mark (when (region-active-p) (mark)))
         (window-configuration (current-window-configuration))
         (export-buffer (get-buffer-create org-ilm-card-export-buffer-name)))
    (setf org-ilm-card-export-window-configuration window-configuration)
    (with-current-buffer export-buffer
      (erase-buffer)
      (insert-buffer source-buffer)
      ;; TODO regex replace extract and card tags?
      (org-mode)
      (org-show-all)
      (goto-char orig-point)
      (when orig-mark
        (set-mark orig-mark)
        (activate-mark))

      (org-srs-item-cloze-dwim)

      ;; Don't apply face to overlays to make it look like normal text. We keep
      ;; the overlays so we preserve positions.  preserve the positions.
      (org-ilm-recreate-overlays t)
      
      (org-ilm-card-export-mode +1)
      (when on-prepare
        (add-hook 'org-ilm-card-export-prepare-finalize-hook on-prepare nil t))
      (when on-after
        (add-hook 'org-ilm-card-export-after-finalize-hook on-after nil t)))
    (pop-to-buffer export-buffer)))
