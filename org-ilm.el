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
(require 'cl-lib)

;;;; Customization
(defgroup org-ilm nil
  "Incremental learning mode."
  :group 'org
  :prefix "org-ilm-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom org-ilm-collections-alist '((ilm . "~/org/ilm.org"))
  "Alist mapping collection name to path to its org file."
  :type '(alist :key-type symbol :value-type file)
  :group 'org-ilm)

(defcustom org-ilm-id-from-attachment-path-func 'org-ilm-infer-id-from-attachment-path
  "Function that accepts a path to an attachment file and returns the Org id of the header."
  :type 'function
  :group 'org-ilm)

;;;; Variables
;;;; Commands
(defun org-ilm-import-website (url)
  ""
  (interactive "sURL: "))

(defun org-ilm-extract ()
  "Extract text in region to attachment Org file that is the child heading of current entry."
  (interactive)
  (unless (use-region-p) (user-error "No region active."))
  (let* ((file-path buffer-file-name)
         (file-name (file-name-sans-extension
                     (file-name-nondirectory
                      file-path)))
         (attach-org-id (org-ilm-infer-id-from-attachment-path file-path))
         (attach-org-id-marker (org-id-find attach-org-id t))
         (file-org-id file-name)
         (file-org-id-marker (org-id-find file-org-id t)))
    (unless attach-org-id-marker
      (error "Current file is not an attachment, or failed to parse org ID."))
    (unless file-org-id-marker
      (error "Current file name not org ID, or failed to find it."))
    
    (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
           (region-text-clean (replace-regexp-in-string "\n" " " region-text))
           (snippet (substring region-text-clean 0 (min 50 (length region-text-clean))))
           (extract-org-id (org-id-new))
           (extract-tmp-path (expand-file-name
                              (format "%s.org" extract-org-id)
                              temporary-file-directory)))

      ;; Save region content into tmp file and move it as attachment to main
      ;; heading.
      (write-region region-text nil extract-tmp-path)
      (org-with-point-at attach-org-id-marker
        (org-attach-attach extract-tmp-path nil 'mv)
        (save-buffer))
        
      (org-with-point-at file-org-id-marker
        (org-insert-heading-respect-content)
        (org-do-demote) ;; make it a child
        (insert snippet)
        (org-set-property "ID" extract-org-id)
        (save-buffer)))))


;;;; Functions

(defun org-ilm-infer-id ()
  "Attempt parsing the org-id of current attachment file."
  (org-ilm-infer-id-from-attachment-path buffer-file-name))
  
(defun org-ilm-infer-id-from-attachment-path (path)
  "Attempt parsing the org-id from the attachment path."
  (let ((parts (split-string (file-name-directory path) "/" t)))
  (pcase (file-name-nondirectory (org-attach-dir-from-id "abc"))
    ("abc" (car (last parts)))
    ("ab/c" (mapconcat #'identity (last parts 2) ""))
    (t nil))))

;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here

