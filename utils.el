;;; utils.el --- Some utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: 

;;; Commentary:

;; Some shared utilities.

;;; Code:

(require 'org-mem)
(require 'url)
(require 'org-id)

;;;; String
(defun utils--slugify-title (title)
  "From TITLE, make a filename slug meant to look nice as URL component.

This is a literal copy-paste from the function
`org-node-slugify-for-web' of the `org-node' package. Hope that's ok..."
  (thread-last title
               (org-link-display-format)
               (string-glyph-decompose)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (string-glyph-compose)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))

;;;; Org

(defmacro utils--org-with-point-at (thing &rest body)
  "THING for now should be an org-id.

Note: Previously used org-id-find but it put point above
headline. org-mem takes me there directly.

Alternatively org-id-goto could be used, but does not seem to respect
save-excursion."
  `(when-let* ((org-id ,thing)
               (entry (org-mem-entry-by-id org-id))
               (file (org-mem-entry-file-truename entry))
               (buf (or (find-buffer-visiting file)
                        (find-file-noselect file))))
     (with-current-buffer buf
       ;; Jump to correct position
       (goto-char (org-find-property "ID" org-id))
       ,@body)))

;;;; Web
(defun utils--get-page-title (url &optional slugify)
  "Get title of web page by URL, or `nil' if not found.

Uses various utilities from `url.el'."
  (let (web-title-str coding-charset)
    (with-current-buffer (url-retrieve-synchronously url)
      ;; find title by grep the html code
      (goto-char (point-min))
      (when (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
        (setq web-title-str (match-string 1)))
      ;; find charset by grep the html code
      (goto-char (point-min))
      ;; downcase the charaset. e.g, UTF-8 is not acceptible for emacs, while utf-8 is ok.
      (setq coding-charset
            (or (when (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
                  (downcase (match-string 1)))
                "utf-8"))
      ;; decode the string of title.
      (when web-title-str
        (setq web-title-str (decode-coding-string web-title-str (intern coding-charset))))
      (kill-buffer))
    (if slugify
        (utils--slugify-title web-title-str)
      web-title-str)))


;;;; Footer

(provide 'utils)

;;; utils.el ends here

