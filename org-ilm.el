;;; org-ilm.el --- Incremental learning mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.0"))
;; Keywords: html, org

;;; Commentary:

;; This package extends org-mode to allow for incremental processing of reading,
;; writing, listening, and watching materials.

;;; Code:

;;;; Requirements
(require 'org)
(require 'cl-lib)

;;;; Customization
(defgroup org-ilm nil
  "Incremental learning mode."
  :group 'org
  :prefix "org-ilm-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom org-ilm-collections-alist nil
  "Alist mapping collection name to path to its org file."
  :type '(alist :key-type symbol :value-type file)
  :group 'webshot)


;;;; Variables
;;;; Commands
;;;; Functions
;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here

