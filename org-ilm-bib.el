;;; org-ilm-bib.el --- Bib -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'zotra)
(require 'parsebib)

;;;; Citation

(defcustom org-ilm-bibtex-fields nil
  "List of field names to include in the bibtex entry.
See `parsebib-read-entry'."
  :type '(repeat string)
  :group 'org-ilm)

(defun org-ilm--citation-get-bibtex (source &optional as-alist)
  "Return the bibtex entry as string from SOURCE."
  ;; Much better results with citoid backend compared to zotra
  (let ((zotra-backend 'citoid)
        bibtex-string)
    (setq bibtex-string (-some-> (zotra-get-entry source "bibtex")
                          s-trim))
    (if (and bibtex-string as-alist)
        (org-ilm--citation-parse-bibtex bibtex-string)
      bibtex-string)))

(defun org-ilm--citation-parse-bibtex (bibtex-string)
  "Parses BIBTEX-STRING as alist."
  (with-temp-buffer
    (insert bibtex-string)
    (when-let* ((bibtexes (car (parsebib-parse-bib-buffer
                                :fields org-ilm-bibtex-fields
                                :expand-strings t
                                :inheritance t
                                :replace-TeX t)))
                (key (car (hash-table-keys bibtexes))))
      (gethash key bibtexes))))

(defun org-ilm--citation-get-zotero (source)
  "Return the zotero reference of SOURCE as an alist."
  (when-let* ((zotra-backend 'citoid)
              (json-string (zotra-get-entry source "zotero")))
    (with-temp-buffer
      (insert json-string)
      (goto-char (point-min))
      (aref (json-read) 0))))

;;; Footer

(provide 'org-ilm-bib)

;;; org-ilm-bib.el ends here
