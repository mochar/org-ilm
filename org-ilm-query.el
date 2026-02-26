;;; org-ilm-query.el --- Query -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-ql)
(require 'ts)

(require 'org-ilm-core)
(require 'org-ilm-utils)
;; (require 'org-ilm-element)
;; (require 'org-ilm-collection)
;; (require 'org-ilm-queue)

;;;; Variables

(defcustom org-ilm-queries
  '((outstanding . org-ilm--queries-query-outstanding))
  "Alist mapping query name to a function that returns an org-ql query."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-ilm)

(defcustom org-ilm-custom-queries nil
  "Do not edit. Auto saved user added queries. See `org-ilm-queries'."
  :type '(alist :key-type symbol :value-type function)
  :group 'org-ilm)

;;;; Queries

;; Optimizations: https://github.com/alphapapa/org-ql/issues/88#issuecomment-570473621
;; + If any data is needed in action and query, use org-ql--value-at
;; + Prefer query over custom predicate
;; + Use regex preambles to quickly filter candidates

(defun org-ilm-query-collection (query collection &optional hash-table-p action)
  "Apply org-ql QUERY on COLLECTION, parse org-ilm data, and return the results.
With HASH-TABLE-P, place each element in a hash table with ID as key."
  (unless (functionp query)
    (setq query (cdr (assoc query org-ilm-queries))))
  (let ((query (funcall query (format "%s" collection)))
        (files (org-ilm--collection-files collection)))
    ;; TODO Pass action as sexp so that org-ql can byte compile it
    (if hash-table-p
        (let ((h (make-hash-table :test #'equal)))
          (org-ql-select files query
            :action (lambda ()
                      ;; TODO this should just do org-id-get
                      (when-let* ((el (org-ilm--element-at-point))
                                  (id (org-ilm-element--id el)))
                        (puthash id el h))))
          h)
      (org-ql-select files
        query
        :action (or action #'org-ilm--element-at-point)))))

(defun org-ilm-query-buffer (query collection buffer &optional narrow)
  "Apply org-ql QUERY on buffer, parse org-ilm data, and return the results."
  (unless (functionp query)
    (setq query (cdr (assoc query org-ilm-queries))))
  (org-ql-select buffer
    (funcall query collection)
    :action #'org-ilm--element-at-point
    :narrow narrow))

(defun org-ilm--query-concepts (collection)
  "Return list of concepts from COLLECTION.

TODO parse-headline pass arg to not sample priority to prevent recusrive concept search?"
  (org-ilm-query-collection #'org-ilm--queries-concepts collection))

(org-ql-defpred ilm-element (&optional collection types)
  ""
  :normalizers (
                (`(ilm-element ,collection ,types)
                ;; ((seq ilm-element collection types)
                 `(and
                   (property "ID")
                   (property ,org-ilm-property-collection ,collection :inherit t)
                   (or ,@(cl-loop for type-sym in (or types org-ilm-element-types)
                                  collect `(property ,org-ilm-property-type
                                                     ,(symbol-name type-sym)))))))
  :body (and
         (property "ID")
         (property org-ilm-property-collection collection :inherit t)
         (or (cl-loop for type in (or types org-ilm-element-types)
                      collect `(property ,org-ilm-property-type ,(symbol-name type))))))

;; (org-ql--normalize-query `(ilm-element))
;; (org-ql--normalize-query `(ilm-element "test" nil))
;; (org-ql--normalize-query `(ilm-element "test" (material card)))

;; TODO Concepts don't need a ilm-collection property but are ignored by the
;; query if they dont
(defun org-ilm--queries-concepts (collection)
  ;; `(ilm-element ,collection (concept)))
  `(and (property "ID")

(defun org-ilm--queries-query-all (collection)
  "Query for org-ql to retrieve all elements."
  `(ilm-element ,collection (material card)))

(defun org-ilm--queries-query-active (collection)
  "Query for org-ql to retrieve not-done elements."
  `(and (not (done)) (ilm-element ,collection (material card))))

(defun org-ilm--queries-query-outstanding (collection)
  "Query for org-ql to retrieve the outstanding elements."
  (let ((today (ts-adjust 'minute (org-ilm-midnight-shift-minutes) (ts-now))))
    `(and
      (ilm-element ,collection (material card))
      (not (done))
      (scheduled :to ,today))))

(defun org-ilm--query-select ()
  "Prompt user for query to select from.
The queries are stored in `org-ilm-queries'."
  (org-ilm--select-alist org-ilm-queries "Query: "))

;;; Footer

(provide 'org-ilm-query)

;;; org-ilm-query.el ends here
