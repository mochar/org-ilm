;;; org-ilm-ost.el --- OST -*- lexical-binding: t; -*-

;;; Commentary:

;; Both the `queue' and `pqueue' structs inherit from `ost-tree'. Some
;; functionality is therefore shared by both, which lives here.

;;; Code:

(require 'org-ilm-collection)
(require 'ost)

(cl-defstruct (org-ilm-ost (:conc-name org-ilm-ost--)
                           (:include ost-tree))
  "An OST tree associated with a collection."
  (name "ost" :type string :documentation "Name used as filename when tree saved to disk.")
  (collection nil :type symbol :documentation "Collection symbol."))

(cl-defmethod ost-serialize ((ost org-ilm-ost))
  (list :collection (org-ilm-ost--collection ost)
        :name (org-ilm-ost--name ost)))

(cl-defmethod ost-deserialize ((ost org-ilm-ost) data)
  (setf (org-ilm-ost--collection ost) (plist-get data :collection)
        (org-ilm-ost--name ost) (plist-get data :name)))

(defun org-ilm--ost-file (collection name)
  "Return the file path where the COLLECTION's OST with NAME is stored."
  (expand-file-name
   (concat name ".el")
   (org-ilm--collection-data-dir collection)))

(defun org-ilm-ost--file (ost)
  "Return the file path where OST is stored."
  (cl-assert (org-ilm-ost-p ost))
  (org-ilm--ost-file
   (org-ilm-ost--collection ost)
   (org-ilm-ost--name ost)))

(defun org-ilm-ost--read (ost)
  "Replace OST data with what is stored in disk."
  (ost-read (org-ilm-ost--file ost) ost))

;; TODO Schedule for write in background process
;; If new scheduled, old one cancelled. Use org-mem el-job?
(defun org-ilm-ost--write (ost)
  (let* ((file (org-ilm-ost--file ost))
         (backup-file (concat file ".bak")))
    (make-directory (file-name-directory file) 'parents)
    (when (file-exists-p file)
      (copy-file file backup-file t))
    (ost-write ost file))
  ;; Return nil prevent printing
  nil)

(defun org-ilm-ost--as-tree (ost)
  (make-ost-tree :root (ost-tree-root ost)
                 :dynamic (ost-tree-dynamic ost)
                 :nodes (ost-tree-nodes ost)))

(defun org-ilm-ost--tree-clear (ost)
  "Empty the root and nodes of the OST tree."
  (setf (org-ilm-ost--root ost) nil
        (org-ilm-ost--nodes ost) (make-hash-table :test #'equal)))

(defun org-ilm-ost--parse-position-str (ost pos-str)
  (let* (;; Elisp interprets "1." as integer 1, i prefer it to be float 1.0
         (pos (if (string= pos-str "1.") 1.0 (string-to-number pos-str)))
         (pos-invalid (and (= pos 0) ;; string-to-number returns 0 for non-numbers
                           (not (string= pos-str "0"))
                           (not (string= pos-str ".0"))
                           (not (string= pos-str "0.0")))))
    (unless pos-invalid
      (setq pos (if (integerp pos) (1- pos) pos))
      (ost-tree-position ost pos))))
  
(defun org-ilm-ost--position-read (ost &optional initial-pos numnew-or-id prompt)
  (let* ((ost-size (ost-size ost))
         (numnew (cond
                  ((null numnew-or-id) 0)
                  ((numberp numnew-or-id) numnew-or-id)
                  ((stringp numnew-or-id)
                   ;; If element not yet in queue, max position is queue size + 1
                   (if (ost-tree-contains-p ost numnew-or-id) 0 1))
                  (t (error "Invalid value for NUMNEW-OR-ID"))))
         (min 1)
         (max (+ ost-size numnew))
         position)
    (while (not position)
      (let* ((number-str (read-string
                          (format "%s (#%s-%s / 0.0-1.0): " (or prompt "Priority") min max)
                          nil nil
                          (when initial-pos
                            (number-to-string (1+ (ost-tree-rank ost initial-pos)))))))
        (setq position (org-ilm-ost--parse-position-str ost number-str))))
    position))

(defun org-ilm-ost--position-format (ost position &optional offset)
  (when-let ((position (ost-tree-position ost position offset)))
    (let ((rank (car position))
          (quantile (cdr position)))
      (format "#%s/%s (%.2f%s)"
              (1+ rank) (ost-size ost) (* 100 quantile) "%"))))

;;; Footer

(provide 'org-ilm-ost)

;;; org-ilm-ost.el ends here
