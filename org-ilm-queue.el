;;; org-ilm-queue.el --- Queue -*- lexical-binding: t; -*-

;;; Commentary:

;; A wrapper around `ost-tree' that holds a collection's element ids sorted in
;; some way.  Both the `bqueue' and `pqueue' structs inherit from
;; `org-ilm-queue'.

;;; Code:

(require 'org-ilm-collection)
(require 'ost)

(cl-defstruct (org-ilm-queue (:conc-name org-ilm-queue--)
                             (:include ost-tree))
  "An OST tree associated with a collection."
  (name
   "generic-queue"
   :type string
   :documentation "Name used as filename when tree saved to disk.")
  (collection
   nil
   :type symbol
   :documentation "Collection symbol."))

;;;; Serialization

(cl-defmethod ost-serialize ((queue org-ilm-queue))
  (list :collection (org-ilm-queue--collection queue)
        :name (org-ilm-queue--name queue)))

(cl-defmethod ost-deserialize ((queue org-ilm-queue) data)
  (setf (org-ilm-queue--collection queue) (plist-get data :collection)
        (org-ilm-queue--name queue) (plist-get data :name)))

(defun org-ilm--queue-file (collection name)
  "Return the file path where the COLLECTION's QUEUE with NAME is stored."
  (expand-file-name
   (concat name ".el")
   (org-ilm--collection-data-dir collection)))

(cl-defgeneric org-ilm-queue--file ((queue org-ilm-queue))
  "Return the file path where QUEUE is stored."
  (org-ilm--queue-file
   (org-ilm-queue--collection queue)
   (org-ilm-queue--name queue)))

(cl-defgeneric org-ilm-queue--read ((queue org-ilm-queue))
  "Replace QUEUE data with what is stored in disk."
  (ost-read (org-ilm-queue--file queue) queue))

;; TODO Schedule for write in background process
;; If new scheduled, old one cancelled. Use org-mem el-job?
(cl-defgeneric org-ilm-queue--write ((queue org-ilm-queue))
  (let* ((file (org-ilm-queue--file queue))
         (backup-file (concat file ".bak")))
    (make-directory (file-name-directory file) 'parents)
    (when (file-exists-p file)
      (copy-file file backup-file t))
    (ost-write queue file))
  ;; Return nil prevent printing
  nil)

(defun org-ilm--queue-saved-queues (&optional collection)
  "Return paths of queues saved in COLLECTION."
  (let* ((collection (or collection (org-ilm--active-collection)))
         (dir (org-ilm--collection-data-dir collection))
         (queue-files (directory-files dir t "\\.el$")))
    (seq-filter
     (lambda (f) (not (string= (file-name-base f) org-ilm-pqueue-name)))
     queue-files)))

;;;; Functions

(defun org-ilm-queue--as-tree (queue)
  (make-ost-tree :root (ost-tree-root queue)
                 :dynamic (ost-tree-dynamic queue)
                 :nodes (ost-tree-nodes queue)))

(defun org-ilm-queue--tree-clear (queue)
  "Empty the root and nodes of the QUEUE ost tree."
  (setf (org-ilm-queue--root queue) nil
        (org-ilm-queue--nodes queue) (make-hash-table :test #'equal)))

(defun org-ilm-queue--contains-p (queue element-or-id)
  "Returns id if element is in QUEUE."
  (ost-tree-contains-p queue (org-ilm--element-id element-or-id)))

(cl-defgeneric org-ilm-queue--remove ((queue org-ilm-queue) element-or-id)
  "REMOVE element from QUEUE, return id."
  (let ((id (org-ilm--element-id element-or-id)))
    (when (org-ilm-queue--contains-p queue id)
      (ost-tree-remove queue id)
      id)))

(cl-defgeneric org-ilm-queue--insert ((queue org-ilm-queue) element-or-id pos)
  "INSERT element into QUEUE at position POS."
  (ost-tree-insert queue pos (org-ilm--element-id element-or-id)))

;; TODO Keep copy of ost to restore in case of error midway?
;; See same todo in ost.el.
(cl-defgeneric org-ilm-queue--move ((queue org-ilm-queue) element-or-id new-pos)
  "Move element in QUEUE to NEW-POS."
  (ost-tree-move queue (org-ilm--element-id element-or-id) new-pos))

(cl-defgeneric org-ilm-queue--move-many ((queue org-ilm-queue) new-pos-alist)
  "Move many elements in QUEUE to a new rank.

NEW-POS-ALIST is an alist of (ID . NEW-POS) pairs."
  (ost-tree-move-many queue new-pos-alist))


;;;; Position select

(defun org-ilm-queue--parse-position-str (queue pos-str)
  (let* (;; Elisp interprets "1." as integer 1, i prefer it to be float 1.0
         (pos (if (string= pos-str "1.") 1.0 (string-to-number pos-str)))
         (pos-invalid (and (= pos 0) ;; string-to-number returns 0 for non-numbers
                           (not (string= pos-str "0"))
                           (not (string= pos-str ".0"))
                           (not (string= pos-str "0.0")))))
    (unless pos-invalid
      (setq pos (if (integerp pos) (1- pos) pos))
      (ost-tree-position queue pos))))
  
(defun org-ilm-queue--position-read (queue &optional initial-pos numnew-or-id prompt)
  (let* ((queue-size (ost-size queue))
         (numnew (cond
                  ((null numnew-or-id) 0)
                  ((numberp numnew-or-id) numnew-or-id)
                  ((stringp numnew-or-id)
                   ;; If element not yet in queue, max position is queue size + 1
                   (if (ost-tree-contains-p queue numnew-or-id) 0 1))
                  (t (error "Invalid value for NUMNEW-OR-ID"))))
         (min 1)
         (max (+ queue-size numnew))
         position)
    (while (not position)
      (let* ((number-str (read-string
                          (format "%s (#%s-%s / 0.0-1.0): " (or prompt "Priority") min max)
                          nil nil
                          (when initial-pos
                            (number-to-string (1+ (ost-tree-rank queue initial-pos)))))))
        (setq position (org-ilm-queue--parse-position-str queue number-str))))
    position))

(defun org-ilm-queue--position-format (queue position &optional offset)
  (when-let ((position (ost-tree-position queue position offset)))
    (let ((rank (car position))
          (quantile (cdr position)))
      (format "#%s/%s (%.2f%s)"
              (1+ rank) (+ (ost-size queue) (or offset 0)) (* 100 quantile) "%"))))

;;; Footer

(provide 'org-ilm-queue)

;;; org-ilm-queue.el ends here
