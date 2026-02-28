;;; org-ilm-pqueue.el --- Priority queue -*- lexical-binding: t; -*-

;;; Commentary:

;; Priority queue.

;;; Code:

;;;; Requirements

(require 'org-ilm-queue)
(require 'org-ilm-query)
;; (require 'org-ilm-collection)

;;;; Variables

(defcustom org-ilm-pqueue-save-every-n-changes 1
  "Save the priority queue to disk after this many changes."
  :type 'integer
  :group 'org-ilm-pqueue)

(defvar org-ilm-pqueue-priority-changed-hook nil
  "Hook called when the priority of an element was actively changed.

Hook arg is (id . priority-rank) or an alist of such cons's if many
moved at once.

Note that priority may change frequently due to the nature of a
queue. Therefore not all changes are logged, only when explicitely
set (`org-ilm-pqueue--move' and `org-ilm-pqueue--move-many'.")

(defconst org-ilm-pqueue-name "element-queue")

;;;; Pqueue object

(cl-defstruct (org-ilm-pqueue
               (:conc-name org-ilm-pqueue--)
               (:include org-ilm-queue (name org-ilm-pqueue-name) (dynamic t)))
  "Priority queue."
  (changes-since-save 0 :type integer))

(defun org-ilm--pqueue-file (collection)
  "Return the file path where the priority queue of COLLECTION is stored."
  (org-ilm--queue-file collection org-ilm-pqueue-name))

(defun org-ilm--pqueue-read (collection)
  "Read the priority queue of COLLECTION from disk."
  (let ((pqueue (make-org-ilm-pqueue :collection collection)))
    (ost-read (org-ilm-queue--file pqueue) pqueue)))

(defun org-ilm-pqueue--write-maybe (pqueue &optional n-changes)
  "Write PQUEUE to disk when enough changes have been applied."
  (cl-assert (org-ilm-pqueue-p pqueue))
  (setq n-changes (or n-changes 1))
  (cl-incf (org-ilm-pqueue--changes-since-save pqueue) n-changes)
  (when (>= (org-ilm-pqueue--changes-since-save pqueue)
            org-ilm-pqueue-save-every-n-changes)
    (org-ilm-queue--write pqueue)
    (setf (org-ilm-pqueue--changes-since-save pqueue) 0)))

(cl-defmethod org-ilm-queue--insert ((pqueue org-ilm-pqueue) element-or-id pos)
  (cl-call-next-method pqueue element-or-id pos)
  (org-ilm-pqueue--write-maybe pqueue))

(cl-defmethod org-ilm-queue--remove ((pqueue org-ilm-pqueue) element-or-id)
  (when (cl-call-next-method pqueue element-or-id)
    (org-ilm-pqueue--write-maybe pqueue)))

(cl-defmethod org-ilm-queue--move ((pqueue org-ilm-pqueue) element-or-id new-pos)
  (let ((id (org-ilm--element-id element-or-id)))
    (cl-call-next-method pqueue id new-pos)
    (run-hook-with-args 'org-ilm-pqueue-priority-changed-hook (cons id new-pos))
    (org-ilm-pqueue--write-maybe pqueue)))

(cl-defmethod org-ilm-queue--move-many ((pqueue org-ilm-pqueue) new-pos-alist)
  (cl-call-next-method pqueue new-pos-alist)
  (run-hook-with-args 'org-ilm-pqueue-priority-changed-hook new-pos-alist)
  (org-ilm-pqueue--write-maybe pqueue (length new-pos-alist)))

(defun org-ilm-pqueue--insert-element (pqueue element &optional position)
  "Insert ELEMENT into PQUEUE, making sure they belong to the same collection.
Read POSITION from user if not given. Return it."
  (cl-assert (and (org-ilm-pqueue-p pqueue) (org-ilm-element-p element)))
  (let* ((element-collection (org-ilm-element--collection element))
         (pqueue-collection (org-ilm-pqueue--collection pqueue)))
    (unless (eq element-collection pqueue-collection)
      (error "Cannot add element of collection \"%s\" into priority queue of collection \"%s\""
             element-collection pqueue-collection))
    (setq position (or position (org-ilm-queue--position-read pqueue nil 1)))
    (org-ilm-queue--insert pqueue id position)
    position))

(defun org-ilm-pqueue--priority (pqueue id &optional position nil-if-absent)
  "Position of the ID in PQUEUE as a cons (rank . quantile).
With POSITION, set the new position in the queue, or insert there if not exists."
  (cl-assert (org-ilm-pqueue-p pqueue))
  (let ((node (ost-tree-node-by-id pqueue id)))
    (cond
     ;; Position specified: Move or add the element 
     (position
      (if node
          (org-ilm-queue--move pqueue id position)
        (org-ilm-queue--insert pqueue id position)))
     ;; Node found, return position
     (node
      (ost-position pqueue node))
     ;; Element not found in priority queue
     (t
      (when (and (not nil-if-absent)
                 (yes-or-no-p 
                  (format "Element \"%s\" not found in priority queue. Add it?" id)))
        (if-let* ((element (org-ilm--element-by-id id))
                  (position (org-ilm-pqueue--insert-element pqueue element)))
            ;; TODO Can we just return position?
            (org-ilm-pqueue--priority pqueue id)
          (error "Org heading \"%s\" not an ilm element." id)))))))

;; TODO This function builds the queue by mapping over the priority queue's OST
;; to determine the priorities, rather than looping over the elements and
;; calling `ost-rank' for each. This can be much faster then
;; `org-ilm--bqueue-build' when we want to sort by priority.
(defun org-ilm-pqueue--bqueue (pqueue &optional query)
  "Build an `org-ilm-bqueue' from all elements in PQUEUE."
  (let* ((collection (org-ilm-pqueue--collection pqueue))
         (bqueue (make-org-ilm-bqueue
                 :name "Priority queue"
                 :collection collection
                 :type 'pqueue))
         (query (or query #'org-ilm--queries-query-active))
         (element-map (org-ilm-query-collection query collection 'hash-table-p)))

    ;; Map over OST
    (ost-do-in-order (node rank pqueue)
      (let* ((id (ost-node-id node)))
        (org-ilm-queue--insert bqueue id rank)
        ;; Remove from map so that we are left with elements missing in the
        ;; priority queue.
        (remhash id element-map)))

    ;; Handle elements that were found by querying the collection, but are
    ;; missing in the priority queue.
    ;; TODO Handle this. Present user options:
    ;; 1. Ignore
    ;; 2. Jump to each element, allow user to set priority, one after another
    ;; 3. Dump elements in seperate queue, show queue view
    (unless (hash-table-empty-p element-map)
      (setq x element-map)
      (warn "Ilm: Parsed %s elements with missing priority." (hash-table-size element-map)))

    bqueue))

;;;; Collection priority queue

(defvar org-ilm--pqueues nil
  "Alist that maps collection symbol -> pqueue.")

(defun org-ilm--pqueue-new (collection)
  "Create a new priority queue associated with COLLECTION.
This does not add the priority queue to `org-ilm--pqueues' or save it to disk."
  (let* ((pqueue (make-org-ilm-pqueue :collection collection))
         ;; TODO Use org-mem instead?
         (elements (org-ilm-query-collection
                    #'org-ilm--queries-query-active
                    collection)))
    (dolist (element elements)
      (org-ilm-queue--insert pqueue element 0))
    pqueue))

(defun org-ilm--pqueue (&optional collection)
  "Return the priority queue of COLLECTION, or active collection."
  (setq collection (or collection (org-ilm--active-collection)))
  (cond-let*
    ([pqueue (alist-get collection org-ilm--pqueues)]
     pqueue)
    ([pqueue (org-ilm--pqueue-read collection)]
     (setf (alist-get collection org-ilm--pqueues) pqueue))
    (t
     (let ((collection-data (alist-get collection (org-ilm-collections)))
           create-pqueue)

       (if collection-data
           (setq create-pqueue (yes-or-no-p (format "No priority queue found for collection \"%s\". Create a new one?" collection)))
         (when (yes-or-no-p (format "Collection \"%s\" does not exist. Create it?" collection))
           (funcall-interactively
            #'org-ilm-new-collection
            collection
            (org-ilm--collection-property (org-ilm--active-collection) :path))
           (setq create-pqueue t)))

       (when create-pqueue
         (let* ((pqueue (org-ilm--pqueue-new collection)))
           (org-ilm-queue--write pqueue)
           (setf (alist-get collection org-ilm--pqueues) pqueue)))))))

(defun org-ilm--pqueue-write-all ()
  "Force-save all pqueues that have pending changes."
  (interactive)
  (dolist (pqueue-cons org-ilm--pqueues)
    (let ((pqueue (cdr pqueue-cons)))
      (when (org-ilm-pqueue-p pqueue)
        (org-ilm-queue--write pqueue)))))

(defun org-ilm--pqueue-size (&optional collection)
  (ost-size (org-ilm--pqueue collection)))

(defun org-ilm--pqueue-insert (id position &optional collection)
  (org-ilm-queue--insert (org-ilm--pqueue collection) id position))

(defun org-ilm--pqueue-remove (id &optional collection)
  (org-ilm-queue--remove (org-ilm--pqueue collection) id))

(defun org-ilm--pqueue-select (position &optional collection)
  (let* ((pqueue (org-ilm--pqueue collection))
         (rank (car (ost-tree-position pqueue position))))
    (ost-node-id (ost-select pqueue rank))))

(defun org-ilm--pqueue-position (position &optional offset collection)
  (ost-tree-position (org-ilm--pqueue collection) position offset))

(defun org-ilm--pqueue-position-new (position &optional collection)
  (ost-tree-position (org-ilm--pqueue collection) position 1))

(defun org-ilm--pqueue-position-read (&optional initial-pos numnew-or-id collection)
  (org-ilm-queue--position-read (org-ilm--pqueue collection) initial-pos numnew-or-id))

(defun org-ilm--pqueue-priority (id &optional position nil-if-absent collection)
  "Position of the ID in PQUEUE as a cons (rank . quantile).
With POSITION, set the new position in the queue, or insert there if not exists."
  (org-ilm-pqueue--priority (org-ilm--pqueue collection) id position nil-if-absent))

(defun org-ilm--pqueue-bqueue (&optional collection)
  "Return `org-ilm-bqueue' from all elements in the priority queue."
  (org-ilm-pqueue--bqueue (org-ilm--pqueue collection)))

(defun org-ilm--pqueue-select-read (&optional init-position collection)
  "Select a position in the priority queue interactively."
  (org-ilm--bqueue-select-read (org-ilm--pqueue-bqueue collection) init-position))

;;; Footer

(provide 'org-ilm-pqueue)

;;; org-ilm-pqueue.el ends here
