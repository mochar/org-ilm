;;; org-ilm-pqueue.el --- Priority queue -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-ilm-ost)
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

Hook arg is (id . priority-rank) or an alist of such cons's if many moved at once.

Note that priority may change frequently due to the nature of a queue. Therefore not all changes are logged, only when explicitely set (`org-ilm-pqueue--move' and `org-ilm-pqueue--move-many'.")

(defconst org-ilm-pqueue-name "element-queue")

;;;; Pqueue object

(cl-defstruct (org-ilm-pqueue
               (:conc-name org-ilm-pqueue--)
               (:include org-ilm-ost (name org-ilm-pqueue-name) (dynamic t)))
  "Priority queue."
  (changes-since-save 0 :type integer))

(defun org-ilm--pqueue-file (collection)
  "Return the file path where the queue of COLLECTION is stored."
  (org-ilm--ost-file collection org-ilm-pqueue-name))

(defun org-ilm--pqueue-read (collection)
  "Read the priority queue of COLLECTION from disk."
  (let ((pqueue (make-org-ilm-pqueue :collection collection)))
    (ost-read (org-ilm-ost--file pqueue) pqueue)))

(defun org-ilm-pqueue--write-maybe (pqueue &optional n-changes)
  "Write PQUEUE to disk when enough changes have been applied."
  (cl-assert (org-ilm-pqueue-p pqueue))
  (setq n-changes (or n-changes 1))
  (cl-incf (org-ilm-pqueue--changes-since-save pqueue) n-changes)
  (when (>= (org-ilm-pqueue--changes-since-save pqueue)
            org-ilm-pqueue-save-every-n-changes)
    (org-ilm-ost--write pqueue)
    (setf (org-ilm-pqueue--changes-since-save pqueue) 0)))

(defun org-ilm-pqueue--insert (pqueue id pos)
  "Insert element with ID in PQUEUE with POS."
  (ost-tree-insert pqueue pos id)
  (org-ilm-pqueue--write-maybe pqueue))

(defun org-ilm-pqueue--remove (pqueue id)
  "Remove element with ID from PQUEUE."
  (when (ost-tree-contains-p pqueue id)
    (ost-tree-remove pqueue id)
    (org-ilm-pqueue--write-maybe pqueue)))

;; TODO Keep copy of ost to restore in case of error midway?
;; See same todo in ost.el.
(defun org-ilm-pqueue--move (pqueue id new-pos)
  "Move element ID in PQUEUE to NEW-POS."
  (ost-tree-move pqueue id new-pos)
  (run-hook-with-args 'org-ilm-pqueue-priority-changed-hook (cons id new-pos))
  (org-ilm-pqueue--write-maybe pqueue))

(defun org-ilm-pqueue--move-many (pqueue new-pos-alist)
  "Move many elements in PQUEUE to a new pos.

NEW-POS-ALIST is an alist of (ID . NEW-POS) pairs."
  (ost-tree-move-many pqueue new-pos-alist)
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
    (setq position (or position (org-ilm-ost--position-read pqueue nil 1)))
    (org-ilm-pqueue--insert pqueue id position)
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
          (org-ilm-pqueue--move pqueue id position)
        (org-ilm-pqueue--insert pqueue id position)))
     ;; Node found, return position
     (node (ost-position pqueue node))
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
;; `org-ilm--queue-build' when we want to sort by priority.
(defun org-ilm-pqueue--queue (pqueue &optional query)
  "Build an `org-ilm-queue' from all elements in PQUEUE.

Because element headlines might be deleted, their ids will map to the id itself
in `org-ilm-queue--elements', rather than `org-ilm-element' object."
  (let* ((collection (org-ilm-pqueue--collection pqueue))
         (queue (make-org-ilm-queue
                 :name (format "Priority queue (%s)" collection)
                 :collection collection
                 :type 'pqueue))
         (query (or query #'org-ilm--queries-query-active))
         (element-map (org-ilm-query-collection query collection 'hash-table-p)))

    ;; Map over OST
    (ost-map-in-order
     (lambda (node rank)
       (let* ((id (ost-node-id node))
              (element (gethash id element-map)))

         (if element
             (org-ilm-queue--insert queue element rank)
           ;; For ids that have not been found by querying the collection but are
           ;; present in the priority queue, put them in the queue as normal, but
           ;; in the node map use the id also as value.
           (puthash id id (org-ilm-queue--elements queue))
           (ost-tree-insert queue rank id))

         ;; Remove from map so that we are left with elements missing in the
         ;; priority queue.
         (remhash id element-map)))
     pqueue)

    ;; Handle elements that were found by querying the collection, but are
    ;; missing in the priority queue.
    ;; TODO Handle this. Present user options:
    ;; 1. Ignore
    ;; 2. Jump to each element, allow user to set priority, one after another
    ;; 3. Dump elements in seperate queue, show queue view
    (unless (hash-table-empty-p element-map)
      (warn "Ilm: Parsed %s elements with missing priority." (hash-table-size element-map)))

    queue))

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
      (org-ilm-pqueue--insert pqueue (org-ilm-element--id element) 0))
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
           (org-ilm-ost--write pqueue)
           (setf (alist-get collection org-ilm--pqueues) pqueue)))))))

(defun org-ilm--pqueue-write-all ()
  "Force-save all pqueues that have pending changes."
  (interactive)
  (dolist (pqueue-cons org-ilm--pqueues)
    (let ((pqueue (cdr pqueue-cons)))
      (when (org-ilm-pqueue-p pqueue)
        (org-ilm-ost--write pqueue)))))

(defun org-ilm--pqueue-size (&optional collection)
  (ost-size (org-ilm--pqueue collection)))

(defun org-ilm--pqueue-insert (id position &optional collection)
  (org-ilm-pqueue--insert (org-ilm--pqueue collection) id position))

(defun org-ilm--pqueue-remove (id &optional collection)
  (org-ilm-pqueue--remove (org-ilm--pqueue collection) id))

(defun org-ilm--pqueue-select (position &optional collection)
  (let* ((pqueue (org-ilm--pqueue collection))
         (rank (car (ost-tree-position pqueue position))))
    (ost-node-id (ost-select pqueue rank))))

(defun org-ilm--pqueue-position (position &optional offset collection)
  (ost-tree-position (org-ilm--pqueue collection) position offset))

(defun org-ilm--pqueue-position-new (position &optional collection)
  (ost-tree-position (org-ilm--pqueue collection) position 1))

(defun org-ilm--pqueue-position-read (&optional initial-pos numnew-or-id collection)
  (org-ilm-ost--position-read (org-ilm--pqueue collection) initial-pos numnew-or-id))

(defun org-ilm--pqueue-priority (id &optional position nil-if-absent collection)
  "Position of the ID in PQUEUE as a cons (rank . quantile).
With POSITION, set the new position in the queue, or insert there if not exists."
  (org-ilm-pqueue--priority (org-ilm--pqueue collection) id position nil-if-absent))

(defun org-ilm--pqueue-queue (&optional collection)
  "Return `org-ilm-queue' from all elements in the priority queue."
  (org-ilm-pqueue--queue (org-ilm--pqueue collection)))

(defun org-ilm--pqueue-select-read (&optional init-position collection)
  "Select a position in the priority queue interactively."
  (org-ilm--queue-select-read (org-ilm--pqueue-queue collection) init-position))

;;; Footer

(provide 'org-ilm-pqueue)

;;; org-ilm-pqueue.el ends here
