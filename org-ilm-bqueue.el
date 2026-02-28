;;; org-ilm-bqueue.el --- Buffer queue -*- lexical-binding: t; -*-

;;; Commentary:

;; The buffer queue is a queue struct that inherits from `org-ilm-queue'. The
;; contents are visualized in a buffer as a table. It is build from a query,
;; interactively by the user, or derived from another queue (most notably the
;; priority queue object `org-ilm-pqueue'). Buffer queues are more transient,
;; they can be reordered, filtered, added to, saved and loaded, etc without it
;; having an effect on the priority queue.

;; Note about naming: user-facing functions and variables will simply be named
;; "queue" rather than "bqueue", as that is more of an implementation detail.

;;; Code:

;;;; Requirements

(require 'consult)

(require 'org-ilm-queue)
(require 'org-ilm-collection)
(require 'org-ilm-query)
(require 'org-ilm-queue)

;;;; Variables

(defcustom org-ilm-queue-concept-nchar 6
  "Truncation size of concept names as displayed in the queue.
If available, the last alias in the ROAM_ALIASES property will be used."
  :type 'integer
  :group 'org-ilm)

(defcustom org-ilm-outstanding-randomness .1
  "The default level of randomness applied to the outstanding queue."
  :type 'number
  :group 'org-ilm)

(defvar-local org-ilm-bqueue nil
  "Holds the `org-ilm-bqueue' object local to the queue buffer.")

(defvar org-ilm-bqueue-active-buffer nil
  "The active bqueue buffer. Reviewing will happen on this queue.")

(defvar org-ilm-bqueue-active-buffer-change-hook nil
  "Hook called when `org-ilm-bqueue-active-buffer' changes.")

;;;; Bqueue object

(cl-defstruct (org-ilm-bqueue 
               (:conc-name org-ilm-bqueue--)
               (:include org-ilm-queue))
  "Queue of elements shown in the queue buffer."
  (query
   nil
   :documentation
   "The query that was called to build this queue.
This can be nil when the queue was build interactively.")
  ;; TODO Remove? This is only used in `org-ilm--bqueue-rebuild' to
  ;; differentiate between bqueues derived from a priority queue, outstanding
  ;; query, and any other bqueue. We do need some way to tell if it is a
  ;; priority queue derived bqueue, but otherwise we can rely on query. Although
  ;; at that point might as well keep the type.
  (type
   nil
   :documentation
   "A symbol used to identify different types of bqueues.
For example, \'pqueue is used to identify queues dervied from the priority queue.")
  (key
   nil
   :documentation
   "Used to call `org-ilm-element--KEY' which is used to sort the table.")
  (reversed
   nil
   :documentation
   "Whether to sort the table in reverse.
The OST remains the same, but operations will instead adjust their calculations.")
  (randomness
   0
   :documentation
   "Value between 0-1 that indicates how much randomness to add to the sort.")
  ;; Pagination
  (page 0)
  (page-size 100))

(cl-defmethod ost-serialize ((bqueue org-ilm-bqueue))
  ;; Don't serialize elements, instead get IDs from ost and parse them again
  ;; when deserializing.
  (with-slots (query type key reversed randomness page-size) bqueue
    (append (cl-call-next-method)
            (list :query query
                  :type type
                  :key key
                  :reversed reversed
                  :randomness randomness
                  :page-size page-size))))

(cl-defmethod ost-deserialize ((bqueue org-ilm-bqueue) data)
  (cl-call-next-method)

  (map-let (:query :type :key :reversed :randomness :page-size) data
    (setf (org-ilm-bqueue--query bqueue) query
          (org-ilm-bqueue--type bqueue) type
          (org-ilm-bqueue--key bqueue) key
          (org-ilm-bqueue--reversed bqueue) reversed
          (org-ilm-bqueue--randomness bqueue) randomness
          (org-ilm-bqueue--page-size bqueue) page-size)))

(defun org-ilm-bqueue--pagination-range (bqueue)
  "Return (start . end) indices of the elements in the current page.
The end index is exclusive to match `ost-map-in-order'."
  (with-slots (page page-size) bqueue
    (if (null page-size)
        (cons 0 (ost-size bqueue))
      (let* ((beg (* page page-size)))
        (cons beg (+ beg page-size))))))

(defun org-ilm-bqueue--pos-location (bqueue pos)
  "Return (page . page-index) of element with position POS."
  (when-let* ((page-size (oref bqueue page-size))
              (rank (ost-tree-rank bqueue pos)))
    (cons
     (floor rank page-size)
     (mod rank page-size))))

(defun org-ilm-bqueue--pos-page (bqueue pos)
  "Return the page where the element with position POS is."
  (car (org-ilm-bqueue--pos-location bqueue pos)))

(defun org-ilm-bqueue--page-max (bqueue)
  "Return the hightes 0-based page index based on the queue- and page size."
  (org-ilm-bqueue--pos-page bqueue (1- (ost-size bqueue))))

(defun org-ilm-bqueue--element-value (bqueue element &optional ignore-key)
  "Return the value of ELEMENT by which it is sorted in BQUEUE."
  (cond-let*
    ;; First check the node key in the ost: A custom value might have been set
    ;; for an element even though the queue sort is based on a key.
    ([_ (not ignore-key)]
     [node (ost-tree-node-by-id bqueue (org-ilm-element--id element))]
     [key (ost-node-key node)]
     key)
    ([key (org-ilm-bqueue--key bqueue)]
     (pcase key
       (:priority
        (car (org-ilm-element--priority element)))
       (:scheduled
        (ts-unix (org-ilm-element--sched element)))
       (_ (error "Unknown key %s" key))))))

(defun org-ilm-bqueue--insert (bqueue element &optional value)
  "Insert ELEMENT into BQUEUE.

VALUE may be passed, which is used to determine the position in the queue.
If nil, `org-ilm-bqueue--key' will be used to determine the value."
  (let ((id (org-ilm-element--id element))
        (value (or value (org-ilm-bqueue--element-value bqueue element 'ignore-key))))
    (ost-tree-insert bqueue value id)))

;; TODO Some type of caching so that repeated selects doesnt search tree again
;; Should be implemented in `ost-tree-select'.
(defun org-ilm-bqueue--select (bqueue index)
  "Return id of element with the INDEX-th smallest key in BQUEUE (0-based)."
  (when (org-ilm-bqueue--reversed bqueue)
    (setq index (- (ost-size bqueue) 1 index)))
  (ost-node-id (ost-select bqueue index)))

(defun org-ilm-bqueue--top (bqueue &optional n)
  "Return the first N element ids in BQUEUE."
  ;; TODO We can use ost-tree-map for better performance
  (unless (ost-tree-empty-p bqueue)
    (mapcar
     (lambda (index)
       (org-ilm-bqueue--select bqueue index))
     (number-sequence 0 (or n (1- (ost-size bqueue)))))))

(defun org-ilm-bqueue--head (bqueue)
  "Return id of first element in BQUEUE."
  (org-ilm-bqueue--select bqueue 0))

(cl-defmethod org-ilm-queue--remove ((bqueue org-ilm-bqueue) element-or-id)
  (cl-call-next-method bqueue element-or-id))

(defun org-ilm-bqueue--pop (bqueue)
  "Remove the top most element in BQUEUE."
  (when (ost-tree-empty-p bqueue)
    (error "Tried popping an empty queue \"%s\"" (org-ilm-bqueue--name bqueue)))
  (let ((head (org-ilm-bqueue--head bqueue)))
    (org-ilm-queue--remove bqueue (org-ilm-element--id head))))

(defun org-ilm-bqueue--shuffle (bqueue randomness)
  "Shuffle the BQUEUE with a RANDOMNESS between 0 (no change) and 1 (complete randomness).

Calling this function repeatedly will stack the shuffling, rather than sorting
the queue and shuffling it afterwards. To achieve the latter, call
`org-ilm--bqueue-sort' first, or pass it the RANDOMNESS parameter directly."
  (cl-assert (<= 0 randomness 1) nil "Queue shuffle RANDOMNESS must be between 0 and 1.")
  (setf (org-ilm-bqueue--randomness bqueue) randomness)
  (unless (= randomness 0)
    (let ((ost (make-ost-tree)))
      (ost-do-in-order (node rank bqueue)
        (let* ((quantile (ost-tree-quantile bqueue rank))
               ;; New score mixing current rank and noise
               (new-score (+ (* (- 1.0 randomness) quantile)
                             (* randomness (org-ilm--random-uniform)))))
          (ost-tree-insert ost new-score (ost-node-id node))))
      (org-ilm-queue--tree-clear bqueue)
      (setf (ost-tree-nodes bqueue) (ost-tree-nodes ost)
            (ost-tree-root bqueue) (ost-tree-root ost)))))

(defun org-ilm-bqueue--sort (bqueue key reversed &optional randomness)
  (unless (and (string= (org-ilm-bqueue--key bqueue) key)
               (= (org-ilm-bqueue--randomness bqueue) randomness))
    (let ((ids (copy-sequence (hash-table-keys (oref bqueue nodes))))
          missing-ids)
      (org-ilm-queue--tree-clear bqueue)
      (setf (org-ilm-bqueue--key bqueue) key)
      
      (dolist (id ids)
        ;; Handle the case where we sort on priority, as that information is
        ;; available in the priority queue with just an id, so if element is
        ;; missing we can still sort by priority.
        (pcase key
          (:priority
           (ost-tree-insert
            bqueue
            (car
             (org-ilm--pqueue-priority
              id nil nil (oref bqueue collection)))
            id))
          (_
           (if-let ((element (org-ilm--element-by-id id)))
               (org-ilm-bqueue--insert bqueue element)
             (push id missing-ids)))))

      ;; Process missing elements so they appear at the bottom of the view
      (when missing-ids
        (let* ((size (ost-tree-size bqueue))
               (base-key
                (if (= size 0)
                    0.0
                  (if reversed
                      ;; If reversed, visual bottom is the START of the tree
                      ;; (smallest keys)
                      (ost-node-key (ost-select bqueue 0))
                    ;; If normal, visual bottom is the END of the tree (largest
                    ;; keys)
                    (ost-node-key (ost-select bqueue (1- size)))))))
          
          ;; Reverse to maintain their original relative order
          (dolist (id (nreverse missing-ids))
            ;; Step the key further outward so they don't clash and stay ordered
            (setq base-key (if reversed (- base-key 1.0) (+ base-key 1.0)))
            (ost-tree-insert bqueue base-key id))))))

  (setf (org-ilm-bqueue--reversed bqueue) reversed)
  (when randomness (org-ilm-bqueue--shuffle bqueue randomness)))

;;;; Building a bqueue 

(cl-defun org-ilm--bqueue-create (collection &key elements query name key reversed randomness type)
  "Create a new bqueue object."
  (setq key (or key "prank")
        randomness (or randomness 0))
  (let ((bqueue (make-org-ilm-bqueue
                 :name (or name
                           (when query (symbol-name query))
                           (symbol-name collection))
                 :collection collection
                 :query query
                 :type type
                 :key key
                 :reversed reversed
                 :randomness randomness)))
    ;; This inserts the elements in order of their value, which when not
    ;; explicitely given to `org-ilm-queue--insert' is the value of the key.
    (dolist (element elements)
      (org-ilm-bqueue--insert bqueue element))
    ;; In addition add some randomness if requested
    (org-ilm-bqueue--shuffle bqueue randomness)
    bqueue))

(defun org-ilm--bqueue-build (&optional collection query name &rest args)
  "Build a bqueue by a QUERY on COLLECTION, and return it."
  (let* ((collection (or collection
                         (org-ilm--collection-from-context)
                         (org-ilm--select-collection)))
         (query (or query (car (org-ilm--query-select))))
         (elements (org-ilm-query-collection query collection)))
    (apply #'org-ilm--bqueue-create
           collection
           :name (or name (format "%s" query))
           :elements elements
           :query query
           args)))

(defun org-ilm--bqueue-build-outstanding (collection)
  "Build a bqueue with all outstanding elements of COLLECTION."
  (org-ilm--bqueue-build
   collection
   'outstanding
   "Outstanding queue"
   :type 'outstanding
   :randomness org-ilm-outstanding-randomness))

(defun org-ilm--bqueue-select-bqueue (&optional collection)
  "Select from where to get the bqueue and (make and) return it."
  (let* ((collection (or collection
                         ;; (org-ilm--collection-from-context)
                         (org-ilm--select-collection)))
         (annotate (lambda (option)
                     (get-text-property 0 :desc option)))
         (choice (consult--multi
                  (list
                   (list
                    :name "Query"
                    :narrow ?q
                    :annotate annotate
                    :items
                    (list
                     (propertize "Priority queue"
                                 :desc "All elements"
                                 :action (lambda ()
                                           (org-ilm--pqueue-bqueue collection)))
                     (propertize "Outstanding queue"
                                 :desc "Due elements"
                                 :action (lambda ()
                                           (org-ilm--bqueue-build-outstanding
                                            collection))))
                    )
                   (list
                    :name "Saved"
                    :narrow ?s
                    :annotate annotate
                    :items
                    (mapcar
                     (lambda (f)
                       (propertize (file-name-base f)
                                   :path f
                                   :action
                                   (lambda ()
                                     (ost-read f))))
                     (org-ilm--queue-saved-queues collection))
                    )
                   )
                  :require-match t
                  :prompt "Queue: ")))
    (funcall (get-text-property 0 :action (car choice)))))

;;;; Bqueue buffer

;; The `org-ilm-bqueue' objects are stored locally within each bqueue buffer in
;; the variable with the same name.

(defun org-ilm--bqueue-buffer-p (buf &optional collection)
  "Tests whether or not BUF is a bqueue buffer.
With optional COLLECTION, also test if it belongs to it."
  (with-current-buffer buf
    (and (bound-and-true-p org-ilm-bqueue)
         (if collection
             (eq collection (org-ilm-bqueue--collection org-ilm-bqueue))
           t))))

(defun org-ilm--bqueue-buffers (&optional collection)
  "Return all bqueue buffers.
With optional COLLECTION, include only if belongs to it."
  (seq-filter
   (lambda (b) (org-ilm--bqueue-buffer-p b collection))
   (buffer-list)))

(defun org-ilm--bqueue-buffers-select ()
  "Select bqueue buffer."
  (let ((choice (consult--multi
                 (mapcar
                  (lambda (collection)
                    (list
                     :name (format "%s" collection)
                     :category 'buffer
                     :face 'consult-buffer
                     :state #'consult--buffer-preview
                     :items (mapcar #'buffer-name (org-ilm--bqueue-buffers collection))
                     ))
                  (mapcar #'car (org-ilm-collections))))))
    (car choice)))

(defun org-ilm-queue-buffers ()
  "View queue buffers using consult-buffer.

Embark export all to view in ibuffer."
  (interactive)
  (when-let ((buf (org-ilm--bqueue-buffers-select)))
    (consult--buffer-action buf)))

(defun org-ilm--bqueue-set-active-buffer (buffer)
  (cl-assert (or (null buffer) (org-ilm--bqueue-buffer-p buffer)))
  (setq org-ilm-bqueue-active-buffer buffer)
  (run-hooks 'org-ilm-bqueue-active-buffer-change-hook)
  buffer)

(defun org-ilm--bqueue-select-active-buffer ()
  (when-let ((buffers (org-ilm--bqueue-buffers)))
    (org-ilm--bqueue-set-active-buffer
     (if (= (length buffers) 1)
         (car buffers)
       (completing-read "Set active queue buffer: " buffers nil t)))))

(defun org-ilm--bqueue-buffer-current ()
  "Return the current bqueue buffer.
This is either the current buffer if its a bqueue buffer, or the active
bqueue buffer."
  (or (when (org-ilm--bqueue-buffer-p (current-buffer)) (current-buffer))
      org-ilm-bqueue-active-buffer))  

(defmacro org-ilm--bqueue-with-buffer (buffer &rest body)
  "Run BODY with BUFFER set to a valid bqueue buffer."
  (declare (indent 1))
  `(let ((buf (or ,buffer (org-ilm--bqueue-buffer-current))))
     (unless (and buf (buffer-live-p buf))
       (error "No valid queue buffer found"))
     (with-current-buffer buf
       ,@body)))

(defun org-ilm--bqueue-write (&optional buffer)
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-queue--write org-ilm-bqueue)))

(defun org-ilm--bqueue-rebuild (&optional buffer)
  "Replace the bqueue object by a rebuild one.
If the bqueue has a query, run it again. Else re-parse elements."
  (org-ilm--bqueue-with-buffer buffer
    (setq org-ilm-bqueue
          (cond-let*
            ((eq (org-ilm-bqueue--type org-ilm-bqueue) 'pqueue)
             (org-ilm--pqueue-bqueue (org-ilm-bqueue--collection org-ilm-bqueue)))
            ;; TODO Can't I just call the query? Same as next conditional
            ((eq (org-ilm-bqueue--type org-ilm-bqueue) 'outstanding)
             (org-ilm--bqueue-build-outstanding
              (org-ilm-bqueue--collection org-ilm-bqueue)))
            ([query (org-ilm-bqueue--query org-ilm-bqueue)]
             (org-ilm--bqueue-build
              (org-ilm-bqueue--collection org-ilm-bqueue) query))
            ;; TODO Do we want to do something when there is no query?
            (t
             (message "Nothing to rebuild for queryless queue"))))
    (current-buffer)))

(defun org-ilm--bqueue-shuffle (randomness &optional buffer)
  "Shuffle the bqueue with a RANDOMNESS between 0 (no change) and 1 (complete randomness)."
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-bqueue--shuffle org-ilm-bqueue randomness)
    (org-ilm-queue-revert)))

(defun org-ilm--bqueue-sort (key reversed &optional randomness buffer)
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-bqueue--sort org-ilm-bqueue key reversed randomness)
    org-ilm-bqueue))

(defun org-ilm--bqueue-completing-read (&optional new-name require-match collection)
  "Choose a bqueue buffer or make a new one."
  (let* ((bqueue-buffers (mapcar (lambda (b) (cons (buffer-name b) b))
                                 (org-ilm--bqueue-buffers collection)))
         (bqueue (completing-read "Select queue or create new: "
                                  bqueue-buffers nil require-match)))
    (or (cdr (assoc bqueue bqueue-buffers))
        (org-ilm--bqueue-buffer-create
         (org-ilm--bqueue-create
          (or collection (org-ilm--select-collection))
          :name (if (string-empty-p bqueue) new-name bqueue))))))

(cl-defun org-ilm--bqueue-insert (element &key buffer exists-ok)
  "Insert ELEMENT into the queue.
When EXISTS-OK, don't throw error if ELEMENT already in queue."
  (cl-assert (org-ilm-element-p element))
  (org-ilm--bqueue-with-buffer buffer
    (if (org-ilm-queue--contains-p org-ilm-bqueue element)
        (unless exists-ok
          (error "Element already in queue (%s)" (org-ilm-element--id element)))
      (org-ilm-bqueue--insert org-ilm-bqueue element)
      t)))

(cl-defun org-ilm--bqueue-count (&key buffer)
  "Return number of elements in the bqueue."
  (org-ilm--bqueue-with-buffer buffer
    (ost-size org-ilm-bqueue)))

(cl-defun org-ilm--bqueue-empty-p (&key buffer)
  (= 0 (org-ilm--bqueue-count :buffer buffer)))

(cl-defun org-ilm--bqueue-select (index &key buffer)
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-bqueue--select org-ilm-bqueue index)))

(cl-defun org-ilm--bqueue-top (&key n buffer)
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-bqueue--top org-ilm-bqueue n)))

(cl-defun org-ilm--bqueue-head (&key buffer)
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-bqueue--head org-ilm-bqueue)))

(cl-defun org-ilm--bqueue-elements (&key ordered buffer)
  "Return elements in the bqueue."
  (org-ilm--bqueue-with-buffer buffer
    (if ordered
        (org-ilm--bqueue-top :buffer buffer)
      (hash-table-keys (ost-tree-nodes org-ilm-bqueue)))))

(cl-defun org-ilm--bqueue-remove (element-or-id &key buffer)
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-queue--remove org-ilm-bqueue element-or-id)))

(cl-defun org-ilm--bqueue-pop (&key buffer)
  "Remove the top most element in the bqueue."
  (org-ilm--bqueue-with-buffer buffer
    (org-ilm-bqueue--pop org-ilm-bqueue)
    (org-ilm-queue-revert)))

;;;; Queue building

;; TODO With prefix arg: transient with additional settings
;; - Mainly queue-specific priorities.
;; - Exclude cards.
;; Replace current arg with double arg
;; TODO Only add child elements of same collection
(defun org-ilm-queue-add-dwim (&optional arg target bqueue-buf)
  "Add element at point + descendants to queue. With ARG, add to new queue.

If point on headline, add headline and descendants.
If point on concept, add all headlines of concept."
  (interactive "P")
  (setq target (or target 'all))
  (cl-assert (member target '(all self children)))
  (when-let* ((headline (org-ilm--org-headline-at-point))
              ;; We also want this to work on headlines that are not ilm elements.
              (type (or (org-ilm--element-type headline) 'headline))
              (collection (org-ilm--collection-from-context))
              (bqueue-buf (or bqueue-buf
                              (org-ilm--bqueue-completing-read
                               (format "[%s] %s" (upcase (symbol-name type))
                                       (org-element-property :title headline)))))
              (n-added 0))
    (pcase type
      ('concept
       ;; TODO This can prob be replaced with org-ql regex on CONCEPTS property
       (dolist (entry (org-ilm--collection-element-entries collection))
         (when-let* ((element (org-ilm--element-by-id (org-mem-entry-id entry)))
                     (concepts (car (org-ilm-element--concepts element)))
                     (ancestor-ids (mapcar #'car concepts)))
           (when (member (org-element-property :ID headline) ancestor-ids)
             (when (org-ilm--bqueue-insert element :buffer bqueue-buf :exists-ok t)
               (cl-incf n-added))))))
      (_
       (save-excursion
         (org-back-to-heading t)
         (let ((end (save-excursion (if (eq target 'self)
                                        (org-end-of-meta-data)
                                      (org-end-of-subtree t))))
               el)
           (when (eq target 'children) (outline-next-heading))
           (while (< (point) end)
             (when (setq el (org-ilm--element-at-point))
               (when (org-ilm--bqueue-insert el :buffer bqueue-buf :exists-ok t)
                 (cl-incf n-added)))
             (outline-next-heading))))))

    (message "Added %s element(s) to the queue" n-added)
    
    (with-current-buffer (switch-to-buffer bqueue-buf)
      (org-ilm-queue-revert))))

(defun org-ilm-queue (new &optional dont-switch)
  "Build a queue and view it in Agenda-like buffer."
  (interactive "P")
  (cond-let*
    ;; With prefix argument, create new buffer
    (new
     (org-ilm--bqueue-buffer-create
      (org-ilm--bqueue-select-bqueue) :switch-p (not dont-switch)))
    
    ;; If current buffer is queue buffer, switch to active queue buffer, or ask
    ;; to make this the active queue buffer.
    ((org-ilm--bqueue-buffer-p (current-buffer))
     (if org-ilm-bqueue-active-buffer
         (unless (eq (current-buffer) org-ilm-bqueue-active-buffer)
           (switch-to-buffer org-ilm-bqueue-active-buffer))
       (when (yes-or-no-p "Make current queue the active queue?")
         (org-ilm--bqueue-set-active-buffer (current-buffer)))))
    
    ;; If outside a queue buffer and there is an active queue buffer, switch to
    ;; it.
    (org-ilm-bqueue-active-buffer
     (switch-to-buffer org-ilm-bqueue-active-buffer))

    ;; If outside a queue buffer and there is no active queue buffer but there
    ;; are inactive ones, user can choose between switching to one of those,
    ;; making it active, or creating a new one, making it active.
    ([buffers (org-ilm--bqueue-buffers)]
     (let* ((candidates (mapcar #'buffer-name buffers))
            (choice (completing-read "Queue: " candidates)))
       (if (string-empty-p choice)
           (org-ilm--bqueue-buffer-create
            (org-ilm--bqueue-select-bqueue)
            :active-p t :switch-p (not dont-switch))
         (with-current-buffer (switch-to-buffer choice)
           (org-ilm--bqueue-set-active-buffer (current-buffer))))))
    
    ;; If outside a queue buffer and there are no active or inactive queue
    ;; buffers, make one and make it active.
    (t
     (org-ilm--bqueue-buffer-create
      (org-ilm--bqueue-select-bqueue)
      :active-p t :switch-p (not dont-switch)))))

;;; Footer

(provide 'org-ilm-bqueue)

;;; org-ilm-bqueue.el ends here
