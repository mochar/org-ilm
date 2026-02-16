;;; org-ilm-queue.el --- Queue -*- lexical-binding: t; -*-

;;; Commentary:

;; "Queue" is a bit of a misnomer, as it can be ordered in any way, and may
;; contain any element, outstanding or not, depending on the query it was used
;; to build it. However, on review start, it forms the review queue, so in that
;; sense it is a queue.

;;; Code:

;;;; Requirements

(require 'consult)

(require 'org-ilm-ost)
(require 'org-ilm-collection)
(require 'org-ilm-query)

;;;; Variables

(defcustom org-ilm-queue-concept-nchar 6
  "Truncation size of concept names as displayed in the queue.
If available, the last alias in the ROAM_ALIASES property will be used."
  :type 'integer
  :group 'org-ilm)

(defvar-local org-ilm-queue nil
  "List of elements.")

(defvar org-ilm-queue-active-buffer nil
  "The active queue buffer. Reviewing will happen on this queue.")

(defvar org-ilm-queue-active-buffer-change-hook nil
  "Hook called when `org-ilm-queue-active-buffer' changes.")

;;;; Queue object

(cl-defstruct (org-ilm-queue 
               (:conc-name org-ilm-queue--)
               (:include org-ilm-ost))
  "Queue of elements."
  query
  type
  (elements (make-hash-table :test #'equal)
            :documentation "Map id to org-ilm-element.")
  key ;; Used to call `org-ilm-element--KEY'
  reversed
  (randomness 0))

(cl-defmethod ost-serialize ((queue org-ilm-queue))
  ;; Don't serialize elements, instead get IDs from ost and parse them again
  ;; when deserializing.
  (append (cl-call-next-method)
          (list :query (org-ilm-queue--query queue)
                :type (org-ilm-queue--type queue)
                :key (org-ilm-queue--key queue)
                :reversed (org-ilm-queue--reversed queue)
                :randomness (org-ilm-queue--randomness queue))))

(cl-defmethod ost-deserialize ((queue org-ilm-queue) data)
  (cl-call-next-method)
  
  (setf (org-ilm-queue--query queue) (plist-get data :query)
        (org-ilm-queue--type queue) (plist-get data :type)
        (org-ilm-queue--key queue) (plist-get data :key)
        (org-ilm-queue--reversed queue) (plist-get data :reversed)
        (org-ilm-queue--randomness queue) (plist-get data :randomness))

  ;; Reconstruct the elements hash table
  (let ((elements (make-hash-table :test #'equal)))
    (dolist (id (hash-table-keys (org-ilm-queue--nodes queue)))
      ;; Try to fetch the full element object. If not found, store ID as fallback
      ;; (Matches `org-ilm-pqueue--queue')
      (let ((element (or (org-ilm--element-by-id id) id)))
        (puthash id element elements)))
    (setf (org-ilm-queue--elements queue) elements)))

(defun org-ilm-queue--element-value (queue element &optional no-key)
  "Return the value of ELEMENT by which it is sorted in QUEUE."
  (cond-let*
    ;; First check the node key in the ost: A custom value might have been set
    ;; for an element even though the queue sort is based on a key.
    ([_ (not no-key)]
     [node (ost-tree-node-by-id queue (org-ilm-element--id element))]
     [key (ost-node-key node)]
     key)
    ([key (org-ilm-queue--key queue)]
     [getter (intern (concat "org-ilm-element--" key))]
     (cl-assert (functionp getter))
     (when-let ((value (funcall getter element)))
       (cl-typecase value
         (ts (ts-unix value))
         (t value))))))

(defun org-ilm-queue--count (queue)
  "Return the number of elements in QUEUE."
  ;; (hash-table-count (org-ilm-queue--elements queue))
  (ost-size queue)
  )

(defun org-ilm-queue--empty-p (queue)
  "Return t if QUEUE is empty."
  (hash-table-empty-p (org-ilm-queue--elements queue)))

(defun org-ilm-queue--insert (queue element &optional value)
  "Insert ELEMENT in QUEUE.
A custom VALUE may be passed on which to sort the element in the queue."
  (let ((id (org-ilm-element--id element))
        (value (or value (org-ilm-queue--element-value queue element 'no-key))))
    (puthash id element (org-ilm-queue--elements queue))
    (ost-tree-insert queue value id)))

;; TODO Some type of caching so that repeated selects doesnt search tree again
;; Should be implemented in `ost-tree-select'.
(cl-defun org-ilm-queue--select (queue index)
  "Return the element with the INDEX-th smallest key in QUEUE (0-based)."
  (when (org-ilm-queue--reversed queue)
    (setq index (- (org-ilm-queue--count queue) 1 index)))
  (let ((node (ost-select queue index)))
    (gethash (ost-node-id node) (org-ilm-queue--elements queue))))

(defun org-ilm-queue--top (queue &optional n)
  "Return the first N elements in QUEUE."
  (unless (org-ilm-queue--empty-p queue)
    (mapcar
     (lambda (index)
       (org-ilm-queue--select queue index))
     (number-sequence 0 (or n (1- (org-ilm-queue--count queue)))))))

(defun org-ilm-queue--head (queue)
  "Return the first element in QUEUE."
  (org-ilm-queue--select queue 0))

(defun org-ilm-queue--contains-p (queue element)
  "Return ELEMENT if it is in QUEUE.
ELEMENT may be an `org-ilm-element' or its id."
  (let ((id (if (org-ilm-element-p element)
                (org-ilm-element--id element)
              element)))
    (gethash id (org-ilm-queue--elements queue))))

(defun org-ilm-queue--element (queue element-or-id)
  (if (org-ilm-element-p element-or-id)
      element-or-id
    (gethash element-or-id (org-ilm-queue--elements queue))))

(defun org-ilm-queue--remove (queue element)
  "Remove ELEMENT from QUEUE.
ELEMENT may be an `org-ilm-element' or its id."
  (let ((id (if (org-ilm-element-p element)
                (org-ilm-element--id element)
              element)))
    (ost-tree-remove org-ilm-queue id)
    (remhash id (org-ilm-queue--elements org-ilm-queue))))

(defun org-ilm-queue--pop (queue)
  "Remove the top most element in QUEUE."
  (when (org-ilm-queue--empty-p queue)
    (error "Tried popping an empty queue \"%s\"" (org-ilm-queue--name queue)))
  (let ((head (org-ilm--queue-head)))
    (org-ilm--queue-remove (org-ilm-element--id head))))

(defun org-ilm-queue--move (queue element new-rank)
  "Move ELEMENT in QUEUE to NEW-RANK."
  (setq element (org-ilm-queue--element queue element))
  (cl-assert (org-ilm-element-p element))
  (ost-tree-move queue (org-ilm-element--id id) new-rank))

(defun org-ilm-queue--move-many (queue new-ranks-alist)
  "Move many elements in QUEUE to a new rank.

NEW-RANKS-ALIST is an alist of (ID . NEW-RANK) pairs."
  (ost-tree-move-many queue new-ranks-alist))

(defun org-ilm-queue--shuffle (queue randomness)
  "Shuffle the queue with a RANDOMNESS between 0 (no change) and 1 (complete randomness).

Calling this function repeatedly will stack the shuffling, rather than sorting
the queue and shuffling it afterwards. To achieve the latter, call
`org-ilm--queue-sort' first, or pass it the RANDOMNESS parameter directly."
  (cl-assert (<= 0 randomness 1) nil "Queue shuffle RANDOMNESS must be between 0 and 1.")
  (setf (org-ilm-queue--randomness queue) randomness)
  (unless (= randomness 0)
    (let ((ost (org-ilm-ost--as-tree queue)))
      (org-ilm-ost--tree-clear queue)
      (ost-map-in-order
       (lambda (node rank)
         (let* ((element (gethash (ost-node-id node)
                                  (org-ilm-queue--elements queue)))
                (quantile (ost-tree-quantile ost rank))
                ;; New score mixing current rank and noise
                (new-score (+ (* (- 1.0 randomness) quantile)
                              (* randomness (org-ilm--random-uniform)))))
           (org-ilm-queue--insert queue element new-score)))
       ost))))

(defun org-ilm--queue-saved-queues (&optional collection)
  (let* ((dir (org-ilm--collection-data-dir (or collection (org-ilm--active-collection))))
         (queue-files (directory-files dir t "\\.el$")))
    (seq-filter
     (lambda (f) (not (string= (file-name-base f) org-ilm-pqueue-name)))
     queue-files)))

;;;; Building a queue 

;; Queues are stored locally within each queue buffer.

(cl-defun org-ilm--queue-create (collection &key elements query name key reversed randomness type)
  "Create a new ilm queue object."
  (setq key (or key "prank"))
  (setq randomness (or randomness 0))
  (let ((queue (make-org-ilm-queue
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
      (org-ilm-queue--insert queue element))
    ;; In addition add some randomness if requested
    (org-ilm-queue--shuffle queue randomness)
    queue))

(defun org-ilm--queue-build (&optional collection query)
  "Build a queue and return it."
  (let* ((collection (or collection
                         (org-ilm--collection-from-context)
                         (org-ilm--select-collection)))
         (query (or query (car (org-ilm--query-select))))
         (elements (org-ilm-query-collection query collection)))
    (org-ilm--queue-create
     collection :elements elements :query query)))

(defun org-ilm-queue-build (&optional collection)
  "Build a `org-ilm-queue' object."
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
                                 :action (lambda () (org-ilm--pqueue-queue collection)))
                     (propertize "Outstanding queue"
                                 :desc "Due elements"
                                 :action (lambda () (org-ilm--queue-build-outstanding
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
                     (org-ilm--queue-saved-queues))
                    )
                   )
                  :require-match t
                  :prompt "Queue: ")))
    (funcall (get-text-property 0 :action (car choice)))))

(defun org-ilm--queue-rebuild (&optional buffer)
  "Replace the queue object by a rebuild one.
If the queue has a query, run it again. Else re-parse elements."
  (org-ilm-with-queue-buffer buffer
    (setq org-ilm-queue
          (cond-let*
            ((eq (org-ilm-queue--type org-ilm-queue) 'pqueue)
             (org-ilm--pqueue-queue (org-ilm-queue--collection org-ilm-queue)))
            ((eq (org-ilm-queue--type org-ilm-queue) 'outstanding)
             (org-ilm--queue-build-outstanding (org-ilm-queue--collection org-ilm-queue)))
            ([query (org-ilm-queue--query org-ilm-queue)]
             (org-ilm--queue-build
              (org-ilm-queue--collection org-ilm-queue) query))
            (t
             (let ((queue (copy-org-ilm-queue org-ilm-queue))
                   (elements (make-hash-table :test #'equal)))
               (dolist (id (hash-table-keys (org-ilm-queue--elements org-ilm-queue)))
                 (puthash id (or (org-ilm--element-by-id id) id) elements))
               (setf (org-ilm-queue--elements queue) elements)
               queue))))
    (current-buffer)))

(defun org-ilm--queue-shuffle (randomness &optional buffer)
  "Shuffle the queue with a RANDOMNESS between 0 (no change) and 1 (complete randomness)."
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--shuffle org-ilm-queue randomness)
    (org-ilm-queue-revert)))

(defun org-ilm--queue-sort (key reversed &optional randomness buffer)
  (org-ilm-with-queue-buffer buffer
    (unless (and (string= (org-ilm-queue--key org-ilm-queue) key)
                 (= (org-ilm-queue--randomness org-ilm-queue) randomness))
      (org-ilm-ost--tree-clear org-ilm-queue)
      (setf (org-ilm-queue--key org-ilm-queue) key)
      ;; TODO Take note of elements where key=nil, and add it them in the end
      ;; with value max+i so that they show up in bottom of queue view.
      (dolist (element (hash-table-values (org-ilm-queue--elements org-ilm-queue)))
        (org-ilm-queue--insert org-ilm-queue element)))
    (setf (org-ilm-queue--reversed org-ilm-queue) reversed)
    (when randomness (org-ilm--queue-shuffle randomness))
    org-ilm-queue))

(cl-defun org-ilm--queue-buffer-create (queue &key active-p switch-p)
  "Create a new queue buffer.
This does not fill the buffer with the queue elements! For this run
`org-ilm--queue-buffer-build' afterwards."
  (let ((buffer (generate-new-buffer
                 (format "*Ilm Queue (%s)*" (org-ilm-queue--name queue)))))
    (with-current-buffer buffer
      (special-mode) ;; has to be first
      (setq-local org-ilm-queue queue)

      ;; Make `save-buffer' command write queue to disk.
      (setq-local write-contents-functions (list #'org-ilm-queue-save))
      ;; This is not necessary, but I added it because org-mem save-buffer hook
      ;; throws an error when file is not buffer visiting. But perhaps there is
      ;; some utility to having this..
      ;; (setq-local buffer-file-name (org-ilm-ost--file org-ilm-queue))
      ;; (set-visited-file-name )

      ;; Make sure that when the queue buffer is killed we update the active
      ;; buffer.
      (add-hook 'kill-buffer-hook
                (lambda ()
                  ;; Though not necessary as buffer will be killed anyway, this
                  ;; prevents the queue buffer from being reverted/rebuilt again
                  ;; before being destroyed, which is unnecessary and slow.
                  (remove-hook 'org-ilm-queue-active-buffer-change-hook
                               #'org-ilm-queue-revert 'local)
                  
                  (when (eq (current-buffer) org-ilm-queue-active-buffer)
                    (org-ilm-queue--set-active-buffer nil)))
                nil 'local)

      ;; Refresh when queue during changes that effect queue
      (add-hook 'org-ilm-review-next-hook
                #'org-ilm-queue-revert nil t)
      (add-hook 'org-ilm-review-quit-hook
                #'org-ilm-queue-revert nil t)
      (add-hook 'org-ilm-queue-active-buffer-change-hook
                #'org-ilm-queue-revert nil t)
      
      (org-ilm--queue-buffer-build :buffer buffer :switch-p switch-p)

      ;; This doesn't seem to activate when called in `org-ilm--queue-buffer-build'
      (eldoc-mode 1)

      (org-ilm-queue-mode 1)
      
      (when active-p
        (org-ilm-queue--set-active-buffer buffer)))
    buffer))

(defun org-ilm--queue-buffer-p (buf &optional collection)
  "Tests whether or not BUF is a queue buffer."
  (with-current-buffer buf
    (and (bound-and-true-p org-ilm-queue)
         (if collection
             (eq collection (org-ilm-queue--collection org-ilm-queue))
           t))))

(defun org-ilm--queue-buffers (&optional collection)
  "Return all queue buffers."
  (seq-filter
   (lambda (b) (org-ilm--queue-buffer-p b collection))
   (buffer-list)))

(defun org-ilm--queue-buffers-select ()
  "Select queue buffer."
  (let ((choice (consult--multi
                 (mapcar
                  (lambda (collection)
                    (list
                     :name (format "%s" collection)
                     :category 'buffer
                     :face 'consult-buffer
                     :state #'consult--buffer-preview
                     :items (mapcar #'buffer-name (org-ilm--queue-buffers collection))
                     ))
                  (mapcar #'car (org-ilm-collections))))))
    (car choice)))

(defun org-ilm-queue-buffers ()
  "View queue buffers using consult-buffer.

Embark export all to view in ibuffer."
  (interactive)
  (when-let ((buf (org-ilm--queue-buffers-select)))
    (consult--buffer-action buf)))

(defun org-ilm-queue--set-active-buffer (buffer)
  (cl-assert (or (null buffer) (org-ilm--queue-buffer-p buffer)))
  (setq org-ilm-queue-active-buffer buffer)
  (run-hooks 'org-ilm-queue-active-buffer-change-hook)
  buffer)

(defun org-ilm--queue-select-active-buffer ()
  (when-let ((buffers (org-ilm--queue-buffers)))
    (org-ilm-queue--set-active-buffer
     (if (= (length buffers) 1)
         (car buffers)
       (completing-read "Set active queue buffer: " buffers nil t)))))

(defun org-ilm--queue-buffer-current ()
  "Return the current queue buffer.
This is either the current buffer if its a queue buffer, or the active
queue buffer."
  (or (and (org-ilm--queue-buffer-p (current-buffer)) (current-buffer))
      org-ilm-queue-active-buffer))  

(defmacro org-ilm-with-queue-buffer (buffer &rest body)
  "Run BODY with BUFFER set to a valid queue buffer."
  (declare (indent 1))
  `(let ((buf (or ,buffer (org-ilm--queue-buffer-current))))
     (unless (and buf (buffer-live-p buf))
       (error "No valid queue buffer found"))
     (with-current-buffer buf
       ,@body)))

(defun org-ilm--queue-completing-read (&optional new-name require-match)
  "Choose a queue buffer or make new."
   (let* ((queue-buffers (mapcar (lambda (b) (cons (buffer-name b) b))
                                 (org-ilm--queue-buffers)))
          (queue (completing-read "Add to queue, or create new: "
                                  queue-buffers nil require-match)))
     (or (cdr (assoc queue queue-buffers))
         (org-ilm--queue-buffer-create
          (org-ilm--queue-create
           (org-ilm--active-collection)
           :name (if (string-empty-p queue) new-name queue))))))

(defun org-ilm-queue (new &optional dont-switch)
  "Build a queue and view it in Agenda-like buffer."
  (interactive "P")
  (cond-let*
    ;; With prefix argument, create new buffer
    (new
     (org-ilm--queue-buffer-create
      (org-ilm-queue-build) :switch-p (not dont-switch)))
    
    ;; If current buffer is queue buffer, switch to active queue buffer, or ask
    ;; to make this the active queue buffer.
    ((org-ilm--queue-buffer-p (current-buffer))
     (if org-ilm-queue-active-buffer
         (unless (eq (current-buffer) org-ilm-queue-active-buffer)
           (switch-to-buffer org-ilm-queue-active-buffer))
       (when (yes-or-no-p "Make current queue the active queue?")
         (org-ilm-queue--set-active-buffer (current-buffer)))))
    
    ;; If outside a queue buffer and there is an active queue buffer, switch to
    ;; it.
    (org-ilm-queue-active-buffer
     (switch-to-buffer org-ilm-queue-active-buffer))

    ;; If outside a queue buffer and there is no active queue buffer but there
    ;; are inactive ones, user can choose between switching to one of those,
    ;; making it active, or creating a new one, making it active.
    ([buffers (org-ilm--queue-buffers)]
     (let* ((candidates (mapcar #'buffer-name buffers))
            (choice (completing-read "Queue: " candidates)))
       (if (string-empty-p choice)
           (org-ilm--queue-buffer-create
            (org-ilm-queue-build)
            :active-p t :switch-p (not dont-switch))
         (with-current-buffer (switch-to-buffer choice)
           (org-ilm-queue--set-active-buffer (current-buffer))))))
    
    ;; If outside a queue buffer and there are no active or inactive queue
    ;; buffers, make one and make it active.
    (t
     (org-ilm--queue-buffer-create
      (org-ilm-queue-build)
      :active-p t :switch-p (not dont-switch)))))

;;;; Queue operations

(cl-defun org-ilm--queue-insert (element &key buffer exists-ok)
  "Insert ELEMENT into the queue.
When EXISTS-OK, don't throw error if ELEMENT already in queue."
  (cl-assert (org-ilm-element-p element))
  (org-ilm-with-queue-buffer buffer
    (if (org-ilm-queue--contains-p org-ilm-queue element)
        (unless exists-ok
          (error "Element already in queue (%s)" (org-ilm-element--id element)))
      (org-ilm-queue--insert org-ilm-queue element)
      t)))

(cl-defun org-ilm--queue-count (&key buffer)
  "Return number of elements in the queue."
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--count org-ilm-queue)))

(cl-defun org-ilm--queue-empty-p (&key buffer)
  (= 0 (org-ilm--queue-count :buffer buffer)))

(cl-defun org-ilm--queue-select (index &key buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--select org-ilm-queue index)))

(cl-defun org-ilm--queue-top (&key n buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--top org-ilm-queue n)))

(cl-defun org-ilm--queue-head (&key buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--head org-ilm-queue)))

(cl-defun org-ilm--queue-elements (&key ordered buffer)
  "Return elements in the queue."
  (org-ilm-with-queue-buffer buffer
    (if ordered
        (org-ilm--queue-top :buffer buffer)
      (hash-table-values (org-ilm-queue--elements org-ilm-queue)))))

(cl-defun org-ilm--queue-remove (element-or-id &key buffer)
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--remove org-ilm-queue element-or-id)))

(cl-defun org-ilm--queue-pop (&key buffer)
  "Remove the top most element in the queue."
  (org-ilm-with-queue-buffer buffer
    (org-ilm-queue--pop org-ilm-queue)
    (org-ilm-queue-revert)))

;;;; Queue building

(defcustom org-ilm-outstanding-randomness .1
  "The default level of randomness applied to the outstanding queue."
  :type 'number
  :group 'org-ilm)

(defun org-ilm--queue-build-outstanding (collection)
  (org-ilm--queue-create
   collection
   :name (format "Outstanding queue (%s)" collection)
   :type 'outstanding
   :elements (org-ilm-query-collection #'org-ilm--queries-query-outstanding collection)
   :randomness org-ilm-outstanding-randomness))

;; TODO With prefix arg: transient with additional settings
;; - Mainly queue-specific priorities.
;; - Exclude cards.
;; Replace current arg with double arg
(defun org-ilm-queue-add-dwim (&optional arg exclude-self)
  "Add element at point + descendants to queue. With ARG, add to new queue.

If point on headline, add headline and descendants.
If point on concept, add all headlines of concept."
  (interactive "P")
  (when-let* ((headline (org-ilm--org-headline-at-point))
              ;; We also want this to work on headlines that are not ilm elements.
              (type (or (org-ilm--element-type headline) 'headline))
              (collection (org-ilm--collection-from-context))
              (queue-buffer (org-ilm--queue-completing-read
                             (format "[%s] %s" (upcase (symbol-name type))
                                     (org-element-property :title headline))))
              (n-added 0))
    (pcase type
      ('concept
       ;; TODO This can prob be replaced with org-ql regex on CONCEPTS property
       (dolist (entry (org-ilm--collection-entries collection))
         (when-let* ((element (org-ilm--element-by-id (org-mem-entry-id entry)))
                     (concepts (car (org-ilm-element--concepts element)))
                     (ancestor-ids (mapcar #'car concepts)))
           (when (member (org-element-property :ID headline) ancestor-ids)
             (when (org-ilm--queue-insert element :buffer queue-buffer :exists-ok t)
               (cl-incf n-added))))))
      (_
       (save-excursion
         (org-back-to-heading t)
         (let ((end (save-excursion (org-end-of-subtree t)))
               el)
           (when exclude-self (outline-next-heading))
           (while (< (point) end)
             (when (setq el (org-ilm--element-at-point))
               (when (org-ilm--queue-insert el :buffer queue-buffer :exists-ok t)
                 (cl-incf n-added)))
             (outline-next-heading))))))

    (message "Added %s element(s) to the queue" n-added)
    
    (with-current-buffer (switch-to-buffer queue-buffer)
      (org-ilm-queue-revert))))

;;; Footer

(provide 'org-ilm-queue)

;;; org-ilm-queue.el ends here
