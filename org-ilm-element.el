;;; org-ilm-element.el --- Element -*- lexical-binding: t; -*-

;;; Commentary:

;; Elements are defined by having a valid ILM_TYPE property, and an
;; ILM_COLLECTION property, the latter which can be inherited. Elements may have
;; a scheduled data, and may or may not be in their collection's priority queue.
;;
;; The following types exist:
;; - Material
;;     Can have attachments named <org-id>.<ext> that can be processed into
;;     extracts and cards.
;; - Card
;;     Must have card attachments (although not enforced).
;; - Concept (?)
;; - Queue
;; - Task (?)

;;; Code:

;;;; Requirements

(require 'cond-let)
(require 'transient)
(require 'org-attach)
(require 'org-node)
(require 'org-ql)
(require 'ts)

(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-review)
(require 'org-ilm-log)
(require 'org-ilm-collection)
(require 'org-ilm-schedule)
(require 'org-ilm-pqueue)
(require 'org-ilm-bqueue-view)
(require 'org-ilm-bqueue)
(require 'org-ilm-concept)
(require 'ost)

;;;; Variables

(defvar org-ilm-element-update-hook nil
  "Hook run when an element has been updated.
Hook is passed the element object.")

(defvar org-ilm-element-delete-hook nil
  "Hook run when an element has been deleted.

Parameters passed: id, type, collection")

;;;; Element

;; TODO: Rather than storing data already stored in org-mem cache, load from it.
;; Eg org-ilm-element--level becomes a function that gets org-mem entry by id
;; and returns org-mem-entry-level
(cl-defstruct (org-ilm-element
               (:conc-name org-ilm-element--))
  "A piece of knowledge."
  id collection state level pcookie rawval title tags sched
  type concepts registry media done)

(defun org-ilm-element--schedrel (element)
  "Return distance from now to scheduled date in days."
  (when-let ((scheduled (org-ilm-element--sched element)))
    ;; convert from sec to days
    (/ (ts-diff (ts-now) scheduled) 86400)))

(defun org-ilm-element--card-p (element)
  (eq (org-ilm-element--type element) 'card))

(defun org-ilm--element-type (&optional thing)
  (let ((type (cl-etypecase thing
                (null (-some-> (org-entry-get nil org-ilm-property-type) intern))
                (symbol thing)
                (string (intern thing))
                (cons ;; could be an org-element
                 (when (eq 'headline (ignore-errors (org-element-type thing)))
                   (-some-> (org-element-property :ILM_TYPE thing) intern)))
                (org-mem-entry (-some-> (org-mem-entry-property org-ilm-property-type thing) intern)))))
    (when (member type org-ilm-element-types)
      type)))

(defun org-ilm--element-from-entry (entry)
  "Returns an `org-ilm-element' from an `org-mem-entry' object ENTRY."
  (cl-assert (org-mem-entry-p entry))
  (let* ((type (org-ilm--element-type entry))
         (id (org-mem-entry-id entry))
         (collection (org-mem-entry-property-with-inheritance
                      org-ilm-property-collection entry)))
    (when (and type collection)
      (make-org-ilm-element
       ;; Headline element properties
       :id id
       :state (org-mem-entry-todo-state entry)
       :level (org-mem-entry-level entry)
       :pcookie (org-mem-entry-priority entry)
       :tags (org-mem-entry-tags entry)
       :title (org-mem-entry-title entry)

       ;; Ilm stuff
       :collection (intern collection)
       :sched (-some-> (org-mem-entry-scheduled entry) ts-parse)
       :type type
       :done (string= (org-mem-entry-todo-state entry) "DONE")
       ;; TODO Is this still used?!?!
       :registry (org-mem-entry-property-with-inheritance "REGISTRY" entry)
       :media (org-ilm--media-compile entry)
       ;; cdr to get rid of headline priority in the car - redundant
       :concepts (org-ilm--concept-cache-gather id)))))

(defun org-ilm--element-by-id (id)
  "Return `org-ilm-element' object of headline with ID."
  (when-let* ((entry (org-ilm--org-mem-get-entry-ensured id)))
    (org-ilm--element-from-entry entry)))

(defun org-ilm--element-at-point ()
  "Parse `org-ilm-element' of headline at point."
  (when-let* ((entry (org-ilm--org-mem-get-entry-ensured)))
    (org-ilm--element-from-entry entry)))

(defun org-ilm--element-from-context ()
  "Return `org-ilm-element' associated with whatever point is at.

If we are in...
- Collection: Return element at point
- Attachment: Return this attachment's element
- Queue: Return element at point in queue."
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
      ('collection
       (org-ilm--element-at-point))
      ('attachment
       (org-ilm--element-by-id (nth 1 location)))
      ('queue
       (org-ilm--element-by-id (nth 2 location))))))

(defun org-ilm-element--entry (element)
  "Return `org-mem-entry' of ELEMENT."
  (org-mem-entry-by-id (org-ilm-element--id element)))

(defmacro org-ilm--element-with-point-at (element &rest body)
  "Evaluate BODY with point at ELEMENT's headline."
  (declare (debug (body)) (indent 1))
  `(org-ilm--org-with-point-at (org-ilm-element--id ,element)
     ,@body))

(defmacro org-ilm--element-with-point-at-registry (element &rest body)
  "Evaluate BODY with point at ELEMENT's registry headline."
  (declare (debug (body)) (indent 1))
  `(org-ilm--org-with-point-at (org-ilm-element--registry ,element)
     ,@body))

(defun org-ilm--element-id (element-or-id)
  "Returns id if element, else return it."
  (if (org-ilm-element-p element-or-id)
      (org-ilm-element--id element-or-id)
    element-or-id))

;;;; Collection

(defun org-ilm-element--collection-move (element new-collection &optional new-priority)
  "Moves ELEMENT to COLLECTION, disregarding child elements.
To safely include the children in the migration, call
`org-ilm-element-set-collection' instead."
  (org-ilm--element-with-point-at element
    (let* ((id                  (org-ilm-element--id element))
           (done-p              (org-ilm-element--done element))
           (current-collection  (org-ilm-element--collection element))
           (already-there-p     (eq new-collection current-collection))
           (current-priority    (org-ilm-element--priority element))
           (property-collection (org-entry-get nil org-ilm-property-collection)))

      ;; TODO Remove property if already there, but recover on error
      
      (unless already-there-p

        ;; First attempt to remove it from the priority queue. This will do
        ;; nothing if the element does not exist in that queue. We try this
        ;; regardless if the element is "done" or not, in case the element is
        ;; done but somehow hasn't been removed from the queue before.
        (org-ilm-queue--remove (org-ilm-element--pqueue element) id)

        ;; Remove the property to match the current state.
        (org-entry-delete nil org-ilm-property-collection)

        ;; Now attempt to move to new priority queue.
        (condition-case err
            (let* ((pqueue   (org-ilm--pqueue new-collection))
                   (exists-p (org-ilm-queue--contains-p pqueue id)))

              (if done-p
                  ;; If for some reason the element is done AND it exists in the
                  ;; new collection's priority queue, make sure to remove it.
                  (when exists-p (org-ilm-queue--remove pqueue id))

                ;; Add to new priority queue
                (unless new-priority 
                  (setq new-priority (org-ilm--pqueue-select-read
                                      ;; Initial position will match the quantile of
                                      ;; previous pqueue
                                      (cdr current-priority)
                                      new-collection)))
                (if exists-p
                    (org-ilm-queue--move pqueue id new-priority)
                  (org-ilm-queue--insert pqueue id new-priority)))
              
              ;; If inherited collection property matches new, no need to set
              ;; the property.
              (let ((inherited (org-entry-get nil org-ilm-property-collection 'inherit)))
                (unless (and inherited (eq (intern inherited) new-collection))
                  (org-entry-put nil org-ilm-property-collection (symbol-name new-collection))))
              
              (save-buffer) ;; For org-mem to update
              
              ;; Update element struct inplace
              (setf (org-ilm-element--collection element) new-collection))
          (error
           ;; Need to undo removal from original priority queue, unless the
           ;; element is marked as done, in which case we leave it. An element
           ;; being done and still present in the pqueue is a bug but may still
           ;; happen.
           (when (and current-priority (not done-p))
             (org-ilm-queue--insert
              (org-ilm--pqueue current-collection) id current-priority))
           ;; Put back property if it was there when we found it.
           (when property-collection
             (org-entry-put nil org-ilm-property-collection property-collection))
           ;; Collection slot of element struct
           (setf (org-ilm-element--collection element) current-collection)

           ;; Throw an error again - this catch was only to do cleanup.
           (error "Error when inserting element into priority queue: %s" err))
          )))))

;;;; Querying

(cl-defun org-ilm--element-query-children (element &key return-type include-self all-descendants more-query any-collection)
  "Return child elements of element in order."
  (unless return-type (setq return-type 'element))
  (cl-assert (member return-type '(entry element headline id)))
  (cl-assert (org-ilm-element-p element))
  (let* ((id (org-ilm-element--id element))
         (collection (unless any-collection
                       (symbol-name (org-ilm-element--collection element))))
         (marker (org-id-find id 'marker))
         (parent (if all-descendants 'ancestors 'parent)))
    (unless marker (error "No entry with ID %s" id))
    (org-ql-select (list (marker-buffer marker))
      `(and
        ,@(if include-self
              `((or (property "ID" ,id) (,parent (property "ID" ,id))))
            `((,parent (property "ID" ,id))))
        (ilm-element ,collection)
        ,@more-query)
      :narrow t
      :action
      (pcase (or return-type 'headline)
        ('headline) ;; default: org headline element
        ('id #'org-id-get)
        ('entry #'org-node-at-point)
        ('element #'org-ilm--element-at-point)
        (_ (error "Unrecognized return type"))))))

;;;; Priority

(defun org-ilm-element--pqueue (element)
  (org-ilm--pqueue (org-ilm-element--collection element)))

(cl-defun org-ilm-element--priority (element &key position collection)
  "Return (rank . quantile) of ELEMENT in its priority queue.
With POSITION given, set the new position in the queue.
ELEMENT may be nil, in which case try to read it from point.
COLLECTION specifies in which queue to look at."
  (unless element (setq element (org-ilm--element-from-context)))
  (cl-assert (org-ilm-element-p element))
  (unless collection
    (setq collection (org-ilm-element--collection element)))
  (org-ilm-pqueue--priority
   (org-ilm--pqueue collection)
   (org-ilm-element--id element)
   position
   'nil-if-absent))

(defun org-ilm-element--prank (element)
  "Position of ELEMENT in the priority queue."
  (car (org-ilm-element--priority element)))

(defun org-ilm-element--pquantile (element)
  "Quantile of ELEMENT in the priority queue."
  (cdr (org-ilm-element--priority element)))

(defun org-ilm-element--priority-formatted (element)
  (org-ilm-queue--position-format
   (org-ilm-element--pqueue element)
   (org-ilm-element--priority element)))

;;;; Actions

(cl-defgeneric org-ilm--element-first-interval (type collection priority &rest args)
  (let* ((pqueue (org-ilm--pqueue collection))
         (priority (ost-tree-position pqueue priority))) 
    (org-ilm--initial-schedule-from-priority (cdr priority))))

(cl-defgeneric org-ilm--element-format-scheduled (type scheduled)
  (org-ilm--ts-format-utc-date scheduled))

(cl-defgeneric org-ilm--element-prepare-new (type collection priority due &rest args)
  (org-ilm--log-log type collection priority due :new))

(cl-defgeneric org-ilm--element-review (type element duration &rest args))

(defun org-ilm-element-set-schedule (element timestamp)
  "Set the schedule of an ilm element."
  (interactive
   (list (org-ilm--element-from-context)
         (org-read-date 'with-time nil nil "Schedule: ")))
  (org-ilm--element-with-point-at element
    (atomic-change-group
      (org-ilm--log-log (org-ilm-element--type element)
                        (org-ilm-element--collection element)
                        (org-ilm-element--priority element)
                        (ts-parse timestamp)
                        :reschedule)
      (org-ilm--schedule :timestamp timestamp))
    (save-buffer)

    ;; Setting the schedule will log a schedule event, so if we are reviewing,
    ;; dont update priority and schedule.
    (when (org-ilm-reviewing-p (oref element id))
      (oset org-ilm--review update-p nil))))

(defun org-ilm-element-set-priority (element)
  "Set the priority of an ilm element."
  (interactive (list (org-ilm--element-from-context)))
  (org-ilm--element-with-point-at element
    (let ((priority (org-ilm-element--priority element)))
      (if (org-ilm-element--done element)
          (org-ilm-element-undone element)
        (let ((position (org-ilm--pqueue-select-read priority)))
          (org-ilm-element--priority element :position position))))))

(defun org-ilm-element-set-collection (element)
  "Move the element to a different collection that shares the same path."
  (interactive (list (org-ilm--element-from-context)))
  (org-ilm--element-with-point-at element
    (let* ((id (org-ilm-element--id element))
           (current-collection      (org-ilm-element--collection element))
           (selected-collection     (org-ilm--select-collection buffer-file-name))
           (descendancy-elements    (org-ilm--element-query-children
                                     element :include-self t
                                     :all-descendants t :any-collection t))
           (descendancy-size        (length descendancy-elements))
           (descendancy-collections (cl-remove-duplicates
                                     (mapcar #'org-ilm-element--collection
                                             descendancy-elements))))
      (cond
       ((and (= 1 (length descendancy-collections))
             (eq (car descendancy-collections) selected-collection))
        (user-error "Element(s) already in collection %s" selected-collection))
       ;; No child elements 
       ((= 1 descendancy-size)
        (org-ilm-element--collection-move element selected-collection))
       ;; Has child elements, move them all 
       (t
        (let ((pqueue (org-ilm--pqueue selected-collection))
              priorities)

          (if (<= (ost-size pqueue) descendancy-size)
              ;; TODO Ideally the queue select thing should allow for selecting
              ;; in a queue that will is about to have additional elements.
              (setq priorities (number-sequence 0 (1- descendancy-size)))
            (let* ((queue          (org-ilm-pqueue--bqueue pqueue))
                   (priority-start (org-ilm--bqueue-select-read queue nil "Start priority: "))
                   (priority-end   (org-ilm--bqueue-select-read queue nil "End priority: ")))
              (setq priorities (org-ilm--spread descendancy-size
                                                (car priority-start)
                                                (car priority-end)
                                                'random-p))))
          (dotimes (i descendancy-size)
            (org-ilm-element--collection-move
             (nth i descendancy-elements)
             selected-collection
             (nth i priorities)))))))))

(defun org-ilm-element-done (element &optional duration timestamp)
  "Apply done on ilm element at point."
  (interactive (list (org-ilm--element-from-context)))
  (cl-assert (org-ilm-element-p element) nil "Not an org-ilm element")

  (org-ilm--element-with-point-at element
    (when (or (not (called-interactively-p))
              (yes-or-no-p "Apply done on element?"))
      (atomic-change-group
        ;; Log done action in drawer
        (org-ilm--log-log
         (org-ilm-element--type element)
         (org-ilm-element--collection element)
         (org-ilm-element--priority element)
         nil ;; due
         :done ;; action
         :duration duration
         :timestamp timestamp)

        ;; Remove scheduling
        (org-schedule '(4))
        
        ;; Set todo state to DONE
        (org-todo "DONE")
        
        ;; Remove from priority queue
        (org-ilm--pqueue-remove (org-ilm-element--id element)
                                (org-ilm-element--collection element))

        ;; TODO Go through all queues and remove from their ost

        (save-buffer)

        ;; When this is the element being reviewed, move on to review the next
        ;; element in the queue.
        (when (org-ilm-reviewing-p (oref element id))
          (org-ilm--review-next 'dont-update))))))

(defun org-ilm-element-undone (element)
  "Undo done on ilm element at point."
  (interactive (list (org-ilm--element-from-context)))
  (cl-assert (org-ilm-element-p element) nil "Not an org-ilm element")
  (org-ilm--element-with-point-at element
    (when (yes-or-no-p "Undo done on element?")
      (let* ((review-log (org-ilm--log-read))
             (last-review (car (last review-log)))
             (priority (org-ilm-log-review--priority last-review))
             (collection (org-ilm-element--collection element))
             (pqueue (org-ilm--pqueue collection))
             (priority (org-ilm--queue-select-read
                        (org-ilm-pqueue--bqueue pqueue)
                        (when priority (/ (car priority) (float (cdr priority))))
                        nil 1))
             ;; TODO Handle this being nil
             (due (org-ilm-log-review--due (seq-find
                                            #'org-ilm-log-review--due
                                            (reverse review-log)))))
        (atomic-change-group
          ;; TODO For card make sure it has the fsrs properties
          (org-ilm--log-log (org-ilm-element--type element) collection priority due :undone)
          (org-ilm--schedule :timestamp due)
          (org-todo 'none)
          (org-ilm-queue--insert pqueue priority element))))))

(defun org-ilm-element-delete (element &optional warn-attach)
  "Delete ilm element at point."
  (interactive (list (org-ilm--element-from-context)))
  (cl-assert (org-ilm-element-p element) nil "Not an ilm element")

  (with-slots (id type collection) element
    (org-ilm--element-with-point-at element
      (when (called-interactively-p)
        (org-mark-subtree)
        (unless (yes-or-no-p "Delete element?")
          (pop-mark)
          (user-error "Abort deletion")))
      
      (when (and (org-attach-dir) (or (called-interactively-p) warn-attach))
        (cond-let*
          ((org-entry-get nil "DIR")
           (when (yes-or-no-p "Delete ENTIRE attachment directory?")
             (org-attach-delete-all 'force)))
          ([attachments
            (apply #'append
                   (mapcar
                    (lambda (headline)
                      (f-glob
                       (file-name-concat
                        (org-attach-dir)
                        (concat (org-element-property :ID headline) "*"))))
                    (org-ilm--element-query-children
                     element :return-type 'headline :include-self t :all-descendants t)))]
           (when (yes-or-no-p (format "Delete element %s attachtment(s)?" (length attachments)))
             (dolist (file attachments)
               (delete-file file))))))
      
      (org-ilm--org-delete-headline)
      (org-ilm--pqueue-remove id collection)

      (run-hook-with-args
       'org-ilm-element-delete-hook
       id type collection))))

;;;; Transient

(defun org-ilm--element-transient-update-scope ()
  "Update the transient scope and run element update hook."
  (org-ilm--org-mem-ensure)
  (let ((element (org-ilm--element-from-context)))
    (org-ilm--transient-set-scope element)
    (run-hook-with-args 'org-ilm-element-update-hook element)))

(transient-define-suffix org-ilm--element-transient-schedule ()
  :key "s"
  :description
  (lambda ()
    (concat
     "Schedule "
     (if (oref (transient-scope) done)
         (propertize "(done)" 'face 'Info-quoted)
       (when-let* ((element (transient-scope))
                   (sched (oref element sched)))
         (propertize (ts-format "%Y-%m-%d" sched) 'face 'transient-value)))))
  :transient 'transient--do-call
  :inapt-if (lambda () (oref (transient-scope) done))
  (interactive)
  (call-interactively #'org-ilm-element-set-schedule)
  (org-ilm--element-transient-update-scope))

(transient-define-suffix org-ilm--element-transient-priority ()
  :key "p"
  :description
  (lambda ()
    (concat
     "Priority "
     (if (org-ilm-element--done (transient-scope))
         (propertize "(done)" 'face 'Info-quoted)
       (when-let* ((element (transient-scope))
                   (priority (org-ilm-element--priority-formatted element)))
         (propertize priority 'face 'transient-value)))))
  :transient 'transient--do-call
  :face (lambda ()
          (when (org-ilm-element--done (transient-scope))
            'Info-quoted))
  ;; :inapt-if (lambda () (org-ilm-element--done (transient-scope)))
  (interactive)
  (call-interactively #'org-ilm-element-set-priority)
  (org-ilm--element-transient-update-scope))

(transient-define-suffix org-ilm--element-transient-collection ()
  :key "c"
  :transient 'transient--do-call
  :description
  (lambda ()
    (concat
     "Collection "
     (propertize 
      (symbol-name (org-ilm-element--collection (transient-scope)))
      'face 'transient-value)))
  (interactive)
  (funcall-interactively #'org-ilm-element-set-collection
                         (transient-scope)))

(transient-define-suffix org-ilm--element-transient-concepts ()
  :key "n"
  :transient 'transient--do-stay
  :description
  (lambda ()
    (concat
     "Concepts "
     (unless (oref (transient-scope) concepts)
       (propertize "nil" 'face 'transient-inactive-value))))
  
  (interactive)
  (org-ilm--concept-set
   (oref (transient-scope) id)
   (lambda ()
     (org-ilm--element-transient-update-scope)
     (unless (eq transient-current-command 'org-ilm--element-transient)
       (org-ilm--element-transient (org-ilm--element-from-context))))))

(transient-define-prefix org-ilm--element-transient (element)
  :refresh-suffixes t
  
  [:description
   (lambda ()
     (capitalize (symbol-name (oref (transient-scope) type))))
   (:info*
    (lambda ()
      (propertize (oref (transient-scope) title) 'face 'italic)))
   ]
  
  [
   :setup-children
   (lambda (children)
     (append
      children
      (pcase-let ((`(,concepts . ,n-direct) (oref (transient-scope) concepts))) 
        (seq-map
         (lambda (concept-id)
           (let* ((entry (org-mem-entry-by-id concept-id))
                  (title (org-ilm--org-mem-title-full entry))
                  (keymap (make-sparse-keymap)))
             ;; TODO Clicking doesnt work
             (define-key keymap [mouse-1]
                         (lambda ()
                           (interactive)
                           (org-ilm--org-goto-id concept-id)))
             (transient-parse-suffix
              'org-ilm--element-transient
              (list :info
                    (propertize title 'face 'transient-value
                                'mouse-face 'highlight
                                'keymap keymap)))))
         (last concepts n-direct)))))
   (org-ilm--element-transient-schedule)
   (org-ilm--element-transient-priority)
   (org-ilm--element-transient-collection)
   (org-ilm--element-transient-concepts)
   ]

  [
   ("a" "Attachment..." org-ilm--element-attachment-transient :transient t)
   ("q" "Queue..." org-ilm--element-queue-transient :transient t)
   ("m" "Media..." org-ilm--element-media-transient :transient t
    :if (lambda () (oref (transient-scope) media)))
   ("v" "Convert..." org-ilm-convert-menu :transient t)
   ]
   
  ["Actions"
   [
    ("o" "Open"
     (lambda ()
       (interactive)
       (org-ilm--org-goto-id (oref (transient-scope) id))))
    ("d" "Done"
     (lambda ()
       (interactive)
       (funcall-interactively #'org-ilm-element-done (transient-scope)))
     :if-not (lambda () (oref (transient-scope) done)))
    ("d" "Undone"
     (lambda ()
       (interactive)
       (funcall-interactively #'org-ilm-element-undone (transient-scope)))
     :if (lambda () (oref (transient-scope) done)))
    ("k" "Delete"
     (lambda ()
       (interactive)
       (funcall-interactively #'org-ilm-element-delete (transient-scope))))
    ]
   [
    ("C" "New card"
     (lambda ()
       (interactive)
       (org-ilm--import-org 'card 'as-capture)))
    ("M" "New material"
     (lambda ()
       (interactive)
       (org-ilm--import-org 'material 'as-capture)))
    ]
   ]

  (interactive "P")
  (transient-setup 'org-ilm--element-transient nil nil :scope element))

(defun org-ilm-element-menu (&optional arg id)
  "Open menu to apply an action on an element."
  (interactive "P")
  (cond-let*
    ([element (if id (org-ilm--element-by-id id) (org-ilm--element-from-context))]
     (if (member (oref element type) '(card material))
         (org-ilm--element-transient element)
       (user-error "Element actions not available for type %s" (oref element type))))
    ([entry (and (eq major-mode 'org-mode)
                 (if id (org-mem-entry-by-id id) (org-mem-entry-at-point)))]
     [_ (eq 'concept (org-ilm--element-type entry))]
     ;; TODO This only works on concept at point
     (org-ilm-concept-menu))
    (t
     (user-error "Ilm element not found"))))

;;;;; Attachment

(transient-define-prefix org-ilm--element-attachment-transient ()
  :refresh-suffixes t

  ["Attachment"
   ("o" "Open"
    (lambda ()
      (interactive)
      (let ((element (transient-scope)))
        (make-thread
         (lambda ()
           (org-ilm--element-with-point-at element
             (org-ilm-open-attachment))))))
    :transient transient--do-exit
    )
   ("d" "Dired"
    (lambda ()
      (interactive)
      (let ((element (transient-scope))
            attach-dir)
        (org-ilm--element-with-point-at element
          (setq attach-dir (org-attach-dir)))
        (unless attach-dir (user-error "No attachment directory"))
        (find-name-dired attach-dir (concat (oref element id) "*"))))
    :transient transient--do-exit)
   ("s" "Set"
    (lambda ()
      (interactive)
      (let* ((attach-dir (org-attach-dir))
             (attachments (org-attach-file-list attach-dir))
             (attachment (completing-read "Attachment: " attachments)))
        (cond
         ((member attachment attachments)
          (rename-file (expand-file-name attachment attach-dir)
                       (expand-file-name
                        (concat (org-id-get-create) "." (file-name-extension attachment))
                        attach-dir)))
         ((org-url-p attachment)
          (let ((f (expand-file-name (concat (org-id-get-create) ".org") attach-dir)))
            (with-temp-file f
              (insert (org-ilm--get-website-as-org attachment)))
            (find-file f)))
         ((string-empty-p attachment)
          (find-file (expand-file-name
                      (concat (org-id-get-create) ".org")
                      attach-dir)))
         (t
          (user-error "Select attachment, pass in URL, or leave empty to create new")
          )))))
   ]

  (interactive)
  (transient-setup 'org-ilm--element-attachment-transient nil nil :scope (transient-scope)))

;;;;; Media

(transient-define-prefix org-ilm--element-media-transient ()
  :refresh-suffixes t
  
  ["Media"
   (:info*
    (lambda ()
      (let* ((entry (org-ilm-element--entry (transient-scope)))
             (parts (org-ilm--media-compile entry)))
        (concat 
         (propertize (car parts) 'face 'Info-quoted)
         (when (or (nth 1 parts) (nth 1 parts))
           (format " [%s-%s]" (nth 1 parts) (nth 2 parts)))))))
   ("o" "Open"
    (lambda ()
      (interactive)
      (org-ilm--media-open (org-ilm-element--entry (transient-scope))))
    :transient transient--do-exit)
   ("r" "Range"
    (lambda ()
      (interactive)
      (let* ((element (transient-scope))
             (entry (org-ilm-element--entry element))
             (parts (org-ilm--media-compile entry))
             (range (read-string "Range: "
                                 (when (stringp (nth 1 parts))
                                   (if (stringp (nth 2 parts))
                                       (string-join (cdr parts) "-")
                                     (nth 1 parts)))))
             (source (car parts)))
        (org-ilm--org-with-point-at (oref element id)
          (org-entry-put nil org-ilm-property-media+ range)
          (save-buffer)
          (org-ilm--element-transient-update-scope))))
    :transient transient--do-call)
   ("s" "Set"
    (lambda ()
      (interactive)
      (let* ((element (transient-scope))
             (id (org-ilm-element--id element))
             (entry (org-mem-entry-by-id id))
             (registry-entry (org-mem-entry-by-id (org-ilm-element--registry element)))
             (element-medias (org-ilm--entry-media-sources entry))
             (registry-medias (org-ilm--entry-media-sources registry-entry))
             (choice (consult--multi
                      (list
                       (list
                        :name "Element"
                        :narrow ?e
                        :items element-medias
                        :action #'message)
                       (list
                        :name "Registry"
                        :narrow ?r
                        :items registry-medias
                        :action #'message))
                      :require-match t
                      :prompt "Media: "))
             (media (car choice)))
        (org-ilm--org-with-point-at id
          (org-entry-put nil org-ilm-property-media media)
          (save-buffer))))
    :transient transient--do-call)
   ]

  (interactive)
  (transient-setup 'org-ilm--element-media-transient nil nil :scope (transient-scope)))


;;;;; Queue

(defun org-ilm--element-mark-in-queue (element target &optional queue-buffer)
  (cl-assert (member target '(self children all))) 
  (let* ((ids (if (eq target 'self)
                  (list (oref element id))
                (org-ilm--element-query-children
                 element
                 :return-type 'id
                 :include-self (eq target 'all)
                 :all-descendants t)))
         (queue-buffer (or queue-buffer (org-ilm--bqueue-completing-read nil t))))
    (with-current-buffer queue-buffer
      (org-ilm--bqueue-mark-objects ids)
      (org-ilm-queue-revert))))

(transient-define-prefix org-ilm--element-queue-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    `((target . all)
      (buffer . ,(or org-ilm-bqueue-active-buffer (org-ilm--bqueue-buffer-current)))))
  
  ["Queue actions"
   ("t" "Target"
    :cons 'target
    :class org-ilm-transient-cons-switches
    :choices (all children self))
   ("b" "Buffer"
    :cons 'buffer
    :class org-ilm-transient-cons-option
    :always-read t
    :reader
    (lambda (&rest _)
      (let ((buf (org-ilm--bqueue-completing-read (oref (transient-scope) title))))
        buf)))
   (:info (lambda () ""))
   ("a" "Add to queue"
    (lambda ()
      (interactive)
      (map-let (target buffer) (org-ilm--transient-parse)
        (unless buffer
          (let ((obj (org-ilm--transient-suffix-by-slot 'key "b")))
            (setq buffer (transient-infix-read obj))
            (transient-infix-set obj buffer)))
        (org-ilm-queue-add-dwim nil target buffer)))
    :transient t)
   ("m" "Mark in queue"
    (lambda ()
      (interactive)
      (map-let (target buffer) (org-ilm--transient-parse)
        (org-ilm--element-mark-in-queue
         (transient-scope) target buffer)))
    :inapt-if-not (lambda () (alist-get 'buffer (org-ilm--transient-parse)))
    :transient t)
   ]
  
  (interactive)
  (transient-setup 'org-ilm--element-queue-transient nil nil :scope (transient-scope)))


;;; Footer

(provide 'org-ilm-element)

;;; org-ilm-element.el ends here
