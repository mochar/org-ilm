;;; org-ilm-concept.el --- Concepts -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'cond-let)
(require 'org-capture)
(require 'org-mem)
(require 'org-node)
(require 'transient)

(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-query)
;; (require 'org-ilm-element)
;; (require 'org-ilm-collection)

;;;; Variables

(defvar-keymap org-ilm-concept-map
  :doc "Keymap for concept actions."
  "a" #'org-ilm-concept-add
  "r" #'org-ilm-concept-remove
  "." #'org-ilm-concept-into
  "n" #'org-ilm-concept-new)

;;;; Functions

(defun org-ilm--concept-select-entry (&optional collection prompt blank-ok predicate)
  "Select org-mem concept entries."
  (setq collection
        (or collection
            (org-ilm--collection-from-context)
            (org-ilm--active-collection)))
  (let* (;; See BLANK-OK in `org-node-read-candidate'
         (org-node-blank-input-hint (propertize "(new concept)" 'face 'completions-annotations))
         (choice
          (org-node-read-candidate
           (or prompt "Concept: ") blank-ok
           (lambda (name entry)
             (or
              (and blank-ok (string= name ""))
              (when-let ((type (org-ilm--element-type entry))
                         (file (org-mem-entry-file entry)))
                (and
                 (eq 'concept type)
                 (org-ilm--collection-file file collection)
                 (if predicate (funcall predicate entry) t)))))
           'require-match)))
    (gethash choice org-node--candidate<>entry)))

(defun org-ilm--concept-parse-property (&optional string inherit)
  "Return (id . title) of concepts in STRING or property of heading at point."
  (unless string (setq string (org-entry-get nil "CONCEPTS" inherit)))
  (when string
    (let ((link-match-pos 0)
          concepts)
      (while-let ((_ (string-match org-link-any-re string link-match-pos))
                  (concept (cons (substring (match-string 2 string) 3)
                                 (match-string 3 string))))
        (push concept concepts)
        (setq link-match-pos (match-end 1)))
      concepts)))

(defun org-ilm--concept-property-prepare (concepts)
  "Prepares the concept property value referencing CONCEPTS.
CONCEPTS is a list or a singular value. If value(s) is string, should be
the org-id. If cons, should be (org-id . title). Otherwise should be
org-mem-entry object."
  (mapconcat
   (lambda (concept)
     (cond
      ((stringp concept)
       (format
        "[[id:%s][%s]]"
        concept
        (or (-some-> (org-mem-entry-by-id concept)
              org-ilm--org-mem-title-full)
            " ")))
      ((consp concept)
       (format "[[id:%s][%s]]" (car concept) (cdr concept)))
      ((org-mem-entry-p concept)
       (format "[[id:%s][%s]]"
               (org-mem-entry-id concept)
               (org-ilm--org-mem-title-full concept)))
      (t
       (error "Invalid parent concept value %s" concept))))
   (ensure-list concepts) " "))

(cl-defun org-ilm--concept-new (collection title &key id target parent-id parent-concepts mem-ensure)
  "Create a new concept in COLLECTION with TITLE.

TARGET is the org-capture target of the concept, which defaults to the
value set in `org-ilm-collections'.

PARENT-ID is the org-id of the outline parent concept. If given, this
overwrites TARGET.

PARENT-CONCEPTS are the concepts to reference in the concepts property,
for which see `org-ilm--concept-property-prepare'.

With MEM-ENSURE non-nil, update org-mem cache synchronously to ensure it
is available after capture."
  (let ((target (or target
                    (when parent-id (list 'id parent-id))
                    (org-ilm--collection-property collection :concept)))
        (id (or id (org-id-new))))
    (org-ilm--org-capture-programmatic
     target
     (format
      "%s
:PROPERTIES:
:ID: %s
:ILM_TYPE: concept
%s:END:"
      title id
      (if (null parent-concepts)
          ""
        (concat
         ":CONCEPTS+: "
         (org-ilm--concept-property-prepare parent-concepts)
         "\n")))
     'entry)
    (when mem-ensure (org-ilm--org-mem-ensure))))

(cl-defun org-ilm--concept-edit-selection (&key collection this parent selection)
  "Add or remove concepts to or from a SELECTION of an existing or new concept.
Returns the new selection.

For an existing concept (THIS), SELECTION are the concepts in its
property, and must therefore not be passed. For new concepts, SELECTION is interactive, and must thus be passed explicitely.

If a new concept is placed as an outline child of another concept,
PARENT should be specified to determine what concepts will be inherited."
  ;; Get the concept in the property of THIS, if specified
  (when this
    (when selection
      (error "SELECTION is parsed from property of THIS so must not be specified"))
    (setq selection (org-ilm--org-with-point-at this
                      (mapcar #'car (org-ilm--concept-parse-property)))))

  ;; THIS or PARENT determine what concepts are inherited (i.e. concepts in
  ;; outline and their properties), so its useful to have a name for both of
  ;; them (reference).
  (let ((reference (or this parent)))

    ;; Get collection from reference if not specified explicitely
    (unless collection
      (unless reference
        (error "COLLECTION must be specified if THIS or PARENT missing"))
      (setq collection (car (org-ilm--collection-file
                             (oref (org-mem-entry-by-id reference)
                                   file-truename))))
      (unless collection
        (error "Could not find COLLECTION of %s" reference)))
    
    (let* ((inherited (when (or reference selection)
                        (seq-difference
                         (apply #'seq-concatenate 'list
                                (mapcar 
                                 (lambda (id)
                                   (car (org-ilm--concept-cache-gather id)))
                                 (cons reference selection)))
                         selection)))
           (choice (consult--multi
                    (list
                     ;; The concepts already in the selection.
                     (list
                      :name "Remove"
                      :narrow ?r
                      :items
                      (mapcar
                       (lambda (id)
                         (let ((entry (org-mem-entry-by-id id)))
                           (propertize
                            (org-ilm--org-mem-title-full entry)
                            :entry entry :selected t)))
                       selection))
                     ;; Inherited concepts, hidden by default.
                     (list
                      :name "Inherited"
                      :narrow ?i
                      :face '(:inherit Info-quoted :strike-through t)
                      :hidden t
                      :items
                      (mapcar
                       (lambda (id)
                         (let ((entry (org-mem-entry-by-id id)))
                           (propertize
                            (org-ilm--org-mem-title-full entry)
                            :entry entry :inherited t)))
                       inherited))
                     ;; The concepts that can be added to the selection must not
                     ;; be in the selection or inherited concepts, cannot be THIS
                     ;; or PARENT, and cannot be children of THIS.
                     (list
                      :name "Add"
                      :narrow ?a
                      :items
                      (seq-keep
                       (lambda (entry)
                         (let ((id (oref entry id)))
                           (when (and (eq 'concept (org-ilm--element-type entry))
                                      (not (member id selection))
                                      (not (member id inherited))
                                      (if reference
                                          (and
                                           (not (string= reference id))
                                           (not (member reference (org-ilm--org-mem-ancestry-ids entry))))
                                        t))
                             (propertize
                              (org-ilm--org-mem-title-full entry)
                              :entry entry))))
                       (org-mem-entries-in-files
                        (org-ilm--collection-files collection)))))
                    :require-match t
                    :prompt "Concept: "))
           (concept (get-text-property 0 :entry (car choice))))
      (cond
       ((get-text-property 0 :inherited (car choice))
        ;; I think we should allow to add inherited if the user wants to, there
        ;; might be a reason for it.
        ;; (message "Inherited concepts cannot be added explicitly")
        ;; selection
        (cons (oref concept id) selection))
       ((get-text-property 0 :selected (car choice))
        (seq-filter
         (lambda (e)
           (not (string= e (oref concept id))))
         selection))
       (t
        (cons (oref concept id) selection))))))


;;;; Commands

(defun org-ilm-concept-dwim ()
  "Add concept if point in ilm element, otherwise create a new concept."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((at-heading-p (org-at-heading-p))
             (ilm-type (org-ilm--element-type)))
        (cond
         (ilm-type
          (call-interactively #'org-ilm-concept-add))
         ((and at-heading-p (null ilm-type))
          (call-interactively #'org-ilm-concept-into))
         (t (call-interactively #'org-ilm-concept-new))))
    (call-interactively #'org-ilm-concept-new)))

(defun org-ilm-concept-into ()
  "Convert headline at point to a concept."
  (interactive)
  (cond
   ((not (and (eq major-mode 'org-mode) (org-at-heading-p)))
    (user-error "Point not on a headline!"))
   ((org-get-todo-state)
    (user-error "Headline already has a TODO state!"))
   (t
    (org-entry-put nil org-ilm-property-type "concept")
    (org-id-get-create)
    (save-buffer))))

(defun org-ilm-concept-remove ()
  "Remove concept from CONCEPTS property for element at point."
  (interactive)
  (unless (org-ilm--element-type)
    (user-error "Not on an element heading"))
  (let* ((concepts-str (org-entry-get nil "CONCEPTS"))
         (concepts (org-ilm--concept-parse-property concepts-str))
         (entries (seq-keep
                   (lambda (c)
                     (when-let* ((entry (org-mem-entry-by-id (car c)))
                                 (title (org-ilm--org-mem-title-full entry)))
                       (cons title entry)))
                   concepts))
         (entry-title (completing-read "Remove concept: " entries nil t)))
    (setf (alist-get entry-title entries nil 'remove #'string=) nil)
    (setq concepts-str (string-join (mapcar (lambda (entry)
                                              (format "[[id:%s][%s]]"
                                                      (org-mem-entry-id (cdr entry))
                                                      (org-mem-entry-title (cdr entry))))
                                            entries)
                                    " "))
    (if (string-empty-p concepts-str)
        (org-entry-delete nil "CONCEPTS+")
      (org-entry-put nil "CONCEPTS+" concepts-str))
    (save-buffer)))

(defun org-ilm-concept-add (&optional concept-entry)
  "Add concept to CONCEPTS property for element at point."
  (interactive)
  (let* ((org-id (org-id-get))
         (concepts-str (org-entry-get nil "CONCEPTS"))
         (cur-concepts (mapcar #'car (org-ilm--concept-parse-property nil 'inherit)))
         (concept-entry
          (or concept-entry
              (org-ilm--concept-select-entry
               nil "Add concept: " t
               (lambda (entry) ;; predicate
                 (not (member (org-mem-entry-id entry) cur-concepts))))))
         concept-id)

    (if (null concept-entry)
        (org-ilm-concept-new
         nil
         :on-success
         (lambda (id &rest _)
           (org-ilm--org-with-point-at org-id
             (org-ilm-concept-add (org-mem-entry-by-id id)))))
      (setq concept-id (org-mem-entry-id concept-entry))
      (cond
       ((string= (org-id-get) concept-id)
        (user-error "Cannot add concept to itself"))
       ((member concept-id cur-concepts)
        ;; TODO offer option to remove
        (user-error "Concept already added"))
       (t
        (let* ((concept-desc (org-ilm--org-mem-title-full concept-entry))
               (concept-link (org-link-make-string
                              (concat "id:" concept-id) concept-desc)))
          (org-entry-put nil "CONCEPTS+"
                         (concat concepts-str " " concept-link))
          (save-buffer)))))))

;;;; Transient

(defun org-ilm-concepts ()
  (interactive)
  (org-ilm--concept-transient))

(defun org-ilm--concept-transient-data (id)
  "Annotate for each concept of element with ID whether it is a direct,
outline, and/or property concept."
  (let ((data (list :id id)))
    (when-let* ((concept-data (org-ilm--concept-cache-gather id)))
      (let* ((concept-ids (copy-sequence (car concept-data)))
             (n-direct (cdr concept-data))
             prop-ids parent-ids)

        (org-ilm--org-with-point-at id
          (setq prop-ids (mapcar #'car (org-ilm--concept-parse-property)))
          (save-excursion
            (while (org-up-heading-safe)
              (push (org-id-get) parent-ids))))
        
        (seq-do-indexed
         (lambda (concept-id i)
           (push (list :direct (and n-direct (< i n-direct))
                       :outline (member concept-id parent-ids)
                       :property (member concept-id prop-ids)
                       :id concept-id
                       :title (when-let ((entry (org-mem-entry-by-id concept-id)))
                                (org-ilm--org-mem-title-full entry)))
                 (plist-get data :concepts)))
         (reverse concept-ids))))
    data))

(defun org-ilm--concept-transient-rebuild-scope ()
  (let ((data (org-ilm--concept-transient-data (plist-get (transient-scope) :id))))
    (org-ilm--transient-set-scope data)))

(transient-define-prefix org-ilm--concept-transient ()
  :refresh-suffixes t

  ["Concepts"
   :setup-children
   (lambda (_)
     (seq-map
      (lambda (concept)
        (pcase-let (((map :outline :direct :property :title :id) concept)
                    (keymap (make-sparse-keymap)))
          ;; TODO Clicking doesnt work
          (define-key keymap [mouse-1]
                      (lambda ()
                        (interactive)
                        (org-ilm--org-goto-id id)))
          (transient-parse-suffix
           'org-ilm--concept-transient
           (list :info*
                 (concat 
                  (propertize "P" 'face (if property 'error 'Info-quoted))
                  (propertize "O" 'face (if outline 'error 'Info-quoted))
                  (propertize "D" 'face (if direct 'error 'Info-quoted))
                  " "
                  (propertize title 'mouse-face 'highlight 'keymap keymap))))))
      (plist-get (transient-scope) :concepts)))
   ]

  [
   ("a" "Add"
    (lambda ()
      (interactive)
      ;; TODO This leads to error when creating a new concept from within
      ;; (org-ilm-concepts-add), happens when capture buffer opens.
      (let ((id (plist-get (transient-scope) :id)))
        (org-ilm--org-with-point-at id
          (org-ilm-concept-add)
          (org-ilm--org-mem-ensure)))
      (org-ilm--concept-transient-rebuild-scope))
    :transient t
    )
   ("r" "Remove"
    (lambda ()
      (interactive)
      (let ((id (plist-get (transient-scope) :id)))
        (org-ilm--org-with-point-at id
          (org-ilm-concept-remove)
          (org-ilm--org-mem-ensure)))
      (org-ilm--concept-transient-rebuild-scope))
    :transient t
    )
   ]
  
  (interactive)
  (transient-setup
   'org-ilm--concept-transient nil nil
   :scope (org-ilm--concept-transient-data
           (or (-some-> (transient-scope) (oref id)) (org-id-get)))))
  
;;;; Parsing

;; TODO Probably still incredibly inefficient as we have to go up the hierarchy
;; everytime. A better option might be to use org-ql to find all headlines with
;; a CONCEPTS property and store mapping org-id -> concepts in a cache. Then use
;; org-mem's cached ancestry to do the lookup, without having to go up each
;; time.

(defun org-ilm--concepts-get-parent-concepts (&optional id all-ancestors)
  "Retrieve parent concepts of a headline with ID, either in outline or
through property. When ALL-ANCESTORS, retrieve full ancestry
recursively.

Eeach call of this function (recursive or not) only retrives _direct_
parents, which is defined differently for concepts and extracts/cards:
- Concept: First outline parent + _not_ inherited property links
- Others: First outline parent + property links of itself or inherited from
          _non-concept_ ancestors only"
  (unless id (setq id (org-id-get)))
  (let* ((entry (org-mem-entry-by-id id))
         (ancestry (org-ilm--org-mem-ancestry-ids entry))
         (is-concept (eq (org-ilm--element-type entry) 'concept))
         (property-concepts-str "")
         outline-parent-concept
         concept-ids)

    ;; Check for ancestor concept headline in outline hierarchy. As we explore
    ;; up the hierarchy, store linked concepts of extracts.
    (cl-block nil
      (dolist (ancestor (org-mem-entry-crumbs entry))
        (let* ((ancestor-id (nth 4 ancestor))
               (is-self (string= ancestor-id id))
               (ancestor-entry (org-mem-entry-by-id ancestor-id))
               (ancestor-state (when ancestor-entry
                                 (org-mem-entry-todo-state ancestor-entry)))
               (ancestor-type (when ancestor-entry
                                (org-ilm--element-type ancestor-entry))))
          (cond
           ;; Headline is self or incremental ancestor, store linked concepts
           ((or is-self
                (and (not is-concept) ; Concept never inherit!
                     (or (eq ancestor-type 'material)
                         (string= ancestor-state "DONE"))))
            (when-let ((prop (org-mem-entry-property "CONCEPTS+" ancestor-entry)))
              (setq property-concepts-str
                    (concat property-concepts-str " " prop))))
           
           ;; Headline is concept, store as outline parent concept
           ((eq ancestor-type 'concept)
            (cl-pushnew ancestor-id concept-ids :test #'equal)
            (unless outline-parent-concept
              (setq outline-parent-concept ancestor-id))
            (unless all-ancestors (cl-return)))))))

    ;; Process inherited CONCEPTS property.

    ;; This is tricky because linked concepts may themselves link to other
    ;; concepts, requiring this to be done recursively if we need all
    ;; concepts. Furthermore test for invalid linking, eg to a descendant or
    ;; circular links.
    (let ((link-match-pos 0)
          property-concept-ids property-concept-ancestors)
      (while-let ((_ (string-match org-link-any-re property-concepts-str link-match-pos))
                  (concept-string (match-string 1 property-concepts-str)))
        (setq link-match-pos (match-end 1))

        ;; First we gather valid concept-ids as well as their individual
        ;; ancestries
        (when-let* ((concept-id (org-ilm--org-id-from-string concept-string))
                    ;; Skip if linked to itself
                    (_ (not (string= concept-id id)))
                    ;; Skip if ancestor of headline
                    ;; TODO we do this later again as post-processing stop, so
                    ;; remove?
                    (_ (not (member concept-id ancestry)))
                    ;; Should have org id and therefore cached by org-mem
                    (concept-entry (org-mem-entry-by-id concept-id))
                    ;; Should be concept todo state
                    (_ (eq (org-ilm--element-type concept-entry) 'concept))
                    ;; Skip if is concept is descendant of headline - this
                    ;; will lead to circular DAG, causing infinite loops, and
                    ;; doesn't make sense anyway. Add a root string so
                    ;; concepts with no concept-id parents don't return nil,
                    ;; terminating the when-let.
                    (concept-ancestry (org-ilm--org-mem-ancestry-ids concept-entry 'with-root-str))
                    (_ (not (member id concept-ancestry))))
          (cl-pushnew concept-id property-concept-ids :test #'equal)
          (cl-pushnew concept-ancestry property-concept-ancestors)

          ;; Recursively handle case where property derived concepts
          ;; themselves might link to other concepts in their properties.
          ;; Note, no need to do this for outline ancestors (previous step),
          ;; as properties are inherited.
          (when all-ancestors
            (dolist (parent-id (org-ilm--concepts-get-parent-concepts concept-id all-ancestors))
              (cl-pushnew parent-id property-concept-ids :test #'equal)
              (cl-pushnew (org-ilm--org-mem-ancestry-ids parent-id) property-concept-ancestors))))

        ;; Filter if concept-id is ancestor of any other concept-id
        (let ((ancestries (apply #'append property-concept-ancestors)))
          (dolist (concept-id property-concept-ids)
            (unless (member concept-id ancestries)
              (cl-pushnew concept-id concept-ids :test #'equal))))))

    concept-ids))

(defun org-ilm--concepts-get-with-descendant-concepts (concept)
  "Retrieve descendant concepts of a headline.
Descendants can be directly in outline or indirectly through property linking."
  (let ((id (plist-get concept :id)))
    (seq-filter
     (lambda (concept)
       (let ((ancestory (nth 2 (org-ilm--concept-cache-gather (plist-get concept :id)))))
         (member id ancestory)))
     (org-ilm--query-concepts))))

(defun org-ilm--concepts-get-with-descendant-concepts--deprecated (concept)
  "DEPRECATED now that we save full ancestor list in cache.

Retrieve descendant concepts of a headline.

Descendants can be directly in outline or indirectly through property linking.

This function does not have to be fast: Currently only called every now
and again by `org-ilm-queue-mark-by-concept'."
  (let ((all-concepts (mapcar (lambda (s)
                                (cons (plist-get s :id) s))
                              ;; Cached:
                              (org-ilm--query-concepts)))
        (queue (list (plist-get concept :id)))
        (concept-and-descendants (list (cons (plist-get concept :id) concept))))
    
    ;; Queue will contain org-ids of CONCEPT and any of its descendants. For
    ;; each element in the queue, loop over concepts and when one has been found
    ;; that has as direct parent this queue element, it is a descendant and thus
    ;; added to the queue. When the loop for the current queue element is
    ;; finished, we found all its direct children and we thus remove it from the
    ;; queue. Repeat until exhausted.
    (while queue
      (let ((current (pop queue)))
        (dolist (concept all-concepts)
          (let ((concept-id (car concept))
                (concept-parents (nth 3 (plist-get (cdr concept) :concepts))))
            (when (and (member current concept-parents)
                       (not (assoc concept-id concept-and-descendants)))
              (push concept concept-and-descendants)
              (push concept-id queue))))))

    concept-and-descendants))

(defun org-ilm--concept-property-or-inherited (id property)
  "Return the PROPERTY value of concept, or that of its earliest ancestor, else nil."
  (pcase-let* ((`(,concepts . ,n-direct) (org-ilm--concept-cache-gather id))
               (values))
    (when (and n-direct (> n-direct 0))
      (setq concepts (last concepts n-direct))
      (dolist (concept concepts)
        (when-let ((entry (org-mem-entry-by-id concept)))
          (if-let ((value (org-mem-entry-property property entry)))
              (setf (alist-get concept values nil nil #'string=) value)
            (dolist (value (org-ilm--concept-property-or-inherited
                            concept property))
              (setf (alist-get (car value) values nil nil #'string=) (cdr value))))))
      values)))


;;;; Concept cache

(defvar org-ilm-concept-cache (make-hash-table :test 'equal)
  "Map concept org-id -> ('(ancestor-concept-ids..) . num-direct-parents)

The direct parent ids are the last num-direct-parents ids in ancestor-concept-ids.

Gets reset after org-mem refreshes.")

(defun org-ilm-concept-cache-reset ()
  (setq org-ilm-concept-cache (make-hash-table :test 'equal)))

(defun org-ilm--concept-cache-gather (&optional headline-or-id)
  "Gather recursively headline's concepts. Headline may itself be a concept."
  (let ((id (cond
             ((null headline-or-id)
              (org-id-get))
             ((stringp headline-or-id)
              headline-or-id)
             (t
              (org-element-property :ID headline-or-id)))))
    
    (or (gethash id org-ilm-concept-cache)
        (let* ((parent-ids
                ;; Non-recursively, just direct parents in DAG
                (org-ilm--concepts-get-parent-concepts id))
               (entry (org-mem-entry-by-id id))
               (type (org-ilm--element-type entry))
               (is-concept (eq type 'concept))
               (ancestor-ids (copy-sequence parent-ids)))

          ;; Recursively call this function on all direct parent concepts to get
          ;; entire ancestory.
          (dolist (parent-id parent-ids)
            (let ((parent-data (org-ilm--concept-cache-gather parent-id)))
              (dolist (parent-ancestor (car parent-data))
                (cl-pushnew parent-ancestor ancestor-ids :test #'equal))))

          ;; Compile data in a list. If this headline is also a concept, add it
          ;; to the cache as well.
          (let ((data (when ancestor-ids (cons ancestor-ids (length parent-ids)))))
            (when is-concept
              (puthash id data org-ilm-concept-cache))
            ;; Return data
            data)))))

;;; Footer

(provide 'org-ilm-concept)

;;; org-ilm-concept.el ends here
