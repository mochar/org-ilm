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

(defvar org-ilm-concept-change-hook nil
  "Hook run when concept of an element or concept changed.")

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
            "???")))
      ((consp concept)
       (format "[[id:%s][%s]]" (car concept) (cdr concept)))
      ((org-mem-entry-p concept)
       (format "[[id:%s][%s]]"
               (org-mem-entry-id concept)
               (org-ilm--org-mem-title-full concept)))
      (t
       (error "Invalid parent concept value %s" concept))))
   (ensure-list concepts) " "))

(defun org-ilm--concept-property-set (concepts)
  "Set the concept property of node at point to match CONCEPTS.
For value of CONCEPTS, see `org-ilm--concept-property-prepare'."
  (let ((value (org-ilm--concept-property-prepare concepts)))
    (if value
        (org-entry-put nil "CONCEPTS+" value)
      (org-entry-delete nil "CONCEPTS+"))
    (save-buffer)
    (org-ilm--org-mem-ensure)
    (run-hooks 'org-ilm-concept-change-hook)))

;; TODO Should this happen through org-ilm-capture?
;; Eventhough i classify concept as an ilm type, their behavior currently is too
;; different to elegantly merge the capture functionality with that of cards and
;; materials. This function is sufficient for now, but if later i want to
;; concepts to have priorities, schedules, and be part of queues, then I can
;; merge this functionality with org-ilm-capture.
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

(cl-defun org-ilm--concept-edit-selection (&key collection this parent selection narrow throw-new)
  "Add or remove concepts to or from a SELECTION of an existing or new concept.
Returns the new selection.

For an existing concept (THIS), SELECTION are the concepts in its
property, and must therefore not be passed. For new concepts, SELECTION is interactive, and must thus be passed explicitely.

If a new concept is placed as an outline child of another concept,
PARENT should be specified to determine what concepts will be inherited.

With THROW-NEW, user is able to input a new name, which will then be
`throw'n with the tag `new-concept'."
  ;; Get the concept in the property of THIS, if specified
  (when this
    (when selection
      (error "SELECTION is parsed from property of THIS so must not be specified"))
    (setq selection (org-ilm--org-with-point-at this
                      (mapcar #'car (org-ilm--concept-parse-property)))))

  ;; THIS or PARENT determine what concepts are inherited (i.e. concepts in
  ;; outline and their properties), so its useful to have a name for both of
  ;; them (reference).
  (let ((reference (or this parent))
        inherited
        descendants)

    ;; Get collection from reference if not specified explicitely
    (unless collection
      (unless reference
        (error "COLLECTION must be specified if THIS or PARENT missing"))
      (setq collection (car (org-ilm--collection-file
                             (oref (org-mem-entry-by-id reference)
                                   file-truename))))
      (unless collection
        (error "Could not find COLLECTION of %s" reference)))

    (when (or reference selection)
      ;; Inherited concepts are redundant 
      (setq inherited
            (seq-difference
             (apply #'append
                    (mapcar 
                     (lambda (id)
                       (car (org-ilm--concept-cache-gather id)))
                     (append (ensure-list reference) selection)))
             selection))

      ;; Descendants should be omited as they can lead to circular DAGs
      (setq descendants
            (delete-dups
             (apply #'append
                    (mapcar
                     (lambda (id)
                       (org-ilm--concepts-descendants id collection))
                     (append (ensure-list reference) selection))))))
    
    
    (let* ((choice (consult--multi
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
                            (if entry
                                (org-ilm--org-mem-title-full entry)
                              id)
                            ;; Add id in case entry not found
                            :entry entry :id id :selected t)))
                       selection))
                     ;; Inherited concepts, hidden by default.
                     (list
                      :name "Inherited"
                      :narrow ?i
                      :face '(:inherit Info-quoted)
                      :hidden t
                      :items
                      (mapcar
                       (lambda (id)
                         (let ((entry (org-mem-entry-by-id id)))
                           (propertize
                            (if entry
                                (org-ilm--org-mem-title-full entry)
                              id)
                            :entry entry :id id :inherited t)))
                       inherited))
                     ;; Descendant concepts, hidden by default, and unselectable
                     (list
                      :name "Descendant"
                      :narrow ?d
                      :face '(:inherit Info-quoted :strike-through t)
                      :hidden t
                      :items
                      (mapcar
                       (lambda (id)
                         (let ((entry (org-mem-entry-by-id id)))
                           (propertize
                            (org-ilm--org-mem-title-full entry)
                            :entry entry :id id :descendant t)))
                       descendants))
                     ;; The concepts that can be added to the selection must not
                     ;; be in the selection, inherited, or descendant concepts,
                     ;; cannot be THIS or PARENT, and cannot be children of
                     ;; THIS.
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
                                      (not (member id descendants))
                                      (if reference
                                          (and
                                           (not (string= reference id))
                                           (not (member reference (org-ilm--org-mem-ancestry-ids entry))))
                                        t))
                             (propertize
                              (org-ilm--org-mem-title-full entry)
                              :entry entry :id id))))
                       (org-ilm--org-mem-entries-in-files
                        (org-ilm--collection-files collection)))))
                    :require-match (not throw-new)
                    :initial-narrow narrow
                    :prompt "Concept: "))
           (candidate (car choice))
           (choice-source (cdr choice))
           ;; Match: nil if candidate does not exist, t if candidate exists, new
           ;; if the candidate has been created.
           (match (plist-get choice-source :match))
           (id (get-text-property 0 :id candidate))
           (concept (get-text-property 0 :entry candidate)))
      (cond
       ((null id)
        (when throw-new
          (throw 'new-concept candidate))
        selection)
       ((get-text-property 0 :descendant candidate)
        (message "Cannot add a descendant concept")
        selection)
       ((get-text-property 0 :inherited candidate)
        ;; I think we should allow to add inherited if the user wants to, there
        ;; might be a reason for it.
        ;; (message "Inherited concepts cannot be added explicitly")
        ;; selection
        (cons (oref concept id) selection))
       ((get-text-property 0 :selected candidate)
        (seq-filter
         (lambda (e)
           ;; Use id in case entry not found
           (not (string= e id)))
         selection))
       (t
        (cons (oref concept id) selection))))))

(defun org-ilm--concept-set (id &optional on-done)
  "Add or remove concepts from the CONCEPTS property of element with ID."
  (let* ((collection (org-ilm--org-with-point-at id
                       (car (org-ilm--collection-file
                             (org-ilm--buffer-file-name)))))
         (result
          (condition-case nil
              (catch 'new-concept
                (while t
                  (let ((selection (org-ilm--concept-edit-selection
                                    :collection collection
                                    :this id
                                    :throw-new t)))
                    (org-ilm--org-with-point-at id
                      (org-ilm--concept-property-set selection)))))
            (quit 'done))))
    (cond
     ((eq result 'done)
      (if on-done
          (funcall on-done)
        (signal 'quit nil)))
     (result ; new-concept
      (org-ilm--import-concept-transient
       (make-org-ilm-concept-import
        :collection collection
        :title result
        :on-success
        (lambda (new-concept-import)
          (org-ilm--org-with-point-at id
            (let ((concepts (cons (oref new-concept-import id)
                                  (mapcar #'car (org-ilm--concept-parse-property)))))
              (org-ilm--concept-property-set concepts)))
          (org-ilm--concept-set id on-done)
          )))))))

;;;; Commands

(defun org-ilm-concept-dwim ()
  "Add concept if point in ilm element, otherwise create a new concept."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((at-heading-p (org-at-heading-p))
             (ilm-type (org-ilm--element-type)))
        (cond
         (ilm-type
          (call-interactively #'org-ilm-concept-set))
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
   (t
    (org-entry-put nil org-ilm-property-type "concept")
    (org-id-get-create)
    (save-buffer))))

(defun org-ilm-concept-set ()
  "Add or remove concepts from the CONCEPTS property."
  (interactive)
  (if-let ((id (ignore-errors (and (eq major-mode 'org-mode) (org-id-get)))))
      (org-ilm--concept-set id)
    (user-error "Cannot set concepts here")))

;;;; Transient

(defun org-ilm-concept-menu ()
  "Menu for concepts."
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
   ("n" "Concepts"
    (lambda ()
      (interactive)
      (let ((id (plist-get (transient-scope) :id)))
        (org-ilm--concept-set
         id
         (lambda ()
           (org-ilm--concept-transient-rebuild-scope)
           (unless (eq transient-current-command 'org-ilm--concept-transient)
             (org-ilm--concept-transient))
           ))))
    :transient transient--do-stay
    )
   ]
  
  (interactive)
  (transient-setup
   'org-ilm--concept-transient nil nil
   :scope (org-ilm--concept-transient-data
           (cond-let*
             ([scope (transient-scope)]
              [id (ignore-errors (oref scope id))]
              id)
             (t
              (org-id-get))))))
  
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
  (when-let ((entry (org-mem-entry-by-id id)))
    (let* ((ancestry (org-ilm--org-mem-ancestry-ids entry))
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

      concept-ids)))

;; TODO Use `org-mem-entry-children' ?
(defun org-ilm--concepts-descendants (id collection)
  "Retrieve descendant concepts of a headline with ID.
Descendants can be directly in outline or indirectly through property linking."
  (seq-filter
   (lambda (concept-id)
     (member id (car (org-ilm--concept-cache-gather concept-id))))
   (org-ilm-query-collection
    (lambda (c) `(and (property "ID") (property "ILM_TYPE" "concept")))
    collection nil #'org-id-get)))

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

;; NOTE This parameter should not be optional with fallback to org-id-get, as
;; buggy behavior such as out of date org-mem cache can lead to passing nil
;; unexpectedly as argument.
(defun org-ilm--concept-cache-gather (headline-or-id)
  "Gather recursively headline's concepts. Headline may itself be a concept."
  (let ((id (cond
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
