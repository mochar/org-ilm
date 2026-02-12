;;; org-ilm-queue-view.el --- Queue buffer -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'transient)
(require 'hl-line)
(require 'ts)

(require 'org-ilm-utils)
(require 'org-ilm-queue)
(require 'org-ilm-pqueue)
(require 'org-ilm-ost)
;; (require 'org-ilm-element)
;; (require 'org-ilm-collection)
(require 'org-ilm-concept)
(require 'org-ilm-review)
(require 'org-ilm-attachment)
(require 'ost)

;;;; Variables

;; TODO Added this just so i can integrate with meow. Move key binding and stuff to here?
(define-minor-mode org-ilm-queue-mode
  "Minor mode in org-ilm queue buffers."
  :lighter nil
  :group 'org-il
  :interactive nil)

(defvar-keymap org-ilm-queue-base-map
  :doc "Base map of ilm queue, regardless if empty or not."
  "n" #'org-ilm-queue-next-element
  "p" #'org-ilm-queue-previous-element
  "b" #'backward-char
  "f" #'forward-char
  "q" #'quit-window
  ;; "k" (lambda ()
  ;;       (interactive)
  ;;       (kill-buffer (current-buffer)))
  "g" #'org-ilm-queue-revert
  "G" #'org-ilm-queue-rebuild)
  
(defvar-keymap org-ilm-queue-map
  :doc "Keymap for the queue buffer."
  :parent org-ilm-queue-base-map
  "M-n" #'org-ilm-queue-next-marked
  "M-p" #'org-ilm-queue-previous-marked
  "r" #'org-ilm-queue-review
  "e" #'org-ilm-element-actions
  "v" #'org-ilm-queue-sort
  "m" #'org-ilm-queue-element-mark
  "<backspace>" #'org-ilm-queue-element-unmark-backwards
  "RET" #'org-ilm-queue-open-element
  "SPC" #'org-ilm-queue-open-attachment
  "P" #'org-ilm-queue-element-priority
  "#" #'org-ilm-queue-element-position
  "S" #'org-ilm-queue-element-schedule
  "D" #'org-ilm-queue-element-delete
  "-" #'org-ilm-queue-element-remove
  "M n" #'org-ilm-queue-mark-by-concept
  "M :" #'org-ilm-queue-mark-by-tag
  "M s" #'org-ilm-queue-mark-by-scheduled
  "M u" #'org-ilm-queue-mark-unmark-all
  "M i" #'org-ilm-queue-mark-invert
  "M ?" #'org-ilm-queue-mark-missing
  )

;; TODO Make this an alist (index . id) as index is used as queue object so that
;; we can do (vtable-update-object (vtable-current-table) index) to update the
;; table when marking rather than doing a whole revert.
(defvar-local org-ilm--queue-marked-objects nil
  "Org id of marked objects in queue.")

(defun org-ilm--queue-object-id (object)
  (if (org-ilm-element-p object)
      (org-ilm-element--id object)
    object))

(defun org-ilm--queue-mark-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--queue-object-id object)))
      (unless (member id org-ilm--queue-marked-objects)
        (push id org-ilm--queue-marked-objects)))))

(defun org-ilm--queue-unmark-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--queue-object-id object)))
      (setq org-ilm--queue-marked-objects
            (delete id org-ilm--queue-marked-objects)))))

(defun org-ilm--queue-mark-toggle-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--queue-object-id object)))
      (if (member id org-ilm--queue-marked-objects)
          (setq org-ilm--queue-marked-objects
                (delete id org-ilm--queue-marked-objects))
        (push id org-ilm--queue-marked-objects)))))

(defun org-ilm--vtable-get-object ()
  "Return (index . queue object) at point."
  ;; when-let because index can be nil when out of table bounds
  ;; (pcase-let ((`(,rank ,id) (vtable-current-object)))
  ;;   (cons rank (gethash id (org-ilm-queue--elements org-ilm-queue)))))
  (vtable-current-object))

(defun org-ilm--vtable-get-object-id ()
  (org-ilm--queue-object-id
   (cdr (org-ilm--vtable-get-object))))

(defun org-ilm-queue-open-attachment (object)
  "Open attachment of object at point."
  (interactive (list (cdr (org-ilm--vtable-get-object))))
  (org-ilm--attachment-open-by-id (org-ilm-element--id object)))

(defun org-ilm-queue-open-element (object)
  "Open attachment of object at point."
  (interactive (list (cdr (org-ilm--vtable-get-object))))
  (let ((id (if (stringp object) object (org-ilm-element--id object)))) 
    (org-ilm--org-goto-id id)))

(defun org-ilm-queue-element-mark (object &optional action)
  "Mark the object at point."
  (interactive (list (cdr (org-ilm--vtable-get-object))))
  (unless action (setq action 'toggle))
  (cl-assert (member action '(mark unmark toggle)))
  (pcase action
    ('mark (org-ilm--queue-mark-objects object))
    ('unmark (org-ilm--queue-unmark-objects object))
    ('toggle (org-ilm--queue-mark-toggle-objects object)))
  ;; TODO gives cache error no idea why
  ;; Maybe worked on?: https://lists.gnu.org/archive/html/bug-gnu-emacs/2025-07/msg00802.html
  (vtable-update-object (vtable-current-table) (vtable-current-object))
  ;; (org-ilm-queue-revert)
  (when (called-interactively-p)
    (forward-line)
    (when (eobp) (forward-line -1))))

(defun org-ilm-queue-element-unmark-backwards ()
  "Jump to previous element and unmark it."
  (interactive)
  (unless (bobp)
    (forward-line -1)
    (org-ilm-queue-element-mark (cdr (org-ilm--vtable-get-object)) 'unmark)))

(defun org-ilm-queue-mark-by-concept (concept-id)
  "Mark all elements in queue that are part of concept with id CONCEPT-ID."
  (interactive
   (list
    (org-mem-entry-id (org-ilm--concept-select-entry))))

  ;; Alternatively, we could have used
  ;; `org-ilm--concepts-get-with-descendant-concepts' to precompute the
  ;; descendancy, but this would require a list-to-list comparison eg with
  ;; `seq-some' per object, instead of just an `assoc'.
  (dolist (object (org-ilm--queue-elements))
    (when (org-ilm-element-p object)
      (let ((ancestor-ids (car (org-ilm-element--concepts object))))
        (when (member concept-id ancestor-ids)
          (org-ilm--queue-mark-objects object)))))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-by-tag (tag)
  "Mark all elements in queue that have tag TAG."
  (interactive
   (list
    (completing-read
     "Tag: "
     (org-ilm--collection-tags (org-ilm-queue--collection org-ilm-queue)))))
  (dolist (object (org-ilm--queue-elements))
    (when (and (org-ilm-element-p object)
               (member tag (org-ilm-element--tags object)))
      (org-ilm--queue-mark-objects object)))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-by-scheduled (days)
  "Mark all elements in queue that are scheduled in DAYS days.
DAYS can be specified as numeric prefix arg."
  (interactive "NNumber of days: ")
  (dolist (object (org-ilm--queue-elements))
    (when (org-ilm-element-p object)
      (when-let ((due (* -1 (org-ilm-element--schedrel object))))
        (when (<= (round due) days) 
          (org-ilm--queue-mark-objects object)))))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-missing ()
  "Mark all elements that are missing, i.e. do not have an ilm-element."
  (interactive)
  (dolist (object (org-ilm--queue-elements))
    (unless (org-ilm-element-p object)
      (org-ilm--queue-mark-objects object)))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-invert ()
  "Mark all unmarked elements, and unmark marked elements."
  (interactive)
  (let* ((marked org-ilm--queue-marked-objects)
         (unmarked (seq-keep
                    (lambda (element)
                      (let ((id (org-ilm-element--id element)))
                        (when (not (member id marked)) id)))
                    (org-ilm--queue-elements))))
    (setq-local org-ilm--queue-marked-objects unmarked)
    (org-ilm-queue-revert)))

(defun org-ilm-queue-mark-unmark-all ()
  "Unmark all marked elements."
  (interactive)
  (setq-local org-ilm--queue-marked-objects nil)
  (org-ilm-queue-revert))

(defun org-ilm-queue-next-element ()
  (interactive)
  (forward-line)
  (unless (vtable-current-object) (forward-line -1)))

(defun org-ilm-queue-previous-element ()
  (interactive)
  (forward-line -1)
  (unless (vtable-current-object) (forward-line)))

(defun org-ilm-queue-next-marked ()
  "Go to next marked element."
  (interactive)
  (let (done point)
    (when org-ilm--queue-marked-objects
      (save-excursion
        (while (not done)
          (forward-line)
          (if-let* ((id (org-ilm--vtable-get-object-id)))
              (when (member id org-ilm--queue-marked-objects)
                (setq done t
                      point (point)))
            (setq done t)))))
    (if point
        (goto-char point)
      (message "No marked element after this point"))))

(defun org-ilm-queue-previous-marked ()
  "Go to previous marked element."
  (interactive)
  (let (done point)
    (when org-ilm--queue-marked-objects
      (save-excursion
        (while (not done)
          (if (= 0 (current-line))
              (setq done t)
            (forward-line -1)
            (if-let* ((id (org-ilm--vtable-get-object-id)))
                (when (member id org-ilm--queue-marked-objects)
                  (setq done t
                        point (point)))
              (setq done t))))))
    (if point
        (goto-char point)
      (message "No marked element before this point"))))

(defun org-ilm-queue--set-header ()
  (setq header-line-format
        (concat
         (when (eq (current-buffer) org-ilm-queue-active-buffer)
           (if (org-ilm-reviewing-p)
               (propertize "[R] " 'face 'error)
             (propertize "[A] " 'face 'bold)))
         (org-ilm-queue--name org-ilm-queue)
         " ("
         (symbol-name (org-ilm-queue--collection org-ilm-queue))
         ")")))

(defun org-ilm-queue-revert (&optional rebuild)
  "Revert/refresh the queue buffer. With REBUILD, reparse elements."
  (interactive "P")
  (let ((was-empty (org-ilm--queue-empty-p)))
    (when rebuild
      (when (yes-or-no-p "Rebuild queue? This will parse the elements again. ")
        (org-ilm--queue-rebuild)))
    (if (or (and was-empty (not (org-ilm--queue-empty-p)))
            (org-ilm--queue-empty-p)
            (not (vtable-current-table)))
        (org-ilm--queue-buffer-build)
      (vtable-revert-command))
    (org-ilm-queue--set-header)))

(defun org-ilm-queue-rebuild ()
  "Rebuild the queue buffer."
  (interactive)
  (setq org-ilm--queue-marked-objects nil)
  (org-ilm-queue-revert 'rebuild))

(defun org-ilm--vtable-format-cell (string marked &optional missing)
  "Apply 'face property to cell STRING based in its MARKED and/or MISSING."
  (if (not (or marked missing))
      string
    (let ((face (list :inherit 'default :slant 'italic)))
      (when marked (setq face (plist-put face :underline t)))
      (when missing (setq face (plist-put face :foreground "#595959")))
      (propertize string 'face face))))
  
(cl-defun org-ilm--queue-make-vtable (&key keymap)
  "Build queue vtable.

A lot of formatting code from org-ql."
  (make-vtable
   :insert nil ; Return vtable object rather than insert at point
   :columns
   `((:name
      "Index"
      :max-width 4
      :align 'right
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,index ,missing) data)
                     (index-str (format "%4d" (1+ index))))
          (org-ilm--vtable-format-cell index-str marked missing))))
     (:name
      "Priority"
      :width 6
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,priority ,missing) data)
                     (rank (car priority))
                     (quantile (cdr priority)))
          (org-ilm--vtable-format-cell
           (if missing
               "NA"
             (propertize (format "%.2f" (* 100 quantile))
                         'face 'shadow))
           marked missing))))
     (:name
      "Type"
      :width 4
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,type ,missing) data))
          (org-ilm--vtable-format-cell
           (if missing
               "NA"
             (org-ilm--org-add-todo-face
              (upcase (pcase type
                        ('material "MTRL")
                        ('card "CARD")
                        (_ "????")))))
           marked missing))))
     ;; (:name
     ;;  "Cookie"
     ;;  :width 4
     ;;  :formatter
     ;;  (lambda (data)
     ;;    (pcase-let ((`(,marked ,p) data))
     ;;      (org-ilm--vtable-format-cell
     ;;       (org-ql-view--add-priority-face (format "[#%s]" p))
     ;;       marked))))
     (:name
      "Title"
      :min-width "50%"
      :max-width "55%"
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,title ,missing) data))
          (org-ilm--vtable-format-cell title marked missing))))
     (:name
      "Due"
      :max-width 8
      ;; :align 'right
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,due ,missing) data))
          (org-ilm--vtable-format-cell
           (cond
            (missing "NA")
            (due (org-add-props
                     (org-ilm--format-relative-date
                      (round due))
                     nil 'face 'org-ql-view-due-date))
            (t ""))
           marked missing))))
     ;; (:name
     ;;  "Tags"
     ;;  :formatter
     ;;  (lambda (data)
     ;;    (pcase-let ((`(,marked ,tags) data))
     ;;      (if tags
     ;;          (org-ilm--vtable-format-cell
     ;;           (--> tags
     ;;                (s-join ":" it)
     ;;                (s-wrap it ":")
     ;;                (org-add-props it nil 'face 'org-tag))
     ;;           marked)
     ;;        ""))))
     (:name
      "Concepts"
      :max-width 15
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,concepts ,missing) data)
                    (names))
          (when concepts
            (setq names
                  (mapcar
                   (lambda (concept)
                     (let ((title (or
                                   (car (last (org-mem-entry-roam-aliases concept)))
                                   (org-mem-entry-title concept))))
                       (substring title 0 (min (length title)
                                               org-ilm-queue-concept-nchar))))
                   concepts)))
          (org-ilm--vtable-format-cell
           (cond
            (names (org-add-props (s-join "," names) nil 'face 'org-tag))
            (missing "NA")
            (t ""))
           marked missing)))))
   :objects-function
   (lambda ()
     (unless (org-ilm--queue-empty-p)
       (let (objects)
         (ost-map-in-order
          (lambda (node rank)
            (push 
             (cons rank (gethash (ost-node-id node) (org-ilm-queue--elements org-ilm-queue)))
             objects))
          org-ilm-queue (not (org-ilm-queue--reversed org-ilm-queue)))
         objects)))
   :getter
   (lambda (row column vtable)
     (let* ((rank (car row))
            (object (cdr row))
            ;; See `org-ilm--pqueue-queue'. Missing elements are mapped back to id.
            ;; When nil, this is a placeholder element.
            (id (if (org-ilm-element-p object)
                    (org-ilm-element--id object)
                  object))
            (marked (member id org-ilm--queue-marked-objects)))
       (cond
        ((org-ilm-element-p object)
         (let* ((element object)
                (concepts (org-ilm-element--concepts element))
                (priority (org-ilm-element--priority element)))
           (pcase (vtable-column vtable column)
             ("Index" (list marked rank))
             ("Type" (list marked (org-ilm-element--type element)))
             ("Priority" (list marked priority))
             ;; ("Cookie" (list marked (org-ilm-element--pcookie element)))
             ("Title" (list marked (org-ilm-element--title element)))
             ("Due" (list marked (org-ilm-element--schedrel element)))
             ;; ("Tags" (list marked (org-ilm-element--tags element)))
             ("Concepts"
              (list marked
                    (mapcar #'org-mem-entry-by-id
                            ;; Only direct concepts
                            (last (car concepts) (cdr concepts))))))))
        ((stringp object)
         (pcase (vtable-column vtable column)
           ("Index" (list marked rank t))
           ("Title" (list marked id t))
           (_ (list marked nil t))))
        ((null object)
         (pcase (vtable-column vtable column)
           ("Index" (list marked rank t))
           ("Title" (list marked "<empty>" t))
           (_ (list marked nil t))))
        (t (error "wrong object value in table row: %s" object)))))
         
   :keymap (or keymap org-ilm-queue-map)
   :actions '("S" ignore)))

(cl-defun org-ilm--queue-buffer-build (&key buffer switch-p keymap)
  "Build the contents of the queue buffer, and optionally switch to it."
  (org-ilm-with-queue-buffer buffer
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (if (org-ilm--queue-empty-p)
        (progn
          (insert (propertize "Queue empty..." 'face 'italic))
          (use-local-map (or keymap org-ilm-queue-base-map)))
      (vtable-insert (org-ilm--queue-make-vtable :keymap keymap)))
    (org-ilm-queue--set-header)
    (setq-local buffer-read-only t)
    (hl-line-mode 1)
    (eldoc-mode 1)
    (add-hook 'eldoc-documentation-functions
              #'org-ilm--collection-eldoc-element-info
              nil 'local)
    (goto-char (point-min))
    (when switch-p
      (switch-to-buffer buffer))))

(defvar org-ilm--queue-transient-buffer nil)

;;;;; Actions

(defun org-ilm-queue-save ()
  "Save queue to disk."
  (interactive)
  (org-ilm-ost--write org-ilm-queue)
  (message "Saved queue to %s" (org-ilm-ost--file org-ilm-queue))
  t)

(defun org-ilm-queue-delete ()
  "Delete queue file from disk."
  (interactive)
  (when (yes-or-no-p "Delete queue file from disk?")
    (delete-file (org-ilm-ost--file org-ilm-queue))
    (message "Deleted queue %s" (org-ilm-ost--file org-ilm-queue))))

(defun org-ilm-queue-review ()
  "Start reviewing current queue."
  (interactive)
  (org-ilm-review-start :queue-buffer (current-buffer)))

(defun org-ilm-queue-element-priority ()
  "Set the priority of the element at point, or bulk spread of marked elements."
  (interactive)
  (if org-ilm--queue-marked-objects
      (org-ilm--queue-spread-priority)
    (org-ilm-element-set-priority (org-ilm--element-from-context))
    (org-ilm-queue-revert)))

(defun org-ilm-queue-element-position ()
  "Set the position of the element at point, or bulk spread of marked elements."
  (interactive)
  (if org-ilm--queue-marked-objects
      (org-ilm--queue-spread-position)
    (ost-tree-move
     org-ilm-queue
     (org-ilm-element--id (org-ilm--element-from-context))
     ;; TODO Pass current position as initial position
     (car (org-ilm--queue-select-read org-ilm-queue)))
    (org-ilm-queue-revert)))

(defun org-ilm-queue-element-schedule ()
  "Set the schedule of the element at point, or bulk set of marked elements."
  (interactive)
  (let ((elements (org-ilm-queue--elements org-ilm-queue)))
    (if org-ilm--queue-marked-objects
        (let ((start-ts (ts-parse (org-read-date nil nil nil "Start date: ")))
              end-ts)
          (while (not end-ts)
            (let ((ts (ts-parse (org-read-date nil nil nil "End date: "))))
              (if (ts>= ts start-ts)
                  (setq end-ts ts)
                (message "End date must be after start date")
                (sleep-for 1))))
          (dolist (id org-ilm--queue-marked-objects)
            (org-ilm--org-with-point-at id
              (let* ((diff-days (plist-get
                                 (ts-human-duration
                                  (ts-difference end-ts start-ts))
                                 :days))
                     (rand-interval (random (1+ diff-days)))
                     (ts (ts-adjust 'day rand-interval start-ts))
                     (date (ts-format "%Y-%m-%d" ts)))
                (org-ilm-element-set-schedule (org-ilm--element-from-context) date)
                (puthash id (org-ilm--element-at-point) elements)))))

      ;; No marked elements, set schedule of element at point
      (call-interactively #'org-ilm-element-set-schedule)
      (let ((element (org-ilm--element-from-context)))
        (puthash (org-ilm-element--id element) element elements)))
    (org-ilm-queue-revert)))

(defun org-ilm-queue-element-remove ()
  "Remove current or selected elements from this queue."
  (interactive)
  (let ((ids (copy-sequence
              (or org-ilm--queue-marked-objects
                  (list (org-ilm--vtable-get-object-id))))))
    (when (yes-or-no-p (format "Remove %s element(s) from queue?" (length ids)))
      (dolist (id ids)
        (org-ilm--queue-remove id))
      (org-ilm--queue-unmark-objects ids)
      (org-ilm-queue-revert))))

(defun org-ilm-queue-element-delete ()
  "Delete current or selected elements from the collection."
  (interactive)
  (let* ((queue-buffer (current-buffer))
         (collection (org-ilm-queue--collection org-ilm-queue))
         (ids (copy-sequence
               (or org-ilm--queue-marked-objects
                   (list (org-ilm--vtable-get-object-id)))))
         (ask-confirmation-p t)
         (confirm-p (lambda (message)
                      (if (not ask-confirmation-p)
                          t
                        (setq message (concat message " (y)es, (n)o, (!)all, (q)uit"))
                        (setq message (propertize message 'face 'bold))
                        (pcase (read-char message '("y" "n" "!" "q"))
                          (?y t)
                          (?n nil)
                          (?q (throw 'abort nil))
                          (?! (setq ask-confirmation-p nil)
                              t)))))
         (delete-id (lambda (id)
                      (org-ilm--queue-remove id :buffer queue-buffer)
                      (org-ilm--pqueue-remove id collection))))
    (catch 'abort
      (dolist (id ids)
        (cond
         ((ignore-errors (progn (org-id-goto id) t))
          (org-mark-subtree)
          (when (funcall confirm-p "Delete element?")
            (org-ilm-element-delete (org-ilm--element-at-point) 'warn-attach)
            (funcall delete-id id)))
         (t
          (switch-to-buffer queue-buffer)
          
          (when (funcall confirm-p "Element not found. Delete from priority queue?")
            (funcall delete-id id))))
        (when ask-confirmation-p
          (with-current-buffer queue-buffer
            (org-ilm-queue-revert)))))
    (switch-to-buffer queue-buffer)
    (org-ilm--queue-unmark-objects ids)
    (org-ilm-queue-revert)))

;;;;; Sort transient

(transient-define-prefix org-ilm--queue-sort-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (org-ilm-with-queue-buffer org-ilm--queue-transient-buffer
      (append
       (let ((key (org-ilm-queue--key org-ilm-queue)))
         (pcase key
           ("prank" '("--key=priority"))
           ("sched" '("--key=schedule"))))
       (when (org-ilm-queue--reversed org-ilm-queue)
         '("--reversed"))
       (when-let ((randomness (org-ilm-queue--randomness org-ilm-queue)))
         (list (format "--randomness=%s" (org-ilm--round-float randomness 3))))
       )))

  ["queue sort"
   ("k" "Key" "--key=" :choices ("priority" "schedule") :always-read t :transient t)
   ("r" "Reversed" "--reversed" :transient t)
   ("m" "Randomness" "--randomness=" :always-read t :prompt "Randomness: " :transient t)
   ("RET" "Sort"
    (lambda ()
      (interactive)
      (let* ((args (transient-args transient-current-command))
             (reversed (transient-arg-value "--reversed" args))
             (key (transient-arg-value "--key=" args))
             (randomness (string-to-number (transient-arg-value "--randomness=" args))))
        (org-ilm-with-queue-buffer org-ilm--queue-transient-buffer
          (pcase key
            ("priority" (org-ilm--queue-sort "prank" reversed randomness))
            ("schedule" (org-ilm--queue-sort "sched" reversed randomness))
            (_ (user-error "Invalid key: %s" key)))
          (org-ilm-queue-revert))))
    :inapt-if-not
    (lambda ()
      (transient-arg-value "--key=" (transient-get-value))))
   ]
  )

(defun org-ilm-queue-sort ()
  (interactive)
  (setq org-ilm--queue-transient-buffer (current-buffer))
  (org-ilm--queue-sort-transient))

;;;;; Priority and position spread transient

;; This transient lets the user set the "position" of an element in an OST,
;; which, when the OST belongs to the collection's priority queue, is the
;; priority of the element. If instead the OST belongs to a regular
;; `org-ilm-queue', this sets the position of the element in that queue. Note
;; that these operations (=setting the position in the OST) is in practice the
;; same, but in the case of the priority queue, has an obviously more permanent
;; and serious effect.

(defun org-ilm--queue-pspread-transient-format (extremum)
  (when-let* ((queue (transient-scope))
              (args (transient-get-value))
              (rank (transient-arg-value (concat "--" extremum "=") args)))
    (propertize
     (org-ilm-ost--position-format
      queue (1- (string-to-number rank)))
     'face 'italic)))

(defun org-ilm--queue-pspread-transient-read (extremum)
  (cl-assert (member extremum '("a" "b")))
  (let* ((minimum-p (string= extremum "a"))
         (queue (transient-scope))
         (rank-this (car (org-ilm--queue-select-read
                          (if (org-ilm-pqueue-p queue) (org-ilm-pqueue--queue queue) queue))))
         (rank-other (transient-arg-value
                      (concat "--" (if minimum-p "max" "min") "=")
                      (transient-args transient-current-command)))
         (rank-other (when rank-other
                       (1- (string-to-number rank-other))))
         (n-marked (length org-ilm--queue-marked-objects)))
    (cond
     (rank-other
      (when (or (< (1+ (abs (- rank-other rank-this ))) n-marked)
                (funcall (if minimum-p #'<= #'>=) rank-other rank-this))
        (org-ilm--transient-set-target-value (if minimum-p "b" "a") nil)))
     (minimum-p
      (org-ilm--transient-set-target-value
       "b" (number-to-string (+ rank-this n-marked))))
     ((not minimum-p)
      (org-ilm--transient-set-target-value
       "a" (number-to-string (- rank-this n-marked)))))
    (number-to-string (1+ rank-this))))

(transient-define-prefix org-ilm--queue-pspread-transient (queue)
  "This transient will set prioritiy if QUEUE is nil. Otherwise an
 `org-ilm-queue' struct can be passed so that the priority/order within that
 queue is changed."
  :refresh-suffixes t

  [:description
   (lambda ()
     (if (transient-scope) "Position spread" "Priority spread"))
   (:info
    (lambda ()
      (let ((n-materials 0)
            (n-cards 0))
        (dolist (id org-ilm--queue-marked-objects)
          (pcase (org-ilm--element-type (org-mem-entry-by-id id))
            ('material (cl-incf n-materials))
            ('card     (cl-incf n-cards))))
        (propertize
         (format "%s element (%s materials, %s cards)"
                 (length org-ilm--queue-marked-objects)
                 n-materials n-cards)
         'face 'transient-value))))
   ("a" "Min" "--min=" :always-read t
    :transient transient--do-call
    :reader (lambda (&rest _) (org-ilm--queue-pspread-transient-read "a")))
   (:info
    (lambda () (or (org-ilm--queue-pspread-transient-format "min") ""))
    :if
    (lambda () (org-ilm--queue-pspread-transient-format "min")))
   ("b" "Max" "--max=" :always-read t
    :transient transient--do-call
    :reader (lambda (&rest _) (org-ilm--queue-pspread-transient-read "b")))
   (:info
    (lambda () (or (org-ilm--queue-pspread-transient-format "max") ""))
    :if
    (lambda () (org-ilm--queue-pspread-transient-format "max")))
   ("r" "Random" "--random")
   ]

  [
   ("RET" "Spread"
    (lambda ()
      (interactive)
      (let* ((queue (transient-scope))
             (args (transient-args transient-current-command))
             (min-rank (1- (string-to-number (transient-arg-value "--min=" args))))
             (max-rank (1- (string-to-number (transient-arg-value "--max=" args))))
             (marked org-ilm--queue-marked-objects)
             (random-p (transient-arg-value "--random" args)))
        (org-ilm-queue--move-many
         queue
         (seq-map-indexed
          (lambda (rank i)
            (cons (nth i marked) rank))
          (org-ilm--spread (length marked) min-rank max-rank random-p)))
        (org-ilm-queue-revert)))
    :inapt-if-not
    (lambda ()
      (let ((args (transient-get-value)))
        (and (transient-arg-value "--min=" args)
             (transient-arg-value "--max=" args)))))
   ]

  (interactive)
  (transient-setup 'org-ilm--queue-pspread-transient nil nil :scope queue))

(defun org-ilm--queue-spread-position (&optional priority-queue-p)
  (unless org-ilm--queue-marked-objects
    (user-error "No elements marked!"))
  (setq org-ilm--queue-transient-buffer (current-buffer))
  (org-ilm--queue-pspread-transient
   (if priority-queue-p
       (org-ilm--pqueue (org-ilm-queue--collection org-ilm-queue))
     org-ilm-queue)))

(defun org-ilm--queue-spread-priority ()
  (org-ilm--queue-spread-position 'priority-queue-p))

;;;; Queue select

;; Using a queue view to select a position in the queue

(defvar org-ilm--queue-select-timer nil)
(defvar org-ilm--queue-select-buffer nil)
(defvar-local org-ilm--queue-select-point 1)
(defvar-local org-ilm--queue-select-minibuffer-window nil)

(defvar-keymap org-ilm-queue-select-map
  :doc "Keymap of queue select buffer."
  "n" (lambda ()
        (interactive)
        (forward-line)
        (when (eobp) (forward-line -1)))
  "p" #'previous-line
  "b" #'backward-char
  "f" #'forward-char
  "RET" (lambda ()
          (interactive)
          (org-ilm--queue-select-accept-minibuffer)))

(defun org-ilm--queue-select-update (pos &optional prompt numnew)
  "Update preview buffer based on minibuffer INPUT."
  (when (and pos (buffer-live-p org-ilm--queue-select-buffer))
    (with-current-buffer org-ilm--queue-select-buffer
      (setq pos (ost-tree-position org-ilm-queue pos numnew))
      (setq header-line-format
            (concat (or prompt "Select position: ")
                    (org-ilm-ost--position-format org-ilm-queue (car pos))))
      (with-selected-window (get-buffer-window)
        (goto-line (1+ (car pos)))
        (hl-line-highlight)
        (recenter-top-bottom)))))

(defun org-ilm--queue-select-update-minibuffer ()
  (with-current-buffer org-ilm--queue-select-buffer
    (when (and org-ilm--queue-select-minibuffer-window
               (window-live-p org-ilm--queue-select-minibuffer-window))
      (when-let* ((element (cdr (org-ilm--vtable-get-object)))
                  (rank (ost-rank org-ilm-queue (org-ilm-element--id element))))
        (with-selected-window org-ilm--queue-select-minibuffer-window
          (delete-minibuffer-contents)
          (insert (number-to-string (1+ rank))))))))

(defun org-ilm--queue-select-accept-minibuffer ()
  (with-current-buffer org-ilm--queue-select-buffer
    (when (and org-ilm--queue-select-minibuffer-window
               (window-live-p org-ilm--queue-select-minibuffer-window))
      (with-selected-window org-ilm--queue-select-minibuffer-window
        (exit-minibuffer)))))

(defun org-ilm--queue-select-read (queue &optional init-position prompt numnew)
  "Show only the preview buffer above the minibuffer during completion."
  (cl-assert (org-ilm-queue-p queue))
  (if (<= (ost-tree-size queue numnew) 1)
      (cons 0 0.0)
    (let* ((saved-config (current-window-configuration))
           newkeys
           position)
      
      (unwind-protect
          (progn
            (setq init-position (ost-tree-position queue init-position))

            (when (and numnew (>= numnew 0))
              (let* ((largest-node (ost-select queue (1- (ost-size queue))))
                     (largest-key (ost-node-key largest-node)))
                (dotimes (i numnew)
                  (let ((key (+ largest-key i 1)))
                    (ost-tree-insert queue key key)
                    (push key newkeys)))))
            
            (setq org-ilm--queue-select-buffer
                  (org-ilm--queue-buffer-create queue))

            ;; Build element table in buffer
            (org-ilm--queue-buffer-build
             :buffer org-ilm--queue-select-buffer
             :keymap org-ilm-queue-select-map)

            ;; Add a hook that detects movement to new row, update priority in minibuffer. 
            (with-current-buffer org-ilm--queue-select-buffer
              (add-hook 'post-command-hook
                        (lambda ()
                          (when (not (= (point) org-ilm--queue-select-point))
                            (setq org-ilm--queue-select-point (point))
                            (org-ilm--queue-select-update-minibuffer)))
                        nil 'local))
      
            ;; Replace all windows with our preview buffer
            (delete-other-windows)
            (switch-to-buffer org-ilm--queue-select-buffer)
            (org-ilm--queue-select-update
             (car (ost-tree-position queue (or init-position 0.0)))
             prompt)

            ;; now set up minibuffer hook
            (minibuffer-with-setup-hook
                (lambda ()
                  (with-current-buffer org-ilm--queue-select-buffer
                    (setq org-ilm--queue-select-minibuffer-window
                          (active-minibuffer-window)))
                  (add-hook 'post-command-hook
                            (lambda ()
                              (when org-ilm--queue-select-timer
                                (cancel-timer org-ilm--queue-select-timer))
                              (setq org-ilm--queue-select-timer
                                    (run-with-idle-timer
                                     0.3 nil
                                     (lambda ()
                                       (when-let ((pos (org-ilm-ost--parse-position-str
                                                        queue 
                                                        (minibuffer-contents-no-properties))))
                                         (setq position pos)
                                         (org-ilm--queue-select-update (car pos) prompt))))))
                            nil t))
              (setq position (org-ilm-ost--position-read queue init-position nil prompt)))
            ) ; Unwind-protect body

        ;;; Unwind forms
        ;; restore old window setup
        (set-window-configuration saved-config)
        (dolist (key newkeys)
          (ost-tree-remove queue key))
        (when (buffer-live-p org-ilm--queue-select-buffer)
          (kill-buffer org-ilm--queue-select-buffer))
        (setq org-ilm--queue-select-buffer nil)
        ) ; Unwind-protect unwind
      
      position)))

;;; Footer

(provide 'org-ilm-queue-view)

;;; org-ilm-queue-view.el ends here
