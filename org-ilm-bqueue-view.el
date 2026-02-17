;;; org-ilm-bqueue-view.el --- BQueue buffer view -*- lexical-binding: t; -*-

;;; Commentary:

;; The queue buffer holds a buffer local variable `org-ilm-bqueue' that contains
;; a `org-ilm-bqueue' object. The creation of these objects, including the
;; buffer, is actually handled in `org-ilm-bqueue.el'. This files handles the
;; logic of populating the bqueue buffer with a table and interacting with this
;; table.

;;; Code:

;;;; Requirements

(require 'transient)
(require 'hl-line)
(require 'ts)

(require 'org-ilm-utils)
(require 'org-ilm-queue)
(require 'org-ilm-bqueue)
(require 'org-ilm-pqueue)
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

(defvar-local org-ilm--bqueue-marked-objects nil
  "Org id of marked objects in the bqueue.")

;;;; Functions

(defun org-ilm--bqueue-object-id (object)
  (org-ilm--element-id object))

(defun org-ilm--bqueue-mark-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--bqueue-object-id object)))
      (unless (member id org-ilm--bqueue-marked-objects)
        (push id org-ilm--bqueue-marked-objects)))))

(defun org-ilm--bqueue-unmark-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--bqueue-object-id object)))
      (setq org-ilm--bqueue-marked-objects
            (delete id org-ilm--bqueue-marked-objects)))))

(defun org-ilm--bqueue-mark-toggle-objects (objects)
  (dolist (object (ensure-list objects))
    (let ((id (org-ilm--bqueue-object-id object)))
      (if (member id org-ilm--bqueue-marked-objects)
          (setq org-ilm--bqueue-marked-objects
                (delete id org-ilm--bqueue-marked-objects))
        (push id org-ilm--bqueue-marked-objects)))))

(defun org-ilm--bqueue-vtable-get-object ()
  "Return (index . queue object) at point."
  ;; when-let because index can be nil when out of table bounds
  ;; (pcase-let ((`(,rank ,id) (vtable-current-object)))
  ;;   (cons rank (gethash id (org-ilm-bqueue--elements org-ilm-bqueue)))))
  (vtable-current-object))

(defun org-ilm--bqueue-vtable-get-object-id ()
  (org-ilm--bqueue-object-id
   (cdr (org-ilm--bqueue-vtable-get-object))))

(defun org-ilm--bqueue-set-header ()
  (setq header-line-format
        (concat
         (when (eq (current-buffer) org-ilm-bqueue-active-buffer)
           (if (org-ilm-reviewing-p)
               (propertize "[R] " 'face 'error)
             (propertize "[A] " 'face 'bold)))
         (org-ilm-bqueue--name org-ilm-bqueue)
         " ("
         (symbol-name (org-ilm-bqueue--collection org-ilm-bqueue))
         ")")))

(defun org-ilm-queue-revert (&optional rebuild)
  "Revert/refresh the queue buffer. With REBUILD, reparse elements."
  (interactive "P")
  (let ((was-empty (org-ilm--bqueue-empty-p)))
    (when rebuild
      (when (yes-or-no-p "Rebuild queue? This will parse the elements again. ")
        (org-ilm--bqueue-rebuild)))
    (if (or (and was-empty (not (org-ilm--bqueue-empty-p)))
            (org-ilm--bqueue-empty-p)
            (not (vtable-current-table)))
        (org-ilm--bqueue-buffer-build)
      (vtable-revert-command))
    (org-ilm--bqueue-set-header)))

(defun org-ilm-queue-rebuild ()
  "Rebuild the queue buffer."
  (interactive)
  (setq org-ilm--bqueue-marked-objects nil)
  (org-ilm-queue-revert 'rebuild))

(defun org-ilm--bqueue-vtable-format-cell (string marked &optional missing)
  "Apply 'face property to cell STRING based in its MARKED and/or MISSING."
  (if (not (or marked missing))
      string
    (let ((face (list :inherit 'default :slant 'italic)))
      (when marked
        (setq face (-> face (plist-put :underline t) (plist-put :weight 'bold))))
      (when missing (setq face (plist-put face :foreground "#595959")))
      (propertize string 'face face))))

(defun org-ilm--bqueue-vtable-objects-func ()
  (unless (org-ilm--bqueue-empty-p)
    (let (objects)
      (ost-map-in-order
       (lambda (node rank)
         (push 
          (cons rank (gethash (ost-node-id node)
                              (org-ilm-bqueue--elements org-ilm-bqueue)))
          objects))
       org-ilm-bqueue
       (not (org-ilm-bqueue--reversed org-ilm-bqueue)))
      objects)))
  
(cl-defun org-ilm--bqueue-make-vtable (&key keymap)
  "Build bqueue vtable."
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
          (org-ilm--bqueue-vtable-format-cell index-str marked missing))))
     (:name
      "Priority"
      :width 6
      :formatter
      (lambda (data)
        (pcase-let* ((`(,marked ,priority ,missing) data)
                     (rank (car priority))
                     (quantile (cdr priority)))
          (org-ilm--bqueue-vtable-format-cell
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
          (org-ilm--bqueue-vtable-format-cell
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
          (org-ilm--bqueue-vtable-format-cell title marked missing))))
     (:name
      "Due"
      :max-width 8
      ;; :align 'right
      :formatter
      (lambda (data)
        (pcase-let ((`(,marked ,due ,missing) data))
          (org-ilm--bqueue-vtable-format-cell
           (cond
            (missing "NA")
            (due (org-add-props
                     (org-ilm--format-relative-date
                      (round due))
                     nil 'face 'bold))
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
          (org-ilm--bqueue-vtable-format-cell
           (cond
            (names (org-add-props (s-join "," names) nil 'face 'org-tag))
            (missing "NA")
            (t ""))
           marked missing)))))
   :objects-function #'org-ilm--bqueue-vtable-objects-func
   :getter
   (lambda (row column vtable)
     (let* ((rank (car row))
            (object (cdr row))
            ;; See `org-ilm--pqueue-bqueue'. Missing elements are mapped back to id.
            ;; When nil, this is a placeholder element.
            (id (org-ilm--element-id object))
            (marked (and id (member id org-ilm--bqueue-marked-objects))))
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

(cl-defun org-ilm--bqueue-buffer-create (bqueue &key active-p switch-p)
  "Create a new bqueue buffer.
This does not fill the buffer with the queue elements! For this run
`org-ilm--bqueue-buffer-build' afterwards."
  (let ((buffer (generate-new-buffer
                 (format "*Ilm Queue (%s)*" (org-ilm-bqueue--name bqueue)))))
    (with-current-buffer buffer
      (special-mode) ;; has to be first
      (setq-local org-ilm-bqueue bqueue)

      ;; Make `save-buffer' command write queue to disk.
      (setq-local write-contents-functions (list #'org-ilm--bqueue-write))
      ;; This is not necessary, but I added it because org-mem save-buffer hook
      ;; throws an error when file is not buffer visiting. But perhaps there is
      ;; some utility to having this..
      ;; (setq-local buffer-file-name (org-ilm-queue--file org-ilm-queue))
      ;; (set-visited-file-name )

      ;; Make sure that when the queue buffer is killed we update the active
      ;; buffer.
      (add-hook 'kill-buffer-hook
                (lambda ()
                  ;; Though not necessary as buffer will be killed anyway, this
                  ;; prevents the queue buffer from being reverted/rebuilt again
                  ;; before being destroyed, which is unnecessary and slow.
                  (remove-hook 'org-ilm-bqueue-active-buffer-change-hook
                               #'org-ilm-queue-revert 'local)
                  
                  (when (eq (current-buffer) org-ilm-bqueue-active-buffer)
                    (org-ilm--bqueue-set-active-buffer nil)))
                nil 'local)

      ;; Refresh when queue during changes that effect queue
      (add-hook 'org-ilm-review-next-hook
                #'org-ilm-queue-revert nil t)
      (add-hook 'org-ilm-review-quit-hook
                #'org-ilm-queue-revert nil t)
      (add-hook 'org-ilm-bqueue-active-buffer-change-hook
                #'org-ilm-queue-revert nil t)
      
      (org-ilm--bqueue-buffer-build :buffer buffer :switch-p switch-p)

      ;; This doesn't seem to activate when called in `org-ilm--bqueue-buffer-build'
      (eldoc-mode 1)

      (org-ilm-queue-mode 1)
      
      (when active-p
        (org-ilm--bqueue-set-active-buffer buffer)))
    buffer))

(cl-defun org-ilm--bqueue-buffer-build (&key buffer switch-p keymap)
  "Build the contents of the queue buffer, and optionally switch to it."
  (org-ilm--bqueue-with-buffer buffer
    (setq-local buffer-read-only nil)
    (erase-buffer)
    (goto-char (point-min))
    (if (org-ilm--bqueue-empty-p)
        (progn
          (insert (propertize "Queue empty..." 'face 'italic))
          (use-local-map (or keymap org-ilm-queue-base-map)))
      (vtable-insert (org-ilm--bqueue-make-vtable :keymap keymap)))
    (org-ilm--bqueue-set-header)
    (setq-local buffer-read-only t)
    (hl-line-mode 1)
    (eldoc-mode 1)
    (add-hook 'eldoc-documentation-functions
              #'org-ilm--collection-eldoc-element-info
              nil 'local)
    (goto-char (point-min))
    (when switch-p
      (switch-to-buffer buffer))))

(defvar org-ilm--bqueue-transient-buffer nil)

;;;; Actions

(defun org-ilm-queue-open-attachment (object)
  "Open attachment of object at point."
  (interactive (list (cdr (org-ilm--bqueue-vtable-get-object))))
  (org-ilm--attachment-open-by-id (org-ilm--bqueue-object-id object)))

(defun org-ilm-queue-open-element (object)
  "Open attachment of object at point."
  (interactive (list (cdr (org-ilm--bqueue-vtable-get-object))))
  (org-ilm--org-goto-id (org-ilm--bqueue-object-id object)))

(defun org-ilm-queue-element-mark (object &optional action)
  "Mark the object at point."
  (interactive (list (cdr (org-ilm--bqueue-vtable-get-object))))
  (unless action (setq action 'toggle))
  (cl-assert (member action '(mark unmark toggle)))
  (pcase action
    ('mark (org-ilm--bqueue-mark-objects object))
    ('unmark (org-ilm--bqueue-unmark-objects object))
    ('toggle (org-ilm--bqueue-mark-toggle-objects object)))
  (vtable-update-object (vtable-current-table) (vtable-current-object))
  (when (called-interactively-p)
    (forward-line)
    (when (eobp) (forward-line -1))))

(defun org-ilm-queue-element-unmark-backwards ()
  "Jump to previous element and unmark it."
  (interactive)
  (unless (bobp)
    (forward-line -1)
    (org-ilm-queue-element-mark (cdr (org-ilm--bqueue-vtable-get-object)) 'unmark)))

(defun org-ilm-queue-mark-by-concept (concept-id)
  "Mark all elements in queue that are part of concept with id CONCEPT-ID."
  (interactive
   (list
    (org-mem-entry-id (org-ilm--concept-select-entry))))

  ;; Alternatively, we could have used
  ;; `org-ilm--concepts-get-with-descendant-concepts' to precompute the
  ;; descendancy, but this would require a list-to-list comparison eg with
  ;; `seq-some' per object, instead of just an `assoc'.
  (dolist (object (org-ilm--bqueue-elements))
    (when (org-ilm-element-p object)
      (let ((ancestor-ids (car (org-ilm-element--concepts object))))
        (when (member concept-id ancestor-ids)
          (org-ilm--bqueue-mark-objects object)))))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-by-tag (tag)
  "Mark all elements in queue that have tag TAG."
  (interactive
   (list
    (completing-read
     "Tag: "
     (org-ilm--collection-tags (org-ilm-queue--collection org-ilm-bqueue)))))
  (dolist (object (org-ilm--bqueue-elements))
    (when (and (org-ilm-element-p object)
               (member tag (org-ilm-element--tags object)))
      (org-ilm--bqueue-mark-objects object)))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-by-scheduled (days)
  "Mark all elements in queue that are scheduled in DAYS days.
DAYS can be specified as numeric prefix arg."
  (interactive "NNumber of days: ")
  (dolist (object (org-ilm--bqueue-elements))
    (when (org-ilm-element-p object)
      (when-let ((due (* -1 (org-ilm-element--schedrel object))))
        (when (<= (round due) days) 
          (org-ilm--bqueue-mark-objects object)))))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-missing ()
  "Mark all elements that are missing, i.e. do not have an ilm-element."
  (interactive)
  (dolist (object (org-ilm--bqueue-elements))
    (unless (org-ilm-element-p object)
      (org-ilm--bqueue-mark-objects object)))
  (org-ilm-queue-revert))

(defun org-ilm-queue-mark-invert ()
  "Mark all unmarked elements, and unmark marked elements."
  (interactive)
  (let* ((marked org-ilm--bqueue-marked-objects)
         (unmarked (seq-keep
                    (lambda (element)
                      (let ((id (org-ilm-element--id element)))
                        (when (not (member id marked)) id)))
                    (org-ilm--bqueue-elements))))
    (setq-local org-ilm--bqueue-marked-objects unmarked)
    (org-ilm-queue-revert)))

(defun org-ilm-queue-mark-unmark-all ()
  "Unmark all marked elements."
  (interactive)
  (setq-local org-ilm--bqueue-marked-objects nil)
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
    (when org-ilm--bqueue-marked-objects
      (save-excursion
        (while (not done)
          (forward-line)
          (if-let* ((id (org-ilm--bqueue-vtable-get-object-id)))
              (when (member id org-ilm--bqueue-marked-objects)
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
    (when org-ilm--bqueue-marked-objects
      (save-excursion
        (while (not done)
          (if (= 0 (current-line))
              (setq done t)
            (forward-line -1)
            (if-let* ((id (org-ilm--bqueue-vtable-get-object-id)))
                (when (member id org-ilm--bqueue-marked-objects)
                  (setq done t
                        point (point)))
              (setq done t))))))
    (if point
        (goto-char point)
      (message "No marked element before this point"))))

(defun org-ilm-queue-save ()
  "Save queue to disk."
  (interactive)
  (org-ilm--bqueue-write)
  (message "Saved queue to %s" (org-ilm-queue--file org-ilm-pqueue))
  t)

(defun org-ilm-queue-delete ()
  "Delete queue file from disk."
  (interactive)
  (when (yes-or-no-p "Delete queue file from disk?")
    (let ((file (org-ilm-queue--file org-ilm-pqueue)))
      (delete-file file)
      (message "Deleted queue %s" file))))

(defun org-ilm-queue-review ()
  "Start reviewing current queue."
  (interactive)
  (org-ilm-review-start :queue-buffer (current-buffer)))

(defun org-ilm-queue-element-priority ()
  "Set the priority of the element at point, or bulk spread of marked elements."
  (interactive)
  (if org-ilm--bqueue-marked-objects
      (org-ilm--bqueue-spread-priority)
    (org-ilm-element-set-priority (org-ilm--element-from-context))
    (org-ilm-queue-revert)))

(defun org-ilm-queue-element-position ()
  "Set the position of the element at point, or bulk spread of marked elements."
  (interactive)
  (if org-ilm--bqueue-marked-objects
      (org-ilm--bqueue-spread-position)
    (ost-tree-move
     org-ilm-bqueue
     (org-ilm-element--id (org-ilm--element-from-context))
     ;; TODO Pass current position as initial position
     (car (org-ilm--bqueue-select-read org-ilm-bqueue)))
    (org-ilm-queue-revert)))

(defun org-ilm-queue-element-schedule ()
  "Set the schedule of the element at point, or bulk set of marked elements."
  (interactive)
  (let ((elements (org-ilm-bqueue--elements org-ilm-bqueue)))
    (if org-ilm--bqueue-marked-objects
        (let ((start-ts (ts-parse (org-read-date nil nil nil "Start date: ")))
              end-ts)
          (while (not end-ts)
            (let ((ts (ts-parse (org-read-date nil nil nil "End date: "))))
              (if (ts>= ts start-ts)
                  (setq end-ts ts)
                (message "End date must be after start date")
                (sleep-for 1))))
          (dolist (id org-ilm--bqueue-marked-objects)
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
              (or org-ilm--bqueue-marked-objects
                  (list (org-ilm--bqueue-vtable-get-object-id))))))
    (when (yes-or-no-p (format "Remove %s element(s) from queue?" (length ids)))
      (dolist (id ids)
        (org-ilm--bqueue-remove id))
      (org-ilm--bqueue-unmark-objects ids)
      (org-ilm-queue-revert))))

(defun org-ilm-queue-element-delete ()
  "Delete current or selected elements from the collection."
  (interactive)
  (let* ((queue-buffer (current-buffer))
         (collection (org-ilm-bqueue--collection org-ilm-bqueue))
         (ids (copy-sequence
               (or org-ilm--bqueue-marked-objects
                   (list (org-ilm--bqueue-vtable-get-object-id)))))
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
                      (org-ilm--bqueue-remove id :buffer queue-buffer)
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
    (org-ilm--bqueue-unmark-objects ids)
    (org-ilm-queue-revert)))

;;;; Sort transient

;; Transient to sort the elements by a different key, and optionally with some
;; randomness.

(transient-define-prefix org-ilm--bqueue-sort-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (org-ilm--bqueue-with-buffer org-ilm--bqueue-transient-buffer
      (append
       (let ((key (org-ilm-bqueue--key org-ilm-bqueue)))
         (pcase key
           ("prank" '("--key=priority"))
           ("sched" '("--key=schedule"))))
       (when (org-ilm-bqueue--reversed org-ilm-bqueue)
         '("--reversed"))
       (when-let ((randomness (org-ilm-bqueue--randomness org-ilm-bqueue)))
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
        (org-ilm--bqueue-with-buffer org-ilm--bqueue-transient-buffer
          (pcase key
            ("priority" (org-ilm--bqueue-sort "prank" reversed randomness))
            ("schedule" (org-ilm--bqueue-sort "sched" reversed randomness))
            (_ (user-error "Invalid key: %s" key)))
          (org-ilm-queue-revert))))
    :inapt-if-not
    (lambda ()
      (transient-arg-value "--key=" (transient-get-value))))
   ]
  )

(defun org-ilm-queue-sort ()
  (interactive)
  (setq org-ilm--bqueue-transient-buffer (current-buffer))
  (org-ilm--bqueue-sort-transient))

;;;; Priority and position spread transient

;; This transient lets the user set the "position" of an element in a queue,
;; which, when the queue belongs to the collection's priority queue, is the
;; priority of the element. If instead the queue belongs to a regular
;; `org-ilm-bqueue', this sets the position of the element in that queue. Note
;; that these operations (=setting the position in the queue) is in practice the
;; same, but in the case of the priority queue, has an obviously more permanent
;; and serious effect.

(defun org-ilm--bqueue-pspread-transient-format (extremum)
  (when-let* ((queue (transient-scope))
              (args (transient-get-value))
              (rank (transient-arg-value (concat "--" extremum "=") args)))
    (propertize
     (org-ilm-queue--position-format
      queue (1- (string-to-number rank)))
     'face 'italic)))

(defun org-ilm--bqueue-pspread-transient-read (extremum)
  (cl-assert (member extremum '("a" "b")))
  (let* ((minimum-p (string= extremum "a"))
         (queue (transient-scope))
         (rank-this (car (org-ilm--bqueue-select-read
                          (if (org-ilm-pqueue-p queue)
                              (org-ilm-pqueue--queue queue)
                            queue))))
         (rank-other (transient-arg-value
                      (concat "--" (if minimum-p "max" "min") "=")
                      (transient-args transient-current-command)))
         (rank-other (when rank-other
                       (1- (string-to-number rank-other))))
         (n-marked (length org-ilm--bqueue-marked-objects)))
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

(transient-define-prefix org-ilm--bqueue-pspread-transient (queue)
  "This transient will set prioritiy if QUEUE is a `org-ilm-pqueue'
object. If it is a `org-ilm-bqueue' object instead, the position within
that bqueue is changed."
  :refresh-suffixes t

  [:description
   (lambda ()
     (cl-etypecase (transient-scope)
       (org-ilm-pqueue "Priority spread")
       (org-ilm-bqueue "Position spread")))
   (:info
    (lambda ()
      (let ((n-materials 0)
            (n-cards 0))
        (dolist (id org-ilm--bqueue-marked-objects)
          (pcase (org-ilm--element-type (org-mem-entry-by-id id))
            ('material (cl-incf n-materials))
            ('card     (cl-incf n-cards))))
        (propertize
         (format "%s element (%s materials, %s cards)"
                 (length org-ilm--bqueue-marked-objects)
                 n-materials n-cards)
         'face 'transient-value))))
   ("a" "Min" "--min=" :always-read t
    :transient transient--do-call
    :reader (lambda (&rest _) (org-ilm--bqueue-pspread-transient-read "a")))
   (:info
    (lambda () (or (org-ilm--bqueue-pspread-transient-format "min") ""))
    :if
    (lambda () (org-ilm--bqueue-pspread-transient-format "min")))
   ("b" "Max" "--max=" :always-read t
    :transient transient--do-call
    :reader (lambda (&rest _) (org-ilm--bqueue-pspread-transient-read "b")))
   (:info
    (lambda () (or (org-ilm--bqueue-pspread-transient-format "max") ""))
    :if
    (lambda () (org-ilm--bqueue-pspread-transient-format "max")))
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
             (marked org-ilm--bqueue-marked-objects)
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
  (transient-setup 'org-ilm--bqueue-pspread-transient nil nil :scope queue))

(defun org-ilm--bqueue-spread-position (&optional priority-queue-p)
  (unless org-ilm--bqueue-marked-objects
    (user-error "No elements marked!"))
  (setq org-ilm--bqueue-transient-buffer (current-buffer))
  (org-ilm--bqueue-pspread-transient
   (if priority-queue-p
       (org-ilm--pqueue (org-ilm-bqueue--collection org-ilm-bqueue))
     org-ilm-bqueue)))

(defun org-ilm--bqueue-spread-priority ()
  (org-ilm--bqueue-spread-position 'priority-queue-p))

;;;; Queue select

;; Using a bqueue view to select a position in the queue.

(defvar org-ilm--bqueue-select-timer nil)
(defvar org-ilm--bqueue-select-buffer nil)
(defvar-local org-ilm--bqueue-select-point 1)
(defvar-local org-ilm--bqueue-select-minibuffer-window nil)

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
          (org-ilm--bqueue-select-accept-minibuffer)))

(defun org-ilm--bqueue-select-update (pos &optional prompt numnew)
  "Update preview buffer based on minibuffer INPUT."
  (when (and pos (buffer-live-p org-ilm--bqueue-select-buffer))
    (with-current-buffer org-ilm--bqueue-select-buffer
      (setq pos (ost-tree-position org-ilm-bqueue pos numnew))
      (setq header-line-format
            (concat (or prompt "Select position: ")
                    (org-ilm-queue--position-format org-ilm-bqueue (car pos))))
      (with-selected-window (get-buffer-window)
        (goto-line (1+ (car pos)))
        (hl-line-highlight)
        (recenter-top-bottom)))))

(defun org-ilm--bqueue-select-update-minibuffer ()
  (with-current-buffer org-ilm--bqueue-select-buffer
    (when (and org-ilm--bqueue-select-minibuffer-window
               (window-live-p org-ilm--bqueue-select-minibuffer-window))
      (when-let* ((element (cdr (org-ilm--bqueue-vtable-get-object)))
                  (rank (ost-rank org-ilm-bqueue (org-ilm-element--id element))))
        (with-selected-window org-ilm--bqueue-select-minibuffer-window
          (delete-minibuffer-contents)
          (insert (number-to-string (1+ rank))))))))

(defun org-ilm--bqueue-select-accept-minibuffer ()
  (with-current-buffer org-ilm--bqueue-select-buffer
    (when (and org-ilm--bqueue-select-minibuffer-window
               (window-live-p org-ilm--bqueue-select-minibuffer-window))
      (with-selected-window org-ilm--bqueue-select-minibuffer-window
        (exit-minibuffer)))))

(defun org-ilm--bqueue-select-read (bqueue &optional init-position prompt numnew)
  "Show only the preview buffer above the minibuffer during completion."
  (cl-assert (org-ilm-bqueue-p bqueue))
  (if (<= (ost-tree-size bqueue numnew) 1)
      (cons 0 0.0)
    (let ((saved-config (current-window-configuration))
          newkeys position)
      
      (unwind-protect
          (progn
            (setq init-position (ost-tree-position bqueue init-position))

            (when (and numnew (>= numnew 0))
              (let* ((largest-node (ost-select bqueue (1- (ost-size bqueue))))
                     (largest-key (ost-node-key largest-node)))
                (dotimes (i numnew)
                  (let ((key (+ largest-key i 1)))
                    (ost-tree-insert bqueue key key)
                    (push key newkeys)))))
            
            (setq org-ilm--bqueue-select-buffer
                  (org-ilm--bqueue-buffer-create bqueue))

            ;; Build element table in buffer
            (org-ilm--bqueue-buffer-build
             :buffer org-ilm--bqueue-select-buffer
             :keymap org-ilm-queue-select-map)

            ;; Add a hook that detects movement to new row, update priority in
            ;; minibuffer.
            (with-current-buffer org-ilm--bqueue-select-buffer
              (add-hook 'post-command-hook
                        (lambda ()
                          (when (not (= (point) org-ilm--bqueue-select-point))
                            (setq org-ilm--bqueue-select-point (point))
                            (org-ilm--bqueue-select-update-minibuffer)))
                        nil 'local))
      
            ;; Replace all windows with our preview buffer
            (delete-other-windows)
            (switch-to-buffer org-ilm--bqueue-select-buffer)
            (org-ilm--bqueue-select-update
             (car (ost-tree-position bqueue (or init-position 0.0)))
             prompt)

            ;; now set up minibuffer hook
            (minibuffer-with-setup-hook
                (lambda ()
                  (with-current-buffer org-ilm--bqueue-select-buffer
                    (setq org-ilm--bqueue-select-minibuffer-window
                          (active-minibuffer-window)))
                  (add-hook 'post-command-hook
                            (lambda ()
                              (when org-ilm--bqueue-select-timer
                                (cancel-timer org-ilm--bqueue-select-timer))
                              (setq org-ilm--bqueue-select-timer
                                    (run-with-idle-timer
                                     0.3 nil
                                     (lambda ()
                                       (when-let ((pos (org-ilm-queue--parse-position-str
                                                        bqueue 
                                                        (minibuffer-contents-no-properties))))
                                         (setq position pos)
                                         (org-ilm--bqueue-select-update (car pos) prompt))))))
                            nil t))
              (setq position (org-ilm-queue--position-read bqueue init-position nil prompt)))
            ) ; Unwind-protect body

        ;;; Unwind forms
        ;; restore old window setup
        (set-window-configuration saved-config)
        (dolist (key newkeys)
          (ost-tree-remove bqueue key))
        (when (buffer-live-p org-ilm--bqueue-select-buffer)
          (kill-buffer org-ilm--bqueue-select-buffer))
        (setq org-ilm--bqueue-select-buffer nil)
        ) ; Unwind-protect unwind
      
      position)))

;;; Footer

(provide 'org-ilm-bqueue-view)

;;; org-ilm-bqueue-view.el ends here
