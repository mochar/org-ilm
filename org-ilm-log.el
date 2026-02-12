;;; org-ilm-log.el --- Review logging -*- lexical-binding: t; -*-

;;; Commentary:

;; Logging reviews and events to the ilm drawer table.
;;
;; Each element contains an ILM drawer with in the review log as org tables. The
;; tables have a name property which corresponds to the collection name. When an
;; element is moved to another collection, a new table is created.
;;
;; Fields of the log table in all elements:
;; - timestamp: when it was reviewed (or created)
;; - delay: num days between timestamp and scheduled date
;; - priority: "rank-size" when it was reviewed
;; - due: the next scheduled review (date without time)
;; - state: new state after review
;; Notes:
;; - The creation of an element is also logged.
;; - The timestamp can be compared with the delay field to calculate delay or how
;; - much earlier it was reviewed.
;; - The delay+timestamp fields can be compared with the previous log's due
;;   field to calculate how much it was postponed.

;;; Code:

;;;; Requirements

(require 'ts)
(require 'org-element)

(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-pqueue)
;; (require 'org-ilm-element)
(require 'ost)

;;;; Variables

(defconst org-ilm-log-element-fields '(timestamp priority due duration action))
(defconst org-ilm-log-material-fields org-ilm-log-element-fields)
(defconst org-ilm-log-card-fields (append
                                   org-ilm-log-element-fields
                                   '(state stability difficulty)))

(defcustom org-ilm-log-card-parameter-precision 3
  "The number of decimals to store for the stability and difficulty card parameters.

Stability is measured as number of days. Negligible difference.
Difficulty is between 1 and 10. Want a bit more precision here."
  :type 'number
  :group 'org-ilm-card)

;;;; Logging

(cl-defstruct (org-ilm-log-review
               (:conc-name org-ilm-log-review--))
  "A review entry in an ilm element log drawer table."
  type collection timestamp priority due duration
  action state stability difficulty)

(defun org-ilm--log-fields (&optional type)
  "Return the log table fields of the element at point."
  (pcase (or type (org-ilm--element-type))
    ('material org-ilm-log-material-fields)
    ('card     org-ilm-log-card-fields)
    (_ (error "Not an ilm element"))))

(defun org-ilm-log-review-format-field (field)
  "Format FIELD column for use in the log."
  (pcase field
    ('stability  "S")
    ('difficulty "D")
    ('priority   "prior")
    (_           (format "%s" field))))

(defun org-ilm-log-review-format-value (review field)
  "Format the value of FIELD in `org-ilm-log-review' object REVIEW for use in the log."
  (let* ((getter (intern (concat "org-ilm-log-review--" (symbol-name field))))
         (value (funcall getter review)))
    (cond
     ((null value) "")
     ((eq field 'duration)
      (org-duration-from-minutes value 'h:mm:ss))
     ((eq field 'action)
      (format "%s" (pcase value
                     (:reschedule :sched)
                     (_ value))))
     ((eq field 'state)
      (format "%s" (pcase value
                     (:learning :learn)
                     (:relearning :relearn)
                     (_ value))))
     ((member field '(stability difficulty))
      (format (concat "%." (number-to-string org-ilm-log-card-parameter-precision) "f") value))
     ((eq field 'priority)
      (format "%s-%s" (car value) (cdr value)))
     ((eq field 'due)
      (org-ilm--ts-format-utc-date (ts-parse value)))
     (t value))))

(defun org-ilm-log-review-from-alist (alist &optional type)
  "Make an `org-ilm-log-review' object from an ALIST.
If element TYPE is omitted, infer from headline at point."
  (unless type (setq type (org-ilm--element-type)))
  (cl-assert (member type '(material card)))
  
  ;; Empty values are parsed as "" so turn them to nil
  (dolist (pair alist)
    (when (and (stringp (cdr pair)) (string= (cdr pair) ""))
      (setcdr pair nil)))
  
  (let ((collection (alist-get 'collection alist))
        (timestamp (alist-get 'timestamp alist))
        (priority (or (alist-get 'priority alist)
                      (alist-get 'prior alist)))
        (due (alist-get 'due alist))
        (duration (alist-get 'duration alist))
        (action (alist-get 'action alist))
        (state (alist-get 'state alist))
        (stability (or (alist-get 'stability alist)
                       (alist-get 'S alist)))
        (difficulty (or (alist-get 'difficulty alist)
                        (alist-get 'D alist))))
    (cl-assert (symbolp collection))
    (cl-assert (org-ilm--timestamp-is-utc-iso8601-p timestamp))
    
    (when priority
      (let* ((parts (string-split priority "-"))
             (rank (string-to-number (car parts)))
             (size (string-to-number (cadr parts))))
        (cl-assert (or (string= (car parts) "0") (> rank 0)))
        (cl-assert (or (string= (cadr parts) "0") (> size 0)))
        (setq priority (cons rank size))))
    
    (when duration
      (setq duration (org-duration-to-minutes duration)))

    (setq action (pcase (-some-> action intern)
                   (:sched :reschedule)
                   (rest rest)))
    (unless (eq action :done)
      (cl-assert (org-ilm--timestamp-is-iso8601-date-p due)))

    (pcase type
      ('card
       (setq state (pcase (-some-> state intern)
                     (:learn :learning)
                     (:relearn :relearning)
                     (rest rest)))
       (cl-assert (or (null state) (member state '(:learning :review :relearning))))
       (cl-assert (member action '(:new :done :undone :reschedule :again :hard :good :easy)))
       ;; Both are absent (new card) or both are present
       (cl-assert (eq (not stability) (not difficulty)))
       (when (and stability difficulty)
         (setq stability (string-to-number stability)
               difficulty (string-to-number difficulty))
         (cl-assert (floatp stability))
         (cl-assert (floatp difficulty))))
      ('material
       (cl-assert (member action '(:new :done :review :reschedule)))))
    
    (make-org-ilm-log-review
     :type type :collection collection :timestamp timestamp :priority priority
     :due due :duration duration :action action
     :state state :stability stability :difficulty difficulty)))

(defun org-ilm-log-review-ensure (data)
  "Make an `org-ilm-log-review' object from alist DATA, or return if already is one."
  (if (org-ilm-log-review-p data)
      data
    (org-ilm-log-review-from-alist data)))

(defun org-ilm--log-data-ensure (data)
  "Make `org-ilm-log-review' object list from alist or list of alists DATA."
  ;; If alist of one row, put it in a list
  (when (or (org-ilm-log-review-p data) (not (listp (car (car data)))))
    (setq data (list data)))
  (mapcar #'org-ilm-log-review-ensure data))

(defun org-ilm--log-beginning-of-drawer (&optional create-if-missing)
  "Move point to the beginning of the ilm drawer.
If found, return start and end positions as cons."
  (let ((bound (cdr (org-ilm--org-headline-bounds)))
        (drawer-begin-regex (rx bol (* blank) ":" (literal org-ilm-log-drawer-name) ":" (* blank) eol))
        (drawer-end-reg (rx bol (* blank) ":END:" (* blank) eol))
        (point (point)))
    (org-back-to-heading)
    (if (re-search-forward drawer-begin-regex bound 'noerror)
        (let ((begin (progn (beginning-of-line) (point)))
              (end (if-let ((p (save-excursion
                                 (re-search-forward drawer-end-reg bound 'noerror))))
                       p
                     (goto-char point)
                     (error "End of drawer missing"))))
          (cons begin end))
      (goto-char point)
      (when create-if-missing
        (org-end-of-meta-data) ; 'full)
        (cond
         ((and (eolp) (not (bolp)))
          (newline))
         ((not (and (eolp) (bolp)))
          (newline)
          (forward-line -1)))
        (save-excursion
          (insert ":" org-ilm-log-drawer-name ":")
          (newline)
          (insert ":END:"))
        (org-ilm--log-beginning-of-drawer)))))

(defun org-ilm--log-table-append (data)
  "Insert DATA at the end of the org table at point.

DATA can be an alist for one row, or a list of alists for multiple
rows. The alist maps column name to entry value.
Ensures only reviews are appended that correspond with the table's collection."
  (let* ((table      (org-element-lineage (org-element-at-point) 'table t))
         (collection (intern (org-element-property :name table))))
    (setq data (org-ilm--log-data-ensure data))
    (atomic-change-group
      (goto-char (org-table-end))
      (forward-line -1)
      (end-of-line)
      (dolist (review data 2)
        (when (eq collection (org-ilm-log-review--collection review))
          (newline)
          (insert "|")
          (dolist (field (org-ilm--log-fields))
            (insert (org-ilm-log-review-format-value review field) "|"))))
      (org-table-align))))

(defun org-ilm--log-beginning-of-next-table ()
  "Go to the next table within the ilm drawer, return table element."
  (when-let* ((drawer      (org-element-lineage (org-element-at-point) 'drawer t))
              (drawer-name (org-element-property :drawer-name drawer))
              (drawer-end  (org-element-property :contents-end drawer)))
    (when (string= drawer-name org-ilm-log-drawer-name)
      (let ((point (point)))
        (goto-char (org-table-end))
        (if (re-search-forward org-table-line-regexp drawer-end t)
            (progn
              (beginning-of-line)
              (org-element-at-point))
          (goto-char point)
          nil)))))

(defun org-ilm--log-beginning-of-collection-table (collection &optional create-if-missing)
  "Go to the beginning of the log drawer table of COLLECTION, and return table element.

With CREATE-IF-MISSING non-nil.... you know. Then returns the table element."
  (let (drawer-bounds table)
    ;; First search for the collection's table
    (save-excursion
      (setq drawer-bounds (org-ilm--log-beginning-of-drawer create-if-missing))
      (while-let ((table-x          (org-ilm--log-beginning-of-next-table))
                  (table-collection (org-element-property :name table-x)))
        (when (string= table-collection (format "%s" collection))
          (setq table table-x))))
    (cond
     (table
      (goto-char (org-element-property :post-affiliated table))
      table)
     (create-if-missing
      (atomic-change-group 
        (goto-char (cdr drawer-bounds))
        (forward-line -1)
        (end-of-line)
        (insert "\n")
        (insert (format "#+NAME: %s\n" collection))
        (save-excursion 
          (insert "|")
          (dolist (field (org-ilm--log-fields))
            (insert (org-ilm-log-review-format-field field) "|"))
          (org-table-insert-hline))
        (org-table-align)
        (org-element-at-point))))))

(defun org-ilm--log-insert (review)
  "Insert REVIEW into the log table, creating one if missing."
  (cl-assert (org-ilm-log-review-p review))
  (let ((collection (org-ilm-log-review--collection review)))
    (cl-assert collection)
    (org-ilm--log-beginning-of-collection-table collection 'create-if-missing)
    (org-ilm--log-table-append review)))

(defun org-ilm--log-log (type collection priority due action &rest review-data)
  "Log a new review for an element and return the review object.

DUE is the new scheduled review timestamp."
  (cl-assert (member type '(material card)))
  (when (ts-p due)
    (setq due (pcase type
                ('card     (org-ilm--ts-format-utc-date-maybe-time due))
                ('material (org-ilm--ts-format-utc-date due)))))
  (let* ((timestamp  (or (plist-get review-data :timestamp) (ts-now)))
         (pqueue     (org-ilm--pqueue collection))
         (priority   (ost-tree-position pqueue priority 1))
         (rank       (car priority))
         (size       (ost-size pqueue)))

    (cl-assert (ts-p timestamp))
    (setf (plist-get review-data :type) type
          (plist-get review-data :collection) collection
          (plist-get review-data :priority) (when rank (cons rank size))
          (plist-get review-data :due) due
          (plist-get review-data :action) action
          (plist-get review-data :timestamp) (org-ilm--ts-format-utc timestamp))

    (let ((review (apply #'make-org-ilm-log-review review-data)))
      (org-ilm--log-insert review)
      review)))

(defun org-ilm--log-remove (timestamp)
  "Remove table row corresponding to TIMESTAMP from element at point."
  (save-excursion
    (when-let ((drawer-bounds (org-ilm--log-beginning-of-drawer)))
      (when (re-search-forward timestamp (cdr drawer-bounds) 'noerror)
        (delete-line)
        (forward-line -1)
        (goto-char (org-table-end))
        (forward-line -1)
        (let ((row (org-element-lineage (org-element-at-point) 'table-row t)))
          (when (eq (org-element-property :type row) 'rule)
            (let ((table (org-element-lineage (org-element-at-point) 'table t))) 
              (delete-region (org-element-begin table) (org-element-end table)))))))))

(defun org-ilm--log-read (&optional collections dont-sort)
  "Read the data of the log drawer table of COLLECTION."
  (let ((collections (ensure-list collections))
        (reviews '()))
    (save-excursion
      (when (org-ilm--log-beginning-of-drawer)
        (while-let ((table-el (org-ilm--log-beginning-of-next-table))
                    (collection (intern (org-element-property :name table-el))))
          (when (or (null collections) (member collection collections))
            (let ((table (org-table-to-lisp)))
              (cl-assert (>= (length table) 3))
              (cl-assert (eq (nth 1 table) 'hline))
              (let* ((columns (mapcar #'intern (car table))))
                (dolist (row (cl-subseq table 2))
                  (let ((row-data 
                         (mapcar
                          (lambda (i)
                            (cons (nth i columns)
                                  (substring-no-properties (nth i row))))
                          (number-sequence 0 (1- (length columns))))))
                    (setf (alist-get 'collection row-data) collection)
                    (push (org-ilm-log-review-ensure row-data) reviews)))))))))
    (if dont-sort
        (nreverse reviews)
      (sort reviews :key #'org-ilm-log-review--timestamp
            :lessp (lambda (a b) (ts<= (ts-parse a) (ts-parse b)))))))

;;; Footer

(provide 'org-ilm-log)

;;; org-ilm-log.el ends here
