;;; org-ilm-utils.el --- Utilities -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'cond-let)
(require 'org-capture)
(require 'org-attach)
(require 'org-element)
(require 'org-clock)
(require 'org-id)
(require 'org-node)
(require 'org-mem)
(require 'consult)
(require 'ts)
(require 'url)

(require 'org-ilm-core)

;;;; Ilm

(defun org-ilm--where-am-i ()
  "Returns org-ilm location.

One of:
- ('collection collection)
- ('registry collection)
- ('attachment org-id collection path type)
- ('queue collection org-id)"
  (require 'org-ilm-collection)
  (require 'org-ilm-attachment)
  (require 'org-ilm-bqueue)
  (cond-let*
    ([attachment (org-ilm--attachment-data)]
     (cons 'attachment attachment))
    ([collections (org-ilm--collection-file (org-ilm--buffer-file-name))]
     [collection (org-ilm--collection-first-valid collections)]
     (if (string= (file-name-base (buffer-file-name (buffer-base-buffer))) "registry")
         (cons 'registry collection)
       (cons 'collection collection)))
    ((bound-and-true-p org-ilm-bqueue)
     (let* ((object (cdr (org-ilm--bqueue-vtable-get-object)))
            ;; TODO Can't use cl-typecase here because org-ilm-element
            ;; not yet defined
            (id (cond
                  ((stringp object) object)
                  ((org-ilm-element-p object)
                   (org-ilm-element--id object)))))
       (list 'queue (org-ilm-queue--collection org-ilm-bqueue) id)))))

(cl-generic-define-context-rewriter ilm-location (var)
  `((car (org-ilm--where-am-i)) (eql ,var)))

(cl-generic-define-context-rewriter ilm-attachment (var)
  `((nth 3 (org-ilm--attachment-data)) (eql ,var)))

(defun org-ilm-midnight-shift-minutes ()
  "Midnight shift as number of minutes past (or before) midnight."
  (let* ((whole-hours org-ilm-midnight-shift))
    (when (or (not (numberp whole-hours)) (> whole-hours 12))
      (message "Midnight shift larger than 12 hours. Cutting it to 12.")
      (setq whole-hours 12))
    (* whole-hours 60)))

;;;; Time and date

(defun org-ilm--ts-to-midnight (ts)
  "Set time to midnight"
  (ts-adjust 'hour (- (ts-hour ts))
             'minute (- (ts-min ts))
             'second (- (ts-sec ts)) ts))

(defun org-ilm--ts-diff-rounded-days (ts1 ts2)
  (plist-get
   (ts-human-duration
    (ts-difference (org-ilm--ts-to-midnight ts1)
                   (org-ilm--ts-to-midnight ts2)))
   :days))

(defun org-ilm--ts-today ()
  (org-ilm--ts-to-midnight (ts-now)))

(defun org-ilm--current-date-utc ()
  "Current date in UTC as string."
  (format-time-string "%Y-%m-%d" (current-time) t))

(defun org-ilm--interval-to-schedule-string (interval)
  "Turn INTERVAL (days) into a date string used in in org headline SCHEDULED."
  (format-time-string
   (org-time-stamp-format)
   (ts-unix (ts-adjust 'day interval (ts-now)))))

(defun org-ilm--timestamp-is-format-p (timestamp format &optional timezone)
  "Check if string TIMESTAMP has the required FORMAT with optional TIMEZONE."
  (cl-assert (and (stringp timestamp) (stringp format)))
  (let ((ts (ts-parse timestamp)))
    (string= timestamp (format-time-string format (ts-unix ts) timezone))))

(defun org-ilm--timestamp-is-utc-iso8601-p (timestamp)
  "Check if string TIMESTAMP is formatted according to ISO 8601 with UTC timezone."
  (org-ilm--timestamp-is-format-p timestamp "%FT%TZ" "UTC0"))

(defun org-ilm--timestamp-is-iso8601-date-p (timestamp)
  "Check if string TIMESTAMP is ISO 8601 formatted calendar date (YYYY-MM-DD)."
  (org-ilm--timestamp-is-format-p timestamp "%Y-%m-%d"))

(defun org-ilm--ts-format-utc (ts)
  (if (ts-p ts)
      (format-time-string "%FT%TZ" (ts-unix ts) "UTC0")
    (cl-assert (org-ilm--timestamp-is-utc-iso8601-p ts))
    ts))

(defun org-ilm--ts-format-utc-date (ts)
  (if (ts-p ts)
      (ts-format "%Y-%m-%d" ts)
    (cl-assert (org-ilm--timestamp-is-iso8601-date-p ts))
    ts))

(defun org-ilm--ts-format-utc-date-maybe-time (ts &optional ignore-time)
  (cl-assert (ts-p ts))
  (if (or ignore-time
          (and (= (ts-hour ts) 0) (= (ts-minute ts) 0)))
      (ts-format "%Y-%m-%d" ts)
    (ts-format "%Y-%m-%d %H:%M" ts)))

(defun org-ilm--format-relative-date (difference)
  "Return relative date string for DIFFERENCE.
DIFFERENCE should be an integer number of days, positive for
dates in the past, and negative for dates in the future."
  (cond ((> difference 0)
         (format "%sd ago" difference))
        ((< difference 0)
         (format "in %sd" (* -1 difference)))
        (t "today")))

;;;; Elisp

(defun org-ilm--select-alist (alist &optional prompt formatter ordered)
  "Prompt user for element in alist to select from.
If empty return nil, and if only one, return it."
  (pcase (length alist)
    (0 nil)
    (1 (nth 0 alist))
    (_
     (let* ((choices
             (mapcar
              (lambda (thing)
                (let ((second (if (consp (cdr thing))
                                  (car (cdr thing))
                                (cdr thing))))
                  (cons
                   (if formatter
                       (funcall formatter thing)
                     (format "%s %s"
                             (propertize
                              (format "%s" (car thing))
                              'face '(:weight bold))
                             (propertize
                              (format "%s" second)
                              'face '(:slant italic))))
                   thing)))
              alist))
            (choice (completing-read
                     (or prompt "Select: ")
                     (if ordered
                         ;; https://emacs.stackexchange.com/a/8177/49372
                         (lambda (string pred action)
                           (if (eq action 'metadata)
                               `(metadata (display-sort-function . ,#'identity))
                             (complete-with-action action choices string pred)))
                       choices)
                     nil t))
            (item (cdr (assoc choice choices))))
       item))))

(defun org-ilm--invert-alist (alist)
  "Turn car into cdr and vice versa for each cons in ALIST."
  (mapcar (lambda (pair)
            (cons (cdr pair) (car pair)))
          alist))

(cl-defun org-ilm--add-hook-once (hook function &optional depth (local t))
  "Add FUNCTION to HOOK and remove it after its first execution.

HOOK is the hook to which FUNCTION will be added.
FUNCTION is the function to run once in the hook.
DEPTH controls where FUNCTION is placed in HOOK and defaults to 0.
LOCAL controls whether to add HOOK buffer-locally and defaults to t.

The returned function can be used to call `remove-hook' if needed."
  (letrec ((hook-function (lambda () (remove-hook hook hook-function local) (funcall function))))
    (add-hook hook hook-function depth local)
    hook-function))

(defun org-ilm--generate-text-snippet (text)
  "Cleanup TEXT so that it can be used in as an org header."
  (let* ((text (replace-regexp-in-string "\n" " " text))
         (text (org-link-display-format text)) ;; Remove org links
         (text (replace-regexp-in-string org-footnote-re "" text)) ;; Remove footnotes (errors otherwise (from ox.el))
         (text (string-trim text))) ;; Trim whitespace
    (substring text 0 (min 70 (length text)))))

(defun org-ilm--buffer-file-name ()
  "Like `buffer-file-name' but support indirect buffers."
  (or buffer-file-name (buffer-file-name (buffer-base-buffer))))

(defun org-ilm--ov-delete (ov &rest _)
  (delete-overlay ov))

(defun org-ilm--spread (size min max &optional random-p)
  "Return indices of SIZE items spread between indices [MIN, MAX].
Note that eg (3, 6) consists of 4 positions."
  (when (< (1+ (- max min)) size)
    (error "Cannot fit %s items between %s and %s" size min max))
  (let ((order (number-sequence (1- size) 0 -1)))
    (when random-p (setq order (org-ilm--list-shuffle order)))
    (mapcar 
     (lambda (i)
       (round (+ min
                 (* (/ (float (nth i order)) (max 1 (1- size)))
                    (- max min)))))
     (number-sequence 0 (1- size)))))

(cl-defun org-ilm--add-hook-once (hook function &optional depth (local t))
  "Add FUNCTION to HOOK and remove it after its first execution.
If HOOK passes arguments, FUNCTION will receive them.

HOOK is the hook to which FUNCTION will be added.
FUNCTION is the function to run once in the hook.
DEPTH controls where FUNCTION is placed in HOOK and defaults to 0.
LOCAL controls whether to add HOOK buffer-locally and defaults to t.

The returned function can be used to call `remove-hook' if needed."
  (letrec ((wrapper (lambda (&rest args)
                      (remove-hook hook wrapper local)
                      (apply function args))))
    (add-hook hook wrapper depth local)
    wrapper))

(cl-defun org-ilm--alist-to-plist (alist &key upcase remove)
  "Convert association list ALIST into the equivalent property-list form.

Direct copy from mm-decode.el"
  (let (plist)
    (while alist
      (let ((el (car alist)))
        (unless (member (car el) remove)
	  (setq plist (cons (cdr el)
                            (cons (if upcase (upcase (car el)) (car el))
                                  plist)))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(defun org-ilm--list-shuffle (list)
  "Return a new list with the elements of LIST randomly shuffled."
  (let ((vec (vconcat list)))       ; convert to vector for easy swapping
    (cl-loop for i from (1- (length vec)) downto 1
             do (cl-rotatef (aref vec i)
                            (aref vec (random (1+ i)))))
    (append vec nil)))

(defun org-ilm--pdf-mode-p ()
  "Return non-nil if current major mode is `pdf-view-mode' or `pdf-virtual-view-mode'."
  (when (member major-mode '(pdf-view-mode pdf-virtual-view-mode)) t))


;;;; Org

(defun org-ilm--org-headline-bounds ()
  "Return (start . end) char that constitutes the headline at point,
 excluding child headlines."
  (save-excursion
    (let ((start (org-back-to-heading))
          (end (progn
                 (org-next-visible-heading 1)
                 (point))))
      (cons start end))))

(defun org-ilm--org-narrow-to-header ()
  "Narrow to headline and content up to next heading or end of buffer."
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))

(defun org-ilm--org-id-p (string)
  "Is STRING an org-id? Return org-mem entry, else nil."
  ;; (org-id-find string)
  (org-mem-entry-by-id string))

(defun org-ilm--org-headline-at-point ()
  "Return headline at point."
  (let ((element (org-element-at-point))
        (headline-text (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
    (when (eq (car element) 'headline)
      
      ;; TODO Seems that when buffer local priority ranges differ from global
      ;; ones, org-element returns nil as priority. Also sometimes priority
      ;; returned as their numerical value, sometimes as char number. Solution
      ;; is to parse the header manually.

      ;; (with-temp-buffer
      ;;   (insert headline-text)
      ;;   (goto-char 0)
      ;;   (looking-at org-priority-regexp)
      ;;   (let ((priority (match-string-no-properties 2)))
      ;;     (when priority
      ;;       (setq priority (string-to-number priority)))
      ;;     (setq element (org-element-put-property element :priority priority))))

      (when (string-match org-priority-regexp headline-text)
        (let ((priority (match-string-no-properties 2 headline-text)))
          (when priority
            (setq priority (string-to-number priority)))
          (setq element (org-element-put-property element :priority priority))))
        
      ;; (save-excursion
      ;;   (beginning-of-line)
      ;;   ;; Previously used org-entry-get but it just gives weird numbers
      ;;   (looking-at org-priority-regexp)
      ;;   (let ((priority (match-string-no-properties 2)))
      ;;     (when priority
      ;;       (setq priority (string-to-number priority)))
      ;;     (setq element (org-element-put-property element :priority priority))))

      element
      )))

(defun org-ilm--org-goto-id (org-id)
  ;; (let ((loc (org-id-find org-id 'marker)))
  ;;   (pop-to-buffer (marker-buffer loc))
  ;;   (goto-char loc)))
  (org-node-goto-id org-id))

(defmacro org-ilm--org-with-headline-contents-visible (&rest body)
  "Necessary when parsing hidden/collapsed data within headline."
  (declare (debug (body)) (indent 1))
  ;; Need to save outline visiblity, otherwise will unfold headers.
  ;; Furthermore, need to set USE-MARKERS to non-nil, otherwise it causes weird
  ;; folding problems.
  `(org-save-outline-visibility 'use-markers
     (org-fold-show-entry)
     ,@body))

;; TODO Export jump logic in own function
(defmacro org-ilm--org-with-point-at (thing &rest body)
  "THING should be an org-id or nil.

Note: Previously used org-id-find but it put point above
headline. org-mem takes me there directly.

Alternatively org-id-goto could be used, but does not seem to respect
save-excursion."
  (declare (debug (body)) (indent 1))
  `(cond
    ;; If id is that of headline at point, dont jump, for performance but also
    ;; so that org-mem is not required to have parsed the headline (useful in
    ;; capture).
    ((or (null ,thing) (string= ,thing (org-id-get)))
     ;; Reveal entry contents, otherwise run into problems parsing the metadata,
     ;; such as with org-srs drawer.
     (org-ilm--org-with-headline-contents-visible ,@body))
    ((stringp ,thing)
     (when-let* ((org-id ,thing)
                 (entry (org-mem-entry-by-id org-id))
                 (file (org-mem-entry-file-truename entry))
                 (buf (or (find-buffer-visiting file)
                          (find-file-noselect file))))
       (with-current-buffer buf
         ;; We need to widen the buffer because `find-buffer-visiting' might
         ;; return an active, narrowed buffer.
         (org-with-wide-buffer
          ;; Jump to correct position
          (goto-char (org-find-property "ID" org-id))
          
          ;; Reveal entry contents, otherwise run into problems parsing the
          ;; metadata, such as with org-srs drawer.
          (org-ilm--org-with-headline-contents-visible ,@body)))))
    (t (error "THING should be org-id or nil"))))

(defun org-ilm--org-headline-element-from-id (org-id)
  "Return headline element from org id."
  (save-excursion
    (org-ilm--org-with-point-at
     org-id
     (org-ilm--org-headline-at-point))))

(defun org-ilm--org-attach-dir-from-id (org-id)
  "Return the attachment directory of element with ORG-ID

Note that we cannot just use org-attach utilities to find this because we might be using buffer local variables or dir-locals.el"
  (save-excursion
    (org-ilm--org-with-point-at
     org-id
     (org-attach-dir))))

(defun org-ilm--org-id-from-string (string)
  "Interpret STRING as org-id, link with org-id, or else nil."
  (cond
   ((org-ilm--org-id-p string) string)
   ((string-match org-link-bracket-re string)
    (let ((link (match-string 1 string)))
      (when (string-prefix-p "id:" link)
        (substring link 3))))))

(defun org-ilm--org-headline-from-thing (&optional thing assert)
  "Return headline from THING: org-id, org-id link, headline, nil -> headline at point.
With non-nil ASSERT, assert headline must be found, else return nil."
  (let ((headline (cond
                   ;; Nil, try getting it from point
                   ((not thing)
                    (org-ilm--org-headline-at-point))
                   ;; Org-element, check if headline
                   ((eq (org-element-type thing) 'headline)
                    thing)
                   ;; String, check if is or contains id
                   ((stringp thing)
                    (org-ilm--org-headline-element-from-id
                     (org-ilm--org-id-from-string thing))))))
    (when assert (cl-assert headline))
    headline))

(defun org-ilm--clock-in-continue-last ()
  "Edit out the clock-out time of the last entry and continue as if never clocked out."
  (when (org-clocking-p)
    (org-clock-out))
  (save-excursion
    (org-back-to-heading 'invisble-ok)
    (save-restriction
      (org-ilm--org-narrow-to-header)
      (when (and
             (re-search-forward org-clock-line-re nil 'noerror)
             (re-search-forward (org-re-timestamp 'inactive) (line-end-position) 'noerror))
        (delete-region (point) (line-end-position))
        (let ((org-clock-in-resume t)
              (org-clock-idle-time nil))
          (org-clock-in))))))

(defun org-ilm--org-priority-set (value)
  "Set the priority of element at point to VALUE.

This function was made to deal with a bug in `org-priority' where a
value of 32 is read as the character ?\s (32) which causes it to remove
the priority, or error if there is no priority."
  (when (numberp value) (setq value (number-to-string value)))
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-priority-regexp)
      (org-priority 'remove))
    (let ((case-fold-search nil))
      (looking-at org-todo-line-regexp))
    (if (match-end 2)
        (progn
	  (goto-char (match-end 2))
	  (insert " [#" value "]"))
      (goto-char (match-beginning 3))
      (insert "[#" value "] "))
    (message "Priority set to %s" value)))

(cl-defun org-ilm--org-schedule (&key timestamp interval-minutes interval-days ignore-time)
  "Call org-schedule with TIMESTAMP."
  (cond
   ((not timestamp)
    (when interval-days
      (setq interval-minutes (* 60 24 interval-days)
            ignore-time t))
    (if (not interval-minutes)
        (error "need timestamp or interval")
      (cl-assert (and (numberp interval-minutes) (>= interval-minutes 0)))
      (setq timestamp (ts-adjust 'minute interval-minutes (ts-now)))))
   ((stringp timestamp)
    (setq timestamp (ts-parse timestamp)))
   ((ts-p timestamp))
   (t (error "timestamp invalid format")))

  (org-schedule
   nil
   (if (or ignore-time
           (and (= (ts-hour timestamp) 0) (= (ts-minute timestamp) 0)))
       (ts-format "%Y-%m-%d" timestamp)
     (ts-format "%Y-%m-%d %H:%M" timestamp))))

(defun org-ilm--org-logbook-parse (logbook)
  "Parses the Org contents of a logbook drawer entry."
  (let ((contents (org-element-contents logbook)))
    (mapcar
     ;; If more data is needed, see:
     ;; https://orgmode.org/worg/dev/org-element-api.html
     ;; See: Timestamp, Clock
     (lambda (clock)
       (let* ((timestamp (org-element-property :value clock))
              (stamps (split-string (org-element-property :raw-value timestamp) "--")))
         (list
          :duration-minutes
          (when-let ((duration (org-element-property :duration clock)))
            (org-duration-to-minutes duration))
          :status (org-element-property :status clock)
          :timestamp-start (ts-parse-org (nth 0 stamps))
           ;; May be nil if active clock
          :timestamp-end (when-let ((s (nth 1 stamps))) (ts-parse-org s))
          :day-end (org-element-property :day-end timestamp)
          :day-start (org-element-property :day-start timestamp)
          :month-end (org-element-property :month-end timestamp)
          :month-start (org-element-property :month-start timestamp)
          :year-end (org-element-property :year-end timestamp)
          :year-start (org-element-property :year-start timestamp))))
     contents)))
    
(defun org-ilm--org-logbook-read (&optional headline)
  "Reads and parses the LOGBOOK drawer of heading at point.

If `HEADLINE' is passed, read it as org-property."
  (unless headline
    (setq headline (save-excursion (org-back-to-heading) (org-element-at-point))))
  (let* ((begin (org-element-property :begin headline))
         (end (save-excursion
                (outline-next-heading)
                (point))))
    (when (and begin end)
      (save-excursion
        (save-restriction
          (narrow-to-region begin end)
          (when-let* ((drawers (org-element-map
                                   (org-element-parse-buffer)
                                   'drawer #'identity))
                      (logbook (seq-find
                                (lambda (drawer)
                                  (string-equal
                                   (org-element-property :drawer-name drawer) "LOGBOOK"))
                                drawers)))
            (org-ilm--org-logbook-parse logbook)))))))

(defun org-ilm--org-delete-headline (&optional delete-attach-dir)
  (when delete-attach-dir (org-attach-delete-all 'force))
  (org-mark-subtree)
  (delete-region (region-beginning) (region-end)))

(defun org-ilm--org-add-todo-face (state)
  (let* ((org-done-keywords org-done-keywords-for-agenda)
         (face (org-get-todo-face state)))
    (when face
      (add-text-properties 0 (length state) (list 'face face) state))
    state))

(defun org-ilm--org-capture-target-buffer (target)
  "Returns the buffer of TARGET."
  (require 'org-capture)
  (let ((org-capture-plist nil))
    (org-capture-set-target-location target)
    ;; TODO Return :pos as well?
    (org-capture-get :buffer)))

(defun org-ilm--org-capture-programmatic (target text type &optional properties)
  "Programmatically insert TEXT into TARGET without a capture buffer.

TARGET is a valid org-capture target specification.
TEXT is the string content to insert.
TYPE is the capture type: 'entry (default), 'item, 'plain, or 'table-line.
PROPERTIES is an optional plist of extra behavior (e.g., '(:prepend t))."
  (require 'org-capture)
  (let ((org-capture-plist (append (list :template text) properties)))
    ;; Adds location and buffer to org-capture-plist
    (org-capture-set-target-location target)

    (with-current-buffer (org-capture-get :buffer)
      (org-with-wide-buffer
       (goto-char (org-capture-get :pos))

       (condition-case err
           (cond
            ((eq type 'entry) (org-capture-place-entry))
            ((eq type 'item) (org-capture-place-item))
            ((eq type 'plain) (org-capture-place-plain-text))
            ((eq type 'table-line) (org-capture-place-table-line)))
         (error (error "Org-ilm failed to place capture: %s" err)))
       
       (save-buffer)))))

;;;; Org-node / org-mem

(defun org-ilm--node-read-candidate-state-only (states &optional prompt blank-ok initial-input)
  "Like `org-node-read-candidate' but for nodes with STATES todo-state only."
  (let* ((states-list (if (stringp states) (list states) states))
         (choice
          (completing-read (or prompt "Node: ")
                           (if blank-ok #'org-node-collection-main
                             #'org-node-collection-basic)
                           (lambda (name node)
                             (member (org-mem-todo-state node) states-list))
                           ()
                           initial-input
                           (if org-node-alter-candidates 'org-node-hist-altered
                             'org-node-hist))))
    (gethash choice org-node--candidate<>entry)))

(defun org-ilm--org-mem-ancestry-ids (entry-or-id &optional with-root-str)
  "Return org ids of ancestors of entry."
  (when-let ((entry (if (stringp entry-or-id)
                        (org-mem-entry-by-id entry-or-id)
                      entry-or-id)))
    (let ((ancestry
           (delq nil ;; Delete nils returned when no org-id
                 (mapcar (lambda (x)
                           (car (last x)))
                         ;; cdr to skip itself
                         (cdr (org-mem-entry-crumbs entry))))))
      (if with-root-str (cons "ROOT" ancestry) ancestry))))

(defun org-ilm--org-mem-get-entry-ensured (&optional id)
  "Returns `org-mem-entry' at point, or if specified, with ID.

If not found in the org-mem cache, this function will then try to force org-mem
to parse the headline."
  ;; TODO https://github.com/meedstrom/org-mem/issues/31
  (or (if id (org-mem-entry-by-id id) (org-node-at-point))
      (progn
        (org-ilm--org-with-point-at id
          (org-mem-updater-ensure-id-node-at-point-known))
        (org-mem-entry-by-id id))))

;; TODO this doesnt properly take care of inherited properties
;; https://github.com/meedstrom/org-mem/issues/31
(defun org-ilm--org-mem-update-cache-after-capture (extent)
  (with-current-buffer (marker-buffer org-capture-last-stored-marker)
    (pcase extent
      ('file
       (org-mem-updater-ensure-buffer-file-known))
      ('entry
       (save-excursion
         (goto-char (marker-position org-capture-last-stored-marker))
         (org-mem-updater-ensure-id-node-at-point-known)))
      (_ (error "EXTENT must be one of 'file or 'entry")))))

(defun org-ilm--org-mem-refs (&optional entry types)
  (let* ((entry (or entry (org-node-at-point))))
    (cl-loop for ref in (org-mem-entry-roam-refs entry)
             for type = (gethash ref org-mem--roam-ref<>type)
             when (or (null types) (member type types))
             collect (if type (concat type ":" ref) ref))))

(defun org-ilm--org-mem-website-refs (&optional entry)
  (org-ilm--org-mem-refs entry '("http" "https")))

(defun org-ilm--org-mem-cite-refs (&optional entry)
  (seq-keep
   (lambda (ref)
     (when (s-starts-with-p "@" ref)
       (substring ref 1)))
   (org-ilm--org-mem-refs entry)))

(defun org-ilm--org-mem-title-full (entry)
  (cl-assert (org-mem-entry-p entry))
  (pcase-let* ((affix (funcall org-node-affixation-fn entry (org-mem-entry-title entry)))
               (`(,title ,prefix ,suffix) affix)
               (title (concat prefix title suffix)))
    title))

(defun org-ilm--entry-media-sources (&optional entry)
  "Return all sources of media of ENTRY or entry at point."
  (setq entry (or entry (org-node-at-point)))
  (cl-assert (org-mem-entry-p entry))
  (let* ((url (org-mem-entry-property "URL" entry))
         (path (org-mem-entry-property "PATH" entry))
         (refs (org-ilm--org-mem-website-refs entry))
         (attachments
          (org-ilm--org-with-point-at (org-mem-entry-id entry)
            (when-let ((attach-dir (org-attach-dir)))
              (seq-filter
               (lambda (attachment)
                 (member (file-name-extension attachment)
                         (append org-media-note--video-types
                                 org-media-note--audio-types)))
               (org-attach-file-list attach-dir))))))
    (cl-remove-duplicates
     (delq nil (append (list url path) refs attachments))
     :test #'string=)))

;;;; Bibtex

(defun org-ilm--format-bibtex-entry (entry &optional key)
  "Format a parsebib ENTRY alist or hash entry as a BibTeX string.
ENTRY may be an alist or association list with keys like \"=type=\", \"=key=\" and field names.
KEY may be the key to use instead of =key=."
  (let* ((key (or key (cdr (assoc "=key=" entry))))
         (type (cdr (assoc "=type=" entry)))
         (fields (cl-remove-if
                  (lambda (f) (string-prefix-p "=" (car f)))
                  entry))
         (pad 2))
    (concat "@"
            (or type "misc")
            "{"
            (or key "")
            ",\n"
            (mapconcat
             (lambda (pair)
               (format "%s%s = {%s}" 
                       (make-string pad ?\s)
                       (car pair) 
                       (cdr pair)))
             fields
             ",\n")
            "\n}\n")))


;;;; Consult

(defun org-ilm--consult-buffer-select ()
  "Select a buffer as if using `consult-buffer'."
  (let* ((captured-buffer nil)
         ;; Override the display function to capture the buffer/file
         (consult--buffer-display (lambda (buf &optional norecord)
                                    (setq captured-buffer 
                                          (if (stringp buf)
                                              (get-buffer-create buf)
                                            buf)))))
    (consult-buffer)
    captured-buffer))

;;;; Math and stats

;; Functions to work with e.g. the beta distribution.
;; TODO Out of laziness this code was mostly generated by Claude. Seems ok from
;; some experiments but need to go through it properly.

(defun org-ilm--round-float (x decimals)
  "Round float X to DECIMALS decimals."
  (let ((factor (expt 10 decimals)))
    (/ (round (* factor x)) (float factor))))

(defun org-ilm--random-uniform ()
  "Generate a uniform random number between 0 and 1."
  (/ (random 1000000) 1000000.0))

(defun org-ilm--mean (&rest numbers)
  "Return the mean of NUMBERS."
  (/ (apply #'+ numbers)
     (float (length numbers))))

(defun org-ilm--random-exponential (rate)
  "Generate an exponential random variable with given rate parameter."
  (- (/ (log (org-ilm--random-uniform)) rate)))

(defun org-ilm--random-gamma (shape)
  "Generate a gamma-distributed random variable with given shape parameter.
Uses Marsaglia and Tsang's method for shape >= 1, and Ahrens-Dieter for shape < 1."
  (cond
   ((< shape 1)
    ;; For shape < 1, use Ahrens-Dieter acceptance-rejection
    (let ((c (/ 1.0 shape))
          (d (- 1.0 shape)))
      (catch 'done
        (while t
          (let* ((u (org-ilm--random-uniform))
                 (v (org-ilm--random-uniform))
                 (x (expt u c))
                 (y (* v (expt x d))))
            (when (<= (+ x y) 1.0)
              (let ((z (- (log v) (* d (log x)))))
                (when (<= z (* shape (1- x)))
                  (throw 'done (* shape (expt (org-ilm--random-uniform) c)))))))))))
   
   ((= shape 1)
    ;; For shape = 1, gamma distribution is exponential
    (org-ilm--random-exponential 1.0))
   
   (t
    ;; For shape > 1, use Marsaglia and Tsang's method
    (let* ((d (- shape (/ 1.0 3.0)))
           (c (/ 1.0 (sqrt (* 9.0 d)))))
      (catch 'done
        (while t
          (let* ((x (org-ilm--random-normal))
                 (v (+ 1.0 (* c x))))
            (when (> v 0)
              (let* ((v (* v v v))
                     (u (org-ilm--random-uniform))
                     (x2 (* x x)))
                (when (or (< u (- 1.0 (* 0.0331 x2 x2)))
                          (< (log u) (+ (* 0.5 x2) (* d (- 1.0 v (log v))))))
                  (throw 'done (* d v))))))))))))

(defun org-ilm--random-normal ()
  "Generate a standard normal random variable using Box-Muller transform."
  (let ((u1 (org-ilm--random-uniform))
        (u2 (org-ilm--random-uniform)))
    (* (sqrt (* -2.0 (log u1)))
       (cos (* 2.0 float-pi u2)))))

(defun org-ilm--random-poisson (lambd)
  "Generate a Poission random variable using Knuth's algorithm."
  (let ((l (exp (- lambd)))
        (k 0)
        (p 1.0))
    (while (> p l)
      (setq k (1+ k))
      (setq p (* p (org-ilm--random-uniform))))
    (1- k)))

(defun org-ilm--random-beta (alpha beta &optional seed)
  "Generate a beta-distributed random variable with parameters ALPHA and BETA.
If SEED is provided, sets the random seed first for reproducible results."
  (when seed
    (unless (stringp seed)
      (error "Seed must be a string."))
    (random seed))
  
  (cond
   ;; Special cases for efficiency
   ((and (= alpha 1) (= beta 1))
    ;; Beta(1,1) is uniform
    (org-ilm--random-uniform))
   
   ((= alpha 1)
    ;; Beta(1,β) = 1 - U^(1/β)
    (- 1.0 (expt (org-ilm--random-uniform) (/ 1.0 beta))))
   
   ((= beta 1)
    ;; Beta(α,1) = U^(1/α)
    (expt (org-ilm--random-uniform) (/ 1.0 alpha)))
   
   (t
    ;; General case: use gamma method
    (let ((x (org-ilm--random-gamma alpha))
          (y (org-ilm--random-gamma beta)))
      (/ x (+ x y))))))

(defun org-ilm--beta-from-mean-sd (mean sd)
  "Compute Alpha and Beta for a Beta distribution given MEAN and standard deviation SD."
  (unless (and (> mean 0) (< mean 1))
    (error "Mean must be between 0 and 1"))
  (unless (> sd 0)
    (error "Standard deviation must be positive"))
  (let ((var (* sd sd)))  ;; convert SD to variance
    (let ((alpha (- (/ (* mean (- 1 mean)) var) mean)))
      (let ((beta (* alpha (/ (- 1 mean) mean))))
        (list alpha beta)))))

(defun org-ilm--beta-mode (beta)
  (let ((alpha (car beta))
        (beta (cdr beta)))
    (/ (- alpha 1) (+ alpha beta -2))))

;;;; Transient

(defun org-ilm--transient-set-target-value (target-key new-value)
  "Finds the target argument by its key and sets its internal value slot."
  (let* (;; Get the list of currently displayed suffix objects (internal variable)
         (all-suffixes transient--suffixes)
         ;; Find the target object by its key
         (target-obj (cl-find target-key all-suffixes
                             :key (lambda (s) (oref s key))
                             :test #'equal)))
    ;; Check if it's a valid object and manually set the 'value' slot (internal slot)
    (when (cl-typep target-obj 'transient-infix)
      (oset target-obj value new-value))))

;;;; Web

(defun org-ilm--slugify-title (title)
  "From TITLE, make a filename slug meant to look nice as URL component.

This is a literal copy-paste from the function
`org-node-slugify-for-web' of the `org-node' package. Hope that's ok..."
  (thread-last title
               (org-link-display-format)
               (string-glyph-decompose)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (string-glyph-compose)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))

(defun org-ilm--get-page-title (url &optional slugify)
  "Get title of web page by URL, or `nil' if not found.

Uses various utilities from `url.el'."
  (let (web-title-str coding-charset)
    (with-current-buffer (url-retrieve-synchronously url)
      ;; find title by grep the html code
      (goto-char (point-min))
      (when (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
        (setq web-title-str (match-string 1)))
      ;; find charset by grep the html code
      (goto-char (point-min))
      ;; downcase the charaset. e.g, UTF-8 is not acceptible for emacs, while utf-8 is ok.
      (setq coding-charset
            (or (when (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
                  (downcase (match-string 1)))
                "utf-8"))
      ;; decode the string of title.
      (when web-title-str
        (setq web-title-str (string-trim (decode-coding-string web-title-str (intern coding-charset)))))
      (kill-buffer))
    (if slugify
        (org-ilm--slugify-title web-title-str)
      web-title-str)))

(defun org-ilm--get-website-as-org (url)
  "Download the HTML content from URL and convert to Org with Defuddle and Pandoc."
  (let ((default-directory (file-name-directory org-ilm-convert-defuddle-path)))
    (with-temp-buffer
      (insert
       (with-output-to-string
         (call-process
          org-ilm-convert-node-path
          nil standard-output nil
          org-ilm-convert-defuddle-path
          "url" url "markdown")))
      (call-process-region (point-min) (point-max)
                           "pandoc" t t nil
                           "-f" "markdown" "-t" "org"
                           "--wrap=preserve")
      (buffer-string))))


;;; Footer

(provide 'org-ilm-utils)

;;; org-ilm-utils.el ends here
