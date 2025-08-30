;;; org-ilm.el --- Incremental learning mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: html, org

;;; Commentary:

;; This package extends org-mode to allow for incremental processing of reading,
;; writing, listening, and watching materials.

;;; Code:

;;;; Requirements
(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'org-element)
(require 'org-ql)
(require 'cl-lib)
(require 'dash)

;;;; Customization
(defgroup org-ilm nil
  "Incremental learning mode."
  :group 'org
  :prefix "org-ilm-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom org-ilm-collections-alist '((ilm . "~/org/ilm.org"))
  "Alist mapping collection name to path to its org file."
  :type '(alist :key-type symbol :value-type file)
  :group 'org-ilm)

(defcustom org-ilm-id-from-attachment-path-func 'org-ilm-infer-id-from-attachment-path
  "Function that accepts a path to an attachment file and returns the Org id of the header."
  :type 'function
  :group 'org-ilm)

(defcustom org-ilm-import-default-method 'cp
  "Default import method, or `nil' to always ask."
  :type '(choice (const :tag "Nil" nil)
                 (const :tag "Move" 'mv)
                 (const :tag "Copy" 'cp))
  :group 'org-ilm)

(defcustom org-ilm-schedule-max-days 60
  "Maximum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-schedule-min-days 1
  "Minimum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

;;;; Variables

(defface org-ilm-face-extract
  '((t :background "yellow"))
  "Face used to highlight extracts.")

(defvar org-ilm-queue nil
  "List of org headings that form the queue.")

(defvar org-ilm-map (make-sparse-keymap)
  "Keymap for `org-ilm-global-mode'.")
  

;;;; Minor mode

;;;###autoload
(define-minor-mode org-ilm-global-mode
  "Prepare some hooks and advices some functions."
  :init-value nil
  :global t
  :lighter nil ;; String to display in mode line
  :group 'org-ilm
  :keymap org-ilm-map
  (if org-ilm-global-mode
      ;; Enable
      (progn
        (add-hook 'org-mode-hook #'org-ilm-prepare-buffer)
        )
    ;; Disable
    (remove-hook 'org-mode-hook #'org-ilm-prepare-buffer)
    ))

;;;; Commands
(defun org-ilm-import-website (url)
  ""
  (interactive "sURL: "))

(defun org-ilm-extract ()
  "Extract text in region to attachment Org file that is the child heading of current entry."
  (interactive)
  (unless (use-region-p) (user-error "No region active."))
  (let* ((file-buf (current-buffer))
         (file-path buffer-file-name)
         (file-name (file-name-sans-extension
                     (file-name-nondirectory
                      file-path)))
         (attach-org-id (org-ilm-infer-id-from-attachment-path file-path))
         (attach-org-id-marker (org-id-find attach-org-id t))
         (file-org-id file-name)
         (file-org-id-marker (org-id-find file-org-id t)))
    (unless attach-org-id-marker
      (error "Current file is not an attachment, or failed to parse org ID."))
    (unless file-org-id-marker
      (error "Current file name not org ID, or failed to find it."))
    
    (let* ((region-begin (region-beginning))
           (region-end (region-end))
           (region-text (buffer-substring-no-properties region-begin region-end))
           (region-text-clean (replace-regexp-in-string "\n" " " region-text))
           (snippet (substring region-text-clean 0 (min 50 (length region-text-clean))))
           (extract-org-id (org-id-new))
           (extract-tmp-path (expand-file-name
                              (format "%s.org" extract-org-id)
                              temporary-file-directory)))

      ;; Save region content into tmp file and move it as attachment to main
      ;; heading.
      (write-region region-text nil extract-tmp-path)
      (org-ilm--capture
       nil
       `(id ,file-org-id)
       extract-org-id
       extract-tmp-path
       snippet
       (lambda ()
         ;; Wrap region with targets.
         (with-current-buffer file-buf
           (save-excursion
             (let ((target-text (format "<<extract:%s>>" extract-org-id))
                   (is-begin-line (or (= region-begin 0) (eq (char-before) ?\n))))
               ;; Insert target before region
               (goto-char region-begin)
               (if is-begin-line
                   ;; Region starts at beginning of line: insert on new line above
                   (insert (format "%s\n" target-text))
                 ;; Otherwise, insert before region inline
                 (insert target-text))

               ;; Insert target after region, adjusting for start target length
               (goto-char (+ region-end (length target-text)))
               (insert target-text)))
           (save-buffer)
           (org-ilm-recreate-overlays)))))))

(defun org-ilm-prepare-buffer ()
  "Recreate overlays if current buffer is an attachment Org file."
  (interactive)
  (when (org-ilm-infer-id)
    (org-ilm-recreate-overlays)))

(defun org-ilm-open ()
  "Open element at point."
  (interactive)
  (if-let* ((attach-dir (org-attach-dir))
            (org-id (org-id-get))
            (path (expand-file-name (format "%s.org" org-id) attach-dir)))
      (progn
        (run-hook-with-args 'org-attach-open-hook path)
        (org-open-file path t)) ;; last param: in-emacs
      (error "No attachment directory exist, no Org id at current heading, or no attachment file associated with Org id.")))

(defun org-ilm-open-highlight ()
  "Open element associated with highlight at point."
  (interactive)
  (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'org-ilm-id)) (overlays-at (point)))))
    (pcase (length ovs)
      (0 nil)
      (1 (org-ilm--open-from-ov (nth 0 ovs)))
      (t
       (let* ((choices (mapcar
                        (lambda (ov)
                          (cons
                           (format "%s: %s"
                                   (propertize (overlay-get ov 'org-ilm-type) 'face '(:weight bold))
                                   (propertize
                                    (overlay-get ov 'org-ilm-id)
                                    'face '(:slant italic)))
                           ov))
                        ovs))
              (choice (completing-read "Element: " choices nil t))
              (ov (cdr (assoc choice choices))))
         (org-ilm--open-from-ov ov))))))

(defun org-ilm-queue (collection)
  "View queue in Agenda-like buffer."
  (interactive (list (org-ilm--select-collection)))
  (setq org-ilm-queue (org-ilm--collect-queue-entries collection))
  (org-ilm--display-queue))

(defun org-ilm-open-dwim ()
  "Open element of highlight or within collection."
  (interactive)
  (let ((collection-files (mapcar
                           (lambda (c) (file-truename (cdr c)))
                           org-ilm-collections-alist)))
    (cond
     ((member (file-truename buffer-file-name) collection-files)
      (org-ilm-open))
     ((org-ilm-infer-id) (org-ilm-open-highlight)))))

(defun org-ilm-set-schedule-from-priority ()
  "Set schedule date based on priority."
  (interactive)
  (org-ilm--set-schedule-from-priority))
  
;;;; Functions

;;;;; Utilities
(defun org-ilm--select-collection ()
  "Prompt user for collection to select from.

The collections are stored in `org-ilm-collections-alist'. If empty
return nil, and if only one, return it."
  (pcase (length org-ilm-collections-alist)
    (0 nil)
    (1 (nth 0 org-ilm-collections-alist))
    (t (let* ((choices (mapcar
                        (lambda (collection)
                          (cons
                           (format "%s %s"
                                   (propertize
                                    (symbol-name (car collection))
                                    'face '(:weight bold))
                                   (propertize (cdr collection) 'face '(:slant italic)))
                           collection))
                        org-ilm-collections-alist))
              (choice (completing-read "Collection: " choices nil t))
              (collection (cdr (assoc choice choices))))
         collection))))

(defun org-ilm--parse-logbook (logbook)
  "Parses the Org contents of a logbook drawer entry."
  (let ((contents (org-element-contents logbook)))
    (mapcar
     ;; If more data is needed, see:
     ;; https://orgmode.org/worg/dev/org-element-api.html
     ;; See: Timestamp, Clock
     (lambda (clock)
       (let ((timestamp (org-element-property :value clock)))
         `(:duration-minutes
           ,(org-duration-to-minutes (org-element-property :duration clock))
           :status ,(org-element-property :status clock)
           :day-end ,(org-element-property :day-end timestamp)
           :day-start ,(org-element-property :day-start timestamp)
           :month-end ,(org-element-property :month-end timestamp)
           :month-start ,(org-element-property :month-start timestamp)
           :year-end ,(org-element-property :year-end timestamp)
           :year-start ,(org-element-property :year-start timestamp))))
     contents)))
    
(defun org-ilm--read-logbook ()
  "Reads and parses the LOGBOOK drawer of heading at point."
  (let* ((headline (save-excursion (org-back-to-heading) (org-element-at-point)))
         (begin (org-element-property :begin headline))
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
            (org-ilm--parse-logbook logbook)))))))

(defun org-ilm--capture (is-source target org-id file-path &optional title on-success)
  "Make an org capture to make a new source heading or extract."
  (let ((org-capture-templates
          `(("i" "Import"
             entry ,target
             ,(format "* [#5] INCR %s %s" title "%?")
             ;; :unnarrowed ; TODO for extracts might be nice?
             :hook (lambda ()
                     (org-entry-put nil "ID" ,org-id)
                     
                     ;; Attach the file. We always use 'mv because we are
                     ;; importing the file from the tmp dir.
                     (org-attach-attach ,file-path nil 'mv)
                     (when ,is-source
                       (org-entry-put
                        nil "DIR"
                        (abbreviate-file-name (org-attach-dir-get-create))))

                     ;; Schedule
                     (org-ilm--set-schedule-from-priority)

                     ;; Add advice around priority change to automatically
                     ;; update schedule, but remove advice as soon as capture
                     ;; is finished.
                     (advice-add 'org-priority
                                 :around #'org-ilm--update-from-priority-change)
                     (add-hook 'kill-buffer-hook
                               (lambda ()
                                 (advice-remove 'org-priority
                                                #'org-ilm--update-from-priority-change))
                               nil t))
             :before-finalize ,on-success))))
    (org-capture nil "i"))))

;;;;; Attachments
(defun org-ilm-infer-id ()
  "Attempt parsing the org-id of current attachment file."
  (org-ilm-infer-id-from-attachment-path buffer-file-name))
  
(defun org-ilm-infer-id-from-attachment-path (path)
  "Attempt parsing the org-id from the attachment path."
  (when path
    (let ((parts (split-string (file-name-directory path) "/" t)))
      (pcase (file-name-nondirectory (org-attach-dir-from-id "abc"))
        ("abc" (car (last parts)))
        ("ab/c" (mapconcat #'identity (last parts 2) ""))
        (t nil)))))

;;;;; Extracts

(defun org-ilm--ov-block-edit (ov after beg end &optional len)
  (unless after
    (user-error "Cannot modify this region")))

(defun org-ilm--create-overlay (target-begin target-end)
  ""
  (message "Creating ilm overlay")
  
  ;; Hide targets
  (dolist (target (list target-begin target-end))
    (let ((begin (org-element-property :begin target))
          (end (org-element-property :end target)))
      (let ((ov (make-overlay
                 begin
                 ;; If next character is newline, include it.  Otherwise hitting
                 ;; backspace on header will look like header is still on
                 ;; newline but is in fact after the target.
                 (if (eq (char-after end) ?\n) (+ end 1) end)
                 nil t t)))
        (overlay-put ov 'org-ilm-target t)
        (overlay-put ov 'invisible t)
        (overlay-put ov 'modification-hooks '(org-ilm--ov-block-edit))
        )
    ))
  
  ;; Highlight region
  (let* ((parts (split-string (org-element-property :value target-begin) ":"))
         (type (nth 0 parts))
         (id (nth 1 parts))
         (ov (make-overlay
             (org-element-property :begin target-begin)
             (org-element-property :end target-end))))
    (overlay-put ov 'face 'org-ilm-face-extract)
    (overlay-put ov 'org-ilm-highlight t)
    (overlay-put ov 'org-ilm-type type)
    (overlay-put ov 'org-ilm-id id)
    (overlay-put ov 'help-echo (format "Highlight %s" id))))

(defun org-ilm-remove-overlays ()
  ""
  (interactive)
  (org-with-wide-buffer
   (remove-overlays (point-min) (point-max) 'org-ilm-highlight t)
   (remove-overlays (point-min) (point-max) 'org-ilm-target t)))

;; TODO Accept begin and end position so that new overlays can be processed by
;; narrowing to just that region, without having to redo the whole file.
(defun org-ilm-recreate-overlays ()
  ""
  (interactive)
  (org-ilm-remove-overlays)
  (org-with-wide-buffer
   (let ((targets (make-hash-table :test 'equal)))
     (org-element-map (org-element-parse-buffer) 'target
       (lambda (target)
         (let* ((value (org-element-property :value target))
                (parts (split-string value ":"))
                (type (nth 0 parts))
                (id (nth 1 parts)))
           (message "Target: %s %s" type id)
           (when (and
                  id
                  (member type '("extract" "cloze")))
             (if-let ((prev-target (gethash value targets)))
                 (org-ilm--create-overlay prev-target target)
               (puthash value target targets)))))))))

(defun org-ilm--open-from-ov (ov)
  ""
  (when-let ((id (overlay-get ov 'org-ilm-id)))
    (find-file (format "%s.org" id))))

;;;;; Queue view

(defun org-ilm--collect-queue-entries (collection)
  "Return entries (headings) that form the outstanding queue."
  ;; TODO This now returns all headings, later should take into account outstanding.
  (with-current-buffer (find-file-noselect (cdr collection))
    (org-ql-select (current-buffer) nil :action 'element-with-markers)))

(defun org-ilm--display-queue ()
  "Open an Org-ql view of elements in `org-ilm-queue'."
  (let ((strings (-map #'org-ql-view--format-element org-ilm-queue)))
    (org-ql-view--display :buffer "*Ilm Queue*" :header "Ilm queue" :strings strings)))


;;;;; Import

(defun org-ilm--select-import-method (&optional force-ask)
  "Ask user to choose whether to copy or move file when importing.

If `org-ilm-import-default-method' is set and `FORCE-ASK' is nil, return it."
  (if (and (not force-ask) org-ilm-import-default-method)
      org-ilm-import-default-method
    (let* ((choices '(("Copy" . cp) ("Move" . mv)))
           (choice (completing-read "Method: " choices nil t))
           (method (cdr (assoc choice choices))))
      method)))

(defun org-ilm-import-org-file (file collection method)
  "Import an Org file."
  (interactive
   (list
    (read-file-name "Import Org file: ")
    (org-ilm--select-collection)
    (org-ilm--select-import-method)))

  (let* ((org-id (org-id-new))
         (file-tmp-path (expand-file-name
                         (format "%s.org" org-id)
                         temporary-file-directory)))
    
    ;; Move or copy the file to tmp directory so we can rename the file with the
    ;; org ID before attaching it.
    (pcase method
      ('mv (rename-file file file-tmp-path))
      ('cp (copy-file file file-tmp-path))
      (t (error "Parameter METHOD must be one of 'mv or 'cp.")))

    (org-ilm--capture t `(file ,(cdr collection)) org-id file-tmp-path)))

;;;;; Stats
;; Functions to work with e.g. the beta distribution.
;; Random generation code from Claude.

(defun org-ilm--random-uniform ()
  "Generate a uniform random number between 0 and 1."
  (/ (random 1000000) 1000000.0))

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
       (cos (* 2.0 pi u2)))))

(defun org-ilm--random-poisson (lambda)
  "Generate a Poission random variable using Knuth's algorithm."
  (let ((l (exp (- lambda)))
        (k 0)
        (p 1.0))
    (while (> p l)
      (setq k (1+ k))
      (setq p (* p (random-uniform))))
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


;;;;; Priority

(defun org-ilm--validated-priority (priority)
  "Returns numeric value of priority if valid value, else nil."
  (cond
   ((stringp priority)
    (when (member priority '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (string-to-number priority)))
   ((numberp priority)
    (when (member priority '(1 2 3 4 5 6 7 8 9))
      priority))))

(defun org-ilm--get-priority ()
  "Return priority value of heading at point.

If no priority is set, return default value of 5."
  (or
   (org-ilm--validated-priority (org-entry-get nil "PRIORITY"))
   5))

(defun org-ilm--beta-from-priority (priority)
  "Beta parameters from priority value."
  (let* ((a priority)
         (b (+ 1 (- 9 a))))
    (list a b)))

(defun org-ilm--adjusted-priority-from-logbook (a b logbook)
  "Calculate the variance adjusted beta parameters from logbook data.

For now, decrease variance by proportionally scaling a and b by some
factor that increases with the number of reviews."
  (let* ((reviews (length logbook))
         (a (* a (+ 1 reviews)))
         (b (* b (+ 1 reviews))))
    (list a b)))

(defun org-ilm--beta-from-priority-and-logbook (priority logbook)
  "Beta parameters from priority value, adjusted by logbook history."
  (let ((params (org-ilm--beta-from-priority priority)))
    (org-ilm--adjusted-priority-from-logbook (nth 0 params)
                                             (nth 1 params)
                                             logbook)))

(defun org-ilm--get-priority-params ()
  "Calculate the beta parameters of the heading at point."
  (when-let ((priority (org-ilm--get-priority))
             (logbook (org-ilm--read-logbook)))
    (org-ilm--beta-from-priority-and-logbook priority logbook)))

(defun org-ilm--sample-priority (beta-params &optional seed)
  "Sample a priority value given the beta params."
  (interactive (list (org-ilm--get-priority-params)))
  (org-ilm--random-beta (nth 0 beta-params)
                        (nth 1 beta-params)
                        seed))

;;;;; Scheduling

(defun org-ilm--schedule-interval-from-priority (priority)
  "Calculate a schedule interval from priority value."
  (setq priority (org-ilm--validated-priority priority))
  (unless priority
    (error "Priority value invalid."))
  (let* ((priority-normalized (/ (- (- 10 priority) 1) 8.0))
         (priority-bounded (min (max priority-normalized 0.05) 0.95))
         (rate (* 25 (- 1 priority-bounded)))
         (interval (+ (org-ilm--random-poisson rate) 1)))
    interval))

(defun org-ilm--set-schedule-from-priority ()
  "Set the schedule based on the priority."
  (when-let ((interval (org-ilm--schedule-interval-from-priority (org-ilm--get-priority))))
    (org-schedule nil (format "+%sd" interval))))

(defun org-ilm--update-from-priority-change (func &rest args)
  "Advice around `org-priority' to detect priority changes to update schedule."
  (let ((old-priority (org-ilm--get-priority)))
    (apply func args)
    (let ((new-priority (org-ilm--get-priority)))
      (org-ilm--set-schedule-from-priority))))

;;;; Footer

(provide 'org-ilm)

;;; org-ilm.el ends here

