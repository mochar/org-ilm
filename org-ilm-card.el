;;; org-ilm-card.el --- Card -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-element)
;; (require 'org-latex-preview)
(require 'cond-let)
(require 'peg)
(require 'fsrs)
(require 'ts)

(require 'org-ilm-utils)
(require 'org-ilm-log)
(require 'org-ilm-element)
(require 'org-ilm-concept)


;;;; Variables

(defcustom org-ilm-card-fsrs-desired-retention .9
  "Target probability of successful recall (0.0-1.0).

FSRS default: 0.9"
  :type 'number
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-learning-steps '((1 :minute) (10 :minute))
  "List of time intervals for initial learning phase.

FSRS default: \'((1 :minute) (10 :minute))"
  :type 'list
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-relearning-steps '((10 :minute))
  "List of time intervals for relearning phase.

FSRS default: \'((10 :minute))"
  :type 'list
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-maximum-interval '(36500 :day)
  "Upper bound for scheduling intervals.

FSRS default: \'(36500 :day)"
  :group 'org-ilm-card)

(defcustom org-ilm-card-fsrs-fuzzing-p t
  "Randomize intervals within bounds.

FSRS default: t"
  :type 'boolean
  :group 'org-ilm-card)

(defcustom org-ilm-card-first-interval 'priority
  "The first interval for cards."
  :type '(choice (const :tag "Immediate" nil)
                 (const :tag "Priority based interval" priority)
                 (string :tag "Custom interval as %H:%S format")
                 (number :tag "Custom interval in minutes"))
  :group 'org-ilm-card)

(defconst org-ilm-property-card-state "ILM_CARD_STATE")
(defconst org-ilm-property-card-stability "ILM_CARD_STABILITY")
(defconst org-ilm-property-card-difficulty "ILM_CARD_DIFFICULTY")
(defconst org-ilm-property-card-retention "ILM_RETENTION")

;;;; Logic

(defun org-ilm-card-first-interval ()
  (let ((interval org-ilm-card-first-interval))
    (cond
     ((eq interval 'priority)
      interval)
     ((and (numberp interval) (>= interval 0))
      interval)
     ((stringp interval)
      (ignore-errors (org-duration-to-minutes interval)))
     (t 0))))

(defun org-ilm--card-default-scheduler ()
  (fsrs-make-scheduler
   :desired-retention org-ilm-card-fsrs-desired-retention
   :learning-steps org-ilm-card-fsrs-learning-steps
   :relearning-steps org-ilm-card-fsrs-relearning-steps
   :maximum-interval org-ilm-card-fsrs-maximum-interval
   :enable-fuzzing-p org-ilm-card-fsrs-fuzzing-p))

;; TODO If property set in element or non-concept ancestor, use that instead
(defun org-ilm--card-element-scheduler (element)
  "Return the FSRS scheduler with parameters based on ELEMENT and its
concept properties."
  (cl-assert (org-ilm-element--card-p element) nil "Element not a card")
  (let ((retention (or
                    (when-let ((r (org-entry-get nil org-ilm-property-card-retention)))
                      (string-to-number r))
                    (when-let ((r (seq-keep
                                   (lambda (x)
                                     (let ((r (string-to-number (cdr x))))
                                       (if (< 0 r 1)
                                           r
                                         (message "Detected invalid retention property: %s" r)
                                         nil)))
                                   (org-ilm--concept-property-or-inherited
                                    (org-ilm-element--id element)
                                    org-ilm-property-card-retention))))
                      (apply #'org-ilm--mean r))
                    org-ilm-card-fsrs-desired-retention)))
    (unless (and (numberp retention) (< 0 retention 1))
      (message "Invalid inherited retention value: %s" retention)
      (setq retention org-ilm-card-fsrs-desired-retention))
    
    (fsrs-make-scheduler
     :desired-retention retention
     :learning-steps org-ilm-card-fsrs-learning-steps
     :relearning-steps org-ilm-card-fsrs-relearning-steps
     :maximum-interval org-ilm-card-fsrs-maximum-interval
     :enable-fuzzing-p org-ilm-card-fsrs-fuzzing-p)))

(cl-defmethod org-ilm--element-review ((type (eql 'card)) element duration &rest args)
  "Rate a card element with RATING, update log and scheduled date."
  (org-ilm--element-with-point-at element
    (let* ((priority (org-ilm-element--priority element))
           (scheduled (org-ilm-element--sched element))
           (rating (plist-get args :rating))
           (scheduler (org-ilm--card-element-scheduler element))
           (timestamp (or (plist-get args :timestamp) (ts-now)))
           (review-log  (org-ilm--log-read))
           (last-review (seq-find
                         (lambda (r)
                           (not (eq (org-ilm-log-review--action r) :done)))
                         (reverse review-log)))
           card)
      (cl-assert last-review nil "Card does not have a proper last review in log")
      (cl-assert rating)
      
      (setq card
            (car
             (fsrs-scheduler-review-card
              scheduler
              (fsrs-make-card
               :step        (org-ilm--card-step-from-log scheduler review-log)
               :state       (intern (org-entry-get nil org-ilm-property-card-state))
               :stability   (string-to-number (org-entry-get nil org-ilm-property-card-stability))
               :difficulty  (string-to-number (org-entry-get nil org-ilm-property-card-difficulty))
               :last-review (org-ilm-log-review--timestamp last-review)
               ;; Due date should be "artificial" one (potentially manually
               ;; edited), not the true date as scheduled in previous review, as
               ;; the algorithm does the calculation based on expected next
               ;; review. Having said that i cannot find this field being accessed
               ;; in `fsrs-scheduler-review-card'.
               :due (org-ilm--ts-format-utc scheduled))
              rating
              (org-ilm--ts-format-utc timestamp)
              nil
              ;; duration ;; TODO Pass this?
              )))

      (atomic-change-group
        (org-ilm--log-log
         'card (org-ilm-element--collection element) priority
         (fsrs-card-due card) rating
         :timestamp timestamp
         :duration duration
         :state (fsrs-card-state card)
         :stability (fsrs-card-stability card)
         :difficulty (fsrs-card-difficulty card))
        (org-entry-put nil org-ilm-property-card-state
                       (symbol-name (fsrs-card-state card)))
        (org-entry-put nil org-ilm-property-card-stability
                       (number-to-string (fsrs-card-stability card)))
        (org-entry-put nil org-ilm-property-card-difficulty
                       (number-to-string (fsrs-card-difficulty card)))
        (org-ilm--org-schedule
         :timestamp (ts-parse (fsrs-card-due card)))))))

(cl-defmethod org-ilm--element-first-interval ((type (eql 'card)) collection priority &rest args)
  (let ((card-interval (org-ilm-card-first-interval)))
    (cond
     ((eq card-interval 'priority)
      (cl-call-next-method type collection priority args))
     ((numberp card-interval)
      (ts-adjust 'minute card-interval (ts-now)))
     (t (error "card-interval unrecognized")))))

(cl-defmethod org-ilm--element-format-scheduled ((type (eql 'card)) scheduled)
  (org-ilm--ts-format-utc-date-maybe-time scheduled))

(cl-defmethod org-ilm--element-prepare-new ((type (eql 'card)) collection priority due &rest args)
  (org-ilm--log-log
   'card collection priority due :new 
   ;; Default state of a new fsrs card
   :state :learning)
  (org-entry-put nil org-ilm-property-card-state ":learning"))

;;;; Logging

;; TODO This can be optimized by truncating based on latest :review state
(defun org-ilm--card-step-from-log (scheduler review-log)
  "Calculate the current step based on a REVIEW-LOG.

Returns the current step (integer) or nil if the card is in :review state.
An empty log implies a new card, so step is 0."
  ;; We could also run the simulation with fsrs-scheduler-review-card but that
  ;; calculates also the model parameters which is unnecessary work. So just
  ;; reimplement the step logic.
  (let ((learn-steps (length (fsrs-scheduler-learning-steps scheduler)))
        (relearn-steps (length (fsrs-scheduler-relearning-steps scheduler)))
        (current-step 0))
    (dolist (review review-log)
      (let ((state  (org-ilm-log-review--state review))
            (rating (org-ilm-log-review--action review)))
        ;; Note, :hard rating does not increment step
        (cond
         ((eq state :review)
          (setq current-step nil))
         ((eq rating :again)
          (setq current-step 0))
         ((eq rating :good)
          (if (= current-step (1- (pcase state
                                    (:learning learn-steps)
                                    (:relearning relearn-steps))))
              (setq current-step nil)
            (cl-incf current-step))))))
    current-step))

;;;; Cloze

(cl-defstruct org-ilm-cloze
  "Cloze content and optional hint, with positions."
  pos content content-pos hint hint-pos)

;; Cloze syntax
;; - No hint: {{c::content}}
;; - Hint: {{c::content}{hint}}
(define-peg-ruleset org-ilm-card-cloze
  (text () (* (or (and (not ["{}"]) (any)) "\\{" "\\}" (nested (peg text)))))
  (nested (content) (and "{" (funcall content) "}"))
  (cloze () (nested (peg (and (nested (peg (and "c::" (region text))))
                              (opt (nested (peg (region text)))))))
         `(v1 v2 v3 v4 -- (when v1 (cons v1 v2)) (cons v3 v4))))

;; Simplified, but this does not handle nesting.
;; Eg: {{c::{}}}
;; (define-peg-ruleset org-ilm-card-cloze
;;   (cloze () 
;;          (and "{{c::"
;;               (substring content)
;;               (or (and "}{" (substring hint) "}}")
;;                   (and "}}" `(-- nil)))
;;               `(c h -- (list :content c :hint h))))
;;   (content () (+ (and (not "}{") (not "}}") (any))))
;;   (hint    () (+ (and (not "}") (any)))))

(defun org-ilm--card-cloze-match-at-point (&optional pos-only-p)
  (with-peg-rules (org-ilm-card-cloze)
    (when-let* ((cloze (peg-run (peg cloze))))
      (setq cloze (if (cadr cloze) (nreverse cloze) cloze))
      (let* ((content-pos (car cloze))
             (hint-pos (cadr cloze))
             content hint)
        (unless pos-only-p
          (setq content (buffer-substring-no-properties
                         (car content-pos) (cdr content-pos))
                hint (when hint-pos (buffer-substring-no-properties
                                     (car hint-pos) (cdr hint-pos)))))
        (make-org-ilm-cloze
         :content content
         :hint hint
         :content-pos content-pos
         :hint-pos hint-pos
         :pos (cons (- (car content-pos) 5)
                    (+ (cdr (or hint-pos content-pos)) 2)))))))

(defun org-ilm--card-cloze-match-around-point (&optional pos-only-p)
  "Match cloze around point."
  (save-excursion
    (cond-let*
      ([cloze (org-ilm--card-cloze-match-at-point pos-only-p)]
       cloze)
      ([pos (org-in-regexp "{{c::")]
       (goto-char (car pos))
       (org-ilm--card-cloze-match-at-point pos-only-p))
      (t (when-let ((point (point))
                    ((save-match-data (re-search-backward "{{c::" nil t)))
                    (cloze (org-ilm--card-cloze-match-at-point pos-only-p)))
           (when (<= point (cdr (org-ilm-cloze-pos cloze)))
             cloze))))))

(defun org-ilm--card-cloze-p ()
  "Return t if point on cloze."
  (if (org-ilm--card-cloze-match-around-point) t nil))

(defun org-ilm--card-cloze-match-forward (&optional end pos-only-p)
  "Jump to and return the first matching cloze."
  (let ((cloze (org-ilm--card-cloze-match-at-point pos-only-p)))
    (while (and (not cloze)
                (re-search-forward "{{c::" end t))
      ;; Back to beginning
      (goto-char (match-beginning 0))
      ;; We might match regex but not cloze
      (setq cloze (org-ilm--card-cloze-match-at-point pos-only-p)))
    cloze))

(defun org-ilm--card-cloze-gather (&optional begin end)
  "Return all clozes found in buffer."
  (let (cloze clozes)
    (save-excursion
      (goto-char (or begin (point-min)))
      (while (setq cloze (org-ilm--card-cloze-match-forward end))
        (push cloze clozes)))
    (nreverse clozes)))

(defun org-ilm-cloze-toggle ()
  "Toggle cloze at point, without creating the card."
  (interactive)
  (if (org-ilm--card-cloze-p)
      (org-ilm--card-uncloze)
    (org-ilm--card-cloze-dwim)))

(defun org-ilm--card-cloze-dwim (&optional hint)
  "Cloze the region or word at point."
  (let ((bounds (org-ilm--card-cloze-bounds)))
    (if (not bounds)
        (error "No active region or word at point")
      (org-ilm--card-cloze-region (car bounds) (cdr bounds) hint)
      bounds)))

(defun org-ilm--card-cloze-bounds ()
  "Return (beg . end) of what will be clozed."
  (cond-let*
   ((region-active-p)
    (cons (region-beginning) (region-end)))
   ([bounds (bounds-of-thing-at-point 'word)]
    bounds)))

(defun org-ilm--card-cloze-region (begin end &optional hint)
  "Coze the region between BEGIN and END."
  (save-excursion
    (goto-char end)
    (insert "}")
    (when hint
      (insert "{" hint "}"))
    (insert "}")
    (goto-char begin)
    (insert "{{c::")))

(defun org-ilm--card-uncloze ()
  "Remove cloze at point."
  (when-let ((cloze (org-ilm--card-cloze-match-around-point)))
    (delete-region (car (org-ilm-cloze-pos cloze))
                   (cdr (org-ilm-cloze-pos cloze)))
    (insert (string-trim (org-ilm-cloze-content cloze)))))

(defun org-ilm--card-uncloze-buffer (&optional begin end)
  "Remove clozes in buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (let (cloze)
      (while (setq cloze (org-ilm--card-cloze-match-forward end))
        (org-ilm--card-uncloze)))))

(defun org-ilm-clozeify (&optional toggle)
  (interactive "P")
  (if toggle
      (call-interactively #'org-ilm-cloze-toggle)
    (let* ((cloze (org-ilm--card-cloze-match-around-point))
           (bounds (and (not cloze) (org-ilm--card-cloze-bounds)))
           content hint begin end)
      (cond
       (cloze 
        (setq content (org-ilm-cloze-content cloze)
              hint (org-ilm-cloze-hint cloze)
              begin (car (org-ilm-cloze-pos cloze))
              end (cdr (org-ilm-cloze-pos cloze))))
       (bounds
        (setq content (buffer-substring-no-properties (car bounds) (cdr bounds))
              begin (car bounds)
              end (cdr bounds)))
       (t ;; Create new and insert at point
        (setq begin (point))))
      
      (let* ((content (read-string "Content: " content))
             (hint (read-string "Hint: " hint)))
        (when (string-empty-p hint) (setq hint nil))
        (when (string-empty-p content) (setq content "<cloze>"))
        (atomic-change-group
          (save-excursion
            (goto-char begin)
            (when end
              (delete-region begin end))
            (insert content)
            (org-ilm--card-cloze-region begin (+ begin (length content)) hint)))))))

;;;; Font lock

(defun org-ilm--card-cloze-font-lock-matcher (limit)
  (when-let ((cloze (org-ilm--card-cloze-match-forward limit 'pos-only)))
    (with-slots (pos content-pos hint-pos content hint) cloze
      ;; Hide opening: {{c::
      (put-text-property (car pos) (car content-pos) 'invisible 'org-ilm-cloze)
      ;; Content face
      (put-text-property (car content-pos) (cdr content-pos) 'face 'org-ilm-cloze-content-face)

      (if (not hint-pos)
          ;; Hide final braces: }}
          (put-text-property (- (cdr pos) 2) (cdr pos) 'invisible 'org-ilm-cloze)
        
        ;; Hide in-between braces: }{
        ;; (put-text-property (- (car hint-pos) 2) (car hint-pos) 'invisible 'org-ilm-cloze)

        ;; Hint face
        (put-text-property (car hint-pos) (cdr hint-pos) 'face 'org-ilm-cloze-hint-face)
        
        ;; Replace surrounding braces with ()
        (add-text-properties (- (car hint-pos) 2) (car hint-pos)
                             '(display " (" face org-ilm-cloze-hint-face))
        (add-text-properties (- (cdr pos) 2) (cdr pos)
                             '(display ")" face org-ilm-cloze-hint-face))
        
        
        )

      ;; Font-lock needs match-data, which we set manually
      (set-match-data (list (car pos) (cdr pos)))

      ;; Matcher needs to return non-nil to indicate success
      t)))

(defvar org-ilm--card-cloze-font-lock-keywords
  '((org-ilm--card-cloze-font-lock-matcher 0 nil)))

(defun org-ilm--card-cloze-font-lock-setup ()
  (add-to-invisibility-spec 'org-ilm-cloze)
  (setq-local font-lock-extra-managed-props
              (cons 'display font-lock-extra-managed-props)))

(add-hook 'org-ilm-global-minor-mode-hook
          (lambda ()
            (cond
             (org-ilm-global-minor-mode
              (add-hook 'org-mode-hook #'org-ilm--card-cloze-font-lock-setup)
              (font-lock-add-keywords
               'org-mode org-ilm--card-cloze-font-lock-keywords))
             (t
              (remove-hook 'org-mode-hook #'org-ilm--card-cloze-font-lock-setup)
              (font-lock-remove-keywords
               'org-mode org-ilm--card-cloze-font-lock-keywords)))))
              
;;;; Review interaction

;; TODO Extensible system where cloze types can be added based on thing at point
;; like in org-ilm-registry. Instead cond in function like now.

(defface org-ilm-cloze-content-face
  '((t (:foreground "black"
        :background "pink" 
        :weight bold
        :height 1.2)))
  "Face for cloze content.")

(defface org-ilm-cloze-hint-face
  '((t (:foreground "black"
        :background "pink"
        :height .8
        :slant italic
        )))
  "Face for cloze hints.")

(defun org-ilm--card-cloze-format-latex (latex)
  "Translate emacs face to latex code and apply to LATEX."
  (when (face-bold-p 'org-ilm-cloze-content-face)
    (setq latex (format "\\textbf{%s}" latex)))
  (when-let ((bg (face-background 'org-ilm-cloze-content-face)))
    (setq latex (format "\\fcolorbox{%s}{%s}{%s}" bg bg latex)))
  (when-let ((height (face-attribute 'org-ilm-cloze-content-face :height))
             (size (cond
                    ((< height 0.8)  "\\tiny")
                    ((< height 0.9)  "\\scriptsize")
                    ((< height 1.0)  "\\footnotesize")
                    ((< height 1.2)  "\\small")
                    ((< height 1.5)  "\\normalsize")
                    ((< height 1.8)  "\\large")
                    ((< height 2.0)  "\\Large")
                    ((< height 2.5)  "\\LARGE")
                    ((< height 3.0)  "\\huge"))))
    (setq latex (format "{%s %s}" size latex)))
  latex)

(defun org-ilm--card-cloze-build-latex (begin end latex &optional hint reveal-p)
  "Place the latex fragment modified by the cloze within it."
  (with-temp-buffer
    (insert latex)
    (goto-char (point-min))
    (while-let ((cloze (org-ilm--card-cloze-match-forward)))
      (delete-region (car (org-ilm-cloze-pos cloze))
                     (cdr (org-ilm-cloze-pos cloze)))
      (insert (org-ilm--card-cloze-format-latex
               (if reveal-p
                   (concat "$" (org-ilm-cloze-content cloze) "$")
                 (concat "[\\dots]" (when hint (concat "(" hint ")")))))))
    (setq latex (buffer-string)))
  
  (org-latex-preview-clear-overlays begin end)
  (org-latex-preview-place
   org-latex-preview-process-default
   (list (list begin end latex))))

(defun org-ilm--card-hide-clozes (&optional begin end)
  "Hide the clozes in the buffer by applying overlays."
  (org-ilm--card-remove-overlays)
  (save-excursion
    (goto-char (or begin (point-min)))
    (while-let ((cloze (org-ilm--card-cloze-match-forward end)))
      (let ((begin (car (org-ilm-cloze-pos cloze)))
            (end (cdr (org-ilm-cloze-pos cloze)))
            (content (org-ilm-cloze-content cloze))
            (hint (org-ilm-cloze-hint cloze)))
        (goto-char begin)
        (let* ((element (org-element-context))
               (ov (make-overlay begin end nil)))
          (overlay-put ov 'org-ilm-cloze cloze)
          (overlay-put ov 'org-ilm-cloze-state 'hidden)
          (overlay-put ov 'evaporate t)
          ;; (overlay-put ov 'modification-hooks '(org-ilm--ov-delete))
          ;; (overlay-put ov 'insert-in-front-hooks '(org-ilm--ov-delete))
          ;; (overlay-put ov 'insert-behind-hooks '(org-ilm--ov-delete))

          (cond
           ;; Latex
           ;; TODO Add modification and insert hooks to entire latex fragment
           ;; that will rebuild the fragment as the cloze is being modified.
           ((member (org-element-type element) '(latex-fragment latex-environment))
            (let ((latex-begin (org-element-begin element))
                  (latex-end (org-element-end element))
                  (latex-text (org-element-property :value element)))

              (org-ilm--card-cloze-build-latex latex-begin latex-end latex-text hint)

              (org-ilm--add-hook-once
               'org-ilm-review-reveal-hook
               (lambda ()
                 (org-ilm--card-cloze-build-latex
                  latex-begin latex-end latex-text hint t))
               nil 'local)

              ;; We are already building all clozes in the latex fragment, so
              ;; skip entire fragment.
              (goto-char latex-end)))
           (t 
            (overlay-put ov 'face 'org-ilm-cloze-content-face)
            (overlay-put ov 'display (concat "[...]" (when hint (concat "(" hint ")"))))
            (org-ilm--add-hook-once
             'org-ilm-review-reveal-hook
             (lambda ()
               (overlay-put ov 'display nil))
             nil 'local)
            (goto-char end))))
        ))))

(defun org-ilm--card-remove-overlays (&optional begin end)
  (dolist (val '(hidden revealed))
    (remove-overlays (or begin (point-min)) (or end (point-max))
                     'org-ilm-cloze-state val))
  (org-latex-preview-clear-overlays)
  (call-interactively #'org-latex-preview))

;;; Footer

(provide 'org-ilm-card)

;;; org-ilm-card.el ends here
