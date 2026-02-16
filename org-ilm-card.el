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

;;;; Functions

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
               :stability   (-some-> (org-entry-get nil org-ilm-property-card-stability) string-to-number)
               :difficulty  (-some-> (org-entry-get nil org-ilm-property-card-difficulty) string-to-number)
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

;;; Footer

(provide 'org-ilm-card)

;;; org-ilm-card.el ends here
