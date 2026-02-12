;;; org-ilm-schedule.el --- Schedule -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'ts)

(require 'org-ilm-utils)
;; (require 'org-ilm-element)
(require 'org-ilm-pqueue)

;;;; Variables

;; 40 days in supermemo
(defcustom org-ilm-schedule-max-days 60
  "Maximum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-schedule-min-days 1
  "Minimum number of days that a topic may be scheduled."
  :type 'number
  :group 'org-ilm)

;;;; Functions

(defun org-ilm--days-from-now (days)
  "Return ts object representing DAYS from now."
  (ts-adjust 'day -1 (ts-now)))

(cl-defun org-ilm--initial-schedule-interval-from-priority
    (priority &key
              (min 1)
              (max 30)
              (exponent 1.0)
              (jitter-proportion 0.25)
              (use-poisson nil)
              (poisson-scale 1.0))
  "Compute an initial schedule interval (days) from PRIORITY in [0,1].

MIN and MAX give the deterministic bounds for the base interval.
EXPONENT controls curvature of the mapping (1.0 → linear).
JITTER-PROPORTION is the max fractional multiplicative jitter when not using Poisson.
If USE-POISSON is non-nil, add a Poisson-distributed component with mean
POISSON-SCALE * base (uses `org-ilm--random-poisson`). The result is
rounded and clamped to at least 1."
  (cl-assert (numberp priority) nil "PRIORITY must be a number")
  (let* ((p (max 0.0 (min 1.0 (float priority))))
         ;; deterministic base interval mapped from priority
         (base (+ min (* (- max min) (expt p exponent)))))
    (if use-poisson
        (let ((rate (* poisson-scale base)))
          (max 1 (+ 1 (org-ilm--random-poisson rate))))
      ;; Multiplicative uniform jitter
      (let* ((rand (/ (float (random 10000)) 10000.0)) ; 0..1
             (signed (- (* 2 rand) 1.0))              ; -1..1
             (multiplier (+ 1.0 (* signed jitter-proportion)))
             (val (round (* base multiplier))))
        (max 1 val)))))

(defun org-ilm--initial-schedule-from-priority (priority &optional timestamp)
  "Calculate the initial schedule date from the normalized priority value."
  (setq timestamp (or timestamp (ts-now)))
  (cl-assert (ts-p timestamp))
  (let ((interval (org-ilm--initial-schedule-interval-from-priority priority)))
    (ts-adjust 'day interval timestamp)))

(cl-defun org-ilm--schedule (&key org-id timestamp interval-minutes interval-days ignore-time)
  (unless (setq org-id (or org-id (org-id-get)))
    (error "No headline with id given or at point."))
  (org-ilm--org-with-point-at org-id
    (org-ilm--org-schedule :timestamp timestamp
                           :interval-minutes interval-minutes
                           :interval-days interval-days
                           :ignore-time ignore-time)))

(defun org-ilm--set-schedule-from-priority (collection &optional priority)
  "Set the schedule based on the priority."
  (when-let* ((id (org-id-get))
              (priority (or priority (org-ilm--pqueue-priority id nil nil collection)))
              (interval-days (org-ilm--initial-schedule-interval-from-priority
                              (cdr priority))))
    (org-ilm--schedule :interval-days interval-days)))

(defun org-ilm--schedule-interval-calculate (priority scheduled last-interval)
  "Calculate the new schedule interval assuming review was done today."
  (if (null last-interval)
      (org-ilm--initial-schedule-interval-from-priority priority)
    (* last-interval 1.5)))

(defun org-ilm--element-schedule-interval-calculate (element)
  (let ((last-interval (org-ilm-element-last-interval element))
        (priority (cdr (org-ilm-element--priority element)))
        (scheduled (org-ilm-element--sched element)))
    (org-ilm--schedule-interval-calculate priority scheduled last-interval)))

;; TODO Turn this to function
;; (due (<= (/ (ts-diff scheduled (org-ilm--ts-today)) 86400) 0)))

;;; Footer

(provide 'org-ilm-schedule)

;;; org-ilm-schedule.el ends here
