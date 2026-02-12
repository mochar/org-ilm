;;; org-ilm-material.el --- Material elements -*- lexical-binding: t; -*-

;;; Commentary:

;; Source material to be consumed incrementally. Includes extracts.

;;; Code:

;;;; Requirements

(require 'ts)

(require 'org-ilm-utils)
(require 'org-ilm-element)
(require 'org-ilm-log)

;;;; Variables

(defconst org-ilm-property-multiplier "ILM_MULTIPLIER")

;;;; Functions

(defun org-ilm--material-schedule-multiplier (priority)
  "Return multiplier (A-factor) from PRIORITY.

Map PRIORITY ∈ [0,1] to a smooth A-Factor ∈ [1.1,3.0].
Lower PRIORITY → higher importance → smaller A."
  (cl-assert (<= 0.0 priority 1.0))
  (let* ((min-a 1.1)
         (max-a 2.0)
         (k 5.0) ;; steepness
         (x (max 0.0 (min 1.0 priority)))
         ;; standard logistic in [0,1]
         (sig (/ 1.0 (+ 1.0 (exp (* (- k) (- x 0.5))))))
         ;; normalize so 0→0, 1→1
         (sig-norm (/ (- sig (/ 1.0 (+ 1.0 (exp (* k 0.5))))) 
                      (- (/ 1.0 (+ 1.0 (exp (* (- k) 0.5))))
                         (/ 1.0 (+ 1.0 (exp (* k 0.5))))))))
    (+ min-a (* (- max-a min-a) sig-norm))))

(defun org-ilm--material-calculate-interval (priority scheduled last-review &optional multiplier)
  "Calculate the new scheduled interval in days.

If the scheduled date is before or equal to LAST-REVIEW this is treated
as a new element and the initial interval based on PRIORITY is returned."
  (cl-assert (and (ts-p scheduled) (ts-p last-review)))

  (let* ((now (ts-now))
         (review-interval (org-ilm--ts-diff-rounded-days now last-review))
         (sched-interval (org-ilm--ts-diff-rounded-days scheduled last-review)))
    ;; If the scheduled date was adjusted to be before the last review, then see
    ;; it as a new element and use initial interval base on priority
    (if (<= sched-interval 0)
        (org-ilm--initial-schedule-interval-from-priority (cdr priority))
      
      (unless multiplier
        (setq multiplier (float (org-ilm--material-schedule-multiplier (cdr priority)))))
      (let ((new-interval (* sched-interval multiplier)))

        ;; Reviewed earlier than scheduled. Take proportion of time from last review
        ;; to today relative to scheduled date and use that to adjust the new
        ;; interval.
        ;; TODO Pretty sure this is the same as just doing (* review-interval multiplier)??
        (when (and (< review-interval sched-interval) (> sched-interval 0))
          (setq new-interval
                (* new-interval (/ (float review-interval) sched-interval))))

        (max 1 (round new-interval))))))

(cl-defmethod org-ilm--element-review ((type (eql 'material)) element duration &rest args)
  "Apply a review on the material element.
This will update the log table and the headline scheduled date."
  (org-ilm--element-with-point-at element
    (let* ((review-log  (org-ilm--log-read))
           (last-review (car (last review-log))))
      (cl-assert last-review nil "Element missing log")
      (let* ((scheduled (org-ilm-element--sched element))
             (priority  (org-ilm-element--priority element))
             ;; TODO Inherit from concepts like fsrs retention?
             (multiplier (when-let ((m (org-entry-get nil org-ilm-property-multiplier)))
                           (setq m (string-to-number m))
                           (if (> m 0)
                               m
                             (message "Multiplier property invalid")
                             nil)))
             (interval (org-ilm--material-calculate-interval
                        priority scheduled
                        (ts-parse (org-ilm-log-review--timestamp last-review))
                        multiplier))
             (due (ts-adjust 'day interval (ts-now))))
        (atomic-change-group
          (org-ilm--log-log 'material (org-ilm-element--collection element)
                            priority due :review
                            :duration duration
                            :timestamp (plist-get args :timestamp))
          (org-ilm--org-schedule :timestamp due :ignore-time t))))))

;;; Footer

(provide 'org-ilm-material)

;;; org-ilm-material.el ends here
