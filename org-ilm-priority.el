;;; org-ilm-priority.el --- Priority -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-ilm-pqueue)

;;;; Variables

(defcustom org-ilm-priority-factor-extract 0.8
  "Multiplication factor for extracts relative to parent rank.
0.5 means the extract is placed halfway between the top of the queue and the parent."
  :type 'float
  :group 'org-ilm-priority)

(defcustom org-ilm-priority-factor-card 0.6
  "Multiplication factor for cards relative to parent rank.
0.1 means the card is placed at 10% of the parent's rank (very high priority)."
  :type 'float
  :group 'org-ilm-priority)

(defun org-ilm--priority-calculate-child-priority (parent-priority type)
  "Calculate child capture's priority based on PARENT-PRIORITY and capture TYPE.
Uses a multiplicative factor to ensure children are prioritized ahead of parents."
  (if-let* ((parent-priority (org-ilm--pqueue-position parent-priority))
            (parent-rank (car parent-priority))
            (factor (pcase type
                      ('card org-ilm-priority-factor-card)
                      ('material org-ilm-priority-factor-extract)
                      (_ 1.0))))
      (let* ((new-rank (floor (* parent-rank factor)))
             ;; Add slight jitter so multiple extracts created in succession
             ;; don't land on the exact same integer index.  This spreads them
             ;; out slightly (e.g., +/- 2 spots).
             (jitter (random 5))
             (new-rank (max 0 (+ new-rank jitter))))
        (org-ilm--pqueue-position-new new-rank))
    ;; If parent not in queue, fallback to top of queue (0) or end?
    ;; Defaulting to 0 (highest priority) for captured items without 
    ;; queued parents is usually safe for active learners.
    ;; TODO This should probably be an error
    (cons 0 0.0)))

;;; Footer

(provide 'org-ilm-priority)

;;; org-ilm-priority.el ends here
