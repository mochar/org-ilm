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

(defun org-ilm--priority-calculate-child-priority (parent-priority collection type)
  "Calculate child capture's priority based on PARENT-PRIORITY and capture TYPE.
Uses a multiplicative factor to ensure children are prioritized ahead of parents."
  (if-let* ((pqueue (org-ilm--pqueue collection))
            (parent-priority (ost-tree-position pqueue parent-priority 1)) 
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
             ;; Make sure 0 <= rank <= ost size
             (new-rank (max 0 (min (+ new-rank jitter)
                                   (1- (ost-tree-size pqueue 1))))))
        (ost-tree-position pqueue new-rank 1))
    ;; If parent not in queue, fallback to top of queue (0) or end?
    ;; Defaulting to 0 (highest priority) for captured items without 
    ;; queued parents is usually safe for active learners.
    ;; TODO This should probably be an error
    (cons 0 0.0)))

;;; Footer

(provide 'org-ilm-priority)

;;; org-ilm-priority.el ends here
