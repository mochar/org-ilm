;;; org-ilm-review.el --- Reviewing the queue -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'transient)
(require 'ts)
(require 'pdf-view)
(require 'org-clock)

(require 'org-ilm-utils)
;; (require 'org-ilm-queue)
;; (require 'org-ilm-element)
;; (require 'org-ilm-pdf)
;; (require 'org-ilm-card)
;; (require 'org-ilm-attachment)

;;;; Variables

;; TODO Pass headline id to clock in to during review
(defcustom org-ilm-review-clock-behavior nil
  "What to do with active clock during review.
Nil to do nothing, \'out to clock out on review, \'out-in to also clock back in."
  :type '(choice
          (const :tag "Nothing" nil)
          (const :tag "Clock out" out)
          (const :tag "Clock out and back in when done" out-in))
  :group 'org-ilm-review)

(cl-defstruct (org-ilm-review
               (:conc-name org-ilm-review--))
  "Review data."
  element
  id
  buffer
  start)

(defvar org-ilm--review-data nil
  "Info of the current element being reviewed.")

(defvar org-ilm--review-interrupted-clock-marker nil
  "Stores the `org-clock-marker' of the interrupted clock when review started.")

(defvar org-ilm-review-next-hook nil
  "Hook run when new element has been setup for review.")

(defvar org-ilm-review-reveal-hook nil
  "Hook run when card should be revealed.")

(defvar org-ilm-review-quit-hook nil
  "Hook run when review session stopped.")

(defvar-keymap org-ilm-review-mode-map
  :doc "Keymap for `org-ilm-review-mode'."
  "<f5>" #'org-ilm-review-rate-easy
  "<f6>" #'org-ilm-review-rate-good
  "<f7>" #'org-ilm-review-rate-hard
  "<f8>" #'org-ilm-review-rate-again
  "<f9>" #'org-ilm-review-ensure-or-next)

;;;; Review logic

;;;###autoload
(define-minor-mode org-ilm-review-mode
  "Minor mode during review."
  :group 'org-ilm
  :global t
  :interactive nil ;; shouldnt be a command
  (if org-ilm-review-mode
      (if (null org-ilm-queue-active-buffer)
          (progn
            (error "No active queue buffer")
            (org-ilm-review-mode -1))

        ;;; Minor mode on
        (with-current-buffer org-ilm-queue-active-buffer
          ;; Add kill hook on queue buffer
          (add-hook 'kill-buffer-hook
                    #'org-ilm--review-confirm-quit nil t))

        ;; Quit review when the active queue changes
        (add-hook 'org-ilm-queue-active-buffer-change-hook
                  #'org-ilm-review-quit)
        
        )
    
    ;;; Minor mode off
    ;; Remove kill buffer hooks
    (when (and org-ilm-queue-active-buffer
               (buffer-live-p org-ilm-queue-active-buffer))
      (with-current-buffer org-ilm-queue-active-buffer
        (remove-hook 'kill-buffer-hook
                     #'org-ilm--review-confirm-quit
                     t)))

    (remove-hook 'org-ilm-queue-active-buffer-change-hook
                 #'org-ilm-review-quit)
    ))

(defun org-ilm-reviewing-p ()
  "Return t when currently reviewing."
  org-ilm-review-mode)

(defun org-ilm-reviewing-id-p (id)
  "Return t when currently reviewing element with id ID."
  (when (org-ilm-reviewing-p)
    (equal id (plist-get org-ilm--review-data :id))))

(defun org-ilm--review-confirm-quit ()
  "Confirmation before killing the active attachment buffer or queue buffer
during review."
  (when (yes-or-no-p "Quit review?")
    (org-ilm-review-quit)))

(cl-defun org-ilm-review-start (&key queue-buffer)
  "Start a review session."
  (interactive)

  (when (org-ilm-reviewing-p)
    (pcase (read-char-choice "Already reviewing! (j)ump to element or (q)uit review? "
                             '("j" "q"))
      (?j (org-ilm-review-open-current))
      (?q (org-ilm-review-quit)))
    (cl-return-from org-ilm-review-start))

  (when queue-buffer
    (org-ilm-queue--set-active-buffer queue-buffer))

  (unless (or org-ilm-queue-active-buffer
              (org-ilm--queue-select-active-buffer))
    ;; TODO let user choose inactive one and make it active
    (user-error "No active queue buffer!"))

  ;; Make sure queue is not empty
  (with-current-buffer org-ilm-queue-active-buffer
    (when (org-ilm--queue-empty-p)
      (user-error "Queue is empty!")))

  (when (yes-or-no-p "Start reviewing?")

    ;; Store clocked-in task so that we can clock back in when done
    (when (member org-ilm-review-clock-behavior '(out out-in))
      (setq org-ilm--review-interrupted-clock-marker
            (when (org-clocking-p) (copy-marker org-clock-marker)))
      (org-clock-out nil 'fail-quietly))

    (org-ilm-review-mode 1)
    (org-ilm--review-next)))

(defun org-ilm-review-quit ()
  "Quit ilm review."
  (interactive)
  (org-ilm--review-cleanup-current-element)
  (org-ilm-review-mode -1)

  ;; Clock back in the active clock before review
  (when org-ilm--review-interrupted-clock-marker
    (when (and (eq org-ilm-review-clock-behavior 'out-in)
               (markerp org-ilm--review-interrupted-clock-marker))
      (with-current-buffer (marker-buffer org-ilm--review-interrupted-clock-marker)
        (save-excursion
          (goto-char org-ilm--review-interrupted-clock-marker)
          (org-clock-in))))
    (setq org-ilm--review-interrupted-clock-marker nil))
  
  (run-hooks 'org-ilm-review-quit-hook))

(defun org-ilm--review-ensure ()
  "Make sure that everything is ready before hitting next.
Return t if already ready."
  (let ((was-ready t)
        (element (plist-get org-ilm--review-data :element)))
    (when (org-ilm-element--card-p element)
      (unless (plist-get org-ilm--review-data :card-revealed)
        (setq was-ready nil)
        (org-ilm-review-reveal)))
    was-ready))

(defun org-ilm-review-ensure-or-next ()
  "If ready, go to next review, else get ready first."
  (interactive)
  (when (org-ilm--review-ensure)
    (org-ilm-review-next)))

(defun org-ilm-review-next (&optional rating)
  "Finish review of current element and go to the next one."
  (interactive)
  (unless (org-ilm-reviewing-p)
    (user-error "Not reviewing."))

  (org-ilm--review-ensure)

  ;; If card, make sure revealed and there is a rating
  (cl-assert (or (null rating) (member rating '(:good :easy :hard :again))))
  (let ((element (plist-get org-ilm--review-data :element)))
    (when (and (org-ilm-element--card-p element)
               (not (plist-get org-ilm--review-data :rating)))
      (setf (plist-get org-ilm--review-data :rating)
            (or rating
                (let ((ratings '(("Good" . :good)
                                 ("Easy" . :easy)
                                 ("Hard" . :hard)
                                 ("Again" . :again))))
                  (alist-get (completing-read "Rate:" ratings nil t)
                             ratings nil nil #'equal))))))
  
  (org-ilm--review-next))

(defun org-ilm-review-rate-good ()
  (interactive)
  (org-ilm-review-next :good))

(defun org-ilm-review-rate-easy ()
  (interactive)
  (org-ilm-review-next :easy))

(defun org-ilm-review-rate-hard ()
  (interactive)
  (org-ilm-review-next :hard))

(defun org-ilm-review-rate-again ()
  (interactive)
  (org-ilm-review-next :again))

(defvar org-ilm--review-update-schedule t)

(defun org-ilm--review-next ()
  (when org-ilm--review-data
    ;; Update priority and schedule
    (cl-destructuring-bind
        (&key buffer id element rating start &allow-other-keys)
        org-ilm--review-data
      
      (when org-ilm--review-update-schedule
        (let (duration)
          (when start
            (let* ((end (current-time))
                   (diff (float-time (time-subtract end start)))
                   (minutes (/ diff 60.0)))
              (setq duration minutes)))
          
          (org-ilm--element-review
           (org-ilm-element--type element)
           element duration :rating rating)
          )))
    
    (let ((element (org-ilm--queue-pop)))
      
      ;; TODO Card might already be due, eg when rating again. So need to check the
      ;; new review time and add it back to the queue. Set a minimum position in
      ;; the queue so that the card is not reviewed too quickly again.
      )
    
    (org-ilm--review-cleanup-current-element))
  
  (if (org-ilm--queue-empty-p)
      (progn
        (message "Finished reviewing queue!")
        (org-ilm-review-quit)
        (switch-to-buffer org-ilm-queue-active-buffer))
    (org-ilm--review-setup-current-element)
    (run-hooks 'org-ilm-review-next-hook)
    (org-ilm--review-open-current-element)))

(defun org-ilm-review-open-current ()
  "Open the attachment of the element currently being reviewed."
  (interactive)
  (let ((buf (plist-get org-ilm--review-data :buffer)))
    (if (and org-ilm--review-data (buffer-live-p buf))
        (switch-to-buffer buf)
      ;; Setup current element again if that data somehow lost. Mainly for when the
      ;; buffer is killed, it needs to be setup again. The setup function doesn't
      ;; change the queue or anything, so its safe to call it again.
      (org-ilm--review-setup-current-element)
      (org-ilm--review-open-current-element))))

(defun org-ilm--review-setup-current-element ()
  "Setup the element about to be reviewed.

The main job is to prepare the variable `org-ilm--review-data', which
needs the attachment buffer."
  (cl-assert (not (org-ilm--queue-empty-p)))

  (let* ((element (org-ilm--queue-head))
         (id (org-ilm-element--id element))
         attachment-buffer)

    (org-ilm--org-with-point-at id
      (setq attachment-buffer
            ;; dont yet switch to the buffer, just return it so we can do some
            ;; processing first.
            (save-window-excursion
              (org-ilm--attachment-open :no-error t))))

    ;; Prepare the attachment buffer if exists
    (when attachment-buffer
      (with-current-buffer attachment-buffer

        ;; Ask to end the review when killing the buffer
        (add-hook 'kill-buffer-hook
                  #'org-ilm--review-confirm-quit
                  nil t)

        ;; We update the buffer-local `org-ilm--data' (see
        ;; `org-ilm--attachment-prepare-buffer') with a start time. This is used
        ;; to calculate the new priority.
        ;; (org-ilm--attachment-ensure-data-object)
        ;; (setf (plist-get org-ilm--data :start) (current-time))

        ;; TODO Run hook here so that individual components can run their own
        ;; preparation without doing it all here.

        ;; If card, hide clozes
        (when (org-ilm-element--card-p element)
          (org-ilm--card-hide-clozes)

          ;; PDF clozes
          (when (org-ilm--pdf-mode-p)
            (pdf-view-redisplay)
            (add-hook 'org-ilm-review-reveal-hook
                      (lambda ()
                        ;; TODO Need a stupid wait here, fix
                        (run-at-time .1 nil 'pdf-view-redisplay))
                      nil 'local)))
          ))

    (setq org-ilm--review-data
          (list :element element
                :id (org-ilm-element--id element)
                :buffer attachment-buffer
                :start (current-time)))))

(defun org-ilm-review-reveal ()
  "Reveal the cloze contents of the current element."
  (interactive)
  (run-hooks 'org-ilm-review-reveal-hook)
  (setf (plist-get org-ilm--review-data :card-revealed) t
        header-line-format (org-ilm--review-header-build)))

(defun org-ilm--review-open-current-element ()
  "Open and prepare the attachment buffer of the element being reviewed."
  (let ((buffer (plist-get org-ilm--review-data :buffer)))
    (if buffer
        (with-current-buffer (switch-to-buffer buffer)
          (setq header-line-format
                (org-ilm--review-header-build)))
      ;; No attachment, simply go to the element in the collection
      (org-ilm--org-goto-id (plist-get org-ilm--review-data :id))
      (message "No attachment found for element"))))

(defun org-ilm--review-cleanup-current-element ()
  "Clean up the element being reviewed, in preparation for the next element."
  (when-let ((buffer (plist-get org-ilm--review-data :buffer))
             (element (plist-get org-ilm--review-data :element)))
    (with-current-buffer buffer
      (when (org-ilm-element--card-p element)
        ;; TODO Clean up card overlays
        ;; (org-srs-item-cloze-remove-overlays (point-min) (point-max))
        )
      (setq header-line-format nil)
      (remove-hook 'kill-buffer-hook #'org-ilm--review-confirm-quit t))
    ;; I know it doesn't make sense to clean the buffer then kill it, but better
    ;; have the cleaning up code ready in case i want an option later to not
    ;; kill the buffer
    (kill-buffer buffer))
  (setq org-ilm--review-data nil))

;;;; Buffer header 

(defun org-ilm--review-header-make-button (title func)
  (propertize
   (substitute-command-keys
    (format 
     "\\<org-ilm-review-mode-map>[%s `\\[%s]']"
     title (symbol-name func)))
   'mouse-face 'highlight
   'local-map (let ((map (make-sparse-keymap)))
                (define-key map [header-line mouse-1] func)
                map)))

(defun org-ilm--review-header-build ()
  "Build the header string of the attachment currently being reviewed."
  (let* ((element (plist-get org-ilm--review-data :element))
         (card-p (org-ilm-element--card-p element))
         (card-revealed (plist-get org-ilm--review-data :card-revealed)))
    (concat
     (propertize "Ilm Review" 'face '(:weight bold :height 1.0))
     "   "
     (funcall
      #'concat
      (unless card-p
         (org-ilm--review-header-make-button
          "Next" 'org-ilm-review-ensure-or-next))
      (when (and card-p (not card-revealed))
        (org-ilm--review-header-make-button
         "Reveal answer" 'org-ilm-review-ensure-or-next))
      (when (and card-p card-revealed)
        (concat

         (org-ilm--review-header-make-button
          "Easy" 'org-ilm-review-rate-easy)
         " "
         (org-ilm--review-header-make-button
          "Good" 'org-ilm-review-rate-good)
         " "
         (org-ilm--review-header-make-button
          "Hard" 'org-ilm-review-rate-hard)
         " "
         (org-ilm--review-header-make-button
          "Again" 'org-ilm-review-rate-again))
        ))

     )))

;;;; Actions

(defun org-ilm-review ()
  (interactive)
  (if (org-ilm-reviewing-p)
      (org-ilm-review-actions)
    (org-ilm-review-start)))

(defun org-ilm-review-postpone ()
  (interactive)
  (cl-assert (org-ilm-reviewing-p))
  (let (date)
    (while (ts<= (ts-parse (setq date (org-read-date nil nil nil "Postpone: ")))
                 (org-ilm--ts-today))
      (message "Minimum postpone date should be tomorrow")
      (sleep-for 1.))
    (org-ilm-element-set-schedule
     (org-ilm--element-by-id (plist-get org-ilm--review-data :id))
     date)
    (let ((org-ilm--review-update-schedule nil))
      (org-ilm--review-next))))

(defun org-ilm-review-done ()
  "Apply done on current element."
  (interactive)
  (cl-assert (org-ilm-reviewing-p))
  (org-ilm--org-with-point-at (plist-get org-ilm--review-data :id)
    ;; `org-ilm-element-done' already checks if we are reviewing this element,
    ;; and if so move on to the next element.
    (call-interactively #'org-ilm-element-done)))

(defun org-ilm-review-open-collection ()
  (interactive)
  (cl-assert (org-ilm-reviewing-p))
  (org-ilm--org-goto-id (plist-get org-ilm--review-data :id)))

(defun org-ilm-review-open-attachment ()
  (interactive)
  (cl-assert (org-ilm-reviewing-p))
  (if-let ((buf (plist-get org-ilm--review-data :buffer)))
      (switch-to-buffer buf)
    (user-error "Element has no buffer!")))

;;;; Transient

(transient-define-prefix org-ilm--review-transient ()
  :refresh-suffixes t
  ["Review"
   (:info*
   (lambda ()
     (propertize
      (org-ilm-element--title (plist-get org-ilm--review-data :element))
      'face '(:slant italic))))]

  [
   ["Actions"
    ("n" "Next" org-ilm-review-next)
    ("s" "Reschedule" org-ilm-review-postpone)
    ("d" "Done" org-ilm-review-done)
    ("q" "Quit" org-ilm-review-quit)]
   ["Element"
    ("e" "Element..." org-ilm-element-actions)
    ("c" "Open collection" org-ilm-review-open-collection)
    ("a" "Open attachment" org-ilm-review-open-attachment)]
   ]
  )

(defun org-ilm-review-actions ()
  (interactive)
  (if (org-ilm-reviewing-p)
      (org-ilm--review-transient)
    (user-error "Not reviewing!")))

;;; Footer

(provide 'org-ilm-review)

;;; org-ilm-review.el ends here
