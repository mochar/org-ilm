;;; org-ilm-transient.el --- Transient -*- lexical-binding: t; -*-

;;; Commentary:

;; Some utilities and custom classes for the transient package.

;;; Code:

;;;; Requirements

(require 'transient)
(require 'cl-lib)

;;;; Functions

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

;;;; Custom patched cons class

(defclass org-ilm-transient-cons-option (transient-cons-option)
  nil
  "Same as transient-cons-option with some patches/fixes.")

(cl-defmethod transient-init-value ((obj org-ilm-transient-cons-option))
  "Properly initialize the value of a built-in cons-option from the prefix's alist."
  (let ((prefix-val (oref transient--prefix value)))
    (oset obj value (alist-get (oref obj argument) prefix-val))))

(cl-defmethod transient-format-description ((obj org-ilm-transient-cons-option))
  "Format the description, allowing it to be a function."
  (or (transient--get-description obj)
      (let ((description (prin1-to-string (oref obj argument) t)))
        (if (string-prefix-p ":" description)
            (substring description 1)
          description))))

;;;; Custom cons switches class

;; For multiple options.

(defclass org-ilm-transient-cons-switches (transient-switches)
  ((argument-format :initform "%s")
   (format          :initform " %k %d %v")) ; remove round brackets
  "Hybrid class: cycles like switches, but returns an alist pair like cons.")

(cl-defmethod transient-infix-value ((obj org-ilm-transient-cons-switches))
  "Override the value extraction to return a (key . value) cons cell."
  (when-let ((val (oref obj value)))
    (cons (oref obj argument) val)))

(cl-defmethod transient-init-value ((obj org-ilm-transient-cons-switches))
  "Override the initialization to read from an alist instead of parsing strings."
  (let ((prefix-val (oref transient--prefix value)))
    (oset obj value (alist-get (oref obj argument) prefix-val))))

(cl-defmethod transient-infix-read ((obj org-ilm-transient-cons-switches))
  "Cycle through the choices. Respects the `:allow-empty' slot.
Supports both symbols and strings."
  (let ((choices (oref obj choices))) 
    (if-let ((value (oref obj value)))
        (let ((next (cadr (member value choices))))
          (cond
           (next next)
           ((oref obj allow-empty) nil)
           (t (car choices))))
      (car choices))))

(cl-defmethod transient-format-value ((obj org-ilm-transient-cons-switches))
  "Override how the value is formatted so it safely handles symbols."
  (with-slots (value choices inapt) obj
    (format (propertize "[%s]" 'face 'transient-delimiter)
            (mapconcat
             (lambda (choice)
               (propertize (format "%s" choice) 'face
                           (if (equal choice value)
                               (if inapt
                                   'transient-inapt-argument
                                 'transient-value)
                             'transient-inactive-value)))
             choices
             (propertize "|" 'face 'transient-delimiter)))))

;;;; Custom cons switch class

;; For boolean values

(defclass org-ilm-transient-cons-switch (transient-switch)
  ((format :initform " %k %d %v"))
  "Boolean toggle class that returns a (key . t) cons cell.")

(cl-defmethod transient-init-value ((obj org-ilm-transient-cons-switch))
  "Read the boolean state from the prefix alist."
  (let ((prefix-val (oref transient--prefix value)))
    (oset obj value (alist-get (oref obj argument) prefix-val))))

(cl-defmethod transient-infix-read ((obj org-ilm-transient-cons-switch))
  "Toggle the value between t and nil."
  (not (oref obj value)))

(cl-defmethod transient-infix-value ((obj org-ilm-transient-cons-switch))
  "Return a (key . t) cons cell if enabled. Drop if disabled."
  (when (oref obj value)
    (cons (oref obj argument) t)))

(cl-defmethod transient-format-value ((obj org-ilm-transient-cons-switch))
  "Display the state clearly."
  (let ((val (oref obj value)))
    (propertize (if val "✔" "⨯")
                'face (if val 'transient-value 'transient-inactive-value))))

;;; Footer

(provide 'org-ilm-transient)

;;; org-ilm-transient.el ends here
