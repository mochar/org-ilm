;;; org-ilm-parameter.el --- Parameters -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-ilm-element)
(require 'org-ilm-concept)

;;;; Functions

;; TODO Maybe this should be `-data' and return a plist with property name,
;; prompt, and maybe even the validate and read function
(cl-defgeneric org-ilm--parameter-data (parameter)
  "Should return plist with properties: parameter, property, prompt, validate, aggregate"
  (error "Unknown parameter %s" parameter))

(defun org-ilm--parameter-data-from (parameter-or-data)
  (cond
   ((symbolp parameter-or-data)
    (org-ilm--parameter-data parameter-or-data))
   ((listp parameter-or-data)
    parameter-or-data)
   (t
    (error "Expected parameter symbol or its data plist"))))

(defun org-ilm--parameter-read (parameter-or-data)
  (map-let (:prompt :validate) (org-ilm--parameter-data-from parameter-or-data)
    (let (value)
      (while (not value)
        (let ((input (read-string prompt)))
          (condition-case err
              (setq value (funcall validate input))
            (error
             (message (error-message-string err))
             (sleep-for 1.)))))
      value)))

(defun org-ilm--parameter-validate-or-warn (validate-f value)
  (condition-case err
      (funcall validate-f value)
    (error
     (warn (error-message-string err)))))

(defun org-ilm--parameter-gather (parameter-or-data &optional id)
  (map-let (:property :validate) (org-ilm--parameter-data-from parameter-or-data)
    (org-ilm--org-with-point-at id
      (seq-keep
       (lambda (concept-value)
         (when-let ((value (org-ilm--parameter-validate-or-warn validate (cdr concept-value))))
           (cons (car concept-value) value)))
       (org-ilm--concept-property-or-inherited
        (org-id-get) property)))))
  
(defun org-ilm--parameter-compile-value (parameter-or-data &optional id)
  (map-let (:property :validate :aggregate)
      (org-ilm--parameter-data-from parameter-or-data)
    (org-ilm--org-with-point-at id
      (cond-let*
        ([value (org-entry-get nil property)]
         (org-ilm--parameter-validate-or-warn validate value))
        ([concept-values (seq-keep
                          (lambda (concept-value)
                            (when-let ((value (org-ilm--parameter-validate-or-warn validate (cdr concept-value))))
                              (cons (car concept-value) value)))
                          (org-ilm--concept-property-or-inherited
                           (org-id-get) property))]
         (funcall aggregate concept-values))))))

(defun org-ilm--parameter-build-suffix-children (prefix key desc id parameter-or-data)
  (pcase-let* ((data (org-ilm--parameter-data-from parameter-or-data))
               ((map :property :prompt :validate :parameter) data)
               (concept-values (org-ilm--parameter-gather data id)))
    (cons
     (transient-parse-suffix
      prefix
      `(
        ,key ,desc
        :cons ',parameter
        :class org-ilm-transient-cons-option
        :always-read t
        :reader
        ,(lambda (&rest _)
           (let ((value (org-ilm--parameter-read data)))
             (org-ilm--org-with-point-at id
               (org-entry-put nil property (format "%s" value)))
             value))
        :transient t))
     (seq-map
      (lambda (concept-value)
        (transient-parse-suffix
         prefix
         (list
          :info*
          (concat
           (propertize
            (format "%s" (cdr concept-value))
            'face 'Info-quoted)
           " "
           (propertize
            (if-let ((entry (org-mem-entry-by-id (car concept-value))))
                (org-ilm--org-mem-title-full entry)
              (substring (car concept-value) 0 8))
            )))))
      concept-values)
     )))

;;; Footer

(provide 'org-ilm-parameter)

;;; org-ilm-parameter.el ends here
