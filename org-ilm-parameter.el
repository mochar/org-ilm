;;; org-ilm-parameter.el --- Parameters -*- lexical-binding: t; -*-

;;; Commentary:

;; Parameters are values that influence the element's scheduling and
;; priority. For example, the interval multiplier determines how fast the
;; interval of material elements grows. Parameters are stored as Org properties,
;; either directly within the heading of the element itself, that of its
;; ancestors, or its concepts.

;;; Code:

;;;; Requirements

(require 'org-ilm-element)
(require 'org-ilm-concept)

;;;; Functions

(cl-defgeneric org-ilm--parameter-get-of-type (element-type)
  "Return all parameters of the element of type ELEMENT-TYPE.
Each element type that has parameters should implement this."
  (list))

(defun org-ilm--parameter-get-all ()
  "Returns alist that maps element type to parameter symbols."
  (mapcar
   (lambda (type)
     (cons type (org-ilm--parameter-get-of-type type)))
   org-ilm-element-types))

(cl-defgeneric org-ilm--parameter-data (parameter)
  "Returns a plist with properties that describe PARAMETER.

Parameter: The parameter symbol itself, i.e. PARAMETER.
Property:  The Org property name where the value is stored.
Prompt:    The prompt used to read a new value from the user.
Validate:  A function that takes in a value, proccesses and returns it. Should
           result in an error if the value is in an invalid format.
Aggregate: Takes in an alist of (concept-id . value) pairs, and aggregates
           these into a final value that will be used."
  (error "Unknown parameter %s" parameter))

(defun org-ilm--parameter-data-from (parameter-or-data)
  "Returns itself if it is a plist, or gets the data if it is a symbol."
  (cond
   ((symbolp parameter-or-data)
    (org-ilm--parameter-data parameter-or-data))
   ((listp parameter-or-data)
    parameter-or-data)
   (t
    (error "Expected parameter symbol or its data plist"))))

(defun org-ilm--parameter-read (parameter-or-data)
  "Continously prompts user for value until a valid one has been given."
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
  "Calls the validator VALIDATE-F on VALUE, but on error warns instead, and
returns nil."
  (condition-case err
      (funcall validate-f value)
    (error
     (warn (error-message-string err))
     nil)))

(defun org-ilm--parameter-gather (parameter-or-data &optional id)
  "Returns alist (concept-id . value) of the parameter."
  (map-let (:property :validate) (org-ilm--parameter-data-from parameter-or-data)
    (org-ilm--org-with-point-at id
      (seq-keep
       (lambda (concept-value)
         (when-let ((value (org-ilm--parameter-validate-or-warn validate (cdr concept-value))))
           (cons (car concept-value) value)))
       (org-ilm--concept-property-or-inherited
        (org-id-get) property)))))
  
(defun org-ilm--parameter-compile-value (parameter-or-data &optional id)
  "Returns the final value of the parameter."
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

(cl-defun org-ilm--parameter-parse (&key element-type parameters id)
  "Returns alist (parameter . value) set explicitely in properties of element."
  (cl-assert (not (and element-type parameters)) nil
             "Must pass in either ELEMENT-TYPE or PARAMETERS, or neither")
  (org-ilm--org-with-point-at id
    (cond-let*
      (parameters)
      (element-type
       (setq parameters (org-ilm--parameter-get-of-type element-type)))
      ([element (org-ilm--element-at-point)]
       (setq parameters (org-ilm--parameter-get-of-type
                         (oref element type))))
      (t
       (setq parameters
             (apply #'append (mapcar #'cdr (org-ilm--parameter-get-all))))))
    (mapcar
     (lambda (parameter)
       (map-let (:property :validate) (org-ilm--parameter-data parameter)
         (cons parameter
               (-some->>
                   (org-entry-get nil property)
                 (org-ilm--parameter-validate-or-warn validate )))))
     parameters)))

(defun org-ilm--parameter-build-suffix-children (prefix key desc id parameter-or-data)
  "Builds a list of transient suffixes for a parameter."
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
        (lambda (&rest _)
          (let ((value (org-ilm--parameter-read ',data)))
            (org-ilm--org-with-point-at ,id
              (org-entry-put nil ,property (format "%s" value))
              (save-buffer))
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
