;;; org-ilm-core.el --- Core -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Global customization

(defgroup org-ilm nil
  "Incremental learning mode."
  :group 'org
  :prefix "org-ilm-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom org-ilm-import-default-method 'cp
  "Default import method, or `nil' to always ask."
  :type '(choice (const :tag "Nil" nil)
                 (const :tag "Move" mv)
                 (const :tag "Copy" cp))
  :group 'org-ilm)

(defcustom org-ilm-update-org-mem-after-capture t
  "Update org-mem cache to include captured entry."
  :type 'boolean
  :group 'org-ilm)

(defcustom org-ilm-midnight-shift 2
  "Number of hours after midnight to count as a new day.

As an example, with a value of 2, elements scheduled for a day will only
be due starting 2am."
  :type 'number
  :group 'org-ilm)

(defcustom org-ilm-data-dir
  (expand-file-name
   (file-name-concat (convert-standard-filename "var/") "org-ilm")
   user-emacs-directory)
  "The directory where the org-ilm stores data.
Currently only the queues are stored for single file collections are stored
here."
  :type 'directory
  :group 'org-ilm)

(defface org-ilm-pulse
  '((default :extend t)
    (t :background "MistyRose3")
    (t :inverse-video t))
  "Face used to pulse line.")

(defface org-ilm-point
  '((t
     ;; :background unspecified
     ;; :inherit default
     :extend t
     :stipple "Stipple"
     ;; :background "pink"
     ;; :box (:line-width (-2 . -2) :color "blue")
     ))
  "Face used to highlight reading point.")

;;;; Variables

(defconst org-ilm-log-drawer-name "ILM")
(defconst org-ilm-property-type "ILM_TYPE")
(defconst org-ilm-property-collection "ILM_COLLECTION")
(defconst org-ilm-property-point "ILM_POINT")
(defconst org-ilm-property-ext "ILM_EXT")

(defconst org-ilm-element-types '(material card concept task queue))

;;;; Generics

(cl-defgeneric org-ilm--extract ()
  (user-error "Cannot extract here"))

(cl-defgeneric org-ilm--cloze ()
  (user-error "Cannot cloze here"))

(cl-defgeneric org-ilm--point ()
  (user-error "Cannot set point here"))

;;; Footer

(provide 'org-ilm-core)

;;; org-ilm-core.el ends here
