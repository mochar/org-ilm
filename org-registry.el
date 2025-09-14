;;; org-registry.el --- Registry in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: html, org

;;; Commentary:

;; This package needs some commentary...

;;; Code:

;;;; Requirements
(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'org-latex-preview)
(require 'org-ql)
(require 'org-node)
(require 'org-transclusion)
(require 'cl-lib)
(require 'dash)

;;;; Customization

(defgroup org-registry nil
  "Org registry"
  :group 'org
  :prefix "org-registry-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom org-registry-registries '("~/org/registry.org")
  "List of Org file paths to be used as registries."
  :type '(repeat file)
  :group 'org-registry)

(defcustom org-registry-types nil
  "Alist mapping registry type name to plist properties.

Properties are:

`:preview'

  See `org-link-parameters' for expected behavior. Accepts org-mem
  entry, link overlay, and link org-element.

`:teardown'

  Called when the link overlay is deleted. Accepts the link overlay.

`:setup'

  Called when `org-registry-global-minor-mode' is activated.

`:cleanup'

  Called when `org-registry-global-minor-mode' is deactivated.

`:paste'

  Should insert the contents of the registry entry in the buffer at
  point. Accepts org-mem entry.

`:parse'

  Return org registry entry data based on thing at point. Should be a
  list where the first element contains the body content of the entry
  headline. The remainder is a plist of headline properties. If there is
  nothing at point to interpret, return nil.

`:aliases'

  Optional list of aliases for this type. You may also use
  `org-registry-type-aliases'.

One of the following properties can be passed to create a new entry of
this type. Note that they are mutually exclusive:

`:template'

  A list containing org-capture template info used to place the entry in
  the registry.  It is a list with first element the template
  string. The remainder is a property list as used in
  `org-capture-templates'.

`:create'

  Return org registry entry data, which will be passed to
  `org-registry--register'. If instead you'd like to add the entry in
  the registry yourself, use `:register' instead. This is useful for
  example when you have an asynchronous process that creates the
  properties.

`:register'

  Add an entry to the registry. This function is responsible for
  creating the entry in the registry, similar to
  `org-registry--register'. If instead you'd like to delegate that to
  org-registry and only return the data for it, use `:create' instead.
"
  :type '(alist :tag "Registry type parameters"
                :key-type string
		:value-type plist)
  :group 'org-registry)

(defcustom org-registry-type-aliases
  '(("file" . ("video")))
  "Alist mapping `org-registry-types' types to aliases that can be used in their place.
This helps share functionality of a type while being able to filter on a more granular level."
  :type '(alist :key-type string :value-type (repeat string)))

(defface org-registry-link-face
  '((t :inherit org-link))
  "Face used by registry link types."
  :group 'org-registry)

;;;; Minor mode

(defun org-registry--delete-overlay-advice (&rest args)
  "Advice to detect if our org-link face was deleted."
  ;; TODO in org-transclusion-remove: (delete-overlay tc-pair-ov)
  ;; Might be nice to detect transclusions being deactivated
  (let ((ov (car args)))
    (when-let* ((type (overlay-get ov 'org-registry-type))
                (data (cdr (org-registry--type-data type)))
                (teardown-func (plist-get data :teardown)))
      (funcall teardown-func ov))))

(define-minor-mode org-registry-global-minor-mode
  "Prepare some hooks and advices some functions."
  :init-value nil
  :global t
  :lighter nil ;; String to display in mode line
  :group 'org-registry
  (if org-registry-global-minor-mode
      ;; Enable
      (progn
        ;; Detect deletion, call type :teardown ov
        (advice-add 'delete-overlay
                    :before #'org-registry--delete-overlay-advice)
        
        (dolist (type-data org-registry-types)
          (when-let ((f (plist-get (cdr type-data) :setup)))
            (funcall f))))
    ;; Disable
    (dolist (type-data org-registry-types)
        (when-let ((f (plist-get (cdr type-data) :cleanup)))
          (funcall f)))
    (advice-remove 'delete-overlay
                   #'org-registry--delete-overlay-advice)))

;;;; Commands

;;;###autoload
(defvar-keymap org-registry-prefix-map
  "f" #'org-registry-find
  "i" #'org-registry-insert
  "p" #'org-registry-paste
  "r" #'org-registry-register-dwim)

;;;###autoload
(defun org-registry-find ()
  (interactive)
  (org-node-goto (org-registry--select-entry)))

;;;###autoload
(defun org-registry-insert ()
  (interactive)
  (let* ((entry (org-registry--select-entry))
         (id (org-mem-entry-id entry)))
    (insert (format "[[registry:%s]]" id))
    (org-link-preview)))

;;;###autoload
(defun org-registry-paste ()
  "Paste the contents of the entry rather than linking to it."
  (interactive)
  (let* ((entry (org-registry--select-entry))
         (type (org-mem-entry-property "TYPE" entry))
         (data (cdr (org-registry--type-data type)))
         (paste-func (plist-get data :paste)))
    (funcall paste-func entry)))

;;;###autoload
(defun org-registry-register ()
  "Add something to the registry."
  (interactive)
  (let* ((type-name (completing-read "Type: " org-registry-types nil t))
         (type (assoc type-name org-registry-types)))
    (cond
     ((when-let ((template (plist-get (cdr type) :template)))
        (org-registry--register-capture type-name template)
        t))
     ((when-let* ((create-func (plist-get (cdr type) :create))
                  (data (funcall create-func)))
        (org-registry--register (car type) data)
        t))
     ((when-let ((register-func (plist-get (cdr type) :register)))
        (funcall register-func)))
     ;; When nothing assigned, create empty capture
     (t (org-registry--register (car type) nil)))))

;;;###autoload
(defun org-registry-register-dwim ()
  "Add element at point to the registry."
  (interactive)
  (let ((types
         (cl-remove-if
          #'null (mapcar
                  (lambda (type)
                    (when-let* ((f (plist-get (cdr type) :parse))
                                (props (funcall f)))
                      (cons (car type) props)))
                  org-registry-types))))
    (pcase (length types)
      (0 (org-registry-register))
      (1 (org-registry--register (car (car types)) (cdr (car types))))
      (_
       (let ((type (completing-read "Type: " (mapcar #'car types) nil t)))
         (org-registry--register type (cdr (assoc type types))))))))


;;;; Functions

;; See: https://github.com/meedstrom/org-node/issues/137
(defun org-registry--org-node-read-candidate (&optional prompt blank-ok initial-input predicate)
  "Like `org-node-read-candidate' but with PREDICATE and returns the node."
  (gethash
   (completing-read (or prompt "Node: ")
                    (if blank-ok #'org-node-collection-main
                      #'org-node-collection-basic)
                    predicate
                    ()
                    initial-input
                    (if org-node-alter-candidates 'org-node-hist-altered
                      'org-node-hist))
   org-node--candidate<>entry))

(cl-defun org-registry--select-entry (&key types registries)
  "Select registry entry."
  (let ((types (if (listp types) types (list types)))
        (registries (mapcar
                     (lambda (x) (expand-file-name x "~/"))
                     (if registries
                         (if (listp registries) registries (list registries))
                       org-registry-registries))))
    (org-registry--org-node-read-candidate
     "Entry: "
     nil nil
     ;; filter predicate
     (lambda (name node)
       (and
        (member (org-mem-entry-file-truename node) registries)
        (if types
          (member (org-mem-entry-property "TYPE" node)
                  types)
          t)
        )))))

(defun org-registry-type-aliases ()
  "Return alist of type name to aliases.
Aliases are retrieved from `org-registry-type-aliases' and the :aliases
 key in `org-registry-types'."
  (mapcar
   (lambda (type)
     (let ((name (car type))
           (data (cdr type)))
       (cons
        name
        (cl-union (cdr (assoc name org-registry-type-aliases))
                  (plist-get data :aliases)
                  :test #'equal))))
   org-registry-types))

(defun org-registry--type-name-and-aliases (typename-or-alias)
  "Return a list of which name is first element and remainder are aliases."
  (let ((type (assoc typename-or-alias org-registry-types))
        (type-aliases (org-registry-type-aliases)))
    (if type
        (assoc typename-or-alias type-aliases)
      (cl-find-if (lambda (aliases)
                    (member typename-or-alias aliases))
                  type-aliases))))

(defun org-registry--type-data (typename-or-alias)
  (when-let ((type-name (car (org-registry--type-name-and-aliases typename-or-alias))))
    (assoc type-name org-registry-types)))

(defun org-registry--type-data-from-entry (entry-or-id)
  (when-let* ((entry (if (org-mem-entry-p entry-or-id)
                         entry-or-id
                       (org-mem-entry-by-id entry-or-id)))
              (type (org-mem-entry-property "TYPE" entry)))
    (org-registry--type-data type)))

(defun org-registry--id-links-to-entry (&optional entry-or-id)
  "All ID-links targeting registry entry with ID.

From: `org-mem-id-links-to-id'
TODO Need to add registry to `org-mem-seek-link-types'? dont think so"
  (when-let ((id (cond
                  ((org-mem-entry-p entry-or-id)
                   (org-mem-entry-id entry-or-id))
                  ((stringp entry-or-id) entry-or-id)
                  (t (org-mem-entry-id (org-node-at-point))))))
    (with-memoization (org-mem--table 14 id)
      (when-let* ((links (org-mem-links-to-target id)))
        ;; For a vast majority of people, this filter is safe to skip.  But it's
        ;; possible, for example, to have a heading named identical to some ID
        ;; that you also have, and have non-ID links targeting that.
        (seq-filter (##equal (org-mem-link-type %) "registry") links)))))

(defun org-registry--registry-select (&optional type)
  (pcase (length org-registry-registries)
    (0 nil)
    (1 (car org-registry-registries))
    (t (completing-read "Registry: " org-registry-registries nil t))))

(defmacro org-registry--org-with-point-at (thing &rest body)
  "THING for now should be an org-id.

Note: Previously used org-id-find but it put point above
headline. org-mem takes me there directly.

Alternatively org-id-goto could be used, but does not seem to respect
save-excursion."
  `(when-let* ((org-id ,thing)
               (entry (org-mem-entry-by-id org-id))
               (file (org-mem-entry-file-truename entry))
               (buf (or (find-buffer-visiting file)
                        (find-file-noselect file))))
     (with-current-buffer buf
       ;; Jump to correct position
       (goto-char (org-find-property "ID" org-id))
       ,@body)))

(defun org-registry--entry-contents (entry)
  (cl-assert (org-mem-entry-p entry))
  (save-excursion
    (org-registry--org-with-point-at
     (org-mem-entry-id entry)
     (org-registry--org-get-contents))))

(defun org-registry--plist-keys (plist)
  "Return a list of keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

;;;; Content capture

;; TODO Finish. Idea is to ahve a capture buffer with more space where eg latex
;; can be typed out before send to `org-registry--register' but i'm facing
;; scoping issues and i dont like having two capture buffers in a row.

(defcustom org-registry-content-capture-path
  (expand-file-name
   "org-registry-input.org"
   temporary-file-directory)
  "Path of file where content is captured to, to be used with type registers."
  :type 'string
  :group 'org-registry)

(defun org-registry--org-capture (on-capture)
  (cl-letf* (((symbol-value 'org-capture-templates)
               (list
                (list
                 "i" "Input"
                 'plain
                 (list 'file org-registry-content-capture-path)
                 ""
                 :hook
                 (lambda ()
                   (save-restriction
                     (erase-buffer)
                     (unless (eq major-mode 'org-mode)
                       (org-mode))
                     (save-buffer))

                   (setq-local header-line-format
                               (substitute-command-keys
                                "Registry concent capture. Finish `\\[org-capture-finalize]' Abort `\\[org-capture-kill]'"))
                   )
                 
                 :prepare-finalize
                 (lambda ()
                   (goto-char (point-min))
                   ;; Remove empty lines and comment lines
                   (flush-lines "^\\([ \t]*\\|#.*\\)$")
                   (goto-char (point-min))
                   (funcall on-capture)
                   ;; ,@body
                   )
                 ;; :after-finalize
                 ;; (lambda ()
                 ;;   (with-current-buffer (find-file-noselect org-registry-content-capture-path)
                 ;;     ,@body))
                 :kill-buffer t
                 ))))
     (org-capture nil "i")))


;;;; Register

(defun org-registry--register-capture (type template &optional registry)
  "Use org-capture to register entry with TYPE to the REGISTRY.

TEMPLATE is a list with as first value the template string, which can be
 nil to use a default template string. The remainder is a standard
 org-capture template plist."
  (cl-letf* ((registry (or registry (org-registry--registry-select)))
             (default-template-props
              (list 
               :hook
               (lambda ()
                 (org-id-get-create)
                 (org-entry-put nil "TYPE" type)
                 (save-excursion
                   (forward-line)
                   (org-fold-hide-drawer-toggle nil))
                 (when-let ((hook2 (plist-get (cdr template) :hook)))
                   (funcall hook2))
                 )))
             (template-props (org-combine-plists (cdr template) default-template-props))
             ((symbol-value 'org-capture-templates)
              (list
               (append 
                (list "r" "Register"
                      'entry
                      `(file ,registry)
                      (or (car template) "* %?"))
                template-props))))
    (org-capture nil "r")))  

(defun org-registry--register (type data &optional registry)
  "Add an entry of TYPE with DATA to the REGISTRY."
  (let ((body (plist-get data :ENTRY_BODY))
        (title (plist-get data :ENTRY_TITLE))
        (properties data))
    (dolist (key (org-registry--plist-keys properties))
      (when (s-starts-with-p ":ENTRY_" (symbol-name key))
        (setq properties (org-plist-delete properties key))))
    (org-registry--register-capture
     type
     (list
      (format "* %s%s%s" (or title "") "%?" (if body (concat "\n" body) ""))
      :hook
      (lambda ()
        (cl-loop for (p v) on properties by #'cddr
                 do (org-entry-put nil (substring (symbol-name p) 1) v)))))))

(defun org-registry--org-get-contents ()
  (save-excursion
    (org-back-to-heading)
    (org-end-of-meta-data 'full)
    (if (or (org-at-heading-p) (eobp))
        ;; If we end up at the next heading or end of buffer, no content except
        ;; metadata.
        ""
      (let ((start (point)))
        (org-next-visible-heading 1)
        (string-trim
         (buffer-substring-no-properties start (point)))))))

;;;; Types

(defun org-registry-set-type (type &rest parameters)
  "Set registry TYPE properties to PARAMETERS.
PARAMETERS should be keyword value pairs. See `org-registry-types'."
  (let ((data (assoc type org-registry-types)))
    (if data
        (setcdr data (org-combine-plists (cdr data) parameters))
      (push (cons type parameters) org-registry-types))))

(defun org-registry--link-preview (ov id link)
  ;; This function must return a non-nil value to indicate success.
  (let* ((entry (org-mem-entry-by-id id))
         (type-data (org-registry--type-data-from-entry entry)))
    (overlay-put ov 'org-registry-type (car type-data))
    (org-with-point-at (org-element-begin link)
      (funcall (plist-get (cdr type-data) :preview) entry ov link))))

(defun org-registry--link-follow (id prefix-arg)
  (org-node-goto-id id))

(defun org-registry--link-face (id)
  (if-let ((type-data (org-registry-type-data-from-entry id))
           (face (plist-get (car data) :face)))
      face
    'org-registry-link-face))

(org-link-set-parameters
 "registry"
 :follow #'org-registry--link-follow
 :preview #'org-registry--link-preview
 :face #'org-registry--link-face)


;;;;; Latex type

(defun org-registry--type-latex-from-entry (entry)
  "Return the latex content from ENTRY.
Can be either in the LATEX property or the body text."
  (or
   (org-mem-entry-property "LATEX" entry)
   (org-registry--entry-contents entry)))

(defun org-registry--type-latex-preview (entry ov link)
  "Place a latex overlay on the link.

The way this is implemented is by using `org-latex-preview-place' which
 actually creates a new overlay. Therefore we need some way to detect
 when the link preview overlay is deleted, so that latex overlay can
 also be deleted. The function called when a preview is turned off is
 `org-link-preview-clear' which calls `delete-overlay'. Unfortunately
 the 'modification-hooks overlay property does not detect overlay
 deletions. So my best solution at the moment is to advice
 `delete-overlay', which is done in the global minor mode."
  (when-let ((latex (org-registry--type-latex-from-entry entry)))
    (overlay-put ov 'org-registry-latex t)
    (org-latex-preview-place
     org-latex-preview-process-default
     (list
      (list (overlay-start ov) (overlay-end ov) latex )))
    t))

(defun org-registry--type-latex-teardown (ov)
  "When latex registry link ov gets deleted, remove the latex ov inside it."
  (dolist (o (overlays-in (overlay-start ov) (overlay-end ov)))
    (when (eq (overlay-get o 'org-overlay-type)
              'org-latex-overlay)
      (delete-overlay o))))

(defun org-registry--type-latex-paste (entry)
  (insert (org-registry--type-latex-from-entry entry)))

(defun org-registry--type-latex-parse ()
  "Parses latex value from text at point.
If inline fragment, use it as entry property value. If
environment (multiline), paste it in headline body."
  (when-let* ((org-element (when (eq major-mode 'org-mode)
                             (org-element-context))))
    (pcase (org-element-type org-element)
      ('latex-fragment
       (list :LATEX (org-element-property :value org-element)))
      ('latex-environment
       (list :ENTRY_BODY (org-element-property :value org-element))))))

(defvar org-registry--type-latex-template
  (list "* %? %^{LATEX}p"))

(defun org-registry--type-latex-create ()
  (list :LATEX ""))

(org-registry-set-type
 "latex"
 :preview #'org-registry--type-latex-preview
 :teardown #'org-registry--type-latex-teardown
 :paste #'org-registry--type-latex-paste
 :parse #'org-registry--type-latex-parse
 ;; :template #'org-registry--type-latex-template
 :create #'org-registry--type-latex-create
 )


;;;;; File type

(defun org-registry--type-file-preview (entry ov link)
  (org-link-preview-file
   ov (expand-file-name
       (org-mem-entry-property "PATH" entry)
       "~/")
   link))

(defun org-registry--type-file-paste (entry)
  (let ((path (org-mem-entry-property "PATH" entry)))
    (insert (org-link-make-string
             (concat "file:" path)
             (file-name-base path)))))

(defun org-registry--type-file-from-path (path)
  "Return data for registry entry of type file from PATH."
  (when (and (stringp path) (file-exists-p path))
    (let* ((ext (file-name-extension path))
           (name (file-name-base path))
           (type (cond
                  ((member ext image-file-name-extensions) "image")
                  (t "file"))))
      (list :ENTRY_TITLE name :PATH path :TYPE type))))

(defun org-registry--type-file-parse ()
  (when-let ((data (org-registry--type-file-from-path buffer-file-name)))
    ;; We only dwim if its a non-generic file
    (unless (string= (plist-get data :TYPE) "file")
      data)))

(defun org-registry--type-file-create ()
  (let ((path (read-file-name "File: ")))
    (org-registry--type-file-from-path path)))
           
(org-registry-set-type
 "file"
 :aliases '("image" "video")
 :preview #'org-registry--type-file-preview
 :paste #'org-registry--type-file-paste
 :parse #'org-registry--type-file-parse
 :create #'org-registry--type-file-create
 )

;;;;; Org type

(defface org-registry-org-header-face
  '((t :slant italic))
  "Face used to display the title above the transcluded content.")

(defun org-registry--type-org-preview (entry ov link)
  "Transclude Org mode content with org-transclusion."
  (let ((begin (overlay-start ov))
        (end (overlay-end ov)))
    (goto-char end)
    (insert (format "\n#+transclude: [[id:%s]] :only-contents" (org-mem-entry-id entry)))
    (org-transclusion-add)
    (overlay-put ov 'display (org-mem-entry-title entry))
    ;; TODO fgiure out how to not inherit org-link properties
    (overlay-put ov 'face 'org-registry-org-header-face)
    t))

(defun org-registry--type-org-teardown (ov)
  (save-excursion
    (goto-char (overlay-end ov))
    ;; TODO might be better to keep checking until non-emtpy line
    (forward-line)
    (cond
     ((org-transclusion-within-transclusion-p)
        (progn
          (org-transclusion-remove)
          (delete-line)))
     ((org-transclusion-check-add)
      (delete-line))
     (t
      (message "Cannot find transclusion to clean up")))))

(defun org-registry--type-org-paste (entry)
  (insert (org-registry--entry-contents entry)))

(defun org-registry--type-org-parse ()
  (when (and (eq major-mode 'org-mode) (region-active-p))
    (list :ENTRY_BODY (buffer-substring-no-properties
                       (region-beginning) (region-end)))))

(org-registry-set-type
 "org"
 :preview #'org-registry--type-org-preview
 :teardown #'org-registry--type-org-teardown
 :paste #'org-registry--type-org-paste
 :parse #'org-registry--type-org-parse
 )

;;;; Footer

(provide 'org-registry)

;;; org-registry.el ends here


