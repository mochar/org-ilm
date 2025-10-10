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
;; (require 'org-latex-preview)
(require 'org-ql)
(require 'org-node)
(require 'org-transclusion)
(require 'cl-lib)
(require 'dash)
(require 'zotra)
(require 'parsebib)

(require 'mochar-utils)
(require 'convtools)

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
  :type '(alist :key-type string :value-type (repeat string))
  :group 'org-registry)

(defcustom org-registry-add-link-in-title nil
  "Add a link to where the entry was registered in the title."
  :type 'string
  :group 'org-registry)

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
  "o" #'org-registry-open
  "f" #'org-registry-find
  "i" #'org-registry-insert
  "p" #'org-registry-paste
  "r" #'org-registry-register-dwim
  "a" #'org-registry-attachments)

;;;###autoload
(defun org-registry-open ()
  (interactive)
  (find-file (org-registry--registry-select)))
  
;;;###autoload
(defun org-registry-find ()
  (interactive)
  (org-node-goto (org-registry--select-entry)))

;;;###autoload
(defun org-registry-insert (entry-id)
  (interactive
   (list (org-mem-entry-id (org-registry--select-entry))))
  (insert (format "[[registry:%s]]" entry-id))
  (org-link-preview))

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
(defun org-registry-register (type-name &optional args)
  "Add something to the registry."
  (interactive
   (list
    (pcase (length org-registry-types)
      (0 (user-error "No registry types"))
      (1 (caar org-registry-types))
      (_ (completing-read "Type: " org-registry-types nil t)))))
  (let ((type (assoc type-name org-registry-types)))
    (cond-let*
      ([create-func (plist-get (cdr type) :create)]
       [data (funcall create-func args)]
       (org-registry--register (car type) data))
      ([register-func (plist-get (cdr type) :register)]
       (funcall register-func args))
      ;; When nothing assigned, create empty capture
      (t (org-registry--register (car type) nil)))))

;;;###autoload
(defun org-registry-register-dwim ()
  "Add element at point to the registry."
  (interactive)
  (if-let ((types
            (cl-remove-if
             #'null
             (mapcar
              (lambda (type)
                (when-let* ((f (plist-get (cdr type) :parse))
                            (data (funcall f)))
                  (cons (car type) data)))
              org-registry-types)))
           (type (if (> (length types) 1)
                     (let ((type (completing-read "Type: " (mapcar #'car types) nil t)))
                       (assoc type types))
                   (car types))))
      (funcall #'org-registry-register
               (car type) (cdr type))
    (call-interactively #'org-registry-register)))

;;;###autoload
(defun org-registry-attachments ()
  "View the attachments of a registry entry."
  (interactive)
  (let* ((attachments (org-attach-file-list (org-attach-dir)))
         (conversion (convtools--conversion-by-id (org-id-get)))
         conversion-name)
    (when (and conversion (not (eq (plist-get conversion :state) 'success)))
      (setq conversion-name
            (concat (propertize (plist-get conversion :name) 'face 'italic)
                    " ("
                    (convtools--conversion-propertize-state (plist-get conversion :state))
                    ")"))
      (push conversion-name attachments))
    (let ((choice (completing-read "Attachment: " attachments nil t)))
      (if (string= choice conversion-name)
          (switch-to-buffer (plist-get conversion :buffer))
        (find-file (expand-file-name choice (org-attach-dir)))))))

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

(defun org-registry--entry-contents (entry)
  (cl-assert (org-mem-entry-p entry))
  (save-excursion
    (mochar-utils--org-with-point-at
     (org-mem-entry-id entry)
     (org-registry--org-get-contents))))

(defun org-registry--plist-keys (plist)
  "Return a list of keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun org-registry--link-target ()
  (if (and (eq major-mode 'org-mode) (org-id-get))
      (concat "id:" (org-id-get))
    ;; Previously parsed with org-store-link but sometimes useless targets
    ;; ;; With arg '(4) do not include within-file context in link
    ;; (let ((link (org-store-link '(4))))
    ;;   (string-match org-link-bracket-re link)
    ;;   (match-string-no-properties 1 link))
    (concat "file:" (buffer-file-name (buffer-base-buffer)))))

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

(defvar org-registry-register-hook nil
  "Hook run after an entry has been registered.")

(cl-defun org-registry--register (type data &key registry template)
  "Add an entry of TYPE with DATA to the REGISTRY.

DATA is a plist that can contain :title, :body, :id, :type, :props,
:region, :on-success.

TEMPLATE is a cons with car optional template string and cdr plist of
org-capture template properties."
  (let* ((registry (or registry (org-registry--registry-select)))
         (title (string-trim (plist-get data :title)))
         (body (plist-get data :body))
         (type (or (plist-get data :type) type))
         (id (or (plist-get data :id) (org-id-new)))
         (props (org-combine-plists (list :ID id) (plist-get data :props)))
         (on-success (plist-get data :on-success))
         (template-string (car template)))
    (when org-registry-add-link-in-title
      (setq title
            (concat
             (format "[[%s][%s]]: " (org-registry--link-target) type)
             title)))
    (unless template-string
      (setq template-string
            (format "* %s%s%s"
                    (or title "")
                    "%?"
                    (if body (concat "\n" body) ""))))
    (cl-letf* ((default-template-props
                (list 
                 :hook
                 (lambda ()
                   ;; Insert properties first so that if ID is specified
                   (cl-loop
                    for (p v) on props by #'cddr
                    do (org-entry-put nil (if (stringp p) p (substring (symbol-name p) 1)) v))
                   (org-entry-put nil "TYPE" type)
                   (org-node-nodeify-entry)
                   ;; Toggle open the properties drawer
                   (save-excursion
                     (forward-line)
                     (org-fold-hide-drawer-toggle nil))
                   ;; Call template hook if given
                   (when-let ((hook2 (plist-get (cdr template) :hook)))
                     (funcall hook2)))
                 :after-finalize
                 (lambda ()
                   (if org-note-abort
                       nil
                     (org-mem-reset)
                     (org-mem-await nil 5)

                     (pcase-let ((`(,begin ,end) (plist-get data :region)))
                       (when (and begin end (yes-or-no-p "Replace with registry link? "))
                         (save-restriction
                           (delete-region begin end)
                           (org-registry-insert id))))

                     (when on-success (funcall on-success id))
                     (run-hook-with-args 'org-registry-register-hook id)))
                 ))
               (template-props (org-combine-plists
                                (cdr template) default-template-props))
               ((symbol-value 'org-capture-templates)
                (list
                 (append 
                  (list "r" "Register" 'entry `(file ,registry) template-string)
                  template-props))))
      (org-capture nil "r"))))

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
         (buffer-substring-no-props start (point)))))))

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
                             (org-element-context)))
              (type (org-element-type org-element))
              (latex (org-element-property :value org-element)))
    (when (member type '(latex-fragment latex-environment))
      (list :latex latex
            :fragment (eq type 'latex-fragment)
            :begin (org-element-property :begin org-element)
            :end (org-element-property :end org-element)))))

(defun org-registry--type-latex-create (&optional args)
  (cl-destructuring-bind (&key latex title fragment begin end) args
    (if (null latex)
        (list :title title :props (list :LATEX ""))
      (list :title title
            :body (unless fragment latex)
            :props (list :LATEX (when fragment latex))
            :region (list begin end)))))

(org-registry-set-type
 "latex"
 :preview #'org-registry--type-latex-preview
 :teardown #'org-registry--type-latex-teardown
 :paste #'org-registry--type-latex-paste
 :parse #'org-registry--type-latex-parse
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
      (list :title name :path path :type type))))

(defun org-registry--type-file-parse ()
  (if-let* ((el (when (eq major-mode 'org-mode) (org-element-context)))
            (_ (and (eq (org-element-type el) 'link)
                    (string= (org-element-property :type el) "file"))))
      (org-combine-plists
       (org-registry--type-file-from-path
        (expand-file-name (org-element-property :path (org-element-context))))
       (list :begin (org-element-property :begin el)
             :end (org-element-property :end el)))
    (when-let ((data (org-registry--type-file-from-path
                      (buffer-file-name (buffer-base-buffer)))))
      ;; We only dwim if its a non-generic file
      (unless (string= (plist-get data :type) "file") data))))

(defun org-registry--type-file-create (&optional args)
  (unless args
    (setq args (org-registry--type-file-from-path (read-file-name "File: "))))
  (cl-destructuring-bind (&key title path type begin end) args
    ;; TODO Debating on whether to batch query-replace this link in all org,
    ;; org-mem files, current dir files or not. Alternative would be to view
    ;; normal org links to this path same as registry link. Currently prefer
    ;; latter.
    (list :title title :type type :props (list :PATH path))))

(org-registry-set-type
 "file"
 :aliases '("image" "video" "audio")
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
    (let ((begin (region-beginning))
          (end (region-end)))
      (list :body (buffer-substring-no-properties begin end)
            :region (list begin end)))))

(defun org-registry--type-org-create (&optional args)
  args)

(org-registry-set-type
 "org"
 :preview #'org-registry--type-org-preview
 :teardown #'org-registry--type-org-teardown
 :paste #'org-registry--type-org-paste
 :parse #'org-registry--type-org-parse
 :create #'org-registry--type-org-create
 )

;;;;; Resource type

;; Generic object that contains zero or more attachments. Think of:
;; - Website article / blog post
;; - Youtube video (media)
;; - Paper from arxiv link or doi (paper)
;; - Paper from local pdf file

(defcustom org-registry--type-resource-fields nil
  "List of field names to include in the entry.
See `parsebib-read-entry'."
  :type '(repeat string)
  :group 'org-registry)

(defvar org-registry--type-resource-data nil)

(defun org-registry--type-resource-url-is-media-p (url)
  ;; Determine if media type by attempting to extract filename from url
  ;; using yt-dlp. If error thrown, yt-dlp failed to extract metadata ->
  ;; not media type.
  ;; NOTE Quite slow (~3 sec)
  ;; (condition-case err
  ;;     (or (convtools--ytdlp-filename-from-url url) t)
  ;;   (error nil))

  ;; Instead just check if its a youtube link for now
  (member (url-domain (url-generic-parse-url url))
          '("youtube.com" "youtu.be")))

(defun org-registry--type-resource-transient-args (&optional args)
  (setq args (or args (transient-args 'org-registry--type-resource-transient)))
  (list :source (transient-arg-value "--source=" args)
        :source-type (plist-get org-registry--type-resource-data :source-type)
        :type (transient-arg-value "--type=" args)
        :bibtex (plist-get org-registry--type-resource-data :bibtex)

        :paper-download (transient-arg-value "--paper-download" args)

        ;; HTML Download
        :html-download (transient-arg-value "--html-download" args)
        :html-simplify
        (cond
         ((transient-arg-value "--html-simplify-to-html" args)
          "html")
         ((transient-arg-value "--html-simplify-to-markdown" args)
          "markdown"))
        :html-orgify (transient-arg-value "--html-orgify" args)

        ;; Media download
        :media-download (transient-arg-value "--media-download" args)
        :media-template (transient-arg-value "--media-template=" args)
        :media-audio-only (transient-arg-value "--media-audio" args)
        :media-sub-langs (cdr (assoc "--media-subs=" args))

        ))

(defun org-registry--type-resource-process-source (&optional source bibtex)
  (let ((source (or source (ffap-read-file-or-url "URL/path/DOI: " nil)))
        (type "resource")
        source-type title)
    
    (cond
     ((or (null source) (string-empty-p source)))

     ;; Source is a url (assuming web)
     ((org-url-p source)
      (setq source-type 'url)

      ;; Figure out what type 
      (cond
       ((org-registry--type-resource-url-is-media-p source)
        (setq type "media"))
       (t (setq type "website"))))

     ;; Source is a file
     ((file-exists-p source)
      (setq source-type 'url
            title (file-name-base source)))
     
     (t
      (setq source-type 'id
            title source)))

    ;; Bibtex
    (when (and (null bibtex) (member source-type '(url id)))
      (when-let ((bibtex-string (zotra-get-entry source "bibtex")))
        (with-temp-buffer
          (insert (s-trim bibtex-string))
          (when-let* ((bibtexes (car (parsebib-parse-bib-buffer
                                      :fields org-registry--type-resource-fields
                                      :expand-strings t
                                      :inheritance t
                                      :replace-TeX t)))
                      (key (car (hash-table-keys bibtexes))))
            (setq bibtex (gethash key bibtexes)
                  title (or (alist-get "title" bibtex nil nil #'equal) title))))))

    (when (and (null title) (eq source-type 'url))
      (setq title (mochar-get-page-title source)))

    (list :source source :source-type source-type :title title
          :bibtex bibtex :type type)))

(transient-define-infix org-registry--type-resource-transient-source ()
  :class 'transient-option
  :transient 'transient--do-call
  :key "s"
  :description "Source"
  :argument "--source="
  :allow-empty nil
  :prompt "URL/path/DOI: "
  :reader
  (lambda (prompt initial-input history)
    (let ((source-data (org-registry--type-resource-process-source)))
      (setq org-registry--type-resource-data source-data)
      (mochar-utils--transiet-set-target-value "t" (plist-get source-data :type))
      (plist-get source-data :source))))

(transient-define-infix org-registry--type-resource-transient-subs ()
  :class 'transient-option
  :transient 'transient--do-call
  :key "ms"
  :description "Subtitles download"
  :argument "--media-subs="
  :multi-value 'rest
  :choices 
  (lambda ()
    (let* ((args (org-registry--type-resource-transient-args))
           (url (plist-get args :source))
           (subs (plist-get org-registry--type-resource-data :media-subs)))
      (unless subs
        (let ((subs-data (convtools--ytdlp-subtitles-from-url url)))
          (setf (plist-get org-registry--type-resource-data :media-subs) subs-data
                subs subs-data)))
      (mapcar (lambda (x) (alist-get 'language x)) (alist-get 'subtitles subs)))))

(transient-define-argument org-registry--type-resource-transient-simplify ()
  :class 'transient-switches
  :transient 'transient--do-call
  :key "hs"
  :description "Simplify"
  :argument-format "--html-simplify-to-%s"
  :argument-regexp "\\(--html-simplify-to-\\(html\\|markdown\\)\\)"
  :choices '("html" "markdown"))

(transient-define-prefix org-registry--type-resource-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (append
     '("--html-simplify-to-markdown" "--html-orgify")
     (let ((source (plist-get org-registry--type-resource-data :source)))
       (unless source
         (setq org-registry--type-resource-data
               (org-registry--type-resource-process-source))
         (setq source (plist-get org-registry--type-resource-data :source)))
       (list (concat "--source=" source)
             (concat "--type=" (plist-get org-registry--type-resource-data :type))))))

  ["Resource"
   (org-registry--type-resource-transient-source)
   (:info
    (lambda ()
      (let ((title (plist-get org-registry--type-resource-data :title)))
          (propertize title 'face 'italic)))
    :if (lambda () (plist-get org-registry--type-resource-data :title)))
   ("t" "Type" "--type=" :choices ("website" "media" "paper" "resource"))
   ("H" "HTML download" "--html-download" :transient transient--do-call
    :if (lambda ()
          (when-let ((args (org-registry--type-resource-transient-args (transient-get-value))))
            (eq (plist-get args :source-type) 'url))))
   ("M" "Media download" "--media-download" :transient transient--do-call
    :if (lambda ()
          (when-let ((args (org-registry--type-resource-transient-args (transient-get-value))))
            (and (string= "media" (plist-get args :type))
                 (eq (plist-get args :source-type) 'url)))))
   ("P" "Paper PDF download" "--paper-download" :transient transient--do-call
    :if (lambda ()
          (when-let ((args (org-registry--type-resource-transient-args (transient-get-value))))
            (and (string= "paper" (plist-get args :type))
                 (member (plist-get args :source-type) '(url id))))))
   ]

  ["HTML download (marker)"
   :hide
   (lambda ()
     (when-let ((args (org-registry--type-resource-transient-args (transient-get-value))))
       (not (and (member (plist-get args :source-type) '(url))
                 (plist-get args :html-download)))))
    [("hs" org-registry--type-resource-transient-simplify)
     ("ho" "Org conversion" "--html-orgify"
      :summary "Convert to Org mode with Pandoc"
      :transient transient--do-call)]]

  ["Media download (yt-dlp)"
   :if
   (lambda ()
     (when-let ((args (org-registry--type-resource-transient-args (transient-get-value))))
       (and (string= (plist-get args :type) "media")
            (plist-get args :media-download))))
   ("mt" "Template" "--media-template=" :prompt "Template: " :transient transient--do-call)
   ("ma" "Audio only" "--media-audio" :transient transient--do-call)
   ("ms" org-registry--type-resource-transient-subs)
   ]

  ["Actions"
   ("RET" "Register"
    (lambda ()
      (interactive)
      (cl-destructuring-bind
          (&key source source-type type bibtex title
                paper-download
                html-download html-simplify html-orgify
                media-download media-template media-audio-only media-sub-langs)
          (org-registry--type-resource-transient-args)

        (when (and (null title) bibtex)
          (setq title
                (or
                 (alist-get "title" bibtex nil nil #'equal)
                 (alist-get "=key=" bibtex nil nil #'equal)
                 (alist-get "url" bibtex nil nil #'equal)
                 source)))

        (let* ((id (org-id-new))
               (key (alist-get "=key=" bibtex nil nil #'equal))
               (registry (org-registry--registry-select))
               ;; Determine attach dir from within registry in case the dir is set
               ;; buffer or dir local
               (attach-dir (with-current-buffer (find-file-noselect registry)
                             (org-attach-dir-from-id id))))

          (org-registry--register
           "resource"
           (list
            :title title
            :id id
            :type type
            :props
            (append
             (when key
               (list :KEY key))
             (mochar-utils--alist-to-plist bibtex :upcase t :remove '("=key=" "=type="))
             (list :ROAM_REFS (if key (concat source " @" key) source)))
            :on-success
            (lambda (_)
              (when (or html-download media-download paper-download)
                (make-directory attach-dir)

                (when paper-download
                  (ignore-errors
                    (zotra-download-attachment
                     source nil
                     (expand-file-name (concat (mochar-utils--slugify-title title) ".pdf")
                                       attach-dir))
                    (mochar-utils--org-with-point-at id
                      (org-attach-sync))))
                
                (when html-download
                  (let ((monolith-args
                         (list :input-path source
                               :output-path
                               (expand-file-name
                                (concat (mochar-utils--slugify-title title) ".html")
                                attach-dir)))
                        (on-success
                         (lambda (proc buf id)
                           (message "[Registry] Website download completed: %s" source)
                           (mochar-utils--org-with-point-at id
                             (org-attach-sync)))))

                    (cond
                     (html-orgify
                      (apply
                       (if html-simplify
                           #'convtools--convert-to-org-with-monolith-defuddle-pandoc
                         #'convtools--convert-to-org-with-monolith-pandoc)
                       (list
                        :process-id id
                        :monolith-args monolith-args
                        :defuddle-args (list :output-format html-simplify)
                        :on-success on-success)))
                     (html-simplify
                      (apply
                       #'convtools--convert-with-monolith-defuddle
                       (list
                        :process-id id
                        :monolith-args monolith-args
                        :defuddle-args (list :output-format html-simplify)
                        :on-success on-success)))
                     (t ; Download, dont simplify or orgify
                      (apply
                       #'convtools--convert-with-monolith
                       :process-id org-id
                       :on-success on-success
                       monolith-args)))))

                (when media-download
                  (convtools--convert-with-ytdlp
                   :process-id id
                   :url source
                   :output-dir attach-dir
                   :filename-template media-template
                   :audio-only-p media-audio-only
                   :sub-langs media-sub-langs
                   :on-success
                   (lambda (proc buf id)
                     (message "[Registry] Media download completed: %s" source)
                     (mochar-utils--org-with-point-at id
                       (org-attach-sync)))))
                
                ))
            ))))))
   ]
  )

(defun org-registry--type-resource-parse ()
  (cond-let*
    ([bibtex (ignore-errors
               ;; TODO Only works if point at start of bibtex entry
               (parsebib-read-entry
                org-registry--type-resource-fields
                (make-hash-table :test #'equal)
                t))]
     (let ((url (or
                 (alist-get "url" bibtex nil nil #'equal)
                 (alist-get "doi" bibtex nil nil #'equal))))
       (list :source url :bibtex bibtex)))
    ([url (thing-at-point 'url)]
     (list :source url))))

(defun org-registry--type-resource-register (&optional args)
  (setq org-registry--type-resource-data
        (org-registry--type-resource-process-source
         (plist-get args :source)
         (plist-get args :bibtex)))
  (mochar-utils--add-hook-once
       'transient-post-exit-hook
       (lambda () (setq org-registry--type-resource-data nil)))
  (org-registry--type-resource-transient))

(org-registry-set-type
 "resource"
 :parse #'org-registry--type-resource-parse
 :register #'org-registry--type-resource-register
 )

;;;; Footer

(provide 'org-registry)

;;; org-registry.el ends here


