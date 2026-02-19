;;; org-ilm-registry.el --- Registry -*- lexical-binding: t; -*-

;;; Commentary:

;; The registry system allows one to store "things" as org nodes, for ease of
;; reference and reuse. Currently "things" can be images, latex code, org mode
;; text.

;; A custom Org link [[registry:id]] will be rendered as the contents of the
;; registry entry with that id. For example, latex entries will display a latex
;; overlay, images in an image overlay, and org mode using org-transclusion.

;; As registry entries such as images can refer to files, org-attach is used in
;; a way similar to org-ilm-elements. However the convention is to store all
;; registry files within one folder, rather than a folder per entry.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-attach)
(require 'org-capture)
;; (require 'org-latex-preview)
(require 'org-ql)
(require 'org-node)
(require 'org-transclusion)
(require 'ol) ;; org-link
(require 'cl-lib)
(require 'dash)
(require 'zotra)
(require 'parsebib)
(require 'transient)
(require 'consult)

(require 'org-ilm-utils)
(require 'org-ilm-collection)
(require 'org-ilm-convert)

;;;; Customization

(defcustom org-ilm-registry-types nil
  "Alist mapping registry type name to plist properties.

Properties are:

`:preview'

  See `org-link-parameters' for expected behavior. Accepts org-mem
  entry, link overlay, and link org-element.

`:teardown'

  Called when the link overlay is deleted. Accepts the link overlay.

`:setup'

  Called when `org-ilm-registry-global-minor-mode' is activated.

`:cleanup'

  Called when `org-ilm-registry-global-minor-mode' is deactivated.

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
  `org-ilm-registry-type-aliases'.

One of the following properties can be passed to create a new entry of
this type. Note that they are mutually exclusive:

`:create'

  Return org registry entry data, which will be passed to
  `org-ilm-registry--register'. If instead you'd like to add the entry in
  the registry yourself, use `:register' instead. This is useful for
  example when you have an asynchronous process that creates the
  properties.

`:register'

  Add an entry to the registry. This function is responsible for
  creating the entry in the registry, similar to
  `org-ilm-registry--register'. If instead you'd like to delegate that to
  org-ilm-registry and only return the data for it, use `:create' instead.
"
  :type '(alist :tag "Registry type parameters"
                :key-type string
		:value-type plist)
  :group 'org-ilm-registry)

(defcustom org-ilm-registry-type-aliases
  '(("file" . ("video")))
  "Alist mapping `org-ilm-registry-types' types to aliases that can be used in their place.
This helps share functionality of a type while being able to filter on a more granular level."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'org-ilm-registry)

(defcustom org-ilm-registry-add-link-in-title nil
  "Add a link to where the entry was registered in the title."
  :type 'string
  :group 'org-ilm-registry)

(defface org-ilm-registry-link-face
  '((t :inherit org-link))
  "Face used by registry link types."
  :group 'org-ilm-registry)

;;;; Set up

(defun org-ilm--registry-delete-overlay-advice (&rest args)
  "Advice to detect if our org-link face was deleted."
  ;; TODO in org-transclusion-remove: (delete-overlay tc-pair-ov)
  ;; Might be nice to detect transclusions being deactivated
  (let ((ov (car args)))
    (when-let* ((type (overlay-get ov 'org-ilm-registry-type))
                (data (cdr (org-ilm-registry--type-data type)))
                (teardown-func (plist-get data :teardown)))
      (funcall teardown-func ov))))

(defun org-ilm--registry-setup ()
  (cond
   (org-ilm-global-minor-mode
    ;; Detect deletion, call type :teardown ov
    (advice-add 'delete-overlay
                :before #'org-ilm--registry-delete-overlay-advice)
        
    (dolist (type-data org-ilm-registry-types)
      (when-let ((f (plist-get (cdr type-data) :setup)))
        (funcall f))))
   (t
    (dolist (type-data org-ilm-registry-types)
      (when-let ((f (plist-get (cdr type-data) :cleanup)))
        (funcall f)))
    (advice-remove 'delete-overlay
                   #'org-ilm--registry-delete-overlay-advice))))

(add-hook 'org-ilm-global-minor-mode-hook #'org-ilm--registry-setup)

;;;; Commands

;;;###autoload
(defvar-keymap org-ilm-registry-prefix-map
  "o" #'org-ilm-registry-open
  "f" #'org-ilm-registry-find
  "i" #'org-ilm-registry-insert
  "p" #'org-ilm-registry-paste
  "g" #'org-ilm-registry-register-dwim
  "a" #'org-ilm-registry-attachments)

;;;###autoload
(defun org-ilm-registry-open ()
  (interactive)
  (find-file (org-ilm--collection-registry-path)))
  
;;;###autoload
(defun org-ilm-registry-find ()
  (interactive)
  (org-node-goto (org-ilm-registry--select-entry)))

;;;###autoload
(defun org-ilm-registry-insert (entry-id)
  (interactive
   (list (org-mem-entry-id (org-ilm-registry--select-entry))))
  (insert (format "[[registry:%s]]" entry-id))
  (org-link-preview))

;;;###autoload
(defun org-ilm-registry-paste ()
  "Paste the contents of the entry rather than linking to it."
  (interactive)
  (let* ((entry (org-ilm-registry--select-entry))
         (type (org-mem-entry-property "TYPE" entry))
         (data (cdr (org-ilm-registry--type-data type)))
         (paste-func (plist-get data :paste)))
    (funcall paste-func entry)))

;;;###autoload
(defun org-ilm-registry-register (type-name &optional args)
  "Add something to the registry."
  (interactive
   (list
    (pcase (length org-ilm-registry-types)
      (0 (user-error "No registry types"))
      (1 (caar org-ilm-registry-types))
      (_ (completing-read "Type: " org-ilm-registry-types nil t)))))
  (let ((type (assoc type-name org-ilm-registry-types)))
    (cond-let*
      ([create-func (plist-get (cdr type) :create)]
       [data (funcall create-func args)]
       (org-ilm-registry--register (car type) data))
      ([register-func (plist-get (cdr type) :register)]
       (funcall register-func args))
      ;; When nothing assigned, create empty capture
      (t (org-ilm-registry--register (car type) nil)))))

;;;###autoload
(defun org-ilm-registry-register-dwim ()
  "Add element at point to the registry."
  (interactive)
  (if-let ((types (seq-keep
                   (lambda (type)
                     (-some->> (plist-get (cdr type) :parse)
                       funcall
                       (cons (car type))))
                   org-ilm-registry-types))
           (type (if (> (length types) 1)
                     (assoc
                      (completing-read "Type: " (mapcar #'car types) nil t)
                      types)
                   (car types))))
      (funcall #'org-ilm-registry-register (car type) (cdr type))
    (call-interactively #'org-ilm-registry-register)))

;;;###autoload
(defun org-ilm-registry-attachments ()
  "View the attachments of a registry entry."
  (interactive)
  (let* ((attachments (org-attach-file-list (org-attach-dir)))
         (conversion (org-ilm-convert--conversion-by-id (org-id-get)))
         conversion-name)
    (when (and conversion (not (eq (plist-get conversion :state) 'success)))
      (setq conversion-name
            (concat (propertize (plist-get conversion :name) 'face 'italic)
                    " ("
                    (org-ilm-convert--conversion-propertize-state (plist-get conversion :state))
                    ")"))
      (push conversion-name attachments))
    (let ((choice (completing-read "Attachment: " attachments nil t)))
      (if (string= choice conversion-name)
          (switch-to-buffer (plist-get conversion :buffer))
        (find-file (expand-file-name choice (org-attach-dir)))))))

;;;; Functions

(cl-defun org-ilm-registry--select-entry (&key types collection)
  "Select registry entry."
  (let* ((types (if types (ensure-list types) (mapcar #'car org-ilm-registry-types)))
         (registry (org-ilm--collection-registry-path collection)))
    (car
     (consult--multi
      (seq-map
       (lambda (type-name)
         (list :name (capitalize type-name)
               :narrow (plist-get (cdr (assoc type-name org-ilm-registry-types)) :key)
               :items
               (lambda ()
                 (seq-keep
                  (lambda (entry)
                    (when (member (org-mem-entry-property "TYPE" entry)
                                  (org-ilm-registry--type-name-and-aliases type-name))
                      (cons (org-mem-entry-title entry) entry)))
                  (org-mem-entries-in-file registry)))))
       types)
      :require-match t
      :prompt "Registry entry: "))))

(defun org-ilm-registry-type-aliases ()
  "Return alist of type name to aliases.
Aliases are retrieved from `org-ilm-registry-type-aliases' and the :aliases
 key in `org-ilm-registry-types'."
  (mapcar
   (lambda (type)
     (let ((name (car type))
           (data (cdr type)))
       (cons
        name
        (cl-union (cdr (assoc name org-ilm-registry-type-aliases))
                  (plist-get data :aliases)
                  :test #'equal))))
   org-ilm-registry-types))

(defun org-ilm-registry--type-name-and-aliases (typename-or-alias)
  "Return a list of which name is first element and remainder are aliases."
  (let ((type (assoc typename-or-alias org-ilm-registry-types))
        (type-aliases (org-ilm-registry-type-aliases)))
    (if type
        (assoc typename-or-alias type-aliases)
      (cl-find-if (lambda (aliases)
                    (member typename-or-alias aliases))
                  type-aliases))))

(defun org-ilm-registry--type-data (typename-or-alias)
  (when-let ((type-name (car (org-ilm-registry--type-name-and-aliases typename-or-alias))))
    (assoc type-name org-ilm-registry-types)))

(defun org-ilm-registry--type-data-from-entry (entry-or-id)
  (when-let* ((entry (if (org-mem-entry-p entry-or-id)
                         entry-or-id
                       (org-mem-entry-by-id entry-or-id)))
              (type (org-mem-entry-property "TYPE" entry)))
    (org-ilm-registry--type-data type)))

(defun org-ilm-registry--id-links-to-entry (&optional entry-or-id)
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

(defun org-ilm-registry--entry-contents (entry)
  (cl-assert (org-mem-entry-p entry))
  (save-excursion
    (org-ilm--org-with-point-at
     (org-mem-entry-id entry)
     (org-ilm-registry--org-get-contents))))

(defun org-ilm-registry--link-target ()
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
;; can be typed out before send to `org-ilm-registry--register' but i'm facing
;; scoping issues and i dont like having two capture buffers in a row.

(defcustom org-ilm-registry-content-capture-path
  (expand-file-name
   "org-ilm-registry-input.org"
   temporary-file-directory)
  "Path of file where content is captured to, to be used with type registers."
  :type 'string
  :group 'org-ilm-registry)

(defun org-ilm-registry--org-capture (on-capture)
  (cl-letf* (((symbol-value 'org-capture-templates)
               (list
                (list
                 "i" "Input"
                 'plain
                 (list 'file org-ilm-registry-content-capture-path)
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
                 ;;   (with-current-buffer (find-file-noselect org-ilm-registry-content-capture-path)
                 ;;     ,@body))
                 :kill-buffer t
                 ))))
     (org-capture nil "i")))


;;;; Register

(defvar org-ilm-registry-register-hook nil
  "Hook run after an entry has been registered.")

(cl-defun org-ilm-registry--register (type data &key registry template)
  "Add an entry of TYPE with DATA to the REGISTRY.

DATA is a plist that can contain :title, :body, :id, :type, :props,
:region, :on-success.

TEMPLATE is a cons with car optional template string and cdr plist of
org-capture template properties."
  (let* ((registry (or registry (org-ilm--collection-registry-path)))
         (title (ignore-errors (string-trim (plist-get data :title))))
         (body (plist-get data :body))
         (type (or (plist-get data :type) type))
         (id (or (plist-get data :id) (org-id-new)))
         (props (org-combine-plists (list :ID id) (plist-get data :props)))
         (on-success (plist-get data :on-success))
         (template-string (car template)))
    (when org-ilm-registry-add-link-in-title
      (setq title
            (concat
             (format "[[%s][%s]]: " (org-ilm-registry--link-target) type)
             title)))
    (setf (plist-get props :SOURCE) (org-store-link '(4)))
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

                     (org-ilm--org-mem-update-cache-after-capture 'entry)

                     (when on-success (funcall on-success id))

                     (pcase-let ((`(,begin ,end) (plist-get data :region)))
                       (when (and begin end (yes-or-no-p "Replace with registry link? "))
                         (save-restriction
                           (delete-region begin end)
                           (org-ilm-registry-insert id))))

                     (run-hook-with-args 'org-ilm-registry-register-hook id)))
                 ))
               (template-props (org-combine-plists
                                (cdr template) default-template-props))
               ((symbol-value 'org-capture-templates)
                (list
                 (append 
                  (list "r" "Register" 'entry `(file ,registry) template-string)
                  template-props))))
      (org-capture nil "r"))))

(defun org-ilm-registry--org-get-contents ()
  (save-excursion
    (org-back-to-heading-or-point-min)
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

(defun org-ilm-registry-set-type (type &rest parameters)
  "Set registry TYPE properties to PARAMETERS.
PARAMETERS should be keyword value pairs. See `org-ilm-registry-types'."
  (let ((data (assoc type org-ilm-registry-types)))
    (if data
        (setcdr data (org-combine-plists (cdr data) parameters))
      (push (cons type parameters) org-ilm-registry-types))))

(defun org-ilm-registry--link-preview (ov id link)
  ;; This function must return a non-nil value to indicate success.
  (when-let* ((entry (org-mem-entry-by-id id))
              (type-data (org-ilm-registry--type-data-from-entry entry)))
    (overlay-put ov 'org-ilm-registry-type (car type-data))
    (org-with-point-at (org-element-begin link)
      (funcall (plist-get (cdr type-data) :preview) entry ov link))))

(defun org-ilm-registry--link-follow (id prefix-arg)
  (org-node-goto-id id))

(defun org-ilm-registry--link-face (id)
  (if-let* ((type-data (org-ilm-registry-type-data-from-entry id))
            (face (plist-get (car type-data) :face)))
      face
    'org-ilm-registry-link-face))

(org-link-set-parameters
 "registry"
 :follow #'org-ilm-registry--link-follow
 :preview #'org-ilm-registry--link-preview
 :face #'org-ilm-registry--link-face)


;;;;; Latex type

(defun org-ilm-registry--type-latex-from-entry (entry)
  "Return the latex content from ENTRY.
Can be either in the LATEX property or the body text."
  (or
   (org-mem-entry-property "LATEX" entry)
   (org-ilm-registry--entry-contents entry)))

(defun org-ilm-registry--type-latex-preview (entry ov link)
  "Place a latex overlay on the link.

The way this is implemented is by using `org-latex-preview-place' which
 actually creates a new overlay. Therefore we need some way to detect
 when the link preview overlay is deleted, so that latex overlay can
 also be deleted. The function called when a preview is turned off is
 `org-link-preview-clear' which calls `delete-overlay'. Unfortunately
 the 'modification-hooks overlay property does not detect overlay
 deletions. So my best solution at the moment is to advice
 `delete-overlay', which is done in the global minor mode."
  (when-let ((latex (org-ilm-registry--type-latex-from-entry entry)))
    (overlay-put ov 'org-ilm-registry-latex t)
    (org-latex-preview-place
     org-latex-preview-process-default
     (list
      (list (overlay-start ov) (overlay-end ov) latex )))
    t))

(defun org-ilm-registry--type-latex-teardown (ov)
  "When latex registry link ov gets deleted, remove the latex ov inside it."
  (dolist (o (overlays-in (overlay-start ov) (overlay-end ov)))
    (when (eq (overlay-get o 'org-overlay-type)
              'org-latex-overlay)
      (delete-overlay o))))

(defun org-ilm-registry--type-latex-paste (entry)
  (insert (org-ilm-registry--type-latex-from-entry entry)))

(defun org-ilm-registry--type-latex-parse ()
  "Parses latex value from text at point.
If inline fragment, use it as entry property value. If
environment (multiline), paste it in headline body."
  (when-let* ((org-element (when (eq major-mode 'org-mode)
                             (org-element-context)))
              (type (org-element-type org-element))
              (latex (org-element-property :value org-element)))
    (when (member type '(latex-fragment latex-environment))
      (when (eq type 'latex-fragment)
        (setq latex (s-replace-regexp "\n" "" latex)))
      (list :latex latex
            :fragment (eq type 'latex-fragment)
            :begin (org-element-property :begin org-element)
            :end (org-element-property :end org-element)))))

(defun org-ilm-registry--type-latex-create (&optional args)
  (cl-destructuring-bind (&key latex title fragment begin end) args
    (if (null latex)
        (list :title title :props (list :LATEX ""))
      (list :title title
            :body (unless fragment latex)
            :props (list :LATEX (when fragment latex))
            :region (list begin end)))))

(org-ilm-registry-set-type
 "latex"
 :key ?l
 :preview #'org-ilm-registry--type-latex-preview
 :teardown #'org-ilm-registry--type-latex-teardown
 :paste #'org-ilm-registry--type-latex-paste
 :parse #'org-ilm-registry--type-latex-parse
 :create #'org-ilm-registry--type-latex-create
 )


;;;;; Image type

;; Relies on var `image-file-name-extensions' to determine image files.

(defun org-ilm-registry--type-image-preview (entry ov link)
  (org-link-preview-file
   ov
   (with-current-buffer (find-file-noselect (org-mem-entry-file entry))
     (car
      (file-expand-wildcards
       (expand-file-name (concat (org-mem-entry-id entry) "*")
                         (org-attach-dir)))))
   link))

(defun org-ilm-registry--type-image-paste (entry)
  (let ((path (org-mem-entry-property "PATH" entry)))
    (insert (org-link-make-string
             (concat "file:" path)
             (file-name-base path)))))

(defun org-ilm-registry--type-image-from-path (path &optional title)
  "Return data for registry entry of type image from PATH."
  (when (and (stringp path) (file-exists-p path))
    (let* ((ext (file-name-extension path))
           (title (or title (file-name-base path))))
      (when (member ext image-file-name-extensions)
        (list :title title :path path)))))

(defun org-ilm-registry--type-image-parse ()
  "Return info of image from context."
  (cond-let*
    ;; In org-mode, check if point on file link that points to an image.
    ([el (when (eq major-mode 'org-mode) (org-element-context))]
     (when (and (eq (org-element-type el) 'link)
                (string= (org-element-property :type el) "file"))
       (when-let ((data (org-ilm-registry--type-image-from-path
                         (expand-file-name (org-element-property :path el))
                         (car (flatten-list
                               (org-element-property :caption el))))))
         (org-combine-plists
          data
          (list :begin (org-element-property :begin el)
                :end (org-element-property :end el))))))
    ;; Current buffer is image.
    ([data (org-ilm-registry--type-image-from-path
            (buffer-file-name (buffer-base-buffer)))]
     data)))
     
;; TODO Deprecated for register function
(defun org-ilm-registry--type-image-create (&optional data)
  (unless data
    (setq data (org-ilm-registry--type-image-from-path
                (read-file-name "File: "))))
  (map-let (:title :path :begin :end) data
    ;; TODO Debating on whether to batch query-replace this link in all org,
    ;; org-mem files, current dir files or not. Alternative would be to view
    ;; normal org links to this path same as registry link. Currently prefer
    ;; latter.
    (list :title title :props (list :PATH path))))

(defun org-ilm-registry--type-image-register (&optional data)
  (unless data
    (setq data (org-ilm-registry--type-image-from-path
                (read-file-name "File: "))))
  (map-let (:title :path :begin :end) data
    (let* ((registry (org-ilm--collection-registry-path))
           (id (org-id-new))
           (attach-dir (with-current-buffer (find-file-noselect registry)
                         (save-excursion
                           (goto-char (point-min))
                           (expand-file-name (org-attach-dir-get-create))))))
      (org-ilm-registry--register
       "image"
       (list
        :title title
        :id id
        :region (when (and begin end) (list begin end))
        :on-success
        (lambda (&rest _)
          (copy-file path
                     (expand-file-name
                      (concat id "." (file-name-extension path))
                      attach-dir))
          (run-hook-with-args 'org-attach-after-change-hook attach-dir)
          (org-attach-tag)))))))

(org-ilm-registry-set-type
 "image"
 :key ?i
 :preview #'org-ilm-registry--type-image-preview
 :paste #'org-ilm-registry--type-image-paste
 :parse #'org-ilm-registry--type-image-parse
 ;; :create #'org-ilm-registry--type-image-create
 :register #'org-ilm-registry--type-image-register
 )

;;;;; Org type

(defface org-ilm-registry-org-header-face
  '((t :slant italic))
  "Face used to display the title above the transcluded content.")

(defun org-ilm-registry--type-org-preview (entry ov link)
  "Transclude Org mode content with org-transclusion."
  (let (line-end-pos) 
    (save-excursion
      (goto-char (overlay-end ov))
      (setq line-end-pos (line-end-position))
      (insert (format "\n#+transclude: [[id:%s]] :only-contents"
                      (org-mem-entry-id entry)))
      (beginning-of-line)
      (overlay-put ov 'org-ilm-registry-marker (point-marker))
      (org-transclusion-add))

    (let ((hide-ov (make-overlay (overlay-start ov) (1+ line-end-pos))))
      ;; (overlay-put hide-ov 'org-ilm-registry-hideov t)
      (overlay-put hide-ov 'invisible t)
      (overlay-put ov 'org-ilm-registry-hideov hide-ov))

    ;; (overlay-put ov 'invisible t)
    
    ;; (overlay-put ov 'display (org-mem-entry-title entry))
    ;; ;; TODO fgiure out how to not inherit org-link properties
    ;; (overlay-put ov 'face 'org-ilm-registry-org-header-face)
    
    t))

(defun org-ilm-registry--type-org-transclusion-cleanup (buf beg end)
  (when-let* ((transclusion (org-transclusion-at-point))
              (location (plist-get transclusion :location)))
    (save-excursion
      (goto-char (car location))
      (forward-line -1)
      (dolist (ov (overlays-at (point)))
        (when (string= (overlay-get ov 'org-ilm-registry-type) "org")
          ;; Need a delay, otherwise infinite loop:
          ;; 1. This hook deletes the overlay
          ;; 2. Ov deletion advice calls org-ilm-registry--type-org-teardown
          ;; 3. Removes org transclusion
          ;; 4. This hook is called again
          (run-at-time .1 nil (lambda () (delete-overlay ov)))
          )))))

(add-hook 'org-transclusion-after-remove-functions
          #'org-ilm-registry--type-org-transclusion-cleanup)

(defun org-ilm-registry--type-org-teardown (ov)
  (let ((marker (overlay-get ov 'org-ilm-registry-marker)))
    (when (and marker (marker-position marker))
      (save-excursion
        (goto-char marker)
        (cond
         ;; Check if the transclusion is still active.
         ((org-transclusion-within-transclusion-p)
          (let ((keyword-beg (org-transclusion-remove)))
            ;; `org-transclusion-remove` deletes the transcluded text and
            ;; re-inserts the original `#+transclude` keyword. It returns the
            ;; start position (`beg`) of the re-inserted keyword.
            (goto-char keyword-beg)
            (org-transclusion-keyword-remove)))
         
         ;; Fallback: if `org-transclusion-add` previously failed or the
         ;; transclusion was manually detached, just delete the keyword line if
         ;; we see it.
         (t
          (org-transclusion-keyword-remove))))))

  (when-let ((hide-ov (overlay-get ov 'org-ilm-registry-hideov)))
    (delete-overlay hide-ov)))

(defun org-ilm-registry--type-org-paste (entry)
  (insert (org-ilm-registry--entry-contents entry)))

(defun org-ilm-registry--type-org-parse ()
  (when (and (eq major-mode 'org-mode) (region-active-p))
    (let ((begin (region-beginning))
          (end (region-end)))
      (list :body (buffer-substring-no-properties begin end)
            :region (list begin end)))))

(defun org-ilm-registry--type-org-create (&optional args)
  args)

(org-ilm-registry-set-type
 "org"
 :key ?o
 :preview #'org-ilm-registry--type-org-preview
 :teardown #'org-ilm-registry--type-org-teardown
 :paste #'org-ilm-registry--type-org-paste
 :parse #'org-ilm-registry--type-org-parse
 :create #'org-ilm-registry--type-org-create
 )

;;; Footer

(provide 'org-ilm-registry)

;;; org-ilm-registry.el ends here
