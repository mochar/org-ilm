;;; org-ilm-import.el --- Import -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'transient)
(require 'org-attach)
(require 'bibtex)
(require 'citar)
(require 'ffap)
(require 'htmlfontify)

(require 'org-ilm-utils)
(require 'org-ilm-collection)
(require 'org-ilm-bib)
(require 'org-ilm-capture)
(require 'org-ilm-element)
(require 'org-ilm-registry)
(require 'org-ilm-convert)

;;;; Import

(transient-define-infix org-ilm--import-transient-collection ()
  :class 'transient-option
  :transient 'transient--do-call
  :allow-empty nil
  :always-read t
  :argument "--collection="
  :reader
  (lambda (&rest _)
    (let* ((choices (map-apply
                     (lambda (collection data)
                       (cons
                        (format
                         "%s %s"
                         (propertize (symbol-name collection) 'face 'bold)
                         (plist-get data :path))
                        collection))
                     (org-ilm-collections)))
           (choice (completing-read "Collection: " choices))
           (collection (map-elt choices choice)))
      (cond
       (collection (symbol-name collection))
       ((not (string-empty-p choice))
        (setq collection choice)
        (org-ilm-new-collection
         (intern collection)
         (org-ilm--collection-property (org-ilm--active-collection) :path))
        collection)))))

(transient-define-prefix org-ilm--import-transient (scope)
  :refresh-suffixes t
  :value
  (lambda ()
    (list (format "--collection=%s" (org-ilm--active-collection))))
  
  ["Ilm"
   ("C" "Collection" org-ilm--import-transient-collection)
   ("." "As child" "--child"
    :inapt-if-not (lambda () (transient-scope)))
   (:info
    (lambda ()
      (let ((element (transient-scope))
            (as-child (transient-arg-value "--child" (transient-get-value))))
        (propertize (org-ilm-element--title element)
                    'face (if as-child 'transient-value 'Info-quoted))))
    :if (lambda () (and (transient-scope)
                        (transient-arg-value "--child" (transient-get-value)))))
   ]

  [
   ["Import"
    ("r" "Resource"
     (lambda ()
       (interactive)
       (let ((args (org-ilm--import-transient-args)))
         (org-ilm--import-resource-transient (plist-get args :collection))))
     :inapt-if org-ilm--import-transient-parent-el
     )
    ("f" "File"
     (lambda ()
       (interactive)
       (org-ilm--import-file-transient (transient-scope)))
     :inapt-if org-ilm--import-transient-parent-el)
    ("m" "Media" org-ilm--import-media-transient
     :inapt-if org-ilm--import-transient-parent-el)
    ("g" "Registry" org-ilm--import-registry-transient
     :inapt-if org-ilm--import-transient-parent-el)
    ]
   ["New"
    ("o" "Org"
     (lambda ()
       (interactive)
       (let ((args (org-ilm--import-transient-args)))
         (org-ilm-org-new-material (plist-get args :collection) (plist-get args :parent)))))
    ("b" "Buffer" org-ilm-import-buffer)
    ("c" "Card"
     (lambda ()
       (interactive)
       (let ((args (org-ilm--import-transient-args)))
         (org-ilm-org-new-card (plist-get args :collection) (plist-get args :parent)))))
    ]
   ]

  (interactive "P")
  (transient-setup 'org-ilm--import-transient nil nil :scope scope))

(defun org-ilm--import-transient-args ()
  (let* ((args (if transient-current-command
                   (transient-args transient-current-command)
                 (transient-get-value)))
         (as-child (transient-arg-value "--child" args))
         (collection (intern (transient-arg-value "--collection=" args))))
    (list :parent (when as-child (transient-scope))
          :collection collection)))

(defun org-ilm--import-transient-parent-el ()
  "Return parent ilm element if chosen to save element as its child."
  (when-let ((parent-el (transient-scope))
             (as-child (transient-arg-value
                        "--child"
                        (transient-get-value))))
                        ;; (transient-args 'org-ilm--import-transient))))
    parent-el))

(defun org-ilm-import ()
  "Import an element into your ilm collection."
  (interactive)
  (org-ilm--import-transient (ignore-errors (org-ilm--element-from-context))))

;;;;; Queue

;; (defun org-ilm-import-queue ()
;;   (interactive)
;;   (let* ((buf (org-ilm--bqueue-buffers-select))
;;          (queue (with-current-buffer buf org-ilm-queue))
;;          (title (org-ilm-queue--name queue))
;;          (id (org-id-new))
;;          (parent-el (org-ilm--import-transient-parent-el)))
;;     (org-ilm--capture-capture
;;      'queue
;;      :id id
;;      :parent (when parent-el (org-ilm-element--id parent-el))
;;      :collection (org-ilm--active-collection)
;;      :title title
;;      :props props
;;      )))    

;;;;; Buffer

(defun org-ilm-import-buffer ()
  (interactive)
  (pcase-let* (((map :parent parent-el :collection collection) (org-ilm--import-transient-args))
               ;;(buf (read-buffer "Buffer: " (current-buffer) 'require-match))
               (buf (org-ilm--consult-buffer-select))
               (title (buffer-name buf))
               (id (org-id-new))
               (props)
               (file))

    (with-current-buffer buf
      ;; Process buffer
      (pcase major-mode
        ('gptel-mode
         (require 'gptel)
         (gptel-org--save-state))
        (_
         ;; Link roam ref
         (when-let ((link (org-store-link '(4))))
           (setq props (list :ROAM_REFS link))
           (when-let* ((m (string-match org-link-bracket-re link))
                       (desc (match-string 2 link)))
             (setq title desc)))

         ;; Convert to html 
         (when-let ((html-buf (ignore-errors (hfy-fontify-buffer)))
                    (tmp-file (make-temp-file nil)))
           (with-current-buffer html-buf
             (write-region nil nil tmp-file))
           (setq file tmp-file)
           (kill-buffer html-buf))
         ))

      ;; Capture
      (org-ilm--capture-capture
       'material
       :id id
       :parent (-some-> parent-el org-ilm-element--id)
       :collection collection
       :content (unless file (buffer-string))
       :title title
       :props props
       :capture-kwargs '(:immediate-finish nil)
       :on-success
       (lambda (id attach-dir collection)
         (when file
           (org-ilm-convert--convert-multi
            :process-name "buffer"
            :process-id id
            :converters
            (list
             (cons #'org-ilm-convert--convert-with-marker
                   (list
                    :input-path file
                    :format "markdown"
                    :output-dir attach-dir
                    :move-content-out t
                    :flags '("--disable_ocr")))
             (cons #'org-ilm-convert--convert-with-pandoc
                   (list
                    :input-path (expand-file-name (concat (file-name-base file) ".md") attach-dir)
                    :input-format "markdown"
                    :output-name id))
             )
            :on-error nil
            :on-final-success
            (lambda (proc buf id)
              (message "[Org-Ilm-Convert] Buffer conversion completed")
              (org-ilm--org-with-point-at id
                (org-attach-sync))))))
       ))))

;;;;; File

(defun org-ilm--import-file-transient-args ()
  (let* ((args (if transient-current-command
                   (transient-args transient-current-command)
                 (transient-get-value)))
         (file (transient-arg-value "--file=" args))
         (method (transient-arg-value "--method=" args)))
    (append
     (org-ilm--import-transient-args)
     (list :file file :method method))))

(transient-define-infix org-ilm--import-file-transient-file ()
  :class 'transient-option
  :transient t
  :argument "--file="
  :allow-empty nil
  :prompt "File: "
  :reader
  (lambda (prompt initial-input history)
    (read-file-name prompt nil initial-input t)))

(transient-define-prefix org-ilm--import-file-transient (scope)
  :refresh-suffixes t
  :value
  (lambda ()
    (append
     '("--method=cp")
     (when buffer-file-name
       (list (concat "--file=" buffer-file-name)))))
  
  ["File import"
   ("f" "File" org-ilm--import-file-transient-file)
   ("m" "Method of attachment" "--method="
    :allow-empty nil
    :choices (mv cp ln lns) :prompt "Method of attachment: ")
   ("RET" "Import"
    (lambda ()
      (interactive)
      (let ((args (org-ilm--import-file-transient-args)))
        (org-ilm--capture-capture
         'material
         :parent (when-let ((el (transient-scope)))
                   (org-ilm-element--id el))
         :collection (org-ilm--active-collection)
         :file (plist-get args :file)
         :method (intern (plist-get args :method))
         :ext t)))
    :inapt-if-not
    (lambda ()
      (let ((args (org-ilm--import-file-transient-args)))
        (not
         (and (plist-get args :file)
              (plist-get args :method)
              (plist-get args :collection))))))
   ]
  
  (interactive "P")
  (transient-setup 'org-ilm--import-file-transient nil nil :scope scope))

;;;;; Media

;; TODO Can this go?
;; Remote videos: Resource import
;; Local videos: File import

(defun org-ilm--import-media-transient-args (&optional args)
  (setq args (or args (transient-args 'org-ilm--import-media-transient)))
  (list :source (transient-arg-value "--source=" args)
        :title (transient-arg-value "--title=" args)
        :collection org-ilm--active-collection))

(defun org-ilm--import-media-read-source ()
  (when-let* ((source (ffap-read-file-or-url "File or URL: " nil))
              (title source))
    (setq title (if (org-url-p source)
                    (org-ilm-convert--ytdlp-title-from-url source)
                  (file-name-base source)))
    (org-ilm--transient-set-target-value "t" title)
    (list source title)))

(transient-define-infix org-ilm--import-media-transient-source ()
  :class 'transient-option
  :transient 'transient--do-call
  :argument "--source="
  :allow-empty nil
  :always-read t
  :reader
  (lambda (prompt initial-input history)
    (car (org-ilm--import-media-read-source))))

(transient-define-prefix org-ilm--import-media-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (append
     (when-let ((source (org-ilm--import-media-read-source)))
       (list (concat "--source=" (car source))
             (concat "--title=" (cadr source))))))

  ["Media import"
   ("s" "Source" org-ilm--import-media-transient-source)
   ("t" "Title" "--title=" :prompt "Title: " :always-read t :transient transient--do-call)
   ("RET" "Import"
    (lambda ()
      (interactive)
      (cl-destructuring-bind (&key source title collection &allow-other-keys)
          (org-ilm--import-media-transient-args)
        (org-ilm--capture-capture
         'material :collection collection
         :title title :content ""
         :props (list :ILM_MEDIA source))))
    :inapt-if-not
    (lambda ()
      (cl-destructuring-bind (&key source &allow-other-keys)
          (org-ilm--import-media-transient-args (transient-get-value))
        source)))])

;;;;; Resource

;; Generic object with citation that contains zero or more attachments. Think of:
;; - Website article / blog post
;; - Youtube video (media)
;; - Paper from arxiv link or doi (paper)
;; - Paper from local pdf file

(defcustom org-ilm-pdf2doi-path (executable-find "pdf2doi")
  "Path to the pdf2doi executable."
  :type 'file
  :group 'org-ilm-import)

(defun org-ilm--pdf2doi (path)
  "Use pd2doi to attempt extraction of doi from pdf in PATH."
  (cl-assert (f-absolute-p path))
  (when org-ilm-pdf2doi-path
    ;; pdf2doi returns table, one line per file. we only pass one file so only
    ;; fetch the first line.
    (when-let* ((output (car (process-lines org-ilm-pdf2doi-path path)))
                (doi (string-trim (nth 1 (string-split output)))))
      (when (and doi (not (string-empty-p doi)) (not (string= doi "n.a.")))
        doi))))

(defvar org-ilm--import-resource-data nil)

(defun org-ilm--import-resource-process-source (&optional source)
  (unless source
    (setq source (ffap-read-file-or-url
                  "URL/DOI/PDF path: "
                  (or (thing-at-point 'url)
                      ;; Works in dired!
                      (thing-at-point 'existing-filename)))))

  (when (and source (not (string-empty-p source)))

    (let ((type "resource")
          (title source)
          source-type data)
      
      (cond
       ((file-exists-p source)
        (setq source (expand-file-name source)
              source-type 'file)
        (when-let ((doi (org-ilm--pdf2doi source)))
          (setq data (org-ilm--citation-get-zotero doi))))
       (t
        (setq data (org-ilm--citation-get-zotero source)
              source-type (if (org-url-p source) 'url 'id))))

      (when data
        (setq title (or (alist-get 'title data)
                        (alist-get 'shortTitle data)
                        (when (eq source-type 'url)
                          (org-ilm--get-page-title source))
                        source))

        (pcase (alist-get 'itemType data)
          ((or "preprint" "conferencePaper" "document" "journalArticle" "manuscript")
           (setq type "paper"))
          ((or "videoRecording" "audioRecording")
           (setq type "media"))
          (_ (setq type "website"))))
      
      (list :source source :source-type source-type
            :title title :type type :data data))))

(defun org-ilm--import-resource-transient-args (&optional args)
  ;; (setq args (or args (transient-args 'org-ilm--import-resource-transient)))
  (setq args (if transient-current-command
                 (transient-args transient-current-command)
               (transient-get-value)))
  (cl-destructuring-bind (&key source-type bibtex id key title &allow-other-keys)
      org-ilm--import-resource-data
    
    (list :source (transient-arg-value "--source=" args)
          :source-type source-type
          :type (transient-arg-value "--type=" args)
          :bibtex bibtex
          :key key
          :id id
          :title title

          :as-child (transient-arg-value "--child" args)
          
          :file-method (intern (or (transient-arg-value "--file-method" args) "cp"))

          :html-download (transient-arg-value "--html-download" args)

          :paper-download (transient-arg-value "--paper-download" args)

          ;; Webpage Download
          :webpage-download (transient-arg-value "--webpage-download" args)
          :webpage-simplify
          (cond
           ((transient-arg-value "--webpage-simplify-to-html" args)
            "html")
           ((transient-arg-value "--webpage-simplify-to-markdown" args)
            "markdown"))
          :webpage-orgify (transient-arg-value "--webpage-orgify" args)

          ;; Media download
          :media-download (transient-arg-value "--media-download" args)
          :media-template (transient-arg-value "--media-template=" args)
          :media-audio-only (transient-arg-value "--media-audio" args)
          :media-sub-langs (cdr (assoc "--media-subs=" args)))))

(transient-define-infix org-ilm--import-resource-transient-source ()
  :class 'transient-option
  :transient 'transient--do-call
  :key "s"
  :description "Source"
  :argument "--source="
  :always-read t
  :allow-empty nil
  :reader
  (lambda (&rest _)
    (let ((source-data (org-ilm--import-resource-process-source)))
      (setq org-ilm--import-resource-data source-data)
      (org-ilm--transient-set-target-value "t" (plist-get source-data :type))
      (plist-get source-data :source))))

(transient-define-infix org-ilm--import-resource-transient-key ()
  :class 'transient-option
  :transient 'transient--do-call
  :key "ck"
  :description
  (lambda ()
    (concat
     "Key"
     (when-let* ((key (plist-get org-ilm--import-resource-data :key))
                 (entry (or (org-mem-entry-by-roam-ref (concat "@" key))
                            (citar-get-entry key))))
       (propertize " DUPLICATE" 'face 'error))))
  :argument "--key="
  :always-read t
  :allow-empty nil
  :inapt-if-not
  (lambda () (plist-get org-ilm--import-resource-data :bibtex))
  :reader
  (lambda (&rest _)
    (let ((key (read-string "Key (empty to auto-generate): "))
          (bibtex (plist-get org-ilm--import-resource-data :bibtex)))
      (unless (and key (not (string-empty-p key)))
        (with-temp-buffer
          (insert (org-ilm--format-bibtex-entry
                   bibtex
                   (plist-get org-ilm--import-resource-data :key)))
          (goto-char (point-min))
          (setq key (ignore-errors (bibtex-generate-autokey)))
          (unless (and key (not (string-empty-p key)))
            (setq key (upcase (substring (org-id-uuid) 0 8))))))
      (setf (plist-get org-ilm--import-resource-data :key) key
            (alist-get "=key=" bibtex nil nil #'string=) key)
      (org-ilm--transient-set-target-value "ck" key))))

(transient-define-prefix org-ilm--import-resource-transient (scope)
  :refresh-suffixes t
  :value
  (lambda ()
    (let ((id (or (plist-get org-ilm--import-resource-data :id) (org-id-new)))
          (source (plist-get org-ilm--import-resource-data :source)))
      (unless source
        (setq org-ilm--import-resource-data
              (org-ilm--import-resource-process-source))
        (setq source (plist-get org-ilm--import-resource-data :source)))
      (setf (plist-get org-ilm--import-resource-data :id) id)
      (append
       '("--webpage-simplify-to-markdown" "--webpage-orgify"
         "--media-template=%(title)s.%(ext)s"
         "--file-method=cp")
       (list (concat "--source=" source)
             (concat "--key=" (plist-get org-ilm--import-resource-data :key))
             (concat "--type=" (plist-get org-ilm--import-resource-data :type))
             ;; Don't want media filename to be org-id as I rely on ILM_MEDIA
             ;; prop to point to media. The dedicated attachment is the org file
             ;; in media elements
             ;; (concat "--media-template=" id ".%(ext)s")
             )
       (when (string= "website" (plist-get org-ilm--import-resource-data :type))
         (list "--html-download")))))

  ["Import Resource"
   (org-ilm--import-resource-transient-source)
   (:info
    (lambda ()
      (let ((title (plist-get org-ilm--import-resource-data :title)))
          (propertize title 'face 'italic)))
    :if (lambda () (plist-get org-ilm--import-resource-data :title)))
   ("t" "Type" "--type=" :choices ("website" "media" "paper" "resource")
    :always-read t :allow-empty nil)
   ]

  ["File"
   :hide (lambda () (not (eq (plist-get org-ilm--import-resource-data :source-type) 'file)))
   ("fm" "Method of attachment" "--file-method="
    :allow-empty nil
    :always-read t
    :choices (mv cp ln lns) :prompt "Method of attachment: ")
   ]
   

  ["HTML -> org download"
   :if (lambda () (not (eq (plist-get org-ilm--import-resource-data :source-type) 'file)))
   ("hd" "Download" "--html-download")
   ]

  ["Webpage download"
   :hide
   (lambda ()
     (when-let ((args (org-ilm--import-resource-transient-args (transient-get-value))))
       (or (not (eq (plist-get args :source-type) 'url))
           (string= (plist-get args :type) "media")
           (plist-get args :html-download))))
   :setup-children
   (lambda (_)
     (org-ilm-convert--transient-webpage-build t t))]

  ["Media download"
   :if
   (lambda ()
     (when-let ((args (org-ilm--import-resource-transient-args (transient-get-value))))
       (and (string= (plist-get args :type) "media")
            (eq (plist-get args :source-type) 'url))))
   :setup-children
   (lambda (_)
     (org-ilm-convert--transient-media-build))
   ]

  ["Paper download"
   :if
   (lambda ()
     (when-let ((args (org-ilm--import-resource-transient-args (transient-get-value))))
       (and (string= (plist-get args :type) "paper")
            (not (eq (plist-get args :source-type) 'file)))))
   ("pd" "Download" "--paper-download" :transient transient--do-call)
   ]

  ["Citation"
   ("cc" "Add citation"
    (lambda ()
      (interactive)
      ;; TODO We already save the zotero data in :data, write a function to
      ;; transform it into bibtex
      (let* ((args (org-ilm--import-resource-transient-args))
             (source (plist-get args :source)))
        (if-let* ((bibtex (org-ilm--citation-get-bibtex source 'as-alist))
                  (key (cdr (assoc "=key=" bibtex))))
            (progn
              (setf (plist-get org-ilm--import-resource-data :bibtex) bibtex
                    (plist-get org-ilm--import-resource-data :key) key)
              (org-ilm--transient-set-target-value "ck" key))
          (message "Bibtex could not be found"))))
    :transient transient--do-call)
   (org-ilm--import-resource-transient-key)
   ]

  ["Actions"
   ("RET" "Import"
    (lambda ()
      (interactive)
      (cl-destructuring-bind
          (&key source source-type type bibtex key title id as-child
                file-method
                html-download
                paper-download
                webpage-download webpage-simplify webpage-orgify
                media-download media-template media-audio-only media-sub-langs)
          (org-ilm--import-resource-transient-args)
        (let ((transient-args (transient-args 'org-ilm--import-resource-transient))
              (media-no-download (and (string= type "media") (not media-download)))
              (file-p (eq source-type 'file))
              content)

          (setq content
                (cond
                 (html-download
                  (org-ilm--get-website-as-org source))
                 (media-no-download
                  ;; Create an org attachment for note taking when importing media
                  ;; without downloading. When downloading, set in on-success
                  ;; callback. HTML download is inactive when type is media, so no
                  ;; clash with org file generated by it.
                  "")))
          
          (org-ilm--capture-capture
           'material
           :collection (transient-scope)
           :title title
           :id id
           :bibtex bibtex
           :content content
           :file (when file-p source)
           :method (when file-p file-method)
           :ext (when file-p t)
           :props
           (append
            (list :ROAM_REFS (unless file-p (if key (concat source " @" key) source)))
            ;; Media URL as media prop when we are not downloading. When
            ;; downloading, the source file is determined by ytdlp and set later
            ;; on.
            (when media-no-download (list :ILM_MEDIA source)))
           :on-success
           (lambda (id attach-dir collection)
             (when (or html-download webpage-download media-download paper-download)
               (make-directory attach-dir t)

               (when paper-download
                 (condition-case nil
                     (progn
                       (zotra-download-attachment
                        source nil
                        (expand-file-name (concat id ".pdf") attach-dir))
                       (org-ilm--org-with-point-at id
                         (org-attach-sync)
                         (org-entry-put nil org-ilm-property-ext "pdf")))
                   (error (message "Failed to download paper for %s" title))))

               (when webpage-download
                 (org-ilm-convert--transient-webpage-run
                  source id attach-dir id transient-args))

               (when (or media-download media-sub-langs)
                 (org-ilm-convert--transient-media-run
                  source attach-dir id transient-args
                  (lambda (id output-path)
                    (org-entry-put nil org-ilm-property-media (file-name-nondirectory output-path))
                    ;; Create org file for annotations
                    (let ((f (expand-file-name (concat id ".org")
                                               (file-name-directory output-path))))
                      (unless (file-exists-p f)
                        (make-empty-file f))))))
                  ))
               )))))
   ]

  (interactive "P")
  (transient-setup 'org-ilm--import-resource-transient nil nil :scope scope)
  (org-ilm--add-hook-once
       'transient-post-exit-hook
       (lambda () (setq org-ilm--import-resource-data nil))))

;;; Footer

(provide 'org-ilm-import)

;;; org-ilm-import.el ends here
