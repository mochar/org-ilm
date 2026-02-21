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
(require 'org-ilm-transient)
(require 'org-ilm-collection)
(require 'org-ilm-capture)
(require 'org-ilm-element)
(require 'org-ilm-registry)
(require 'org-ilm-convert)

;;;; Data

(cl-defstruct (org-ilm-import
               (:conc-name org-ilm-import--))
  ""
  (id
   (org-id-new)
   :documentation "Org-id")
  (source
   nil
   :documentation "User input (file/url/doi)")
  (ref
   nil
   :documentation "Global identifier (url/doi)")
  (type
   nil
   :documentation "One of: plain, buffer, file, media, webpage, org, card")
  (bibtex
   nil
   :documentation "Cons (key . bibtex entry)")
  (info
   nil
   :documentation "Info from zotero translation server
See `org-ilm--citation-get-zotero'")
  )

(defun org-ilm-import--zotero-type (data)
  (alist-get 'itemType (oref data info)))

(defun org-ilm-import--zotero-category (data)
  (when-let ((zotero-type (org-ilm-import--zotero-type data)))
    (pcase zotero-type
      ((or "preprint" "conferencePaper" "document" "journalArticle" "manuscript")
       'paper)
      ((or "videoRecording" "audioRecording")
       'media)
      (_
       'other))))

;;;; Dispatcher transient

;; Transient to let user select what type to import.

(transient-define-prefix org-ilm--import-dispatch-transient ()
  [
   ["Import"
    ("w" "Webpage" org-ilm-import-webpage)
    ("f" "File" org-ilm-import-file)
    ("m" "Media" org-ilm-import-media)]
   [""
    ("u" "URL" org-ilm-import-url)
    ("b" "Buffer" org-ilm-import-buffer)]
   ["New"
   ("o" "Org" org-ilm-org-new-material)
   ("c" "Card" org-ilm-org-new-card)]
   ]
  )

(defun org-ilm-import ()
  "Import an element into your ilm collection."
  (interactive)
  (org-ilm--import-dispatch-transient))

(defun org-ilm-import-file (path)
  "Import a file to your collection."
  (interactive "fFile: ")
  (unless (file-exists-p path)
    (user-error "File %s does not exist" path))
  (let* ((ext (file-name-extension path))
         ref)
    ;; (cond
    ;;  ((string= "pdf" ext)
    ;;   (setq ref (org-ilm--pdf2doi (expand-file-name path)))))
    (org-ilm--import-transient
     (make-org-ilm-import :source path :ref ref :type 'file))))

(defun org-ilm-import-media ()
  "Import a audio or video to your collection."
  (interactive)
  (let ((source (ffap-read-file-or-url
                 "URL or file: "
                 (or (thing-at-point 'url)
                     ;; Works in dired!
                     (thing-at-point 'existing-filename)))))
    (cond
     ((file-exists-p source)
      (unless (org-ilm--media-file-p source)
        (user-error "File %s not video or audio" source))
      (org-ilm-import-file (expand-file-name source)))
     ((org-url-p source)
      (org-ilm--import-transient
       (make-org-ilm-import :source source :ref source :type 'media)))
     (t
      (user-error "Not a file or URL: %s" source)))))

(defun org-ilm-import-url (url)
  "Import something from a URL to your collection."
  (interactive (list (read-string "URL: " (thing-at-point 'url))))
  (unless (org-url-p url)
    (user-error "Not a valid URL: %s" url))
  (org-ilm--import-transient
   (make-org-ilm-import :source url :ref url :type 'url)))

(defun org-ilm-import-webpage (url)
  "Import a webpage to your collection."
  (interactive (list (read-string "URL: " (thing-at-point 'url))))
  (unless (org-url-p url)
    (user-error "Not a valid URL: %s" url))
  (org-ilm--import-transient
   (make-org-ilm-import :source url :ref url :type 'webpage)))


;;;; Queue

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

;;;; Buffer

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

;;;; Import transient

;; Main import transient. The transient is passed a `org-ilm-import' struct
;; object, which is used to determine which suffixes/infixes to present to the
;; user.

(defun org-ilm--import-transient-parse ()
  (let ((args (if transient-current-command
                  (transient-args transient-current-command)
                (transient-get-value))))
    (with-slots (id source ref type bibtex info) (transient-scope)
      (append
       args
       `((source . ,source)
         (id . ,id)
         (ref . ,ref)
         (type . ,type)
         (bibtex . ,bibtex)
         (info . ,info))))))

(transient-define-infix org-ilm--import-transient-collection ()
  :class 'org-ilm-transient-cons-option
  :transient 'transient--do-call
  :always-read t
  :allow-empty nil
  :key "c"
  :description "Collection"
  :argument 'collection
  :reader
  (lambda (&rest _)
    (let* ((cur-collection (alist-get 'collection (org-ilm--import-transient-parse)))
           (choices (map-apply
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
      (when (and (null collection) (not (string-empty-p choice)))
        (setq collection (intern choice))
        (org-ilm-new-collection
         collection
         (org-ilm--collection-property (org-ilm--active-collection) :path)))
      (unless (eq collection cur-collection)
        (org-ilm--transient-set-target-value "p" nil)
        (org-ilm--transient-set-target-value "s" nil))
      collection)))

(transient-define-infix org-ilm--import-transient-schedule ()
  :class 'org-ilm-transient-cons-option
  :transient 'transient--do-call
  :always-read t
  :allow-empty nil
  :key "s"
  :description "Schedule"
  :argument 'schedule
  :reader
  (lambda (&rest _)
    (org-read-date 'with-time nil nil "Schedule: ")))

(transient-define-infix org-ilm--import-transient-priority ()
  :class 'org-ilm-transient-cons-option
  :transient 'transient--do-call
  :always-read t
  :allow-empty nil
  :key "p"
  :format " %k %d"
  :description
  (lambda ()
    (concat
     "Priority: "
     (map-let (priority collection) (org-ilm--import-transient-parse)
       (cond
        (priority
         (propertize 
          (org-ilm-queue--position-format (org-ilm--pqueue collection) priority 1)
          'face 'transient-value))
        (t
         (propertize "nil" 'face 'transient-inactive-value))))))
  :argument 'priority
  :reader
  (lambda (&rest _)
    (let* ((collection (alist-get 'collection (org-ilm--import-transient-parse))) 
           (priority (org-ilm--bqueue-select-read
                      (org-ilm--pqueue-bqueue collection)
                      nil ;; init
                      "Priority: "
                      1))
           (sched (org-ilm--initial-schedule-from-priority (cdr priority))))
      (org-ilm--transient-set-target-value "s" (ts-format "%Y-%m-%d" sched))
      priority)))

(transient-define-infix org-ilm--import-transient-key ()
  :class 'org-ilm-transient-cons-option
  :transient 'transient--do-call
  :always-read t
  :allow-empty nil
  :key "k"
  :argument 'key
  :description
  (lambda ()
    (concat
     "Key"
     (when-let* ((key (car (oref (transient-scope) bibtex)))
                 (entry (or (org-mem-entry-by-roam-ref (concat "@" key))
                            (citar-get-entry key))))
       (propertize " DUPLICATE" 'face 'error))))
  :inapt-if-not
  (lambda () (cdr (oref (transient-scope) bibtex)))
  :reader
  (lambda (&rest _)
    (let ((key (read-string "Key (empty to auto-generate): "))
          (bibtex (cdr (oref (transient-scope) bibtex))))
      (unless (and key (not (string-empty-p key)))
        (with-temp-buffer
          (insert (org-ilm--format-bibtex-entry
                   bibtex
                   (car (oref (transient-scope) bibtex))))
          (goto-char (point-min))
          (setq key (ignore-errors (bibtex-generate-autokey)))
          (unless (and key (not (string-empty-p key)))
            (setq key (upcase (substring (org-id-uuid) 0 8))))))
      (setf (oref (transient-scope) bibtex) (cons key bibtex)
            (alist-get "=key=" bibtex nil nil #'string=) key)
      key)))

(transient-define-prefix org-ilm--import-transient (data)
  :refresh-suffixes t
  :incompatible '((html-download webpage-download)) 
  
  [""
   :description (lambda () (format "Import %s" (oref (transient-scope) type)))
   ("x" "X"
    (lambda ()
      (interactive)
      (setq x (org-ilm--import-transient-parse))
      (message "%s" (org-ilm--import-transient-parse)))
    :transient t)
   (org-ilm--import-transient-collection)
   (org-ilm--import-transient-priority)
   (org-ilm--import-transient-schedule)
   ;; TODO Parent element
   ("t" "Title"
    :cons 'title
    :class org-ilm-transient-cons-option
    :prompt "Title: "
    :always-read t :allow-empty nil :transient t)
   ]

  ["File"
   :if (lambda () (eq (oref (transient-scope) type) 'file))
   ("a" "Method of attachment"
    :cons 'file-method
    :class org-ilm-transient-cons-switches
    :choices (cp mv ln lns)
    :allow-empty t)
   (:info
    (lambda ()
      (let ((method (alist-get 'file-method (org-ilm--import-transient-parse))))
        (propertize 
         (pcase method
           ('cp "Copy")
           ('mv "Move")
           ('ln "Hard link")
           ('lns "Symbolic link")
           (_ "Don't attach, add to ref"))
         'face 'Info-quoted)))
    :transient t)
   ]

  ["HTML -> org download"
   :if (lambda () (member (oref (transient-scope) type) '(webpage url)))
   (:info* (lambda () "Convert HTML to Org"))
   ("hd" "Download"
    :cons 'html-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ]

  ["Webpage download"
   :if (lambda () (member (oref (transient-scope) type) '(webpage url)))
   (:info* (lambda () "Download a full snapshot of the webpage"))
   ("wd" "Download"
    :cons 'webpage-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ("ws" "Simplify"
    :cons 'webpage-simplify
    :class org-ilm-transient-cons-switches
    :transient transient--do-call
    :choices ("markdown" "html")
    :allow-empty t
    :if (lambda () (alist-get 'webpage-download (org-ilm--import-transient-parse))))
   ("wo" "Orgify"
    :cons 'webpage-orgify
    :class org-ilm-transient-cons-switch
    :summary "Convert to Org mode with Pandoc"
    :transient transient--do-call
    :if (lambda () (alist-get 'webpage-download (org-ilm--import-transient-parse))))
   ]
  
  ["Media download"
   :if
   (lambda ()
     (eq 'media (oref (transient-scope) type)))
   ("md" "Download"
    :cons 'media-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ("mt" "Template"
    :cons 'media-template
    :class org-ilm-transient-cons-option
    :prompt "Template: "
    :transient transient--do-call
    :inapt-if (lambda () (not (alist-get 'media-download (org-ilm--import-transient-parse)))))
   ("ma" "Audio only"
    :cons 'media-audio
    :class org-ilm-transient-cons-switch
    :transient transient--do-call
    :inapt-if (lambda () (not (alist-get 'media-download (org-ilm--import-transient-parse)))))
   ("ms" "Subtitles download"
    :cons 'media-subs
    :class org-ilm-transient-cons-option
    :transient transient--do-call
    :multi-value rest
    :choices
    (lambda ()
      (let* ((url (oref (transient-scope) source))
             (subs (org-ilm-convert--ytdlp-subtitles-from-url url)))
        (mapcar (lambda (x) (alist-get 'language x)) (alist-get 'subtitles subs)))))
   ]

  ["Website attachment"
   :if (lambda () (eq 'url (oref (transient-scope) type)))
   ("ad" "Download"
    :cons 'webattach-download
    :class org-ilm-transient-cons-option
    :transient t
    :reader
    (lambda (&rest _)
      (with-slots (source) (transient-scope)
        (if-let ((attachments (ignore-errors (zotra-get-attachment source 'all))))
            (if (= (length attachments) 1)
                (car attachments)
              (completing-read "Select attachment: " attachments nil t))
          (message "No attachments were found")
          nil))))
   ("am" "Main attachment"
    :cons 'webattach-main
    :class org-ilm-transient-cons-switch
    :transient transient--do-call
    :if
    (lambda ()
      (map-let (webattach-download webpage-download html-download) (org-ilm--import-transient-parse)
        (and webattach-download
             (or webpage-download html-download))))
    )
   ]

  ["Citation"
   ("C" "Add citation"
    (lambda ()
      (interactive)
      ;; TODO We already save the zotero data in :data, write a function to
      ;; transform it into bibtex
      (let ((ref (oref (transient-scope) ref)))

        (unless ref
          (let ((option (read-string "URL or DOI (empty for manual entry): ")))
            (unless (string-empty-p option)
              (setq ref option))))

        (when ref
          (if-let* ((bibtex (org-ilm--citation-get-bibtex ref 'as-alist))
                    (key (cdr (assoc "=key=" bibtex))))
              (progn
                (oset (transient-scope) bibtex (cons key bibtex))
                (org-ilm--transient-set-target-value "k" key))
            (message "Bibtex could not be found")))
        
        ))
    :transient transient--do-call)
   (org-ilm--import-transient-key)
   ]

  ["Actions"
   ("<tab>" "Auto retrieve info"
    (lambda ()
      (interactive)
      (with-slots (source type ref) (transient-scope)
        (unless ref
          (cond
           ((and (eq type 'file) (string= (file-name-extension source) "pdf"))
            (setq ref (org-ilm--pdf2doi (expand-file-name source))))))

        (unless ref
          (setq ref (read-string "URL or identifier: ")))

        (let* ((t-info (make-thread
                        (lambda ()
                          (org-ilm--citation-get-zotero ref))))
               (t-bib (make-thread
                       (lambda ()
                         (org-ilm--citation-get-bibtex ref 'as-alist))))
               (t-attachs (make-thread
                           (lambda ()
                             (zotra-get-attachment ref 'all))))
               (info (thread-join t-info))
               (bib (thread-join t-bib))
               (attachments (thread-join t-attachs))
               (title (or (alist-get 'title info)
                          (alist-get 'shortTitle info)
                          (pcase type
                            ('webpage
                             (org-ilm--get-page-title source))
                            ('media
                             (org-ilm-convert--ytdlp-title-from-url source)))))
               (key (cdr (assoc "=key=" bib))))
          (when info
            (oset (transient-scope) info info))
          (when title
            (org-ilm--transient-set-target-value "t" title))
          (when bib
            (oset (transient-scope) bibtex (cons key bib))
            (org-ilm--transient-set-target-value "k" key))
          (when attachments
            (org-ilm--transient-set-target-value "ad" (car attachments))))))
    :transient transient--do-call)
   ("RET" "Import"
    (lambda ()
      (interactive)
      (map-let
          (source type bibtex title id collection priority schedule
                  file-method
                  webattach-download webattach-main
                  html-download
                  webpage-download webpage-simplify webpage-orgify
                  media-download media-template media-audio media-subs)
          (org-ilm--import-transient-parse)
        (let ((media-no-download (and (eq type 'media) (not media-download)))
              (file-media-p (and (eq type 'file) (org-ilm--media-file-p source)))
              (file-p (eq type 'file))
              props
              content)

          (setq content
                (cond
                 (html-download
                  (org-ilm--get-website-as-org source))
                 ((or media-no-download file-media-p)
                  ;; Create an org attachment for note taking when importing media
                  ;; without downloading. When downloading, set in on-success
                  ;; callback. HTML download is inactive when type is media, so no
                  ;; clash with org file generated by it.
                  "")))

          ;; Property: ROAM_REFS
          (let (roam-refs)
            (when (or (and (eq type 'file) (null file-method))
                      (member type '(media webpage)))
              (push source roam-refs))

            (when (car bibtex)
              (push (concat "@" (car bibtex)) roam-refs))
            
            (when roam-refs
              (setf (plist-get props :ROAM_REFS) (string-join roam-refs " "))))

          ;; Property: ILM_MEDIA
          (let (media)
            (cond
             ((or media-no-download file-media-p)
              (if file-method
                  (setq media (concat id "." (file-name-extension source)))
                (setq media source)))
             ((and (eq type 'media) media-download)
              (setq media (org-ilm-convert--ytdlp-filename-from-url source media-template 'restrict))))

            (when media
              (setf (plist-get props :ILM_MEDIA) media)))
          
          (org-ilm--capture-capture
           'material
           :collection collection
           :title title
           :id id
           :priority priority
           :scheduled schedule
           :bibtex (cdr bibtex)
           :content content
           :file (when file-p source)
           :method (when file-p file-method)
           :ext (cond
                 ;; For media elements, make sure the org file is seen as the
                 ;; element attachment, rather than the media file. The actual
                 ;; org file itself is created in the on-success callback.
                 ((or (eq type 'media) file-media-p)
                  "org")
                 ;; Otherwise, if there is a file attachment, let
                 ;; org-ilm-capture figure out extension by passed t.
                 (file-p
                  t))
           :props props
           :on-success
           (lambda (id attach-dir collection)
             (when (or html-download webpage-download media-download webattach-download)
               (make-directory attach-dir t))

             ;; For media elements, make sure there is an empty org file for
             ;; annotations.
             (when (or file-media-p (eq type 'media))
               (let ((f (expand-file-name (concat id ".org") attach-dir)))
                 (unless (file-exists-p f)
                   (make-empty-file f))))

             (when webattach-download
               (make-thread
                (lambda ()
                  (condition-case nil
                      (progn
                        (url-copy-file
                         webattach-download
                         (expand-file-name (concat id ".pdf") attach-dir)
                         'ok-if-exists)
                        (org-ilm--org-with-point-at id
                          (org-attach-sync)
                          (when (or webattach-main (and (not webpage-download) (not html-download)))
                            (org-entry-put nil org-ilm-property-ext "pdf"))))
                    (error (message "Failed to download paper for %s" title))))))

             (when webpage-download
               (org-ilm-convert--transient-webpage-run
                source id attach-dir id))

             (when (or media-download media-subs)
               (org-ilm-convert--convert-with-ytdlp
                :process-id id
                :url source
                :output-dir attach-dir
                :filename-template media-template
                :audio-only-p media-audio
                :sub-langs media-subs
                :no-download (not media-download)
                :on-success
                (lambda (proc buf id output-path)
                  (message "[Org-Ilm-Convert] Media download completed: %s" source)
                  (org-ilm--org-with-point-at id
                    (org-attach-sync)))))

             )
             
           )))))
   ]

  (interactive "P")
  
  (transient-setup
   'org-ilm--import-transient nil nil
   :scope data
   :value
   (lambda ()
     (with-slots (type source) data
       `((file-method . cp)
         (webpage-simplify . "markdown")
         (webpage-orgify . t)
         (html-download . ,(eq type 'webpage))
         ;; Don't want media filename to be org-id as I rely on ILM_MEDIA prop
         ;; to point to media. The dedicated attachment is the org file in
         ;; media elements.
         ;; (media-template . " id ".%(ext)s")
         (media-template . "%(title)s.%(ext)s")
         (title . ,(pcase type
                     ('file (file-name-base source))))
         (collection . ,(org-ilm--active-collection)))))))


;;; Footer

(provide 'org-ilm-import)

;;; org-ilm-import.el ends here
