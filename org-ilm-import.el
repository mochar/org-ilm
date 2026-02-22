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
               (:conc-name org-ilm-import--)
               (:include org-ilm-capture
                         (type 'material)
                         (parent (org-ilm--element-from-context))
                         (collection (org-ilm--active-collection))))
  (ref nil :documentation "Global identifier (url/doi)")
  (bib-alist nil :documentation "Bibtex alist")
  (info nil :documentation "Info from zotero translation server.
See `org-ilm--citation-get-zotero'"))

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
   ("o" "Org" org-ilm-import-org)
   ("c" "Card" org-ilm-import-card)]
   ]
  )

(defun org-ilm-import ()
  "Import an element into your ilm collection."
  (interactive)
  (org-ilm--import-dispatch-transient))

;;;; Generics

(cl-defgeneric org-ilm--import-default-args ((capture org-ilm-capture))
  "Default args for the import transient."
  (with-slots (collection title priority scheduled parent) capture
    `((collection . ,collection)
      (title . ,title)
      (priority . ,priority)
      (scheduled . ,scheduled)
      (location . ,(if parent 'child 'default)))))

(cl-defgeneric org-ilm--import-default-args ((import org-ilm-import))
  ;; Since org-ilm-import always default to having a parent element, and the
  ;; main use case is importing new source elements, we default the location to
  ;; the standard target.
  (map-merge
   'alist
   (cl-call-next-method)
   '((location . default))))

(cl-defgeneric org-ilm--import-get-ref ((import org-ilm-import))
  "Get a reference identifier/url to pass to Zotero translation server."
  (if-let ((ref (oref import ref)))
      ref
    (read-string "URL or identifier: ")))

(cl-defgeneric org-ilm--import-get-title ((import org-ilm-import))
  "Infer title from IMPORT.")

(cl-defgeneric org-ilm--import-process ((import org-ilm-capture) args)
  "Fill in capture data in IMPORT using transient state ARGS."
  (map-let (title collection location priority scheduled cite-key) args
    (oset import title title)
    (oset import priority priority)
    (oset import scheduled scheduled)
    (oset import collection collection)
    (pcase location
      ((or 'default 'custom)
       ;; We by default set the parent slot to the element at point in the
       ;; initializer of `org-ilm-import' struct definition, so we need to
       ;; remove it.
       (oset import parent nil))
      ('child
       (oset import target nil)))
    (when (org-ilm-import-p import)
      (when-let ((bib-alist (oref import bib-alist)))
        (oset import bibtex (org-ilm--format-bibtex-entry bib-alist cite-key))
        (setf (plist-get (oref import props) :ROAM_REFS) (concat "@" cite-key))))))

;;;; Main suffixes

(transient-define-infix org-ilm--import-suffix-title ()
  :class org-ilm-transient-cons-option
  :key "t"
  :description "Title"
  :argument 'title
  :prompt "Title: "
  :always-read t :allow-empty nil :transient t)

(transient-define-infix org-ilm--import-suffix-collection ()
  :class 'org-ilm-transient-cons-option
  :transient 'transient--do-call
  :always-read t
  :allow-empty nil
  :key "c"
  :description "Collection"
  :argument 'collection
  :reader
  (lambda (&rest _)
    (let* ((cur-collection (alist-get 'collection (org-ilm--transient-parse)))
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

(transient-define-infix org-ilm--import-suffix-location ()
  :class org-ilm-transient-cons-switches
  :allow-empty nil
  :argument 'location
  :key "l"
  :description "Location"
  :choices
  (lambda ()
    (if (oref (transient-scope) parent)
        '(default child custom)
      '(default custom)))
  :on-change
  (lambda (location)
    (let* ((args (org-ilm--transient-parse))
           (collection (alist-get 'collection args)))
      (pcase location
        ('default
         ;; (oset (transient-scope) target (org-ilm--collection-property collection :import))
         (oset (transient-scope) target nil))
        ('child
         (oset (transient-scope) target nil))
        ('custom
         (pcase (completing-read "Type: " '("file" "heading"))
           ("file"
            (let ((file (org-ilm--select-collection-file collection 'relative)))
              (oset (transient-scope) target (list 'file (expand-file-name file)))))
           ("heading"
            (let ((entry (org-ilm--collection-select-entry collection)))
              (oset (transient-scope) target (list 'id (oref entry id)))))))))))

(transient-define-infix org-ilm--import-suffix-scheduled ()
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

(transient-define-infix org-ilm--import-suffix-priority ()
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
     (map-let (priority collection) (org-ilm--transient-parse)
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
    (let* ((collection (alist-get 'collection (org-ilm--transient-parse))) 
           (priority (org-ilm--bqueue-select-read
                      (org-ilm--pqueue-bqueue collection)
                      nil ;; init
                      "Priority: "
                      1))
           (sched (org-ilm--initial-schedule-from-priority (cdr priority))))
      (org-ilm--transient-set-target-value "s" (ts-format "%Y-%m-%d" sched))
      priority)))

(transient-define-group org-ilm--import-group-main
  [
   ("x" "X"
    (lambda ()
      (interactive)
      (setq x (cons (transient-scope) (org-ilm--transient-parse)))
      (message "%s" x))
    :transient t)
   (org-ilm--import-suffix-collection)
   (org-ilm--import-suffix-location)
   (:info
    (lambda ()
      (map-let (location collection) (org-ilm--transient-parse)
      (pcase location
        ('default
         (let ((target (org-ilm--collection-property collection :import)))
           (concat
            (propertize (format "%s" target) 'face 'Info-quoted))))
        ('child
         (let ((parent (oref (transient-scope) parent)))
           (propertize (org-ilm-element--title parent) 'face 'Info-quoted)))
        ('custom
         (let ((target (oref (transient-scope) target)))
           (concat
            (propertize (format "%s" target) 'face 'Info-quoted))))))))
   (org-ilm--import-suffix-priority)
   (org-ilm--import-suffix-scheduled)
   (org-ilm--import-suffix-title)
   ])


;;;; Citation suffixes
  
(transient-define-infix org-ilm--import-suffix-key ()
  :class 'org-ilm-transient-cons-option
  :transient 'transient--do-call
  :always-read t
  :allow-empty nil
  :key "k"
  :argument 'cite-key
  :description
  (lambda ()
    (concat
     "Key"
     (when-let* ((key (alist-get 'cite-key (org-ilm--transient-parse)))
                 (entry (or (org-mem-entry-by-roam-ref (concat "@" key))
                            (citar-get-entry key))))
       (propertize " DUPLICATE" 'face 'error))))
  :inapt-if-not
  (lambda () (oref (transient-scope) bib-alist))
  :reader
  (lambda (&rest _)
    (let* ((args (org-ilm--transient-parse))
           (key (read-string "Key (empty to auto-generate): "))
           (bib-alist (oref (transient-scope) bib-alist)))

      ;; Generate new key if no key specified 
      (when (or (null key) (string-empty-p key))
        (with-temp-buffer
          (insert (org-ilm--format-bibtex-entry bib-alist))
          (goto-char (point-min))
          (setq key (ignore-errors (bibtex-generate-autokey)))
          (unless (and key (not (string-empty-p key)))
            (setq key (upcase (substring (org-id-uuid) 0 8))))))
      
      (setf (alist-get "=key=" bib-alist nil nil #'string=) key)
      
      key)))

(transient-define-group org-ilm--import-group-citation
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
                (oset (transient-scope) bib-alist bibtex)
                (org-ilm--transient-set-target-value "k" key))
            (message "Bibtex could not be found")))))
    :transient transient--do-call)
   (org-ilm--import-suffix-key)
   ])

;;;; Action suffixes

(transient-define-suffix org-ilm--import-suffix-import ()
  :key "RET"
  :description "Import"
  (interactive)
  (let ((import (transient-scope))
        (args (org-ilm--transient-parse)))
    (org-ilm--import-process import args)
    (org-ilm--capture import)))

(transient-define-suffix org-ilm--import-suffix-autoinfo ()
  :key "<tab>"
  :description "Auto retrieve info"
  :transient 'transient--do-call
  (interactive)
  (let* ((import (transient-scope))
         (ref (org-ilm--import-get-ref import))
         (t-info (make-thread
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
                    (org-ilm--import-get-title import))))
    (when info (oset import info info))
    (when title (org-ilm--transient-set-target-value "t" title))
    (when bib
      (oset import bib-alist bib)
      (org-ilm--transient-set-target-value
       "k" (cdr (assoc "=key=" bib))))
    (when attachments
      (org-ilm--transient-set-target-value "ad" (car attachments)))))

(transient-define-group org-ilm--import-group-actions
  ["Actions"
   (org-ilm--import-suffix-autoinfo)
   (org-ilm--import-suffix-import)
   ])


;;;; Plain import

(transient-define-prefix org-ilm--import-plain-transient (import)
  :refresh-suffixes t
  
  ["Import"
   org-ilm--import-group-main
   ]
  
  ["Actions"
   (org-ilm--import-suffix-import)
   ]
  
  (interactive "P")
  (transient-setup
   'org-ilm--import-plain-transient
   nil nil
   :scope import
   :value (lambda () (org-ilm--import-default-args import))))

(defun org-ilm--import-org (type)
  "Create a new Org element of TYPE by letting user type in attachment
content in a capture buffer.

This functions creates a capture for the org attachments, while
`org-ilm--capture-capture' creates a capture for the collection
element. The way it works is by writing the capture in a tmp file, which
is then passed as a file move capture."
  ;; Use `org-capture's after-finalize hook to immediately capture the content
  ;; into an element.
  (let* ((tmp-file (make-temp-file "" nil ".org"))
         (after-finalize (lambda ()
                           (unless org-note-abort
                             (org-ilm--import-plain-transient
                              (make-org-ilm-import
                               :type type
                               :file tmp-file
                               :attach-method 'mv))))))
    (cl-letf (((symbol-value 'org-capture-templates)
               (list (list "n" "New" 'plain (list 'file tmp-file) ""
                           :after-finalize after-finalize))))
      (org-capture nil "n"))))

(defun org-ilm-import-org ()
  (interactive)
  (org-ilm--import-org 'material))

(defun org-ilm-import-card ()
  (interactive)
  (org-ilm--import-org 'card))

;;;; File import

(cl-defstruct (org-ilm-file-import
               (:conc-name org-ilm-file-import--)
               (:include org-ilm-import)))

(cl-defmethod org-ilm--import-default-args ((import org-ilm-file-import))
  (append
   (cl-call-next-method import)
   `((attach-method . cp))))

(cl-defmethod org-ilm--import-get-ref ((import org-ilm-file-import))
  (if-let ((ref (when (string= (file-name-extension (oref import file)) "pdf")
                  (org-ilm--pdf2doi (expand-file-name (oref import file))))))
      ref
    (cl-call-next-method)))

(cl-defmethod org-ilm--import-process ((import org-ilm-file-import) args)
  (cl-call-next-method import args)
  (pcase-let* (((map attach-method) args)
               (file (oref import file)))

    (oset import attach-method attach-method)
    (oset import ext t)
    (unless attach-method (oset import file nil))

    ;; When file is a media file, the element becomes a media element.
    (when (org-ilm--media-file-p file)
      ;; Pass an empty content string so that an org file is created as main
      ;; attachment in which the media will be annotated.
      (oset import content "")
      (oset import ext "org")

      ;; Set the ILM_MEDIA property.
      (let ((media-prop (if attach-method
                            (setq media-prop (concat (oref import id) "."
                                                     (file-name-extension file)))
                          (setq media file))))
        (setf (plist-get (oref import props) :ILM_MEDIA) media-prop)))

    ;; If we dont add the file as an attachment, set its path in the roam refs.
    (unless attach-method
      (let ((roam-refs (concat (plist-get (oref import props) :ROAM_REFS)
                               " "
                               (oref import file))))
        (setf (plist-get (oref import props) :ROAM_REFS) roam-refs)))))
  
(transient-define-prefix org-ilm--import-file-transient (import)
  :refresh-suffixes t
  
  ["Import file"
   org-ilm--import-group-main
   ]
  
  ["File"
   ("a" "Method of attachment"
    :cons 'attach-method
    :class org-ilm-transient-cons-switches
    :choices (cp mv ln lns)
    :allow-empty t)
   (:info
    (lambda ()
      (let ((method (alist-get 'attach-method (org-ilm--transient-parse))))
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

  org-ilm--import-group-citation
  org-ilm--import-group-actions
  
  (interactive "P")
  (transient-setup
   'org-ilm--import-file-transient
   nil nil
   :scope import
   :value
   (lambda ()
     (org-ilm--import-default-args import))))

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
    (org-ilm--import-file-transient
     (make-org-ilm-file-import
      :title (file-name-base path)
      :file path
      :ref ref))))


;;;; Media import

(cl-defstruct (org-ilm-media-import
               (:conc-name org-ilm-media-import--)
               (:include org-ilm-import))
  url)

(cl-defmethod org-ilm--import-default-args ((import org-ilm-media-import))
  (append
   (cl-call-next-method import)
   `(;; Don't want media filename to be org-id as I rely on ILM_MEDIA prop
     ;; to point to media. The dedicated attachment is the org file in media
     ;; elements.
     ;; (media-template . " id ".%(ext)s")
     (media-template . "%(title)s.%(ext)s"))))

(cl-defmethod org-ilm--import-get-title ((import org-ilm-media-import))
  (org-ilm-convert--ytdlp-title-from-url (oref import url)))

(cl-defmethod org-ilm--import-process ((import org-ilm-media-import) args)
  (cl-call-next-method import args)
  (pcase-let (((map media-download media-template media-audio media-subs) args)
              (url (oref import url)))

    ;; Create an org attachment for note taking.
    (oset import content "")
    (oset import ext "org")

    ;; If we do not download the media, set the ILM_MEDIA property to the
    ;; URL. Otherwise we need to figure out what the filename will be based
    ;; on the template passed to yt-dlp.
    (let ((media-prop (if media-download
                          (org-ilm-convert--ytdlp-filename-from-url
                           url media-template 'restrict)
                        url)))
      (setf (plist-get (oref import props) :ILM_MEDIA) media-prop))

    ;; Always add the URL in ROAM_REFS.
    (let ((roam-refs (concat (plist-get (oref import props) :ROAM_REFS)
                             " " url)))
      (setf (plist-get (oref import props) :ROAM_REFS) roam-refs))

    ;; After capture, download the media file or subtitles.
    (oset import on-success
          (lambda (id attach-dir collection)
            (when (or media-download media-subs)
              (org-ilm-convert--convert-with-ytdlp
               :process-id id
               :url url
               :output-dir attach-dir
               :filename-template media-template
               :audio-only-p media-audio
               :sub-langs media-subs
               :no-download (not media-download)
               :on-success
               (lambda (proc buf id output-path)
                 (message "[Org-Ilm-Convert] Media download completed: %s" url)
                 (org-ilm--org-with-point-at id (org-attach-sync)))))))))

(transient-define-prefix org-ilm--import-media-transient (import)
  :refresh-suffixes t
  
  ["Import media"
   org-ilm--import-group-main
   ]

  ["Media download"
   ("md" "Download"
    :cons 'media-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ("mt" "Template"
    :cons 'media-template
    :class org-ilm-transient-cons-option
    :prompt "Template: "
    :transient transient--do-call
    :inapt-if-not (lambda () (alist-get 'media-download (org-ilm--transient-parse))))
   ("ma" "Audio only"
    :cons 'media-audio
    :class org-ilm-transient-cons-switch
    :transient transient--do-call
    :inapt-if-not (lambda () (alist-get 'media-download (org-ilm--transient-parse))))
   ("ms" "Subtitles download"
    :cons 'media-subs
    :class org-ilm-transient-cons-option
    :transient transient--do-call
    :multi-value rest
    :choices
    (lambda ()
      (let* ((url (oref (transient-scope) url))
             (subs (org-ilm-convert--ytdlp-subtitles-from-url url)))
        (mapcar (lambda (x) (alist-get 'language x)) (alist-get 'subtitles subs)))))
   ]

  org-ilm--import-group-citation
  org-ilm--import-group-actions
  
  (interactive "P")
  (transient-setup
   'org-ilm--import-media-transient
   nil nil
   :scope import
   :value
   (lambda ()
     (org-ilm--import-default-args import))))

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
      (org-ilm--import-media-transient
       (make-org-ilm-media-import :url source :ref source)))
     (t
      (user-error "Not a file or URL: %s" source)))))


;;;; Buffer import

(cl-defstruct (org-ilm-buffer-import
               (:conc-name org-ilm-buffer-import--)
               (:include org-ilm-import))
  buffer)

(cl-defmethod org-ilm--import-default-args ((import org-ilm-buffer-import))
  (append
   (cl-call-next-method import)
   `((buffer-download . t))))

(cl-defmethod org-ilm--import-process ((import org-ilm-buffer-import) args)
  (cl-call-next-method import args)
  (pcase-let (((map buffer-download buffer-orgify) args)
              (buffer (oref import buffer)))

    (with-current-buffer buffer
      ;; Store buffer contents as string
      (when (and buffer-download (not buffer-orgify))
        (pcase major-mode
          ;; Gptel in org mode setup will store model and session info in org
          ;; properties after calling `gptel-org--save-state'.
          ('gptel-mode
           (require 'gptel)
           (gptel-org--save-state)))
        (oset import content (buffer-string)))

      ;; Use org-store-link to add link to roam refs
      (when-let ((link (org-store-link '(4))))
        (let ((roam-refs (concat (plist-get (oref import props) :ROAM_REFS)
                                 " " link)))
          (setf (plist-get (oref import props) :ROAM_REFS) roam-refs)))

      ;; Convert to org after capture if specified
      (when (and buffer-download buffer-orgify)
        (oset import on-success
              (lambda (id attach-dir collection)
                (make-directory attach-dir t)
                (let ((tmp-file (make-temp-file nil)))
                  (with-current-buffer buffer
                    (if-let ((html-buf (ignore-errors (hfy-fontify-buffer))))
                        (progn
                          (with-current-buffer html-buf
                            (write-region nil nil tmp-file))
                          (kill-buffer html-buf))
                      (user-error "Failed to html fontify buffer")))
                  (org-ilm-convert--convert-multi
                   :process-name "buffer"
                   :process-id id
                   :converters
                   (list
                    (cons #'org-ilm-convert--convert-with-marker
                          (list
                           :input-path tmp-file
                           :format "markdown"
                           :output-dir attach-dir
                           :move-content-out t
                           :flags '("--disable_ocr")))
                    (cons #'org-ilm-convert--convert-with-pandoc
                          (list
                           :input-path (expand-file-name (concat (file-name-base tmp-file) ".md") attach-dir)
                           :input-format "markdown"
                           :output-name id)))
                   :on-error nil
                   :on-final-success
                   (lambda (proc buf id)
                     (message "[Org-Ilm-Convert] Buffer conversion completed")
                     (org-ilm--org-with-point-at id (org-attach-sync)))))))))))

(transient-define-prefix org-ilm--import-buffer-transient (import)
  :refresh-suffixes t
  
  ["Import buffer"
   org-ilm--import-group-main
   ]

  ["Buffer"
   ("d" "Download"
    :cons 'buffer-download
    :class org-ilm-transient-cons-switch)
   ("o" "Orgify"
    :cons 'buffer-orgify
    :class org-ilm-transient-cons-switch
    :if (lambda () (alist-get 'buffer-download (org-ilm--transient-parse))))
   ]

  ["Actions"
   (org-ilm--import-suffix-import)
   ]
  
  (interactive "P")
  (transient-setup
   'org-ilm--import-buffer-transient
   nil nil
   :scope import
   :value (lambda () (org-ilm--import-default-args import))))

(defun org-ilm-import-buffer ()
  "Import an emacs buffer to your collection."
  (interactive)
  (let* ((buf (org-ilm--consult-buffer-select))
         (import (make-org-ilm-buffer-import :buffer buf)))
    (with-current-buffer buf
      (cond-let*
        ([link (org-store-link '(4))]
         [m (string-match org-link-bracket-re link)]
         [desc (match-string 2 link)]
         (oset import title desc))
        (t
         (oset import title (buffer-name buf)))))
    (org-ilm--import-buffer-transient import)))


;;;; Webpage import

(cl-defstruct (org-ilm-webpage-import
               (:conc-name org-ilm-webpage-import--)
               (:include org-ilm-import))
  url)

(cl-defmethod org-ilm--import-default-args ((import org-ilm-webpage-import))
  (append
   (cl-call-next-method import)
   `((webpage-simplify . "markdown")
     (webpage-orgify . t)
     (html-download . t))))

(cl-defmethod org-ilm--import-get-title ((import org-ilm-webpage-import))
  (org-ilm--get-page-title (oref import url)))

(cl-defmethod org-ilm--import-process ((import org-ilm-webpage-import) args)
  (cl-call-next-method import args)
  (pcase-let (((map webattach-download webattach-main
                    html-download
                    webpage-download webpage-simplify webpage-orgify) args)
              (url (oref import url)))

    (when html-download
      (oset import content (org-ilm--get-website-as-org url)))

    ;; Always add the URL in ROAM_REFS.
    (let ((roam-refs (concat (plist-get (oref import props) :ROAM_REFS)
                             " " url)))
      (setf (plist-get (oref import props) :ROAM_REFS) roam-refs))

    ;; After capture, download the webpage if requested.
    (oset import on-success
          (lambda (id attach-dir collection)
            (when (or html-download webpage-download webattach-download)
              (make-directory attach-dir t))
            
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
               url id attach-dir id))))))

(transient-define-prefix org-ilm--import-webpage-transient (import)
  :refresh-suffixes t
  :incompatible '((html-download webpage-download)) 
  
  ["Import webpage"
   org-ilm--import-group-main
   ]

  ["HTML -> org download"
   (:info* (lambda () "Convert HTML to Org"))
   ("hd" "Download"
    :cons 'html-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ]

  ["Webpage download"
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
    :if (lambda () (alist-get 'webpage-download (org-ilm--transient-parse))))
   ("wo" "Orgify"
    :cons 'webpage-orgify
    :class org-ilm-transient-cons-switch
    :summary "Convert to Org mode with Pandoc"
    :transient transient--do-call
    :if (lambda () (alist-get 'webpage-download (org-ilm--transient-parse))))
   ]

  ["Website attachment"
   ("ad" "Download"
    :cons 'webattach-download
    :class org-ilm-transient-cons-option
    :transient t
    :reader
    (lambda (&rest _)
      (with-slots (url) (transient-scope)
        (if-let ((attachments (ignore-errors (zotra-get-attachment url 'all))))
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
      (map-let (webattach-download webpage-download html-download) (org-ilm--transient-parse)
        (and webattach-download (or webpage-download html-download))))
    )
   ]

  org-ilm--import-group-citation
  org-ilm--import-group-actions
  
  (interactive "P")
  (transient-setup
   'org-ilm--import-webpage-transient
   nil nil
   :scope import
   :value
   (lambda ()
     (org-ilm--import-default-args import))))

(defun org-ilm-import-webpage (url)
  "Import a webpage to your collection."
  (interactive (list (read-string "URL: " (thing-at-point 'url))))
  (unless (org-url-p url)
    (user-error "Not a valid URL: %s" url))
  (org-ilm--import-webpage-transient
   (make-org-ilm-webpage-import :url url :ref url)))

;;;; URL import

(defun org-ilm-import-url (url)
  "Import something from a URL to your collection."
  (interactive (list (read-string "URL: " (thing-at-point 'url))))
  (unless (org-url-p url)
    (user-error "Not a valid URL: %s" url))
  (cond-let*
    ([media-title (org-ilm-convert--ytdlp-title-from-url url)]
     (org-ilm--import-media-transient
      (make-org-ilm-media-import :url url :ref url :title media-title)))
    (t
     (org-ilm--import-webpage-transient
      (make-org-ilm-webpage-import :url url :ref url)))))

;;; Footer

(provide 'org-ilm-import)

;;; org-ilm-import.el ends here
