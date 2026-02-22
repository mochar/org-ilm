;;; org-ilm-convert.el --- Convert between file types -*- lexical-binding: t; -*-

;;; Commentary:

;; Some tools to handle file conversions and stuff

;;; Code:

;;;; Requirements

(require 'org)
(require 'cl-lib)
(require 'dash)
(require 'vtable)
(require 'org-attach)
(require 'transient)
(require 'org-mem)
(require 'org-node)
(require 'consult)

(require 'org-ilm-utils)

;;;; Variables

(defcustom org-ilm-convert-node-path (executable-find "node")
  "Path to the node executable."
  :type 'file
  :group 'org-ilm-convert)

(defcustom org-ilm-convert-keep-buffer-alive 'always
  "Keep the conversion process buffer alive, regardless of success or fail."
  :type '(choice (const :tag "Always" always)
                 (const :tag "On success" success)
                 (const :tag "On error" error))
  :group 'org-ilm-convert)

(defvar-local org-ilm-convert--convert-state nil)

;;;; Convert process

(cl-defun org-ilm-convert--convert-make-process (&key name id command on-success on-error keep-buffer-alive prevent-success-state buffer-data &allow-other-keys)
  "Create an async process to run COMMAND."
  (let* ((process-name (format "org-ilm-convert-%s (%s)" name id))
         (process-buffer (get-buffer-create (concat "*" process-name "*"))))
    (with-current-buffer process-buffer
      (setq-local org-ilm-convert--convert-name name
                  org-ilm-convert--convert-id id
                  org-ilm-convert--convert-state 'busy
                  org-ilm-convert--convert-data buffer-data)
      (goto-char (point-max))
      (insert (format "\n====== START: %s ======\n" process-name)))
    (make-process
     :name process-name
     :buffer process-buffer
     :command command
     :sentinel
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer process-buffer
           (goto-char (point-max))
           (insert (format "\n====== END: %s ======\n" process-name)))
         
         (let ((success (= (process-exit-status proc) 0)))

           (unless (and success prevent-success-state)
             (with-current-buffer process-buffer
               (setq-local org-ilm-convert--convert-state
                           (if success 'success 'error))))
           
           (if success
               (when on-success
                 (funcall on-success proc process-buffer id))
             (when on-error
               (funcall on-error proc process-buffer id)))
           (unless (or keep-buffer-alive
                       (eq org-ilm-convert-keep-buffer-alive 'always)
                       (and success (eq org-ilm-convert-keep-buffer-alive 'success))
                       (and (not success) (eq org-ilm-convert-keep-buffer-alive 'error)))
             (kill-buffer process-buffer))))))
    process-buffer))

(cl-defun org-ilm-convert--convert-multi (&rest args &key process-name process-id converters on-error on-final-success)
  "Run multiple convert processes successively.

CONVERTERS is an alist. Each element's car is the converter function and
cdr the function arguments. All converters share a process buffer with
ID and NAME.

The callback ON-ERROR can be given which will be called after the
on-error callback of each converter. Note that the process buffer local
variables `org-ilm-convert--convert-name' and `org-ilm-convert--convert-id' can be used
to determine the failing process, so just this one ON-ERROR callback
should be enough.

A callback ON-FINAL-SUCCESS can be passed which will be called when the
final converter succeeds."
  (unless (and process-name process-id converters)
    (error "Required args: PROCESS-NAME PROCESS-ID CONVERTERS"))
  (let* ((first (car converters))
         (rest (cdr converters))
         (converter (car first))
         (converter-args (cdr first))
         ;; Have to store the inner callback here, otherwise infinite recursion
         (converter-on-error (plist-get converter-args :on-error))
         (converter-on-success (plist-get converter-args :on-success))
         (on-error
          (lambda (proc buf id)
            (when converter-on-error
              (funcall converter-on-error proc buf id))
            (when on-error
              (funcall on-error proc buf id))))
         ;; on-success recursively calls remaining converters if not last
         (on-success
          (lambda (proc buf id)
            ;; Always call the converter-specific on-success callback
            (when converter-on-success
              (funcall converter-on-success proc buf id))
            (if rest
                ;; If there are converters left, call the next one
                (apply #'org-ilm-convert--convert-multi
                       (plist-put args :converters rest))
              ;; If final converter, call on-final-success
              (when on-final-success
                (funcall on-final-success proc buf id))))))
    (setq converter-args
          (org-combine-plists
           converter-args
           (list
            :on-error on-error
            :on-success on-success
            :process-name process-name
            :process-id process-id
            ;; org-ilm-convert--convert-make-process adds a buffer local variable that
            ;; signifies success or failure of a process. dont set success
            ;; unless we are on the last converter.
            :prevent-success-state (if rest t nil)
            )))
    (apply converter converter-args)))

;;;; Pandoc

(cl-defun org-ilm-convert--convert-with-pandoc (&rest args &key process-id process-name input-path input-format output-dir output-name &allow-other-keys)
  "Convert a file to Org mode format using Pandoc."
  (unless (and process-id input-path input-format)
    (error "Required args: PROCESS-ID INPUT-PATH INPUT-FORMAT"))
  (let* ((input-path (expand-file-name input-path))
         (output-dir (expand-file-name
                      (or output-dir (file-name-directory input-path))))
         (output-name (or output-name (file-name-base input-path)))
         (output-path (expand-file-name (concat output-name ".org") output-dir))
         ;; Make sure we are in output dir so that media files references correct
         (default-directory output-dir))
    (apply
     #'org-ilm-convert--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "pandoc")
       :id process-id
       :buffer-data '(:output-path 'output-path)
       :command
       (append
        (list
         "pandoc"
         "--from" input-format
         "--to" "org"
         "--wrap=preserve"
         ;; Relative to output-dir, or absolute path. Since we set
         ;; default-directory to output path and want to store media in same
         ;; folder, set to "."
         "--extract-media" "."
         "--verbose"
         input-path
         "-o" output-path)))))))

;;;; Defuddle

;; Defuddle: https://github.com/kepano/defuddle
;; Simplify html to markdown

(defconst org-ilm-convert-defuddle-path
  (expand-file-name "scripts/defuddle.mjs"
                    (file-name-directory (or load-file-name buffer-file-name))))

(cl-defun org-ilm-convert--convert-with-defuddle (&rest args &key process-id process-name source input-format output-format output-name output-folder &allow-other-keys)
  "Convert a URL or HTML file to Markdown using Defuddle."
  (unless (and process-id source output-format)
    (error "Required args: PROCESS-ID SOURCE OUTPUT-FORMAT"))
  (unless (member output-format '("markdown" "html"))
    (error "OUTPUT-FORMAT must be one of [markdown|html]"))
  (let* ((source-type (cond
                       ((file-exists-p source)
                        (setq source (expand-file-name source))
                        (unless output-name
                          (setq output-name (file-name-base source)))
                        (unless output-folder
                          (setq output-folder (file-name-directory source)))
                        "html")
                       ((org-url-p source)
                        (unless output-folder
                          (error "Must specify OUTPUT-FOLDER when SOURCE is URL"))
                        (unless output-name
                          (setq output-name (org-ilm--get-page-title source 'slugify)))
                        "url")
                       (t (error "SOURCE not file or URL"))))
         (to-markdown (string= output-format "markdown"))
         (output-path (expand-file-name
                       (concat output-name "." (if to-markdown "md" "html"))
                       output-folder)))
    (apply
     #'org-ilm-convert--convert-make-process
     (map-merge
      'plist
      args
      (list
       :name (or process-name "defuddle")
       :id process-id
       :command
       (list
        org-ilm-convert-node-path
        org-ilm-convert-defuddle-path
        source-type
        source
        output-format
        output-path))))))

(cl-defun org-ilm-convert--convert-to-org-with-defuddle-pandoc (&key process-id on-success on-error defuddle-args pandoc-args)
  "Convert a URL or HTML file to Markdown with Defuddle, then to Org mode using Pandoc."
  (setq monolith-args
        (plist-put monolith-args :process-id process-id))
  
  (let* ((defuddle-output-format (plist-get defuddle-args :output-format))
         (defuddle-output-path (concat
                                (file-name-sans-extension output-path)
                                "."
                                (if (string= defuddle-output-format "markdown")
                                    "md" "html"))))
    (org-ilm-convert--convert-multi
     :process-name "defuddle-pandoc"
     :process-id process-id
     :converters
     (list
      (cons #'org-ilm-convert--convert-with-defuddle
            defuddle-args)
      (cons #'org-ilm-convert--convert-with-pandoc
            (append
             pandoc-args
             (list
              :input-path defuddle-output-path
              :input-format defuddle-output-format))))
     :on-error on-error
     :on-final-success on-success))))


;;;; Marker
;; Marker: https://github.com/datalab-to/marker
;; Much better Org formatting when converting from markdown

(defcustom org-ilm-convert-marker-path (executable-find "marker_single")
  "Path to the marker_single executable."
  :type 'file
  :group 'org-ilm-convert-marker)

(cl-defun org-ilm-convert--convert-with-marker (&rest args &key process-id process-name input-path format output-dir pages disable-image-extraction move-content-out new-name to-org flags on-success &allow-other-keys)
  "Convert a PDF document or image using Marker.

OUTPUT-DIR is the output directory. If not specified, will be the same
directory as INPUT-PATH. Note that Marker stores the output in another dir
within this directory, named after the file. With MOVE-CONTENT-OUT set
to non-nil, the directory contents will be moved up to be in OUTPUT-DIR.

NEW-NAME if non-nil can be a string to rename the output file. Marker
does not have an option for this so it is done here.
"
  (unless (and process-id input-path format)
    (error "Required args: PROCESS-ID INPUT-PATH FORMAT"))
  (unless (and org-ilm-convert-marker-path (file-executable-p org-ilm-convert-marker-path))
    (user-error "Marker executable not available. See org-ilm-convert-marker-path."))
  (unless (member format '("markdown" "json" "html" "chunks"))
    (error "FORMAT must be one of [markdown|json|html|chunks]"))
  (unless (or (not to-org) (member format '("markdown" "html")))
    (error "When TO-ORG, FORMAT must be one of [markdown|html]"))
  (let* ((input-path (expand-file-name input-path))
         (input-base-name (file-name-base input-path))
         ;; for rename-file, NEWNAME recognized as dir if ends in slash
         (output-dir (if output-dir
                         (concat (expand-file-name output-dir) "/")
                       (file-name-directory input-path)))
         (output-dir-dir (file-name-concat output-dir (file-name-base input-path))))
    (apply
     #'org-ilm-convert--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "marker")
       :id process-id
       :command
       (append
        (list
         org-ilm-convert-marker-path
         input-path
         "--output_format" format
         "--output_dir" output-dir)
        (when pages
          (list
           "--page_range"
           (cond
            ((stringp pages) pages)
            ((integerp pages) (number-to-string pages))
            ;; TODO allow "0,5-10,20" as list of integers and cons
            ((and (consp pages) (integerp (car pages)) (integerp (cdr pages)))
             (format "%s-%s" (car pages) (cdr pages)))
            (t (error "Argument PAGES not correctly specified.")))))
        ;; "--disable_tqdm"
        ;; Extract images from the document. Default is True.
        (when disable-image-extraction '("--disable_image_extraction"))
        ;; Whether to flatten the PDF structure. 
        ;; "--flatten_pdf BOOLEAN"
        (list
         "--detection_batch_size" "1"
         "--ocr_error_batch_size" "1" ; slowest
         "--layout_batch_size" "1"
         "--recognition_batch_size" "1"
         "--equation_batch_size" "1"
         "--table_rec_batch_size" "1")
        flags)
       :on-success
       (lambda (proc buf id)
         ;; Renaming the files is done by replacing the input base file
         ;; name in each otuput folder file name, but only if the file
         ;; name starts with it. This is to hit less false positives if
         ;; the base file name is small, which won't be the case i think
         ;; since org-ilm-convert works with org-ids all the time.

         ;; TODO Works ok but I think a better approach would be to create
         ;; a symlink with the new name and point marker to that. Then
         ;; delete symlink afterwards.
         (when (or move-content-out new-name)
           ;; Appearently that regex is needed otherwise returns "." and
           ;; ".." as files lol
           (dolist (file (directory-files output-dir-dir 'abs-file-name directory-files-no-dot-files-regexp))
             (let ((file-base-name (file-name-base file))
                   (file-ext (file-name-extension file)))
               (rename-file
                file
                (file-name-concat
                 (if move-content-out output-dir output-dir-dir)
                 (when new-name
                   (concat
                    (replace-regexp-in-string
                     ;; Only replace name if filename starts with it
                     (concat "^" input-base-name)
                     new-name
                     file-base-name)
                    "." file-ext)))
                'ok-if-exists)))
           (when move-content-out
             (delete-directory output-dir-dir)))

         (when on-success
           (funcall on-success proc buf id))))))))

(cl-defun org-ilm-convert--convert-to-org-with-marker-pandoc (&key process-id input-path new-name pdf-pages on-success on-error)
  "Convert a PDF file or image to Markdown using Marker, then to Org mode using Pandoc."
  (unless (and process-id input-path new-name)
    (error "Required args: PROCESS-ID INPUT-PATH NEW-NAME"))
  (let ((input-dir (file-name-directory input-path))
        (process-name "marker-pandoc"))
    (org-ilm-convert--convert-multi
     :process-name process-name
     :process-id process-id
     :converters
     (list
      (cons #'org-ilm-convert--convert-with-marker
            (list
             :input-path input-path
             :format "markdown"
             :new-name new-name
             :pages pdf-pages
             :move-content-out t
             :to-org t))
      (cons #'org-ilm-convert--convert-with-pandoc
            (list
             :input-path (file-name-concat input-dir (concat new-name ".md"))
             :input-format "markdown")))
     :on-error on-error
     :on-final-success on-success)))
     

;;;; Monolith

(defcustom org-ilm-convert-monolith-path (executable-find "monolith")
  "Path to the monolith executable."
  :type 'file
  :group 'org-ilm-convert-monolith)

(defcustom org-ilm-convert-monolith-args
  '("--no-fonts" "--no-js")
  "Arguments passed to monolith."
  :type '(repeat string)
  :group 'org-ilm-convert-monolith)

(cl-defun org-ilm-convert--monolith-compile-paths (&rest args &key process-id input-path output-path &allow-other-keys)
    (unless (and process-id input-path)
    (error "Required args: PROCESS-ID INPUT-PATH"))

  ;; TODO For now determining if input is file or url using file-exists-p, need
  ;; something more robust
  (let ((input-is-file (file-exists-p input-path)))
    (setq input-path
          (if input-is-file
              (expand-file-name input-path)
            input-path))
    
    (setq output-path
          (expand-file-name
           (format "%s.html"
                   (or (when output-path
                         (file-name-base output-path))
                       (when input-is-file
                         (file-name-base input-path))
                       process-id))
           (file-name-directory
            (or output-path
                (when input-is-file input-path)
                temporary-file-directory))))
    (list :input-path input-path :output-path output-path)))

(cl-defun org-ilm-convert--convert-with-monolith (&rest args &key process-id process-name input-path output-path &allow-other-keys)
  "Convert a web URL or local HTML file to a single HTML file using Monolith."
  (unless (and org-ilm-convert-monolith-path (file-executable-p org-ilm-convert-monolith-path))
    (user-error "Monolith executable not available. See org-ilm-convert-monolith-path."))

  (cl-destructuring-bind (&key input-path output-path)
      (apply #'org-ilm-convert--monolith-compile-paths args)
    (apply
     #'org-ilm-convert--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "monolith")
       :id process-id
       :command
       (append 
        (list
         org-ilm-convert-monolith-path
         input-path
         "-o" output-path)
        org-ilm-convert-monolith-args))))))

(cl-defun org-ilm-convert--convert-with-monolith-defuddle (&key process-id on-success on-error monolith-args defuddle-args)
  "Convert a URL or HTML file to single file HTML using Monolith, and
 simplify it to Markdown or replaced HTML file with Pandoc."
  (unless (and process-id monolith-args)
    (error "Required args: PROCESS-ID MONOLITH-ARGS"))

  (setq monolith-args
        (plist-put monolith-args :process-id process-id))
  
  (cl-destructuring-bind (&key input-path output-path)
      (apply #'org-ilm-convert--monolith-compile-paths monolith-args)
    (let* ((monolith-output-path output-path)
           (output-format (plist-get defuddle-args :output-format))
           (defuddle-output-path (concat
                                  (file-name-sans-extension output-path)
                                  "."
                                  (if (string= output-format "markdown")
                                      "md" "html"))))
      (org-ilm-convert--convert-multi
       :process-name "monolith-defuddle"
       :process-id process-id
       :converters
       (list
        (cons #'org-ilm-convert--convert-with-monolith monolith-args)
        (cons #'org-ilm-convert--convert-with-defuddle
              (append
               (list :source monolith-output-path)
               defuddle-args)))
       :on-error on-error
       :on-final-success on-success))))

(cl-defun org-ilm-convert--convert-to-org-with-monolith-pandoc (&key process-id on-success on-error monolith-args pandoc-args)
  "Convert a URL or HTML file to single file HTML using Monolith, then to Org mode using Pandoc."
  (unless (and process-id monolith-args)
    (error "Required args: PROCESS-ID MONOLITH-ARGS"))

  (setq monolith-args
        (plist-put monolith-args :process-id process-id))
  
  (cl-destructuring-bind (&key input-path output-path)
      (apply #'org-ilm-convert--monolith-compile-paths monolith-args)
    (org-ilm-convert--convert-multi
     :process-name "monolith-pandoc"
     :process-id process-id
     :converters
     (list
      (cons #'org-ilm-convert--convert-with-monolith monolith-args)
      (cons #'org-ilm-convert--convert-with-pandoc
            (append
             pandoc-args
             (list
              :input-path output-path
              :input-format "html"))))
     :on-error on-error
     :on-final-success on-success)))

(cl-defun org-ilm-convert--convert-to-org-with-monolith-defuddle-pandoc (&key process-id on-success on-error monolith-args defuddle-args pandoc-args)
  "Convert a URL or HTML file to single file HTML using Monolith, simplify to Markdown with Defuddle, then to Org mode using Pandoc."
  (unless (and process-id monolith-args)
    (error "Required args: PROCESS-ID MONOLITH-ARGS"))

  (setq monolith-args
        (plist-put monolith-args :process-id process-id))
  
  (cl-destructuring-bind (&key input-path output-path)
      (apply #'org-ilm-convert--monolith-compile-paths monolith-args)
    (let* ((monolith-output-path output-path)
           (defuddle-output-format (plist-get defuddle-args :output-format))
           (defuddle-output-path (concat
                                  (file-name-sans-extension output-path)
                                  "."
                                  (if (string= defuddle-output-format "markdown")
                                      "md" "html"))))
    (org-ilm-convert--convert-multi
     :process-name "monolith-defuddle-pandoc"
     :process-id process-id
     :converters
     (list
      (cons #'org-ilm-convert--convert-with-monolith monolith-args)
      (cons #'org-ilm-convert--convert-with-defuddle
            (append
             (list :source monolith-output-path)
             defuddle-args))
      (cons #'org-ilm-convert--convert-with-pandoc
            (append
             pandoc-args
             (list
              :input-path defuddle-output-path
              :input-format defuddle-output-format))))
     :on-error on-error
     :on-final-success on-success))))

;;;; yt-dlp

(defcustom org-ilm-convert-ytdlp-path (executable-find "yt-dlp")
  "Path to the yt-dlp executable."
  :type 'file
  :group 'org-ilm-convert-ytdlp)

;; https://github.com/yt-dlp/yt-dlp/wiki/FAQ#how-do-i-pass-cookies-to-yt-dlp
(defcustom org-ilm-convert-ytdlp-args
  '("--cookies-from-browser" "firefox"
    "--remote-components" "ejs:github")
  "Arguments to always pass to yt-dlp."
  :type '(repeat string)
  :group 'org-ilm-convert-ytdlp)

(defun org-ilm-convert--ytdlp-filename-from-url (url &optional template restrict-p)
  "Get the filename that will be generated for URL and TEMPLATE.

See: https://github.com/yt-dlp/yt-dlp?tab=readme-ov-file#output-template-examples"
  (-some->
   (ignore-errors
     (apply #'process-lines
            (append
             '("yt-dlp" "--print" "filename")
             (when template (list "-o" template))
             (list url)
             (when restrict-p '("--restrict-filenames"))
             org-ilm-convert-ytdlp-args
             '("--no-warnings"))))
   car
   string-trim))

(defun org-ilm-convert--ytdlp-title-from-url (url)
  (org-ilm-convert--ytdlp-filename-from-url url "%(title)s"))

(defun org-ilm-convert--ytdlp-subtitles-from-url (url)
  "Returns two alists of alist: 'subtitles and 'auto.

Parse the OUTPUT string from:
   yt-dlp --print subtitles_table --print automatic_captions_table."
  (let* ((lines (ignore-errors
                  (apply #'process-lines
                         (append
                          '("yt-dlp" "--print" "subtitles_table" "--print" "automatic_captions_table")
                          (list url)
                          org-ilm-convert-ytdlp-args
                          '("--no-warnings")))))
         (result '())
         (section nil))
    (dolist (line lines)
      ;; detect section headers
      (cond
       ((string-match-p "^Language[[:space:]]+Name[[:space:]]+Formats" line)
        (setq section 'subtitles)
        (push (cons section '()) result))
       ((string-match-p "^Language[[:space:]]+Formats" line)
        (setq section 'auto)
        (push (cons section '()) result))
       ((string-empty-p line) nil)
       (t
        ;; parse table row
        (pcase section
          ('subtitles
           (when (string-match
                  "^\\([^[:space:]]+\\)[[:space:]]+\\([^[:space:]]*\\)[[:space:]]+\\(.+\\)$" line)
             (let ((lang (match-string 1 line))
                   (name (match-string 2 line))
                   (formats (split-string (match-string 3 line) "," t "[[:space:]]+")))
               (push `((language . ,lang)
                       (name . ,(if (string-empty-p name) nil name))
                       (formats . ,formats))
                     (cdr (assq section result))))))
          ('auto
           (when (string-match
                  "^\\([^[:space:]]+\\)[[:space:]]+\\(.+\\)$" line)
             (let ((lang (match-string 1 line))
                   (formats (split-string (match-string 2 line) "," t "[[:space:]]+")))
               (push `((language . ,lang)
                       (formats . ,formats))
                     (cdr (assq section result))))))))))
    ;; reverse to preserve order
    (dolist (r result)
      (setcdr r (nreverse (cdr r))))
    (nreverse result)))

(cl-defun org-ilm-convert--convert-with-ytdlp (&rest args &key process-id process-name url output-dir output-path filename-template sub-langs audio-only-p no-download working-dir on-success &allow-other-keys)
  "Download media from url using yt-dlp.

SUB-LANGS may also be 'all' to download all subtitles."
  (unless (and process-id url (or (xor output-path output-dir) sub-langs))
    (error "Required args: PROCESS-ID URL [OUTPUT-DIR|OUTPUT-PATH]"))
  (unless (and org-ilm-convert-ytdlp-path (file-executable-p org-ilm-convert-ytdlp-path))
    (user-error "The yt-dlp executable not available. See org-ilm-convert-ytdlp-path."))

  (when (and (not output-path) output-dir)
    (setq output-path (expand-file-name
                       (org-ilm-convert--ytdlp-filename-from-url url filename-template 'restrict)
                       output-dir)))

  (when sub-langs
    (cond
     ((stringp sub-langs)
      (setq sub-langs (list sub-langs)))
     ((listp sub-langs))
     (t (error "SUB-LANGS must be string or list of strings"))))

  (let ((default-directory (or working-dir default-directory)))
    (apply
     #'org-ilm-convert--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "ytdlp")
       :id process-id
       :command
       (append
        (list
         org-ilm-convert-ytdlp-path
         url
         "--embed-chapters")
        org-ilm-convert-ytdlp-args
        (cond
         (output-path (list "-o" output-path))
         (filename-template (list  "-o" filename-template "--no-download"))
         (t (list "--no-download")))
        (when audio-only-p '("-x"))
        (when no-download '("--no-download"))
        (when sub-langs
          (append '("--write-sub" "--sub-lang") (list (string-join sub-langs ","))))
        )
       :on-success
       (lambda (proc buf id)
         (when on-success
           (funcall on-success proc buf id output-path)))
       )))))



;;;; Conversions view

(defconst org-ilm-convert--conversions-buffer-name
  "*org-ilm-convert conversions*")

(defvar-keymap org-ilm-convert-conversions-map
  :doc "Keymap for the conversions view."
  ;; Navigation
  "n" (lambda ()
        (interactive)
        (forward-line)
        (when (eobp) (forward-line -1)))
  "p" #'previous-line
  "b" #'backward-char
  "f" #'forward-char
  "q" #'quit-window
  "k" (lambda ()
        (interactive)
        (kill-buffer (current-buffer)))
  ;; Actions
  "SPC" #'org-ilm-convert-conversions-goto
  "RET" #'org-ilm-convert-conversions-buffer-open
  "d" #'org-ilm-convert-conversions-buffer-delete
  "B" #'org-ilm-convert-conversions-ibuffer
  "g" #'org-ilm-convert-conversions-revert)

(defun org-ilm-convert-conversions-goto ()
  "View element of object at point in collection."
  (interactive)
  (let ((org-id (plist-get (vtable-current-object) :id)))
    (org-id-goto org-id)))

(defun org-ilm-convert-conversions-buffer-open ()
  "Open buffer of object at point."
  (interactive)
  (pop-to-buffer (plist-get (vtable-current-object) :buffer)))

(defun org-ilm-convert-conversions-buffer-delete ()
  "Kill buffer of object at point."
  (interactive)
  (when (yes-or-no-p "Kill buffer?")
    (kill-buffer (plist-get (vtable-current-object) :buffer))
    (org-ilm-convert-conversions-revert)))

(defun org-ilm-convert-conversions-ibuffer ()
  "Open conversion buffers in ibuffer."
  (interactive)
  (ibuffer nil "*Ilm Conversion Buffers*"
           '((name . "^\\*org-ilm-convert-"))))

(defun org-ilm-convert--conversions-buffers ()
  "Return all make-process buffers of convertors."
  (seq-filter
   (lambda (buf)
     (string-match-p
      (rx bol "*org-ilm-convert-" (+? anything) " (" (+? anything) ")*" eol)
      (buffer-name buf)))
   (buffer-list)))

(defun org-ilm-convert--conversions ()
  "Return list of plists containing info of each conversion."
  (mapcar
   (lambda (buf)
     (let* ((buffer-name (buffer-name buf))
            (_ (string-match
                "^\\*org-ilm-convert-\\([^ ]+\\) (\\([^)]*\\))\\*$"
                buffer-name))
            (name (match-string 1 buffer-name))
            (id (match-string 2 buffer-name))
            (state (with-current-buffer buf
                     org-ilm-convert--convert-state)))
       (list :name name :id id :buffer buf :state state)))
   (org-ilm-convert--conversions-buffers)))

(defun org-ilm-convert--conversion-by-id (id)
  (seq-find
   (lambda (conversion)
     (string= (plist-get conversion :id) id))
   (org-ilm-convert--conversions)))

(defun org-ilm-convert--conversion-propertize-state (state)
  (pcase state
    ('error (propertize "Error" 'face 'error))
    ('success (propertize "Success" 'face 'success))
    ('busy (propertize "Busy" 'face 'italic))))

(defun org-ilm-convert-conversions-revert ()
  (interactive)
  (cond
   ((not (string= (buffer-name) org-ilm-convert--conversions-buffer-name))
    (user-error "Not in conversions buffer"))
   ((org-ilm-convert--conversions)
    (vtable-revert-command))
   (t
    (kill-buffer org-ilm-convert--conversions-buffer-name)
    (message "No conversions found!"))))

(defun org-ilm-convert--conversions-make-vtable ()
  "Build conversions view vtable."
  (make-vtable
   :insert nil ; Return vtable object rather than insert at point
   :objects-function
   #'org-ilm-convert--conversions
   :columns
   `((:name
      "State")
     (:name
      "ID")
     (:name
      "Converter"
      :min-width "100%"))
   :getter
   (lambda (object column vtable)
     (pcase (vtable-column vtable column)
       ("ID" (plist-get object :id))
       ("State" (org-ilm-convert--conversion-propertize-state (plist-get object :state)))
       ("Converter" (plist-get object :name))))
   :keymap org-ilm-convert-conversions-map))

(defun org-ilm-convert-conversions ()
  "Open the conversions view."
  (interactive)
  (let ((buf (get-buffer-create org-ilm-convert--conversions-buffer-name))
        (conversions (org-ilm-convert--conversions)))
    (cond
     (conversions
      (with-current-buffer buf
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (goto-char (point-min))
        (vtable-insert (org-ilm-convert--conversions-make-vtable))
        (setq-local buffer-read-only t)
        (goto-char (point-min)))
      (switch-to-buffer buf))
     (t
      (kill-buffer buf)
      (message "No conversions found!")))))

;;;; Org transient

;; Convert using metadata in Org heading properties and attachments.

;;;;; Webpage group

(transient-define-group org-ilm--convert-webpage-transient-group
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
   ])

(defun org-ilm--convert-transient-webpage-run (source title output-dir id)
  (let* ((transient-args (org-ilm--transient-parse))
         (download (alist-get 'webpage-download transient-args))
         (simplify (alist-get 'webpage-simplify transient-args))
         (orgify (alist-get 'webpage-orgify transient-args))
         (html-path (expand-file-name (concat title ".html") output-dir))
         (on-success
          (lambda (proc buf id)
            (message "[Org-Ilm-Convert] HTML conversion completed: %s" source)
            (org-ilm--org-with-point-at id (org-attach-sync))))
         converters)

    (when download
      (push
       (cons #'org-ilm-convert--convert-with-monolith
             (list :input-path source :output-path html-path))
       converters))

    (when simplify
      (push
       (cons #'org-ilm-convert--convert-with-defuddle
             (list :source html-path :output-format simplify))
       converters))

    (when orgify
      (push
       (cons #'org-ilm-convert--convert-with-pandoc
             (list :input-path
                   (if simplify
                       (concat
                        (file-name-sans-extension html-path)
                        "."
                        (if (string= simplify "markdown") "md" "html"))
                     html-path)
                   :input-format
                   (or simplify "html")))
       converters))

    (setq converters (reverse converters))
    
    (org-ilm-convert--convert-multi
     :process-name "webpage"
     :process-id id
     :converters converters
     :on-error nil
     :on-final-success on-success)))

;;;;; HTML download group

(transient-define-group org-ilm--convert-html-transient-group
  ["HTML -> org download"
   (:info* (lambda () "Convert HTML to Org"))
   ("hd" "Download"
    :cons 'html-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ])

(defun org-ilm--convert-transient-html-run (source title output-dir id)
  (org-ilm-convert--convert-with-defuddle
   :process-name "html"
   :process-id id
   :source source
   :output-name title
   :output-folder output-dir))


;;;;; Main transient

(defun org-ilm-convert-menu ()
  (interactive)
  (when-let ((entry (org-node-at-point)))
    (let* ((attach-dir (org-attach-dir))
           (attachments (when attach-dir (org-attach-file-list attach-dir)))
           (refs (org-ilm--org-mem-refs entry)))
      (when-let ((url (org-entry-get nil "URL")))
        (cl-pushnew url refs :test #'equal))
      (setq refs (seq-filter #'org-url-p refs))
      
      (let* ((choice (consult--multi
                      (list
                       (list
                        :name "Refs"
                        :narrow ?r
                        :items refs
                        :action #'message)
                       (list
                        :name "Attachments"
                        :narrow ?a
                        :items attachments
                        :action #'message))
                      :require-match t
                      :prompt "Convert: " ))
             (source (car choice))
             (type (if (string= (plist-get (cdr choice) :name) "Attachments")
                       'attachment 'url)))
        (org-ilm--convert-transient
         (list :title (org-mem-entry-title entry) :source source :type type))))))

(transient-define-prefix org-ilm--convert-transient (data)
  :refresh-suffixes t
  :value
  (lambda ()
    `((webpage-simplify . "markdown")
      (webpage-orgify . t)
      (html-download . t)))

  ["Convtool"
   (:info*
    (lambda ()
      (propertize (plist-get (transient-scope) :title) 'face 'italic))
    :if (lambda () (plist-get (transient-scope) :title)))
   (:info*
    (lambda ()
      (propertize (plist-get (transient-scope) :source) 'face 'transient-value))
    :if (lambda () (plist-get (transient-scope) :source)))
   ]

  [:if (lambda () (eq (plist-get (transient-scope) :type) 'url))
   org-ilm--convert-html-transient-group
   ]

  [:if (lambda () (eq (plist-get (transient-scope) :type) 'url))
   org-ilm--convert-webpage-transient-group
   ]

  ;; ["Media"
  ;;  :if
  ;;  (lambda ()
  ;;    (and (eq (plist-get (transient-scope) :type) 'url)
  ;;         ;; (org-ilm-convert--transient-media-url-is-media-p
  ;;         ;;  (plist-get (transient-scope) :source))
  ;;         ))
  ;;  :setup-children
  ;;  (lambda (_)
  ;;    (org-ilm-convert--transient-media-build))
  ;;  ]

  ["Actions"
   ("RET" "Convert"
    (lambda ()
      (interactive)
      (pcase-let* (((map webpage-download webpage-simplify webpage-orgify)
                    (org-ilm--transient-parse))
                   ((map :source :type) (transient-scope))
                   (org-id (org-id-get))
                   (attach-dir (org-attach-dir-get-create)))

        ;; Webpage
        (when webpage-download
          (let ((title (org-ilm--get-page-title source 'slugify)))
            (org-ilm--convert-transient-webpage-run
             source title attach-dir org-id)))

        ;; Media
        ;; (when (or (transient-arg-value "--media-download" args)
        ;;           (cdr (assoc "--media-subs=" args)))
        ;;   (org-ilm-convert--transient-media-run source attach-dir org-id args))

        ))
    :inapt-if
    (lambda ()
      (pcase-let (((map webpage-download media-download media-subs)
                   (org-ilm--transient-parse))
                  ((map type) (transient-scope)))
        (and (eq type 'url)
             (not webpage-download)
             (not media-download)
             (not media-subs)))))
   ]

  (interactive "P")
  (transient-setup 'org-ilm--convert-transient nil nil :scope data))


;;;; Footer

(provide 'org-ilm-convert)

;;; org-ilm-convert.el ends here
