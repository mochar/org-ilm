;;; convtools.el --- Some tools to convert between things -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: 

;;; Commentary:

;; Some tools to handle file conversions and stuff

;;; Code:

;;;; Requirements
(require 'cl-lib)
(require 'dash)
(require 'vtable)

;;;; Global customization

(defgroup convtools nil
  "Conversion tools"
  :prefix "convtools-"
  :link '(url-link :tag "GitHub" "https://github.com/mochar/org-ilm"))

(defcustom convtools-node-path (executable-find "node")
  "Path to the node executable."
  :type 'file
  :group 'convtools)

(defcustom convtools-keep-buffer-alive 'always
  "Keep the conversion process buffer alive, regardless of success or fail."
  :type '(choice (const :tag "Always" always)
                 (const :tag "On success" success)
                 (const :tag "On error" error))
  :group 'convtools)

;;;; Convert process

(cl-defun convtools--convert-make-process (&key name id command on-success on-error keep-buffer-alive prevent-success-state buffer-data &allow-other-keys)
  "Create an async process to run COMMAND."
  (let* ((process-name (format "convtools-%s (%s)" name id))
         (process-buffer (get-buffer-create (concat "*" process-name "*"))))
    (with-current-buffer process-buffer
      (setq-local convtools--convert-name name
                  convtools--convert-id id
                  convtools--convert-state 'busy
                  convtools--convert-data buffer-data)
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
               (setq-local convtools--convert-state
                           (if success 'success 'error))))
           
           (if success
               (when on-success
                 (funcall on-success proc process-buffer id))
             (when on-error
               (funcall on-error proc process-buffer id)))
           (unless (or keep-buffer-alive
                       (eq convtools-keep-buffer-alive 'always)
                       (and success (eq convtools-keep-buffer-alive 'success))
                       (and (not success) (eq convtools-keep-buffer-alive 'error)))
             (kill-buffer process-buffer))))))
    process-buffer))

(cl-defun convtools--convert-multi (&rest args &key process-name process-id converters on-error on-final-success)
  "Run multiple convert processes successively.

CONVERTERS is an alist. Each element's car is the converter function and
cdr the function arguments. All converters share a process buffer with
ID and NAME.

The callback ON-ERROR can be given which will be called after the
on-error callback of each converter. Note that the process buffer local
variables `convtools--convert-name' and `convtools--convert-id' can be used
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
                (apply #'convtools--convert-multi
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
            ;; convtools--convert-make-process adds a buffer local variable that
            ;; signifies success or failure of a process. dont set success
            ;; unless we are on the last converter.
            :prevent-success-state (if rest t nil)
            )))
    (apply converter converter-args)))

;;;; Pandoc

(cl-defun convtools--convert-with-pandoc (&rest args &key process-id process-name input-path input-format output-dir output-name &allow-other-keys)
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
     #'convtools--convert-make-process
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

(defconst convtools-defuddle-path
  (expand-file-name "scripts/defuddle.mjs"
                    (file-name-directory (or load-file-name buffer-file-name))))

(cl-defun convtools--convert-with-defuddle (&rest args &key process-id process-name input-path output-format &allow-other-keys)
  "Convert an HTM file to Markdown using Defuddle."
  (unless (and process-id input-path output-format)
    (error "Required args: PROCESS-ID INPUT-PATH OUTPUT-FORMAT"))
  (unless (member output-format '("markdown" "html"))
    (error "OUTPUT-FORMAT must be one of [markdown|html]"))
  (let* ((input-path (expand-file-name input-path))
         (to-markdown (string= output-format "markdown"))
         (output-path (expand-file-name
                       (format "%s.%s"
                               (file-name-base input-path)
                               (if to-markdown "md" "html"))
                       (file-name-directory input-path))))
    (apply
     #'convtools--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "defuddle")
       :id process-id
       :command
       (list
        convtools-node-path
        convtools-defuddle-path
        "html"
        input-path
        output-format
        output-path))))))


;;;; Marker
;; Marker: https://github.com/datalab-to/marker
;; Much better Org formatting when converting from markdown

(defcustom convtools-marker-path (executable-find "marker_single")
  "Path to the marker_single executable."
  :type 'file
  :group 'convtools-marker)

(cl-defun convtools--convert-with-marker (&rest args &key process-id process-name input-path format output-dir pages disable-image-extraction move-content-out new-name to-org flags on-success &allow-other-keys)
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
  (unless (and convtools-marker-path (file-executable-p convtools-marker-path))
    (user-error "Marker executable not available. See convtools-marker-path."))
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
     #'convtools--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "marker")
       :id process-id
       :command
       (append
        (list
         convtools-marker-path
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
         ;; since convtools works with org-ids all the time.

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

(cl-defun convtools--convert-to-org-with-marker-pandoc (&key process-id input-path new-name pdf-pages on-success on-error)
  "Convert a PDF file or image to Markdown using Marker, then to Org mode using Pandoc."
  (unless (and process-id input-path new-name)
    (error "Required args: PROCESS-ID INPUT-PATH NEW-NAME"))
  (let ((input-dir (file-name-directory input-path))
        (process-name "marker-pandoc"))
    (convtools--convert-multi
     :process-name process-name
     :process-id process-id
     :converters
     (list
      (cons #'convtools--convert-with-marker
            (list
             :input-path input-path
             :format "markdown"
             :new-name new-name
             :pages pdf-pages
             :move-content-out t
             :to-org t))
      (cons #'convtools--convert-with-pandoc
            (list
             :input-path (file-name-concat input-dir (concat new-name ".md"))
             :input-format "markdown")))
     :on-error on-error
     :on-final-success on-success)))
     

;;;; Monolith

(defcustom convtools-monolith-path (executable-find "monolith")
  "Path to the monolith executable."
  :type 'file
  :group 'convtools-monolith)

(defcustom convtools-monolith-args
  '("--no-fonts" "--no-js")
  "Arguments passed to monolith."
  :type '(repeat string)
  :group 'convtools-monolith)

(cl-defun convtools--monolith-compile-paths (&rest args &key process-id input-path output-path &allow-other-keys)
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

(cl-defun convtools--convert-with-monolith (&rest args &key process-id process-name input-path output-path &allow-other-keys)
  "Convert a web URL or local HTML file to a single HTML file using Monolith."
  (unless (and convtools-monolith-path (file-executable-p convtools-monolith-path))
    (user-error "Monolith executable not available. See convtools-monolith-path."))

  (cl-destructuring-bind (&key input-path output-path)
      (apply #'convtools--monolith-compile-paths args)
    (apply
     #'convtools--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "monolith")
       :id process-id
       :command
       (append 
        (list
         convtools-monolith-path
         input-path
         "-o" output-path)
        convtools-monolith-args))))))

(cl-defun convtools--convert-with-monolith-defuddle (&key process-id on-success on-error monolith-args defuddle-args)
  "Convert a URL or HTML file to single file HTML using Monolith, and
 simplify it to Markdown or replaced HTML file with Pandoc."
  (unless (and process-id monolith-args)
    (error "Required args: PROCESS-ID MONOLITH-ARGS"))

  (setq monolith-args
        (plist-put monolith-args :process-id process-id))
  
  (cl-destructuring-bind (&key input-path output-path)
      (apply #'convtools--monolith-compile-paths monolith-args)
    (let* ((monolith-output-path output-path)
           (output-format (plist-get defuddle-args :output-format))
           (defuddle-output-path (concat
                                  (file-name-sans-extension output-path)
                                  "."
                                  (if (string= output-format "markdown")
                                      "md" "html"))))
      (convtools--convert-multi
       :process-name "monolith-defuddle"
       :process-id process-id
       :converters
       (list
        (cons #'convtools--convert-with-monolith monolith-args)
        (cons #'convtools--convert-with-defuddle
              (append
               (list :input-path monolith-output-path)
               defuddle-args)))
       :on-error on-error
       :on-final-success on-success))))

(cl-defun convtools--convert-to-org-with-monolith-pandoc (&key process-id on-success on-error monolith-args pandoc-args)
  "Convert a URL or HTML file to single file HTML using Monolith, then to Org mode using Pandoc."
  (unless (and process-id monolith-args)
    (error "Required args: PROCESS-ID MONOLITH-ARGS"))

  (setq monolith-args
        (plist-put monolith-args :process-id process-id))
  
  (cl-destructuring-bind (&key input-path output-path)
      (apply #'convtools--monolith-compile-paths monolith-args)
    (convtools--convert-multi
     :process-name "monolith-pandoc"
     :process-id process-id
     :converters
     (list
      (cons #'convtools--convert-with-monolith monolith-args)
      (cons #'convtools--convert-with-pandoc
            (append
             pandoc-args
             (list
              :input-path output-path
              :input-format "html"))))
     :on-error on-error
     :on-final-success on-success)))

(cl-defun convtools--convert-to-org-with-monolith-defuddle-pandoc (&key process-id on-success on-error monolith-args defuddle-args pandoc-args)
  "Convert a URL or HTML file to single file HTML using Monolith, simplify to Markdown with Defuddle, then to Org mode using Pandoc."
  (unless (and process-id monolith-args)
    (error "Required args: PROCESS-ID MONOLITH-ARGS"))

  (setq monolith-args
        (plist-put monolith-args :process-id process-id))
  
  (cl-destructuring-bind (&key input-path output-path)
      (apply #'convtools--monolith-compile-paths monolith-args)
    (let* ((monolith-output-path output-path)
           (defuddle-output-format (plist-get defuddle-args :output-format))
           (defuddle-output-path (concat
                                  (file-name-sans-extension output-path)
                                  "."
                                  (if (string= defuddle-output-format "markdown")
                                      "md" "html"))))
    (convtools--convert-multi
     :process-name "monolith-defuddle-pandoc"
     :process-id process-id
     :converters
     (list
      (cons #'convtools--convert-with-monolith monolith-args)
      (cons #'convtools--convert-with-defuddle
            (append
             (list :input-path monolith-output-path)
             defuddle-args))
      (cons #'convtools--convert-with-pandoc
            (append
             pandoc-args
             (list
              :input-path defuddle-output-path
              :input-format defuddle-output-format))))
     :on-error on-error
     :on-final-success on-success))))

;;;; yt-dlp

(defcustom convtools-ytdlp-path (executable-find "yt-dlp")
  "Path to the yt-dlp executable."
  :type 'file
  :group 'convtools-ytdlp)

;; https://github.com/yt-dlp/yt-dlp/wiki/FAQ#how-do-i-pass-cookies-to-yt-dlp
(defcustom convtools-ytdlp-args '("--cookies-from-browser" "firefox")
  "Arguments to always pass to yt-dlp."
  :type '(repeat string)
  :group 'convtools-ytdlp)

(defun convtools--ytdlp-filename-from-url (url &optional template restrict-p)
  "Get the filename that will be generated for URL and TEMPLATE.

See: https://github.com/yt-dlp/yt-dlp?tab=readme-ov-file#output-template-examples"
  (string-trim
   (car
    (apply #'process-lines
     (append
      '("yt-dlp" "--print" "filename")
      (when template (list "-o" template))
      (list url)
      (when restrict-p '("--restrict-filenames"))
      convtools-ytdlp-args
      '("--no-warnings"))))))

(defun convtools--ytdlp-title-from-url (url)
  (convtools--ytdlp-filename-from-url url "%(title)s"))

(defun convtools--ytdlp-subtitles-from-url (url)
  "Returns two alists of alist: 'subtitles and 'auto.

Parse the OUTPUT string from:
   yt-dlp --print subtitles_table --print automatic_captions_table."
  (let* ((lines (ignore-errors
                  (apply #'process-lines
                         (append
                          '("yt-dlp" "--print" "subtitles_table" "--print" "automatic_captions_table")
                          (list url)
                          convtools-ytdlp-args
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

(cl-defun convtools--convert-with-ytdlp (&rest args &key process-id process-name url output-dir output-path filename-template sub-langs audio-only-p no-download working-dir on-success &allow-other-keys)
  "Download media from url using yt-dlp.

SUB-LANGS may also be 'all' to download all subtitles."
  (unless (and process-id url (or (xor output-path output-dir) sub-langs))
    (error "Required args: PROCESS-ID URL [OUTPUT-DIR|OUTPUT-PATH]"))
  (unless (and convtools-ytdlp-path (file-executable-p convtools-ytdlp-path))
    (user-error "The yt-dlp executable not available. See convtools-ytdlp-path."))

  (when (and (not output-path) output-dir)
    (setq output-path (expand-file-name
                       (convtools--ytdlp-filename-from-url url filename-template 'restrict)
                       output-dir)))

  (when sub-langs
    (cond
     ((stringp sub-langs)
      (setq sub-langs (list sub-langs)))
     ((listp sub-langs))
     (t (error "SUB-LANGS must be string or list of strings"))))

  (let ((default-directory (or working-dir default-directory)))
    (apply
     #'convtools--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "ytdlp")
       :id process-id
       :command
       (append
        (list
         convtools-ytdlp-path
         url
         "--embed-chapters")
        convtools-ytdlp-args
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

(defconst convtools--conversions-buffer-name
  "*convtools conversions*")

(defvar-keymap convtools-conversions-map
  :doc "Keymap for the conversions view."
  ;; Navigation
  "n" (lambda ()
        (interactive)
        (next-line)
        (when (eobp) (previous-line)))
  "p" #'previous-line
  "b" #'backward-char
  "f" #'forward-char
  "q" #'quit-window
  "k" (lambda ()
        (interactive)
        (kill-buffer (current-buffer)))
  ;; Actions
  "SPC" #'convtools-conversions-goto
  "RET" #'convtools-conversions-buffer-open
  "d" #'convtools-conversions-buffer-delete
  "B" #'convtools-conversions-ibuffer
  "g" #'convtools-conversions-revert)

(defun convtools-conversions-goto ()
  "View element of object at point in collection."
  (interactive)
  (let ((org-id (plist-get (vtable-current-object) :id)))
    (org-id-goto org-id)))

(defun convtools-conversions-buffer-open ()
  "Open buffer of object at point."
  (interactive)
  (pop-to-buffer (plist-get (vtable-current-object) :buffer)))

(defun convtools-conversions-buffer-delete ()
  "Kill buffer of object at point."
  (interactive)
  (when (yes-or-no-p "Kill buffer?")
    (kill-buffer (plist-get (vtable-current-object) :buffer))
    (convtools-conversions-revert)))

(defun convtools-conversions-ibuffer ()
  "Open conversion buffers in ibuffer."
  (interactive)
  (ibuffer nil "*Ilm Conversion Buffers*"
           '((name . "^\\*convtools-"))))

(defun convtools--conversions-buffers ()
  "Return all make-process buffers of convertors."
  (seq-filter
   (lambda (buf)
     (string-match-p
      (rx bol "*convtools-" (+? anything) " (" (+? anything) ")*" eol)
      (buffer-name buf)))
   (buffer-list)))

(defun convtools--conversions ()
  "Return list of plists containing info of each conversion."
  (mapcar
   (lambda (buf)
     (let* ((buffer-name (buffer-name buf))
            (_ (string-match
                "^\\*convtools-\\([^ ]+\\) (\\([^)]*\\))\\*$"
                buffer-name))
            (name (match-string 1 buffer-name))
            (id (match-string 2 buffer-name))
            (state (with-current-buffer buf
                     convtools--convert-state)))
       (list :name name :id id :buffer buf :state state)))
   (convtools--conversions-buffers)))

(defun convtools--conversion-by-id (id)
  (seq-find
   (lambda (conversion)
     (string= (plist-get conversion :id) id))
   (convtools--conversions)))

(defun convtools--conversion-propertize-state (state)
  (pcase state
    ('error (propertize "Error" 'face 'error))
    ('success (propertize "Success" 'face 'success))
    ('busy (propertize "Busy" 'face 'italic))))

(defun convtools-conversions-revert ()
  (interactive)
  (cond
   ((not (string= (buffer-name) convtools--conversions-buffer-name))
    (user-error "Not in conversions buffer"))
   ((convtools--conversions)
    (vtable-revert-command))
   (t
    (kill-buffer convtools--conversions-buffer-name)
    (message "No conversions found!"))))

(defun convtools--conversions-make-vtable ()
  "Build conversions view vtable."
  (make-vtable
   :insert nil ; Return vtable object rather than insert at point
   :objects-function
   #'convtools--conversions
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
       ("State" (convtools--conversion-propertize-state (plist-get object :state)))
       ("Converter" (plist-get object :name))))
   :keymap convtools-conversions-map))

(defun convtools-conversions ()
  "Open the conversions view."
  (interactive)
  (let ((buf (get-buffer-create convtools--conversions-buffer-name))
        (conversions (convtools--conversions)))
    (cond
     (conversions
      (with-current-buffer buf
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (goto-char (point-min))
        (vtable-insert (convtools--conversions-make-vtable))
        (setq-local buffer-read-only t)
        (goto-char (point-min)))
      (switch-to-buffer buf))
     (t
      (kill-buffer buf)
      (message "No conversions found!")))))

;;;; Org headings

;; Convert using metadata in Org heading properties and attachments.

(defvar convtools--org-data nil)

(defun convtools-org-convert ()
  (interactive)
  (let* ((attach-dir (org-attach-dir))
         (attachments (when attach-dir (org-attach-file-list attach-dir)))
         (entry (org-node-at-point))
         (refs (mochar-utils--org-mem-refs entry)))
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
      (setq convtools--org-data
            (list :title (org-mem-entry-title entry) :source source :type type))
      (mochar-utils--add-hook-once
       'transient-post-exit-hook
       (lambda () (setq convtools--org-data nil)))
      (convtools--org-transient))))

(transient-define-prefix convtools--org-transient ()
  :refresh-suffixes t
  :value
  (lambda ()
    (list (concat "--source=" (plist-get convtools--org-data :source))))

  ["Convtool"
   (:info*
    (lambda ()
      (propertize (plist-get convtools--org-data :title) 'face 'italic))
    :if (lambda () (plist-get convtools--org-data :title)))
   (:info*
    (lambda ()
      (propertize (plist-get convtools--org-data :source) 'face 'transient-value))
    :if (lambda () (plist-get convtools--org-data :source)))
   ]

  [:hide (lambda () t) ("s" "Source" "--source=")]

  ["Webpage"
   :if
   (lambda ()
     (or (eq (plist-get convtools--org-data :type) 'url)
         (string= (file-name-extension (plist-get convtools--org-data :source)) "webpage")))
   :setup-children
   (lambda (_)
     (convtools--transient-webpage-build
      (eq (plist-get convtools--org-data :type) 'url)))
   ]

  ["Media"
   :if
   (lambda ()
     (and (eq (plist-get convtools--org-data :type) 'url)
          (convtools--transient-media-url-is-media-p
           (plist-get convtools--org-data :source))))
   :setup-children
   (lambda (_)
     (convtools--transient-media-build))
   ]

  ["Actions"
   ("RET" "Convert"
    (lambda ()
      (interactive)
      (when-let* ((args (transient-args 'convtools--org-transient))
                  (source (plist-get convtools--org-data :source))
                  (type (plist-get convtools--org-data :type))
                  (org-id (org-id-get))
                  (attach-dir (org-attach-dir-get-create)))

        ;; Webpage
        (when-let ((_ (transient-arg-value "--webpage-download" args))
                   (title (if (eq type 'url)
                              (if-let ((title (mochar-utils--get-page-title source)))
                                  (mochar-utils--slugify-title title)
                                org-id)
                            (file-name-base source))))
          (convtools--transient-webpage-run source title attach-dir org-id args))

        ;; Media
        (when (or (transient-arg-value "--media-download" args)
                  (cdr (assoc "--media-subs=" args)))
          (convtools--transient-media-run source attach-dir org-id args))))
    :inapt-if
    (lambda ()
      (and (eq (plist-get convtools--org-data :type) 'url)
           (not (transient-arg-value "--webpage-download" (transient-get-value)))
           (not (transient-arg-value "--media-download" (transient-get-value)))
           (not (cdr (assoc "--media-subs=" (transient-get-value)))))))
   ]
  )

;;;;; Webpage

(defvar convtools--transient-webpage-download-suffix
  '("wd" "Download" "--webpage-download" :transient transient--do-call))

(defvar convtools--transient-webpage-simplify-suffix
  '("ws" "Simplify" "--webpage-simplify="
     :class transient-switches
     :transient transient--do-call
     :argument-format "--webpage-simplify-to-%s"
     :argument-regexp "\\(--webpage-simplify-to-\\(markdown\\|html\\)\\)"
     :choices ("markdown" "html")))

(defvar convtools--transient-webpage-orgify-suffix
  '("wo" "Orgify" "--webpage-orgify"
    :summary "Convert to Org mode with Pandoc"
    :transient transient--do-call))

(defun convtools--transient-webpage-build (&optional with-download hide-invalid-p)
  (let ((condition
         (lambda ()
           (and with-download
                (null (transient-arg-value "--webpage-download" (transient-get-value)))))))
    (mapcar
     (lambda (suffix)
       (transient-parse-suffix 'transient--prefix suffix))
     (append
      (when with-download
        (list convtools--transient-webpage-download-suffix))
      (mapcar
       (lambda (suffix)
         (append suffix (list (if hide-invalid-p :if-not :inapt-if) condition)))
       (list convtools--transient-webpage-simplify-suffix
             convtools--transient-webpage-orgify-suffix))))))

(defun convtools--transient-webpage-run (source title output-dir id transient-args)
  (let* ((download (transient-arg-value "--webpage-download" transient-args))
         (simplify (cond
                    ((transient-arg-value "--webpage-simplify-to-html" transient-args)
                     "html")
                    ((transient-arg-value "--webpage-simplify-to-markdown" transient-args)
                     "markdown")))
         (orgify (transient-arg-value "--webpage-orgify" transient-args))
         (html-path (expand-file-name (concat title ".html") output-dir))
         (on-success
          (lambda (proc buf id)
            (message "[Convtools] HTML conversion completed: %s" source)
            (mochar-utils--org-with-point-at id
              (org-attach-sync))))
         converters)

    (when download
      (push
       (cons #'convtools--convert-with-monolith
             (list :input-path source :output-path html-path))
       converters))

    (when simplify
      (push
       (cons #'convtools--convert-with-defuddle
             (list :input-path html-path :output-format simplify))
       converters))

    (when orgify
      (push
       (cons #'convtools--convert-with-pandoc
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
    
    (convtools--convert-multi
     :process-name "webpage"
     :process-id id
     :converters converters
     :on-error nil
     :on-final-success on-success)))

;;;;; Media

(defun convtools--transient-media-url-is-media-p (url)
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

(defvar convtools--transient-media-download-suffix
  '("md" "Download" "--media-download" :transient transient--do-call))

(defvar convtools--transient-media-template-suffix
  '("mt" "Template" "--media-template="
    :prompt "Template: " :transient transient--do-call))

(defvar convtools--transient-media-audio-suffix
  '("ma" "Audio only" "--media-audio" :transient transient--do-call))

(defvar convtools--transient-media-subs-suffix
  '("ms" "Subtitles download" "--media-subs="
    :class transient-option
    :transient transient--do-call
    :multi-value rest
    :choices
    (lambda ()
      (let* ((url (transient-arg-value "--source=" (transient-args transient-current-command)))
             (subs (convtools--ytdlp-subtitles-from-url url)))
        (mapcar (lambda (x) (alist-get 'language x)) (alist-get 'subtitles subs))))))

(defun convtools--transient-media-build (&optional no-template)
  (let ((inapt-if
         (lambda ()
           (null (transient-arg-value "--media-download" (transient-get-value)))))
        suffixes)
        
    (push convtools--transient-media-subs-suffix suffixes)
    (push (append
           convtools--transient-media-audio-suffix
           (list :inapt-if inapt-if)) suffixes)
    (push (append
           convtools--transient-media-template-suffix
           (if no-template
               (list :inapt-if-nil nil)
           (list :inapt-if inapt-if)))
          suffixes)
    (push convtools--transient-media-download-suffix suffixes)
    
    (mapcar
     (lambda (suffix)
       (transient-parse-suffix 'transient--prefix suffix))
     suffixes)))

(defun convtools--transient-media-run (url output-dir id transient-args &optional on-success)
  "Convert a media URL to a local video file."
  (let* ((download (transient-arg-value "--media-download" transient-args))
         (template (transient-arg-value "--media-template=" transient-args))
         (audio-only (transient-arg-value "--media-audio" transient-args))
         (sub-langs (cdr (assoc "--media-subs=" transient-args))))

    (convtools--convert-with-ytdlp
     :process-id id
     :url url
     :output-dir output-dir
     :filename-template template
     :audio-only-p audio-only
     :sub-langs sub-langs
     :no-download (not download)
     :on-success
     (lambda (proc buf id output-path)
       (message "[Convtools] Media conversion completed: %s" url)
       (mochar-utils--org-with-point-at id
         (org-attach-sync)
         (when on-success
           (funcall on-success id output-path)))))))

        

;;;; Footer

(provide 'convtools)

;;; convtools.el ends here
