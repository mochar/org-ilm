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

(defcustom convtools-marker-path (executable-find "marker_single")
  "Path to the marker_single executable."
  :type 'file
  :group 'convtools)

(defcustom convtools-keep-buffer-alive 'always
  "Keep the conversion process buffer alive, regardless of success or fail."
  :type '(choice (const :tag "Always" always)
                 (const :tag "On success" success)
                 (const :tag "On error" error))
  :group 'convtools)

;;;; Convertors

(cl-defun convtools--convert-make-process (&key name id command on-success on-error keep-buffer-alive dont-update-state &allow-other-keys)
  "Create an async process to run COMMAND."
  (let* ((process-name (format "convtools-%s (%s)" name id))
         (process-buffer (get-buffer-create (concat "*" process-name "*"))))
    (with-current-buffer process-buffer
      (setq-local convtools--convert-name name
                  convtools--convert-id id
                  convtools--convert-state nil)
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

           (unless dont-update-state
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
         (convert-on-error (plist-get converter-args :on-error))
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
            ;; signifies success or failure of a process. dont set this unless we are
            ;; on the last converter.
            :dont-update-state (if rest t nil)
            )))
    (apply converter converter-args)))

(cl-defun convtools--convert-with-pandoc (&rest args &key process-id process-name input-path input-format output-dir &allow-other-keys)
  "Convert a file to Org mode format using Pandoc."
  (unless (and process-id input-path input-format)
    (error "Required args: PROCESS-ID INPUT-PATH INPUT-FORMAT"))
  (let* ((input-path (expand-file-name input-path))
         (output-dir (expand-file-name (or output-dir (file-name-directory input-path))))
         ;; Make sure we are in output dir so that media files references correct
         (default-directory output-dir))
    (apply
     #'convtools--convert-make-process
     (org-combine-plists
      args
      (list
       :name (or process-name "pandoc")
       :id process-id
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
         "-o" (concat (file-name-sans-extension input-path) ".org"))))))))

;; Marker: https://github.com/datalab-to/marker
;; Much better Org formatting when converting from markdown
(cl-defun convtools--convert-with-marker (&rest args &key process-id process-name input-path format output-dir pages disable-image-extraction move-content-out new-name to-org on-success &allow-other-keys)
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
         "--table_rec_batch_size" "1"))
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
         
         (funcall on-success proc buf id)))))))

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
     

;;;; Conversions view

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
  "B" #'convtools-conversions-ibuffer)

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
    (kill-buffer (plist-get (vtable-current-object) :buffer))))

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
       ("State" (pcase (plist-get object :state)
                  ('error (propertize "Error" 'face 'error))
                  ('success (propertize "Success" 'face 'success))
                  (t "Busy")))
       ("Converter" (plist-get object :name))))
   :keymap convtools-conversions-map))

(defun convtools-conversions ()
  "Open the conversions view."
  (interactive)
  (let ((buf (get-buffer-create "*ilm conversions*")))
    (with-current-buffer buf
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (vtable-insert (convtools--conversions-make-vtable))
      (setq-local buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

;;;; Footer

(provide 'convtools)

;;; convtools.el ends here
