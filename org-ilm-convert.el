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

;;;; Jobs

(cl-defstruct (org-ilm-convert-chain
               (:conc-name org-ilm-convert-chain--))
  name id prev next)
  
(cl-defstruct (org-ilm-convert-job
               (:conc-name org-ilm-convert-job--))
  type ; pandoc, marker, ...
  (id (org-id-new))
  name
  (status :pending) ; :pending, :busy, :success, :error
  method ; :cli, :elisp
  payload ; elispL func. cli: command list, or func make one
  data ; arbitrary
  buffer
  chain ; org-ilm-convert-chain object
  start end ;; ts
  on-success
  on-error
  default-dir)

(defvar org-ilm-convert--jobs (make-hash-table :test 'equal)
  "Registry of all conversion jobs.")

(defvar org-ilm-convert-job-update-hook nil
  "Hook run when status of job has changed. Passes the job object.")

(defun org-ilm-convert-job--update (job status)
  "Set status slot of JOB to STATUS and run hook."
  (cl-assert (member status '(:pending :busy :success :error)))
  (oset job status status)
  (run-hook-with-args 'org-ilm-convert-job-update-hook job))

(defun org-ilm-convert--job-buf-get-create (name id)
  "Get-create the buffer where output of job with NAME and ID are stored."
  (get-buffer-create (format "*org-ilm-convert %s (%s)*" name id)))

(defun org-ilm-convert-job--run (job)
  "Run JOB. Registers it in `org-ilm-convert--jobs'."
  (unless (oref job buffer)
    (oset job buffer (org-ilm-convert--job-buf-get-create
                      (oref job name) (oref job id))))
  (unless (gethash (oref job id) org-ilm-convert--jobs)
    (puthash (oref job id) job org-ilm-convert--jobs))

  (with-slots (buffer name method payload default-dir data) job
    (when (and (eq method :cli) (functionp payload))
      (setq payload (funcall payload)))
    
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (propertize (format "\n====== START: %s ======\n" name) 'face 'bold))
      (insert (propertize (oref job id) 'face 'Info-quoted) "\n")
      (when default-dir
        (insert (propertize "Default directory:" 'face 'highlight) " " default-dir "\n"))
      (let ((print-length nil))
        (when data
          (insert (propertize "Data:" 'face 'highlight) "\n")
          (insert (pp-to-string data)))
        (insert (propertize "Payload:" 'face 'highlight) "\n")
        (insert (pp-to-string payload)))
      (insert "\n\n\n" (propertize "Log:" 'face 'highlight) "\n"))

    (oset job start (ts-now))
    (org-ilm-convert-job--update job :busy)
    
    (pcase method
      (:cli
       (let ((default-directory (or (oref job default-dir) default-directory)))
         (make-process
          :name name
          :buffer buffer
          :command payload
          :sentinel
          (lambda (proc event)
            (when (memq (process-status proc) '(exit signal))
              (let ((success (= (process-exit-status proc) 0)))
                (org-ilm-convert-job--handle-completion job success)))))))
      
      (:elisp
       (make-thread
        (lambda ()
          (condition-case err
              (let ((default-directory (or (oref job default-dir) default-directory)))
                (funcall payload job)
                ;; Send the UI/completion update back to the MAIN thread safely
                (run-at-time 0 nil #'org-ilm-convert-job--handle-completion job t))
            (error
             (with-current-buffer (oref job buffer)
               (goto-char (point-max))
               (insert (format "%s" err)))
             (run-at-time 0 nil #'org-ilm-convert-job--handle-completion job nil))))
        name)))))

(defun org-ilm-convert-job--handle-completion (job success)
  "Handles completion of a JOB. SUCCESS indicates if job finished
successfully."
  (oset job end (ts-now))
  (with-slots (buffer name on-success on-error) job
    (let* ((status (if success :success :error))
           (chain (oref job chain))
           (next-job-id (when chain (oref chain next)))
           (next-job (gethash next-job-id org-ilm-convert--jobs)))
      
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (propertize (format "\n====== END: %s (" name) 'face 'bold)
                (org-ilm-convert--propertize-job-status status)
                (propertize ") ======\n" 'face 'bold)))

      (org-ilm-convert-job--update job status)

      (cond
       (success
        (when on-success (funcall on-success job))
        (when next-job
          (org-ilm-convert-job--run next-job)))
       (t ;; error
        (when on-error (funcall on-error job))
        (when next-job
          (org-ilm-convert-job--handle-completion next-job nil)))))))

(defun org-ilm-convert--propertize-job-status (status)
  (pcase status
    (:pending (propertize "Pending" 'face 'Info-quoted))
    (:error (propertize "Error" 'face 'error))
    (:success (propertize "Success" 'face 'success))
    (:busy (propertize "Busy" 'face 'italic))))

;; The following functions and generics relate to the API of creating
;; converters.  The idea is to register the converter by implementing the
;; generic `org-ilm--convert-make-converter', which works by specifying a unique
;; converter type symbol. The ARGS parameter are converter-specific
;; arguments. If you wish to pass values for the job slots, add an argument
;; "job-args". Make sure to call `org-ilm--convert-make-job' at the end of the
;; implementation, and to return the job. If the converter is to be used in a
;; chain, implement the generic `org-ilm--convert-make-converter-in-chain' for
;; the different types.

(defun org-ilm--convert-make-job (run-p &rest args)
  "Create a job using ARGS and registers it in the global hashmap."
  (let ((job (apply #'make-org-ilm-convert-job args)))
    (unless (oref job id) (oset job id (org-id-new)))
    (unless (oref job name) (oset job name (symbol-name (oref job type))))
    (puthash (oref job id) job org-ilm-convert--jobs)
    (when run-p (org-ilm-convert-job--run job))
    job))

(cl-defgeneric org-ilm--convert-make-converter (type run-p &rest args)
  "Given ARGS, creates a job for converter of TYPE.
Implementations should call `org-ilm--convert-make-job' to register the job.")

(cl-defgeneric org-ilm--convert-make-converter-in-chain (type from-type from-job args)
  "Given a previous job FROM-JOB with type FROM-TYPE, make a new converter
of TYPE with the given ARGS."
  (apply #'org-ilm--convert-make-converter type nil args))

(cl-defun org-ilm--convert-make-converter-chain
    (&key name id buffer run-p on-success on-error converters)
  "Run a sequence of converters sequentially.

Jobs contain a slot :chain that holds the next and previous job ids in
the chain.

CONVERTERS is an alist converter-type -> converter-args.

ON-SUCCESS wil be run only when the last job finishes
successfully.. ON-ERROR will be called when any job errors."
  (when converters
    (let (jobs)

      ;; Create the job objects from the converter arguments. Since the
      ;; converter arguments do not necessarily contain the job name and ids, we
      ;; first have to create the objects which will ensure these values are
      ;; set.
      (dotimes (i (length converters))
        (pcase-let* ((`(,type . ,args) (nth i converters))
                     (job-args (plist-get args :job-args))
                     (job-on-success (plist-get job-args :on-success))
                     (job-on-error (plist-get job-args :on-error))
                     (prev-job (car jobs)))

          ;; If ON-ERROR is specified, it needs to be part of every job's
          ;; on-error callback.
          (when on-error
            (setf (plist-get (plist-get args :job-args) :on-error)
                  (lambda (j)
                    (when job-on-error (funcall job-on-error j))
                    (funcall on-error j))))

          ;; If ON-SUCCESS is specified, it needs to be part of the last job's
          ;; on-success callback only.
          (when (and on-success (= i (1- (length converters))))
            (setf (plist-get (plist-get args :job-args) :on-success)
                  (lambda (j)
                    (when job-on-success (funcall job-on-success j))
                    (funcall on-success j))))
          
          (push
           (org-ilm--convert-make-converter-in-chain
            type
            (when prev-job (oref prev-job type))
            prev-job args)
           jobs)))

      (setq jobs (reverse jobs))

      (unless name
        (setq name (mapconcat #'org-ilm-convert-job--name jobs "-")))
      (unless id (setq id (org-id-new)))
      (unless buffer
        (setq buffer (org-ilm-convert--job-buf-get-create name id)))

      (with-current-buffer buffer
        (insert (propertize (format "CHAIN: %s" name) 'face 'bold) "\n")
        (insert (propertize id 'face 'Info-quoted) "\n")
        (dolist (job jobs)
          (insert "- " (propertize (format "%s (%s)" (oref job name) (oref job id)) 'face 'highlight) "\n")))
      
      ;; Assign chain ids after the jobs are created to ensure id exists.
      (dotimes (i (length jobs))
        (let ((job (nth i jobs))
              (prev-job (when (> i 0) (nth (1- i) jobs)))
              (next-job (nth (1+ i) jobs)))
          (oset job buffer buffer) ; also the buffer ;)
          (oset job chain
                (make-org-ilm-convert-chain
                 :id id :name name
                 :prev (when prev-job (oref prev-job id))
                 :next (when next-job (oref next-job id))))))

      ;; Running a chain is done by running the first job. In
      ;; `org-ilm-convert-job--handle-completion', the next job in the chain
      ;; will be run.
      (when (and run-p jobs)
        (org-ilm-convert-job--run (car jobs)))
      jobs)))

;;;; Conversions view

(defconst org-ilm-convert--conversions-buffer-name
  "*org-ilm-convert conversions*")

(defvar-keymap org-ilm-convert-conversions-base-map
  "n" (lambda ()
        (interactive)
        (forward-line)
        (when (eobp) (forward-line -1)))
  "p" #'previous-line
  "b" #'backward-char
  "f" #'forward-char
  "q" #'quit-window
  ;; "k" (lambda ()
  ;;       (interactive)
  ;;       (kill-buffer (current-buffer)))
  "g" #'org-ilm-convert-conversions-revert
  )

(defvar-keymap org-ilm-convert-conversions-map
  :doc "Keymap for the conversions view."
  :parent org-ilm-convert-conversions-base-map
  "SPC" #'org-ilm-convert-conversions-goto
  "RET" #'org-ilm-convert-conversions-buffer-open
  "d" #'org-ilm-convert-conversions-delete
  "D" #'org-ilm-convert-conversions-delete-all
  "B" #'org-ilm-convert-conversions-ibuffer
  "r" #'org-ilm-convert-conversions-rerun)

(defun org-ilm-convert-conversions-goto ()
  "View element of object at point in collection."
  (interactive)
  (let ((org-id (oref (vtable-current-object) id)))
    (org-id-goto org-id)))

(defun org-ilm-convert-conversions-buffer-open ()
  "Open buffer of object at point."
  (interactive)
  (let ((buf (oref (vtable-current-object) buffer)))
    (if (and buf (buffer-live-p buf))
        (pop-to-buffer buf)
      (user-error "Buffer not found"))))

(defun org-ilm-convert-conversions-rerun ()
  "Rerun job at point."
  (interactive)
  (when (yes-or-no-p "Run again?")
    (let ((job (vtable-current-object)))
      (org-ilm-convert-job--run job)
      (org-ilm-convert-conversions-revert))))

(defun org-ilm-convert-conversions-delete ()
  "Remove job at point and its status buffer."
  (interactive)
  (when (yes-or-no-p "Delete?")
    (let ((job (vtable-current-object)))
      (kill-buffer (oref job buffer))
      (remhash (oref job id) org-ilm-convert--jobs)
      (org-ilm-convert-conversions-revert))))

(defun org-ilm-convert-conversions-delete-all ()
  "Remove all jobs and their status buffers."
  (interactive)
  (when (yes-or-no-p "Delete all?")
    (maphash
     (lambda (id job)
       (when-let ((buf (oref job buffer)))
         (when (buffer-live-p buf)
           (kill-buffer buf))))
     org-ilm-convert--jobs)
    (clrhash org-ilm-convert--jobs)
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

(defun org-ilm-convert-conversions-revert ()
  (interactive)
  (org-ilm-convert--conversions-rebuild))

(defun org-ilm-convert--conversions-make-vtable ()
  "Build conversions view vtable."
  (make-vtable
   :insert nil ; Return vtable object rather than insert at point
   :objects-function
   (lambda ()
     (let (jobs)
       (dolist (job (hash-table-values org-ilm-convert--jobs))
         (let ((chain (oref job chain)))
           (cond
            ((null chain)
             (push job jobs))
            ((null (oref chain prev))
             (while job
               (push job jobs)
               (setq job (-some-> job (oref chain) (oref next)
                                  (gethash org-ilm-convert--jobs))))))))
       (reverse jobs)))
   :columns
   `((:name
      "Status"
      :formatter
      (lambda (status)
        (org-ilm-convert--propertize-job-status status))
      )
     (:name
      "Start"
      :width 6
      )
     (:name
      "End"
      :width 6
      )
     (:name
      "Converter"
      ;; :min-width "30%"
      ;; :min-width "100%"
      )
     (:name
      "Chain"
      :formatter
      (lambda (chain)
        (if chain (oref chain name) ""))
      :max-width "25%"
      )
     (:name
      "ID"
      :formatter
      (lambda (id)
        (propertize id 'face 'Info-quoted))
      :max-width "30%"
      )
     )
   :getter
   (lambda (job column vtable)
     (pcase (vtable-column vtable column)
       ("ID" (oref job id))
       ("Start" (ts-format "%H:%M" (oref job start)))
       ("End" (if (oref job end) (ts-format "%H:%M" (oref job end)) ""))
       ("Status" (oref job status))
       ("Converter" (oref job name))
       ("Chain" (oref job chain))
       ))
   :keymap org-ilm-convert-conversions-map))

(defun org-ilm-convert--conversions-buffer ()
  (let* ((buf-name org-ilm-convert--conversions-buffer-name)
         (buf (get-buffer buf-name)))
    (unless buf
      (with-current-buffer (setq buf (get-buffer-create buf-name))
        (add-hook 'org-ilm-convert-job-update-hook
                  (lambda (&rest _)
                    (org-ilm-convert--conversions-rebuild))
                  nil 'local)))
    buf))

(defun org-ilm-convert--conversions-rebuild ()
  (with-current-buffer (org-ilm-convert--conversions-buffer)
    (let ((no-jobs (hash-table-empty-p org-ilm-convert--jobs)))
      (if (and (vtable-current-table) (not no-jobs))
          ;; If the table is already drawn, just update it in place
          (vtable-revert-command)
        ;; Otherwise, build it for the first time
        (setq-local buffer-read-only nil)
        (erase-buffer)
        (goto-char (point-min))
        (if (hash-table-empty-p org-ilm-convert--jobs)
            (progn
              (insert (propertize "No conversions..." 'face 'italic))
              (use-local-map org-ilm-convert-conversions-base-map))
          (vtable-insert (org-ilm-convert--conversions-make-vtable)))
        (setq-local buffer-read-only t)
        (hl-line-mode 1)
        (goto-char (point-min)))
      (current-buffer))))

(defun org-ilm-convert-conversions ()
  "Open the conversions view."
  (interactive)
  (switch-to-buffer (org-ilm-convert--conversions-rebuild)))

;;;; Pandoc

(cl-defun org-ilm--convert-make-pandoc
    (run-p
     &key
     job-args
     input-path input-format
     output-path output-format output-name)
  "Convert with Pandoc."
  (setq input-path (expand-file-name input-path)
        input-format (or input-format (file-name-extension input-path)))
  (when (string= input-format "md") (setq input-format "markdown"))
  (let (output-dir)
    (cond
     ((null output-path)
      (cl-assert output-format)
      (setq output-dir (file-name-directory input-path)
            output-name (or output-name (file-name-base input-path))
            output-path (expand-file-name
                         (concat output-name "." output-format)
                         output-dir))) 
     ((file-directory-p output-path)
      (cl-assert output-format)
      (setq output-dir output-path
            output-name (or output-name (file-name-base input-path))
            output-path (expand-file-name
                         (concat output-name "." output-format)
                         output-dir)))
     (t ; File
      (setq output-dir (file-name-directory output-path)
            output-name (file-name-base output-path)
            output-format (file-name-extension output-path))))

    (apply
     #'org-ilm--convert-make-job
     run-p
     :type 'pandoc
     :method :cli
     ;; Make sure we are in output dir so that media files references correct
     :default-dir output-dir
     :data (list :output-path output-path
                 :output-format output-format)
     :payload
     (list "pandoc"
           "--from" input-format
           "--to" output-format
           "--wrap=preserve"
           ;; Relative to output-dir, or absolute path. Since we set
           ;; default-directory to output path and want to store
           ;; media in same folder, set to "."
           "--extract-media" "."
           "--verbose"
           input-path "-o" output-path)
     job-args)))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'pandoc)) run-p &rest args)
  (apply #'org-ilm--convert-make-pandoc run-p args))

(when nil
  (org-ilm--convert-make-pandoc
   'run
   :input-path "~/ilm/test/.ilm/attach/cceaf254-5bce-42db-a023-36b76d792045/log-normal-distribution-wikipedia.html"
   ;; :input-format "html"
   :output-path "~/tmp/marker/"
   :output-format "org"
   ;; :output-name
   :job-args
   (list
    )
   )
)


;;;; Defuddle

;; Defuddle: https://github.com/kepano/defuddle
;; Extract main content from webpages. Convert to markdown.

(defcustom org-ilm-convert-defuddle-path (executable-find "defuddle")
  "Path to the defuddle executable."
  :type 'file
  :group 'org-ilm-convert)

(cl-defun org-ilm--convert-make-defuddle
    (run-p
     &key
     job-args
     input input-format
     output-format output-name output-folder)
  "Convert a URL or HTML file to Markdown using Defuddle."
  (cl-assert (member output-format '("markdown" "html")) nil
             "OUTPUT-FORMAT must be one of [markdown|html]")
  (let* ((input-type (cond
                      ((org-url-p input)
                       (unless output-folder
                         (error "Must specify OUTPUT-FOLDER when INPUT is URL"))
                       (unless output-name
                         (setq output-name (org-ilm--get-page-title input 'slugify)))
                       "url")
                      ;; Don't want to do file-exists-p as the job can be part
                      ;; of a pipeline where previous job will create the input
                      ;; file. So: Just assume it is a valid file path if it is
                      ;; not an url.
                      (t
                       (setq input (expand-file-name input))
                       (unless output-name
                         (setq output-name (file-name-base input)))
                       (unless output-folder
                         (setq output-folder (file-name-directory input)))
                       "html")))
         (to-markdown (string= output-format "markdown"))
         (output-path (expand-file-name
                       (concat output-name "." (if to-markdown "md" "html"))
                       output-folder)))

    (apply
     #'org-ilm--convert-make-job
     run-p
     :type 'defuddle
     :method :cli
     :data (list :output-path output-path
                 :output-format output-format)
     :payload
     (-non-nil
      (list
       org-ilm-convert-defuddle-path
       "parse"
       input
       (when to-markdown "--markdown")
       "--output"
       output-path))
     job-args)))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'defuddle)) run-p &rest args)
  (apply #'org-ilm--convert-make-defuddle run-p args))

(cl-defmethod org-ilm--convert-make-converter-in-chain
  ((type (eql 'pandoc)) (from-type (eql 'defuddle)) defuddle-job args)
  (map-let (:output-path :output-format) (oref defuddle-job data)
    (setf (plist-get args :input-path) output-path
          (plist-get args :input-format) output-format))
  (cl-call-next-method type from-type defuddle-job args))

(when nil
  (org-ilm--convert-make-defuddle
   'run
   :input "~/ilm/test/.ilm/attach/49bfcc2b-2d3d-406f-8898-bfe444ea3a09/49bfcc2b-2d3d-406f-8898-bfe444ea3a09.html"
   :output-folder "~/tmp/marker/"
   :output-format "markdown"
   :job-args
   (list
    )
   )

  (org-ilm--convert-make-converter-chain
   :run-p t
   :on-success (lambda (j) (message "WOWOWOWO"))
   :converters
   '((defuddle
      :input "~/ilm/test/.ilm/attach/49bfcc2b-2d3d-406f-8898-bfe444ea3a09/49bfcc2b-2d3d-406f-8898-bfe444ea3a09.html"
      :output-folder "~/tmp/marker/"
      :output-format "markdown"
      )
     (pandoc
      :output-path "~/tmp/marker/"
      :output-format "org"
      )))
  )


;;;; QPDF

;; For cutting PDF files

(defcustom org-ilm-convert-qpdf-path (executable-find "qpdf")
  "Path to the qpdf executable."
  :type 'file
  :group 'org-ilm-convert-qpdf)

(cl-defun org-ilm--convert-make-qpdf
    (run-p
     &key
     job-args
     input-path 
     output-dir output-path output-name
     pages-spec flags)
  "Cut PDF using QPDF."
  (cl-assert pages-spec)
  (setq input-path (expand-file-name input-path))
  (unless (xor output-path output-name)
    (setq output-name (concat (file-name-base input-path) " " (ts-format (ts-now)))))
  (unless output-name
    (setq output-name (file-name-base output-path)))
  (unless output-dir
    (setq output-dir (if output-path (file-name-directory output-path)
                       (file-name-directory input-path))))
  (unless output-path
    (setq output-path (expand-file-name (concat output-name ".pdf") output-dir)))
  
  (apply
   #'org-ilm--convert-make-job
   run-p
   :type 'qpdf
   :method :cli
   :data (list :output-path output-path)
   :payload
   (append
    (list
     org-ilm-convert-qpdf-path
     "--empty"
     "--pages"
     input-path
     pages-spec
     "--"
     output-path)
    flags)
   job-args))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'qpdf)) run-p &rest args)
  (apply #'org-ilm--convert-make-qpdf run-p args))


;;;; Marker

;; Marker: https://github.com/datalab-to/marker
;; Much better Org formatting when converting from markdown

(defcustom org-ilm-convert-marker-path (executable-find "marker_single")
  "Path to the marker_single executable."
  :type 'file
  :group 'org-ilm-convert-marker)

(cl-defun org-ilm--convert-make-marker
    (run-p
     &key
     job-args
     input-path
     output-format output-dir new-name
     pages disable-image-extraction
     (move-content-out t) flags)
  "Convert a PDF document or image using Marker.

OUTPUT-DIR is the output directory. If not specified, will be the same
directory as INPUT-PATH. Note that Marker stores the output in another dir
within this directory, named after the file. With MOVE-CONTENT-OUT set
to non-nil, the directory contents will be moved up to be in OUTPUT-DIR.

NEW-NAME if non-nil can be a string to rename the output file. Marker
does not have an option for this so it is done here. "
  (cl-assert (and input-path output-format)
             nil "Required args: INPUT-PATH OUTPUT-FORMAT")
  (cl-assert (and org-ilm-convert-marker-path (file-executable-p org-ilm-convert-marker-path))
             nil "Marker executable not available. See org-ilm-convert-marker-path.")
  (cl-assert (member output-format '("markdown" "json" "html"))
             nil "OUTPUT-FORMAT must be one of [markdown|json|html]")
  (let* ((input-path (expand-file-name input-path))
         (input-base-name (file-name-base input-path))
         ;; for rename-file, NEWNAME recognized as dir if ends in slash
         (output-dir (if output-dir
                         (expand-file-name (file-name-as-directory output-dir))
                       (file-name-directory input-path)))
         (output-dir-dir (file-name-concat output-dir (file-name-base input-path)))
         (output-path (expand-file-name
                       (concat (or new-name input-base-name) "."
                               (pcase output-format
                                 ("markdown" "md")
                                 (_ output-format)))
                       (if move-content-out output-dir output-dir-dir))))
    (apply
     #'org-ilm--convert-make-job
     run-p
     :type 'marker
     :method :cli
     :data (list :output-format output-format
                 :output-path output-path)
     :payload
     (append
      (list
       org-ilm-convert-marker-path
       input-path
       "--output_format" output-format
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
     (lambda (job)
       ;; Renaming the files is done by replacing the input base file
       ;; name in each otuput folder file name, but only if the file
       ;; name starts with it. This is to hit less false positives if
       ;; the base file name is small, which won't be the case i think
       ;; since org-ilm-convert works with org-ids all the time.

       ;; TODO Works ok but I think a better approach would be to create
       ;; a symlink with the new name and point marker to that. Then
       ;; delete symlink afterwards.
       (when (or move-content-out new-name)
         (dolist (file (directory-files
                        output-dir-dir 'abs-file-name
                        ;; Appearently this regex is needed otherwise returns
                        ;; "." and ".." as files lol
                        directory-files-no-dot-files-regexp))
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
         (when move-content-out (delete-directory output-dir-dir)))
       (when-let ((on-success (plist-get job-args :on-success)))
         (funcall on-success job)))
     job-args)))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'marker)) run-p &rest args)
  (apply #'org-ilm--convert-make-marker run-p args))

(cl-defmethod org-ilm--convert-make-converter-in-chain
  ((type (eql 'pandoc)) (from-type (eql 'marker)) marker-job args)
  (map-let (:output-path :output-format) (oref marker-job data)
    (setf (plist-get args :input-path) output-path
          (plist-get args :input-format) output-format))
  (cl-call-next-method type from-type marker-job args))

(when nil
  (org-ilm--convert-make-marker
   'run
   :input-path "~/ilm/test/.ilm/attach/e32bc7d8-c776-4b98-9b30-157f43745521/e32bc7d8-c776-4b98-9b30-157f43745521.pdf"
   :output-dir "~/tmp/marker/"
   :output-format "markdown"
   :pages 33
   :job-args
   (list
    )
   )

  (org-ilm--convert-make-converter-chain
   :run-p t
   :on-success (lambda (j) (message "WOWOWOWO"))
   :converters
   '((marker
      :input-path "~/ilm/test/.ilm/attach/e32bc7d8-c776-4b98-9b30-157f43745521/e32bc7d8-c776-4b98-9b30-157f43745521.pdf"
      :output-dir "~/tmp/marker/"
      :output-format "markdown"
      :pages 33
      )
     (pandoc
      :output-path "~/tmp/marker/"
      :output-format "org"
      ))
   )
)


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

(cl-defun org-ilm--convert-make-monolith
    (run-p
     &key
     job-args
     input output-path output-dir)
  "Convert a web URL or local HTML file to a single HTML file using Monolith."
  (unless (and org-ilm-convert-monolith-path
               (file-executable-p org-ilm-convert-monolith-path))
    (user-error "Monolith executable not available. See org-ilm-convert-monolith-path."))

  (let ((job-id (or (plist-get job-args :id) (org-id-new)))
        (input-is-file (cond
                        ((file-exists-p input)
                         (setq input (expand-file-name input))
                         t)
                        ((org-url-p input) nil)
                        (t (error "INPUT must be file or URL, got %s" input)))))

    (if output-path
        (setq output-path (expand-file-name output-path)
              output-dir (file-name-directory output-path))
      (setq output-path (expand-file-name
                         (format "%s.html"
                                 (if input-is-file
                                     (file-name-base input)
                                   job-id))
                         (or output-dir
                             (when input-is-file (file-name-directory input))
                             temporary-file-directory))))

    (apply
     #'org-ilm--convert-make-job
     run-p
     :type 'monolith
     :method :cli
     :id job-id
     :data (list :output-format "html"
                 :output-path output-path)
     :payload
     (append 
      (list
       org-ilm-convert-monolith-path
       input
       "-o" output-path)
      org-ilm-convert-monolith-args)
     job-args)))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'monolith)) run-p &rest args)
  (apply #'org-ilm--convert-make-monolith run-p args))

(cl-defmethod org-ilm--convert-make-converter-in-chain
  ((type (eql 'pandoc)) (from-type (eql 'monolith)) monolith-job args)
  (map-let (:output-path :output-format) (oref monolith-job data)
    (setf (plist-get args :input-path) output-path
          (plist-get args :input-format) output-format))
  (cl-call-next-method type from-type monolith-job args))

(cl-defmethod org-ilm--convert-make-converter-in-chain
  ((type (eql 'defuddle)) (from-type (eql 'monolith)) monolith-job args)
  (map-let (:output-path) (oref monolith-job data)
    (setf (plist-get args :input) output-path))
  (cl-call-next-method type from-type monolith-job args))

(when nil
  (org-ilm--convert-make-monolith
   'run-p
   :input "https://en.wikipedia.org/wiki/Log-normal_distribution"
   :output-dir "~/tmp/marker/"
   )
  
  (org-ilm--convert-make-converter-chain
   :run-p t
   :on-success (lambda (j) (notifications-notify :title "wow"))
   :on-error (lambda (j) (notifications-notify :title "nonnonono"))
   :converters
   '((monolith
      :input "https://en.wikipedia.org/wiki/Log-normal_distribution"
      ;; :input "https://example.com/"
      :output-dir "~/tmp/marker/")
     (defuddle
      :output-format "markdown"
      )
     (pandoc
      ;; :output-path "~/tmp/wowoow.org"
      :output-format "org"
      ))
   )
  )


;;;; Docling

(defcustom org-ilm-convert-docling-path (executable-find "docling")
  "Path to the docling executable."
  :type 'file
  :group 'org-ilm-convert-docling)

(defcustom org-ilm-convert-docling-args nil
  "Arguments to always pass to docling."
  :type '(repeat string)
  :group 'org-ilm-convert-docling)

(cl-defun org-ilm--convert-make-docling
    (run-p
     &key
     job-args
     input-path input-format
     output-format output-dir
     (image-export-mode "embedded") ; docling default
     (ocr-p t) ; default: on
     (ocr-engine "auto") ; default: auto
     ocr-lang ; comma seperated, depends on engine
     enrich-code ; default: off
     enrich-formula ; default: off
     enrich-picture-description ; default: off
     artifacts-path
     (allow-external-plugins t)
     (page-batch-size 4) ; default: 4
     flags)
  "Convert a file using Docling."
  (cl-assert (member image-export-mode '(nil "placeholder" "embedded" "referenced")))
  (cl-assert (member ocr-engine '(nil "auto" "easyocr" "ocrmac" "rapidocr" "tesserocr" "tesseract" "onnxtr")))
  (let* ((input-path (expand-file-name input-path))
         (output-dir (or output-dir (file-name-directory input-path)))
         (output-path (expand-file-name
                       (concat (file-name-base input-path) "." output-format)
                       output-dir)))
    (apply
     #'org-ilm--convert-make-job
     run-p
     :type 'docling
     :method :cli
     :data (list :output-format output-format
                 :output-path output-path)
     :payload
     (append
      (list
       org-ilm-convert-docling-path
       input-path
       "--to" output-format
       "--output" output-dir
       "--image-export-mode" image-export-mode
       (if enrich-code "--enrich-code" "--no-enrich-code")
       (if enrich-formula "--enrich-formula" "--no-enrich-formula")
       (if enrich-picture-description
           "--enrich-picture-description"
         "--no-enrich-picture-description")
       (if ocr-p "--ocr" "--no-ocr")
       "--ocr-engine" ocr-engine
       "--page-batch-size" (format "%s" page-batch-size)
       )
      (when ocr-lang
        `("--ocr-lang" ,ocr-lang))
      (when input-format
        `("--from" ,input-format))
      (when artifacts-path
        `("--artifacts-path" ,artifacts-path))
      (when allow-external-plugins
        '("--allow-external-plugins")
        )
      flags
      org-ilm-convert-docling-args)
     job-args)))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'docling)) run-p &rest args)
  (apply #'org-ilm--convert-make-docling run-p args))

(cl-defmethod org-ilm--convert-make-converter-in-chain
  ((type (eql 'docling)) (from-type (eql 'qpdf)) qpdf-job args)
  (map-let (:output-path) (oref qpdf-job data)
    (setf (plist-get args :input-path) output-path))
  (cl-call-next-method type from-type qpdf-job args))

(cl-defmethod org-ilm--convert-make-converter-in-chain
  ((type (eql 'pandoc)) (from-type (eql 'docling)) docling-job args)
  (map-let (:output-path :output-format) (oref docling-job data)
    (setf (plist-get args :input-path) output-path
          (plist-get args :input-format) output-format))
  (cl-call-next-method type from-type docling-job args))

(when nil
  (org-ilm--convert-make-docling
   'run
   :input-path "~/tmp/docling/192.png"
   :output-format "html"
   :ocr-engine "onnxtr"
   :ocr-lang "nl"
   :artifacts-path "/home/mochar/.cache/docling/models"
   )

  (org-ilm--convert-make-converter-chain
   :run-p t
   :on-success (lambda (j) (message "WOWOWOWO"))
   :converters
   `((qpdf
      :input-path "~/tmp/docling/a.pdf"
      :output-name "a_part"
      :pages-spec "2,3"
      )
     (docling
      :output-format "html"
      :ocr-engine "onnxtr"
      :artifacts-path "/home/mochar/.cache/docling/models"
      :enrich-formula t
      :enrich-code t
      :page-batch-size 1
      ))
   )

  (org-ilm--convert-make-converter-chain
   :run-p t
   :on-success (lambda (j) (message "WOWOWOWO"))
   :converters
   `((docling
      :input-path "~/tmp/docling/math.png"
      :output-format "md"
      :artifacts-path "/home/mochar/.cache/docling/models"
      :enrich-formula t)
     (pandoc
      :output-format "org"
      )
   ))
  )



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

(defun org-ilm-convert--ytdlp-filename-from-url (url &optional template restrict-p format)
  "Get the filename that will be generated for URL and TEMPLATE.

See: https://github.com/yt-dlp/yt-dlp?tab=readme-ov-file#output-template-examples"
  (-some->
   (ignore-errors
     (apply #'process-lines
            (append
             '("yt-dlp" "--print" "filename")
             (when template (list "-o" template))
             (when format (list "-f" format))
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

(defun org-ilm-convert--ytdlp-read-filename (path)
  (car (string-split (string-trim (f-read path)))))

(cl-defun org-ilm--convert-make-ytdlp
    (run-p
     &key
     job-args
     url output-dir output-path output-format
     filename-path
     filename-template sub-langs
     audio-only-p no-download)
  "Download media from url using yt-dlp.

SUB-LANGS may also be 'all' to download all subtitles.

FILENAME-PATH should be a txt file where the last non-empty line
contains the filename. When passed, OUTPUT-DIR must be specified as
well.

OUTPUT-FORMAT is a format expression. Can be a file extension (currently
3gp, aac, flv, m4a, mp3, mp4, ogg, wav, webm are supported). See
https://github.com/yt-dlp/yt-dlp?tab=readme-ov-file#format-selection. When
OUTPUT-PATH, FILENAME-PATH, or FILENAME-TEMPLATE with explicit extension
specified, it overthrows this option."
  (unless (and url (or (xor output-path output-dir) sub-langs))
    (error "Required args: URL [OUTPUT-DIR|OUTPUT-PATH]"))
  (unless (and org-ilm-convert-ytdlp-path
               (file-executable-p org-ilm-convert-ytdlp-path))
    (user-error "The yt-dlp executable not available. See org-ilm-convert-ytdlp-path."))
  (when filename-path
    (unless output-dir
      (error "Must specify OUTPUT-DIR when FILENAME-PATH is given")))

  (setq sub-langs (ensure-list sub-langs)) 

  (apply
   #'org-ilm--convert-make-job
   run-p
   :type 'ytdlp
   :method :cli
   :payload
   (lambda ()
     (cond
      (filename-path
       (setq output-path
             (expand-file-name
              (org-ilm-convert--ytdlp-read-filename filename-path)
              output-dir)))
      ((and (not output-path) output-dir)
       (setq output-path (expand-file-name
                          (org-ilm-convert--ytdlp-filename-from-url
                           url filename-template 'restrict output-format)
                          output-dir))))
     (append
      (list
       org-ilm-convert-ytdlp-path
       url
       "--embed-chapters"
       "--newline" ; Output progress bar as new lines
       )
      org-ilm-convert-ytdlp-args
      (cond
       (output-path (list "-o" output-path))
       (filename-template (list  "-o" filename-template "--no-download"))
       (t (list "--no-download")))
      (when output-format `("--format" ,output-format))
      (when audio-only-p '("-x"))
      (when no-download '("--no-download"))
      (when sub-langs
        (append '("--write-sub" "--sub-lang") (list (string-join sub-langs ","))))
      ))
   job-args))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'ytdlp)) run-p &rest args)
  (apply #'org-ilm--convert-make-ytdlp run-p args))

(cl-defmethod org-ilm--convert-make-converter ((type (eql 'ytdlp-filename)) run-p &rest args)
  "Get the filename that will be generated for URL and TEMPLATE.

See: https://github.com/yt-dlp/yt-dlp?tab=readme-ov-file#output-template-examples"
  (map-let (:job-args :url :template :restrict-p :format :output-path) args
    (if output-path
        (setq output-path (expand-file-name output-path))
      (setq output-path (make-temp-file "" nil ".txt")))
    (apply
     #'org-ilm--convert-make-job
     run-p
     :type type
     :method :cli
     :data (list :url url :template template
                 :format format :output-path output-path)
     :payload
     (append
      `("yt-dlp" "--print-to-file" "filename" ,output-path)
      (when template (list "-o" template))
      (when format (list "-f" format))
      `(,url)
      (when restrict-p '("--restrict-filenames"))
      org-ilm-convert-ytdlp-args
      '("--no-warnings"))
     job-args)))

(cl-defmethod org-ilm--convert-make-converter-in-chain
  ((type (eql 'ytdlp)) (from-type (eql 'ytdlp-filename)) from-job args)
  (map-let (:output-path :format :template :url) (oref from-job data)
    (setf (plist-get args :filename-path) output-path
          (plist-get args :url) url
          (plist-get args :output-format) format
          (plist-get args :filename-template) template))
  (cl-call-next-method type from-type from-job args))

(when nil
  (org-ilm--convert-make-ytdlp
   :run-p
   :url "https://www.youtube.com/shorts/gqQ_6zcz-i8"
   ;; :output-dir "~/tmp/marker/"
   :output-path "~/tmp/marker/a.webm"
   )
  (org-ilm--convert-make-ytdlp
   :run-p
   :url "https://www.youtube.com/shorts/gqQ_6zcz-i8"
   :output-dir "~/tmp/marker/"
   ;; :output-format "m4a"
   ;; :output-path "~/tmp/marker/a.webm"
   :filename-template "wow.webm"
   )
  (org-ilm--convert-make-converter-chain
   :run-p t
   :on-success (lambda (j) (notifications-notify :title "wow"))
   :on-error (lambda (j) (notifications-notify :title "nonnonono"))
   :converters
   `((ytdlp-filename
      :url "https://www.youtube.com/shorts/gqQ_6zcz-i8"
      :template "%(title)s.%(ext)s"
      :restrict-p t
      :format "m4a"
      )
     (ytdlp
      :output-dir "~/tmp/marker/"
      )
     )
   )
)

;;;; Org transient

;; Convert using metadata in Org heading properties and attachments.

;;;;; Webpage group

(transient-define-group org-ilm--convert-webpage-transient-group
  ["Webpage download"
   (:info* (lambda () "Download a webpage and convert to Org mode"))
   ("wd" "Download"
    :cons 'webpage-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ("wf" "Full snapshot"
    :cons 'webpage-full
    :class org-ilm-transient-cons-switch
    :transient transient--do-call
    :if (lambda () (alist-get 'webpage-download (org-ilm--transient-parse))))
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

(defun org-ilm--convert-transient-webpage-run (source title output-dir id &optional transient-args)
  (let* ((transient-args (or transient-args (org-ilm--transient-parse)))
         (download (alist-get 'webpage-download transient-args))
         (full (alist-get 'webpage-full transient-args))
         (simplify (alist-get 'webpage-simplify transient-args))
         (orgify (alist-get 'webpage-orgify transient-args))
         (html-path (expand-file-name (concat title ".html") output-dir))
         (on-success
          (lambda (job)
            (message "[Org-Ilm-Convert] HTML conversion completed: %s" source)
            (org-ilm--org-with-point-at
                (oref (oref job chain) id)
              (org-attach-sync))))
         converters)

    (if (not full)
        ;; When not downloading a full snapshot, download the page instead with
        ;; defuddle.
        (push
         (list 'defuddle
               :input source
               :output-folder output-dir
               :output-name title
               :output-format simplify)
         converters)

      (push
       (list 'monolith :input source :output-path html-path)
       converters)

      (when simplify
        (push
         (list 'defuddle :output-format simplify)
         converters)))

    (when orgify
      (push
       (list 'pandoc :output-format "org")
       converters))

    (setq converters (reverse converters))
    
    (org-ilm--convert-make-converter-chain
     :run-p t
     :id id
     :converters converters
     :on-success on-success)))

;;;;; Media group

(transient-define-group org-ilm--convert-media-transient-group
  ["Media download"
   ("md" "Download"
    :cons 'media-download
    :class org-ilm-transient-cons-switch
    :transient transient--do-call)
   ("ma" "Audio only"
    :cons 'media-audio
    :class org-ilm-transient-cons-switch
    :transient transient--do-call
    :if (lambda () (alist-get 'media-download (org-ilm--transient-parse))))
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
   ("mt" "Template"
    :cons 'media-template
    :class org-ilm-transient-cons-option
    :prompt "Template: "
    :transient transient--do-call
    :if (lambda ()
          (map-let (media-download media-subs) (org-ilm--transient-parse)
            (or media-download media-subs))))
   ]
  )

(defun org-ilm--convert-transient-media-run (url output-dir id set-media-prop &optional transient-args title)
  (map-let (media-download media-subs media-template media-subs media-audio)
      (or transient-args (org-ilm--transient-parse))
    (when (or media-download media-subs)
      (when title (setq media-template (concat title "." "%(ext)s")))
      (org-ilm--convert-make-converter-chain
       :run-p t
       :converters
       `((ytdlp-filename
          :url ,url
          :template ,media-template
          :restrict-p t
          :job-args
          (:on-success
           ,(when set-media-prop
              (lambda (job)
                (if-let* ((filename-path (plist-get (oref job data) :output-path))
                          (filename (org-ilm-convert--ytdlp-read-filename
                                     filename-path)))
                    (progn
                      (org-ilm--org-with-point-at id
                        (org-entry-put nil org-ilm-property-media filename))
                      (save-buffer))
                  (warn "Ilm media filename not found")))))
          )
         (ytdlp
          :output-dir ,output-dir
          ;; TODO This wont be handled correctly when we depend on ytdlp-filename
          :audio-only-p ,media-audio
          :sub-langs ,media-subs
          :no-download ,(not media-download)
          )
         )
       :id id
       :on-success
       (lambda (job)
         (message "[Org-Ilm-Convert] Media download completed: %s" url)
         (org-ilm--org-with-point-at (oref (oref job chain) id) (org-attach-sync)))))))

;;;;; PDF group

(transient-define-group org-ilm--convert-pdf-transient-group
  ["PDF conversion"
   :if (lambda () (and (eq (oref (transient-scope) type) 'attachment)
                       (s-ends-with-p "pdf" (oref (transient-scope) source))))
   (:info* (lambda () "Convert a PDF document to Markdown or Org mode"))
   ("pc" "Convert"
    :cons 'pdf-convert
    :transient t
    :class org-ilm-transient-cons-switch)
   ("pp" "Pages"
    :cons 'pdf-pages
    :transient t
    :class org-ilm-transient-cons-option
    :if (lambda () (alist-get 'pdf-convert (org-ilm--transient-parse))))
   ("pt" "Tool"
    :cons 'pdf-tool
    :transient t
    :class org-ilm-transient-cons-switches
    :choices (docling marker)
    :allow-empty nil
    :if (lambda () (alist-get 'pdf-convert (org-ilm--transient-parse))))
]
  [""
   :description (lambda () (propertize "Docling options" 'face 'default))
   :hide (lambda ()
           (map-let (pdf-convert pdf-tool) (org-ilm--transient-parse)
             (not (and pdf-convert (eq pdf-tool 'docling)))))
   ("po" "OCR engine"
    :cons 'pdf-ocr-engine
    :class org-ilm-transient-cons-switches
    :transient t
    :choices ("onnxtr" "auto")
    :allow-empty t)
   ("pl" "OCR language"
    :cons 'pdf-ocr-lang
    :class org-ilm-transient-cons-option
    :transient t
    :allow-empty t)
   ("pC" "Enrich code"
    :cons 'pdf-enrich-code
    :class org-ilm-transient-cons-switch
    :transient t)
   ("pf" "Enrich formula"
    :cons 'pdf-enrich-formula
    :class org-ilm-transient-cons-switch
    :transient t)
   ("pb" "Page batch size"
    :cons 'pdf-page-batch
    :class org-ilm-transient-cons-option
    :transient t
    :always-read t
    :allow-empty nil)
   ]
  )

(defun org-ilm--convert-transient-pdf-run (path id main-p &optional transient-args)
  (pcase-let* ((transient-args (or transient-args (org-ilm--transient-parse)))
               ((map pdf-pages pdf-tool) transient-args)
               (name (if main-p id
                       (concat (file-name-base path) " " (ts-format (ts-now)))))
               (pdf-pages (when (and pdf-pages (not (string-empty-p pdf-pages)))
                            pdf-pages))
               (converters))

    (when pdf-pages
      (push
       `(qpdf
         :input-path ,path
         :pages-spec ,pdf-pages
         :output-name ,name)
       converters))

    (push
     (pcase pdf-tool
       ('marker
        `(marker
          :input-path ,(unless pdf-pages path)
          :output-format "markdown"
          ))
       ('docling
        (map-let (pdf-ocr-engine pdf-ocr-lang pdf-enrich-code pdf-enrich-formula pdf-page-batch) transient-args
          `(docling
            :input-path ,(unless pdf-pages path)
            :output-format "md"
            :ocr-p ,(when pdf-ocr-engine t)
            :ocr-engine ,(or pdf-ocr-engine "auto")
            :enrich-code ,pdf-enrich-code
            :enrich-formula ,pdf-enrich-formula
            :page-batch-size ,(if (and pdf-page-batch (not (string-empty-p pdf-page-batch)))
                                  (let ((bs (string-to-number pdf-page-batch)))
                                    (if (= bs 0) 4 bs)))
            ))))
     converters)

    (push '(pandoc :output-format "org") converters)
    
    (org-ilm--convert-make-converter-chain
     :run-p t
     :id id
     :converters (reverse converters)
     :on-success
     (lambda (job)
       (dolist (ext (if main-p '("md") '("pdf" "md")))
         (delete-file (expand-file-name
                       (concat name "." ext)
                       (file-name-directory path))))
       (when main-p
         (org-ilm--org-with-point-at (oref (oref job chain) id)
           (org-entry-put nil org-ilm-property-ext "org")))))))

   
;;;;; Main transient

(cl-defstruct (org-ilm-convert-menu-data
               (:conc-name org-ilm-convert-menu-data--))
  id type title source defaults)
               
(defun org-ilm-convert-menu (&optional id)
  (interactive)
  (org-ilm--org-with-point-at id
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
          (when (eq type 'attachment)
            (setq source (expand-file-name source attach-dir)))
          (org-ilm--convert-transient
           (make-org-ilm-convert-menu-data
            :id (org-mem-entry-id entry)
            :title (org-mem-entry-title entry)
            :source source
            :type type
            :defaults
            (append
             (when (and (eq type 'attachment)
                        (string= (file-name-extension source) "pdf"))
               '((pdf-convert . t)))
             (when-let* ((props (org-mem-entry-properties entry))
                         (pdf-ranges (map-elt props "ILM_PDF"))
                         (spec (org-ilm--pdf-spec-parse pdf-ranges))
                         (range (org-ilm-pdf-spec--range spec)))
               (if (= (car range) (cdr range))
                   `((pdf-pages . ,(car range)))
                 `((pdf-pages . ,(format "%s-%s" (car range) (cdr range))))))
            ))))))))

(transient-define-prefix org-ilm--convert-transient (data)
  :refresh-suffixes t
  ;; :incompatible '((media-download webpage-download))
  
  ["Convtool"
   (:info*
    (lambda ()
      (propertize (oref (transient-scope) title) 'face 'italic))
    :if (lambda () (oref (transient-scope) title)))
   (:info*
    (lambda ()
      (propertize (oref (transient-scope) source) 'face 'transient-value))
    :if (lambda () (oref (transient-scope) source)))
   ]

  [:if (lambda () (eq (oref (transient-scope) type) 'url))
       org-ilm--convert-webpage-transient-group
       ]
  
  [:if (lambda () (eq (oref (transient-scope) type) 'url))
       org-ilm--convert-media-transient-group
       ]

  org-ilm--convert-pdf-transient-group

  [
   :if (lambda ()
         (map-let (media-download webpage-download pdf-convert) (org-ilm--transient-parse)
           (or media-download webpage-download pdf-convert)))
   ("a" "Main attachment"
    :cons 'main-attachment
    :class org-ilm-transient-cons-switches
    :allow-empty t
    :choices
    (lambda ()
      (map-let (media-download webpage-download pdf-convert) (org-ilm--transient-parse)
        (let (options)
          (when media-download (push 'media options))
          (when webpage-download (push 'webpage options))
          (when pdf-convert (push 'pdf options))
          options)))
    )
   ]

  ["Actions"
   ("RET" "Convert"
    (lambda ()
      (interactive)
      (org-ilm--org-with-point-at (oref (transient-scope) id)
        (pcase-let* ((args (org-ilm--transient-parse)) 
                     ((map webpage-download media-download media-subs pdf-convert pdf-pages main-attachment) args)
                     (data (transient-scope))
                     (source (oref data source))
                     (type (oref data type))
                     (org-id (org-id-get))
                     (attach-dir (org-attach-dir-get-create)))

          ;; Webpage
          (when webpage-download
            (make-thread
             (lambda ()
               (let ((title (if (eq main-attachment 'webpage)
                                org-id
                              (org-ilm--get-page-title source 'slugify))))
                 (org-ilm--convert-transient-webpage-run
                  source title attach-dir org-id args)))))

          ;; Media
          (when (or media-download media-subs)
            (org-ilm--convert-transient-media-run
             source attach-dir org-id (eq main-attachment 'media) args))

          ;; PDF
          (when pdf-convert
            (org-ilm--convert-transient-pdf-run
             source org-id (eq main-attachment 'pdf) args))
          )))
    :inapt-if
    (lambda ()
      (with-slots (type) (transient-scope)
        (pcase-let (((map webpage-download media-download media-subs)
                     (org-ilm--transient-parse)))
          (and (eq type 'url)
               (not webpage-download)
               (not media-download)
               (not media-subs))))))
   ]

  (interactive "P")
  (transient-setup
   'org-ilm--convert-transient nil nil
   :scope data 
   :value
   (lambda ()
     (with-slots (title source type defaults) data
       (map-merge
        'alist
        `((webpage-simplify . "markdown")
          (webpage-orgify . t)
          (media-template . "%(title)s.%(ext)s")
          (pdf-tool . docling)
          (pdf-page-batch . "4"))
        defaults)))))


;;;; Footer

(provide 'org-ilm-convert)

;;; org-ilm-convert.el ends here
