;;; org-ilm-media.el --- Media attachments -*- lexical-binding: t; -*-

;;; Commentary:

;; Media attachments are actually just org attachments with an ILM_MEDIA
;; property. This property consists of a source, and optional start and end
;; timestamps. The source can be a URL, a path, or simply a filename in which
;; case the media file has to be within the element's attachment directory.

;; A custom "ilmmedia" org link type is used to jump to the timestamp/range
;; using MPV. Only the timestamp(s) are stored in the url parameters: the source
;; is determined from the ILM_MEDIA property.

;; Much of the functionality relies on org-media-note, but the plan is to
;; implement them myself. Mostly because I need to advice and hack around
;; org-media-note a lot.

;;; Code:

;;;; Requirements

(require 'cond-let)
(require 'org-attach)
(require 'org-timer) ; for org-timer-re
(require 'org-media-note)
(require 'org-node)
(require 'mpv)
(require 'url)

(require 'org-ilm-utils)
(require 'org-ilm-attachment)
(require 'org-ilm-convert)
(require 'org-ilm-transient)

;;;; Variables

(defconst org-ilm-property-media "ILM_MEDIA")
(defconst org-ilm-property-media+ "ILM_MEDIA+")
(defconst org-ilm-media-link "ilmmedia")
(defvar org-ilm-media-audio-extensions '("flac" "mp3" "wav" "m4a" "aac" "opus"))
(defvar org-ilm-media-video-extensions '("avi" "rmvb" "ogg" "ogv" "mp4" "mkv" "mov" "webm" "flv" "ts" "mpg"))

;;;; Functions

(defun org-ilm--media-file-p (path)
  "Return t if PATH is a video or audio file."
  (when (and (file-exists-p path)
             (or
              (member (file-name-extension path) org-ilm-media-audio-extensions)
              (member (file-name-extension path) org-ilm-media-video-extensions)))
    t))

(defun org-ilm--media-url-p (url)
  ;; Determine if media type by attempting to extract filename from url
  ;; using yt-dlp. If error thrown, yt-dlp failed to extract metadata ->
  ;; not media type.
  ;; NOTE Quite slow (~3 sec)
  ;; (condition-case err
  ;;     (or (org-ilm-convert--ytdlp-filename-from-url url) t)
  ;;   (error nil))

  ;; Instead just check if its a youtube link for now
  (member (url-domain (url-generic-parse-url url))
          '("youtube.com" "youtu.be")))

(defun org-ilm--source-recover (source element-id &optional registry-id)
  "Return path to media attachment or its url."
  (cond-let*
    ((org-url-p source) source)
    ((file-exists-p source) source)
    ([attach-dir (seq-some
                  (lambda (id)
                    (when id 
                      (org-ilm--org-with-point-at id
                        (when-let ((attach-dir (org-attach-dir)))
                          (when (member source (org-attach-file-list attach-dir))
                            (expand-file-name attach-dir))))))
                  (list element-id registry-id))]
     (expand-file-name source attach-dir))))

(defun org-ilm--media-prop-parse (media)
  "Parse the ILM_MEDIA property MEDIA and return (source start end)."
  (org-media-note--split-link media))

(defun org-ilm--media-compile (entry)
  "Return (source start end)"
  (let ((media (org-mem-entry-property-with-inheritance org-ilm-property-media entry))
        (media2 (org-mem-entry-property-with-inheritance org-ilm-property-media+ entry)))
    (when media
      (cl-destructuring-bind (source start end) (org-ilm--media-prop-parse media)
        (cl-destructuring-bind (start2 end2) (or (ignore-errors (split-string media2 "-"))
                                                 '(nil nil))
          (setq start (or start2 start)
                end (if start2 end2 end))
          (list source start end))))))

(defun org-ilm--media-open (&optional entry)
  "Open the media of ENTRY (or at point) with `org-media-note'."
  (when-let* ((entry (or entry (org-node-at-point)))
              (media (org-ilm--media-compile entry)))
    (cl-destructuring-bind (source start end) media
      (setq source (org-ilm--source-recover
                    source (org-mem-entry-id entry)
                    (org-mem-entry-property-with-inheritance "REGISTRY" entry)))
      (org-media-note--follow-link source start end))))

;; TODO too fragile. should at the very least check in org-media-note org links
(defun org-ilm--media-extract-range (text)
  "Extract the first and last timers in TEXT as start and end points."
  (let (start end)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (re-search-forward org-timer-re nil t)
        (setq start (match-string-no-properties 0))
        (delete-region (point-min) (point)))
      (goto-char (point-max))
      (when (re-search-backward org-timer-re nil t)
        (setq end (match-string-no-properties 0))))
    (when start (if end (concat start "-" end) start))))

(defun org-ilm--media-subtitles-from-dir ()
  "Return subtitle file contents from current dir."
  (when-let ((files (directory-files default-directory nil "\\.\\(srt\\|ass\\|vtt\\)\\'"))
             (file (cdr
                    (org-ilm--select-alist
                     (mapcar
                      (lambda (f)
                        (cons (cl-subseq (string-split f "\\.") 1) f))
                      files)
                     "File: ")))
             (codec (file-name-extension file)))
    (with-temp-buffer
      (insert-file-contents file)
      (cons codec (buffer-string)))))

(defun org-ilm--media-subtitles-from-media ()
  "Return the subtitle file contents that were detected by MPV for the
currently playing media.

MPV finds local subtitles by matching the video file's name with the
subtitle file's name, while ignoring file extensions and language
codes. This is not terribly useful for us as we can do this part
ourselves for locally installed media and subtitles. For online videos,
MPV can also get the subtitles itself, but it requires manually setting
the ytdlp flags which I don't care to do atm."
  (when-let* ((tracks (seq-keep
                       (lambda (track)
                         ;; TODO `org-media-note--convert-from-subtitle' supports
                         ;; vvts but `org-media-note--is-subtitle-track' excludes
                         ;; them, make PR.
                         ;; (when (org-media-note--is-subtitle-track track)
                         (let ((codec (alist-get 'codec track))
                               (title (alist-get 'title track))
                               (type (alist-get 'type track)))
                           (when (and (string= type "sub")
                                      (or (member codec '("srt" "ass" "subrip" "vtt"))
                                          (member title '("srt" "ass" "vtt"))
                                          (string-suffix-p ".srt" title)
                                          (string-suffix-p ".ass" title)
                                          (string-suffix-p ".vtt" title)))
                             (cons (or (alist-get 'lang track) (alist-get 'title track))
                                   track))))
                       (mpv-get-property "track-list")))
              (choice (completing-read "Track: " (mapcar #'car tracks) nil t))
              (track (cdr (assoc choice tracks)))
              (track-file (alist-get 'external-filename track)))
    ;; TODO From `org-media-note--selected-subtitle-content' it seems
    ;; `external-filename' can contain the subtitle contents? Hence the "else"
    ;; part of the if below. Currently not bothering with it (see docstring this
    ;; func).
    (if (file-exists-p track-file)
        (with-temp-buffer
          (insert-file-contents track-file)
          (cons (alist-get 'codec track) (buffer-string)))
      ;; TODO Make cons (codec . content)
      (let* ((srt-lines (split-string track-file "\n"))
             (processed-srt-lines (cons "1" (cdr srt-lines))))
        (mapconcat 'identity processed-srt-lines "\n")))))

(defun org-ilm--media-insert-subtitles ()
  ;; TODO We can also use `org-ilm--media-subtitles-from-media'???
  (let ((subtitles (org-ilm--media-subtitles-from-dir)))
    (insert (org-media-note--convert-from-subtitle
             (cdr subtitles) "time1-time2"
             org-ilm-media-link ""))))

(defun org-ilm--media-time-to-duration (time type)
  (pcase type
    ('seconds (setq time (/ (float time) 60)))
    ('minutes)
    (_ (error "Unknown value for TYPE")))
  (org-duration-from-minutes time 'h:mm:ss))

(defun org-ilm--media-insert-chapters ()
  (if-let ((chapters (mpv-get-property "chapter-list")))
      (dotimes (i (length chapters))
        (when-let* ((chapter (aref chapters i))
                    (title (or (alist-get 'title chapter) ""))
                    (start (alist-get 'time chapter))
                    (end (if (< (1+ i) (length chapters))
                             (alist-get 'time (aref chapters (1+ i)))
                           (mpv-get-property "duration")))
                    (range (concat
                            (org-ilm--media-time-to-duration start 'seconds)
                            "-"
                            (org-ilm--media-time-to-duration end 'seconds))))
          (insert "- [[ilmmedia:" range "][" range "]] " title "\n")))
    (message "No chapter data available")))

;;;; Custom link

;; Will get the media file name from ILM_MEDIA property so that we only have to
;; store the timestamps in the link.
;; Format:
;; [[ilmmedia:ts(-ts)]] or [[ilmmedia:#ts(-ts)]] to deal with org-media-note #

(defun org-ilm--media-link-folow (link)
  (if-let* ((element-id (car (org-ilm--attachment-data)))
            (entry (org-mem-entry-by-id element-id))
            (source (car (org-ilm--media-compile entry))))
      (pcase-let* ((ts (car (nreverse (string-split link "#"))))
                   (`(,start ,end) (string-split ts "-")))
        ;; (string-match org-timer-re start)
        (org-media-note--follow-link source start end))
    (user-error "Cannot find element or its ILM_MEDIA property")))

(org-link-set-parameters
 org-ilm-media-link
 :follow #'org-ilm--media-link-folow
 )

;;;; Org-media-note advice

;; Advice org-media-note to use our link type when inserting

(defun org-ilm--advice--org-media-note--link (&rest r)
  (pcase-let* ((`(,file-path ,filename ,timestamp) (org-media-note--current-media-info))
               (link-type org-ilm-media-link))
    (if (org-media-note--ab-loop-p)
        ;; ab-loop link
        (let ((time-a (org-media-note--seconds-to-timestamp (mpv-get-property "ab-loop-a")))
              (time-b (org-media-note--seconds-to-timestamp (mpv-get-property "ab-loop-b"))))
          (format "[[%s:%s-%s][%s]]"
                  link-type
                  time-a
                  time-b
                  (org-media-note--link-formatter org-media-note-ab-loop-link-format
                                                  `(("filename" . ,filename)
                                                    ("ab-loop-a" . ,time-a)
                                                    ("ab-loop-b" . ,time-b)
                                                    ("file-path" . ,file-path)))))
      ;; timestamp link
      (format "[[%s:%s][%s]]"
              link-type
              timestamp
              (org-media-note--link-formatter org-media-note-timestamp-link-format
                                              `(("filename" . ,filename)
                                                ("timestamp" . ,timestamp)
                                                ("file-path" . ,file-path)))))))

(defun org-ilm--ilm-hook-media ()
  (if org-ilm-global-minor-mode
      (advice-add #'org-media-note--link
                  :override #'org-ilm--advice--org-media-note--link)
    (advice-remove #'org-media-note--link
                   #'org-ilm--advice--org-media-note--link)))

(add-hook 'org-ilm-global-minor-mode-hook #'org-ilm--ilm-hook-media)

;;;; Conversion transient

;; TODO Decide what to do here. The idea is to have reusable transient for media
;; conversion but it ends up messy due to contexts being to different (different
;; object in transient-scope, etc).

;; (defun org-ilm-convert--transient-media-build (&optional no-template)
;;   (let ((inapt-if
;;          (lambda ()
;;            (null (transient-arg-value "--media-download" (transient-get-value)))))
;;         suffixes)
        
;;     (push org-ilm-convert--transient-media-subs-suffix suffixes)
;;     (push (append
;;            org-ilm-convert--transient-media-audio-suffix
;;            (list :inapt-if inapt-if)) suffixes)
;;     (push (append
;;            org-ilm-convert--transient-media-template-suffix
;;            (if no-template
;;                (list :inapt-if-nil nil)
;;            (list :inapt-if inapt-if)))
;;           suffixes)
;;     (push org-ilm-convert--transient-media-download-suffix suffixes)
    
;;     (mapcar
;;      (lambda (suffix)
;;        (transient-parse-suffix 'transient--prefix suffix))
;;      suffixes)))

;; (transient-define-group org-ilm--media-convert-group
;;   ""
;;   ["Media"
;;    ("md" "Download"
;;     :cons 'media-download
;;     :class org-ilm-transient-cons-switch
;;     :transient transient--do-call)
;;    ("mt" "Template"
;;     :cons 'media-template
;;     :class org-ilm-transient-cons-option
;;     :prompt "Template: "
;;     :transient transient--do-call)
;;    ("ma" "Audio only"
;;     :cons 'media-audio
;;     :class org-ilm-transient-cons-switch
;;     :transient transient--do-call)
;;    ("ms" "Subtitles download"
;;     :cons 'media-subs
;;     :class org-ilm-transient-cons-option
;;     :transient transient--do-call
;;     :multi-value rest
;;     :choices
;;     (lambda ()
;;       (let* ((url (transient-arg-value "--source=" (transient-args transient-current-command)))
;;              (subs (org-ilm-convert--ytdlp-subtitles-from-url url)))
;;         (mapcar (lambda (x) (alist-get 'language x)) (alist-get 'subtitles subs)))))
;;    ])

;; (defun org-ilm--media-convert-transient-run (url output-dir id transient-args &optional on-success)
;;   "Convert a media URL to a local video file."
;;   (map-let (media-download media-template media-audio media-subs sub-langs)
;;       transient-args
;;     (org-ilm-convert--convert-with-ytdlp
;;      :process-id id
;;      :url url
;;      :output-dir output-dir
;;      :filename-template template
;;      :audio-only-p audio-only
;;      :sub-langs sub-langs
;;      :no-download (not download)
;;      :on-success
;;      (lambda (proc buf id output-path)
;;        (message "[Org-Ilm-Convert] Media conversion completed: %s" url)
;;        (org-ilm--org-with-point-at id
;;          (org-attach-sync)
;;          (when on-success
;;            (funcall on-success id output-path)))))))

;;; Footer

(provide 'org-ilm-media)

;;; org-ilm-media.el ends here
