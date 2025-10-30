;;; mochar-utils.el --- Some utility functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: M Charrout
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: 

;;; Commentary:

;; Some shared utilities.

;;; Code:

(require 'org-mem)
(require 'url)
(require 'org-id)

;;;; Elisp

(cl-defun mochar-utils--alist-to-plist (alist &key upcase remove)
  "Convert association list ALIST into the equivalent property-list form.

Direct copy from mm-decode.el"
  (let (plist)
    (while alist
      (let ((el (car alist)))
        (unless (member (car el) remove)
	  (setq plist (cons (cdr el)
                            (cons (if upcase (upcase (car el)) (car el))
                                  plist)))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(defun mochar-utils--list-shuffle (list)
  "Return a new list with the elements of LIST randomly shuffled."
  (let ((vec (vconcat list)))       ; convert to vector for easy swapping
    (cl-loop for i from (1- (length vec)) downto 1
             do (cl-rotatef (aref vec i)
                            (aref vec (random (1+ i)))))
    (append vec nil)))

;;;; Emacs

(cl-defun mochar-utils--add-hook-once (hook function &optional depth (local t))
  "Add FUNCTION to HOOK and remove it after its first execution.
If HOOK passes arguments, FUNCTION will receive them.

HOOK is the hook to which FUNCTION will be added.
FUNCTION is the function to run once in the hook.
DEPTH controls where FUNCTION is placed in HOOK and defaults to 0.
LOCAL controls whether to add HOOK buffer-locally and defaults to t.

The returned function can be used to call `remove-hook' if needed."
  (letrec ((wrapper (lambda (&rest args)
                      (remove-hook hook wrapper local)
                      (apply function args))))
    (add-hook hook wrapper depth local)
    wrapper))

;;;; String
(defun mochar-utils--slugify-title (title)
  "From TITLE, make a filename slug meant to look nice as URL component.

This is a literal copy-paste from the function
`org-node-slugify-for-web' of the `org-node' package. Hope that's ok..."
  (thread-last title
               (org-link-display-format)
               (string-glyph-decompose)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (string-glyph-compose)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))

;;;; Org

(defmacro mochar-utils--org-with-headline-contents-visible (&rest body)
  "Necessary when parsing hidden/collapsed data within headline."
  (declare (debug (body)) (indent 1))
  ;; Need to save outline visiblity, otherwise will unfold headers.
  ;; Furthermore, need to set USE-MARKERS to non-nil, otherwise it causes weird
  ;; folding problems.
  `(org-save-outline-visibility 'use-markers
     (org-fold-show-entry)
     ,@body))

(defmacro mochar-utils--org-with-point-at (thing &rest body)
  "THING should be an org-id or nil.

Note: Previously used org-id-find but it put point above
headline. org-mem takes me there directly.

Alternatively org-id-goto could be used, but does not seem to respect
save-excursion."
  (declare (debug (body)) (indent 1))
  `(cond
    ((null ,thing)
     ;; Reveal entry contents, otherwise run into problems parsing the metadata,
     ;; such as with org-srs drawer.
     (mochar-utils--org-with-headline-contents-visible ,@body))
    ((stringp ,thing)
     (when-let* ((org-id ,thing)
                 (entry (org-mem-entry-by-id org-id))
                 (file (org-mem-entry-file-truename entry))
                 (buf (or (find-buffer-visiting file)
                          (find-file-noselect file))))
       (with-current-buffer buf
         ;; We need to widen the buffer because `find-buffer-visiting' might
         ;; return an active, narrowed buffer.
         (org-with-wide-buffer
          ;; Jump to correct position
          (goto-char (org-find-property "ID" org-id))
          
          ;; Reveal entry contents, otherwise run into problems parsing the
          ;; metadata, such as with org-srs drawer.
          (mochar-utils--org-with-headline-contents-visible ,@body)))))
    (t (error "THING should be org-id or nil"))))

;;;; org-mem / org-node

(defun mochar-utils--org-mem-refs (&optional entry types)
  (let* ((entry (or entry (org-node-at-point))))
    (cl-loop for ref in (org-mem-entry-roam-refs entry)
             for type = (gethash ref org-mem--roam-ref<>type)
             when (or (null types) (member type types))
             collect (if type (concat type ":" ref) ref))))

(defun mochar-utils--org-mem-website-refs (&optional entry)
  (mochar-utils--org-mem-refs entry '("http" "https")))

(defun mochar-utils--org-mem-cite-refs (&optional entry)
  (seq-keep
   (lambda (ref)
     (when (s-starts-with-p "@" ref)
       (substring ref 1)))
   (mochar-utils--org-mem-refs entry)))

;; TODO this doesnt properly take care of inherited properties
;; https://github.com/meedstrom/org-mem/issues/31
(defun mochar-utils--org-mem-update-cache-after-capture (extent)
  (with-current-buffer (marker-buffer org-capture-last-stored-marker)
    (pcase extent
      ('file
       (org-mem-updater-ensure-buffer-file-known))
      ('entry
       (save-excursion
         (goto-char (marker-position org-capture-last-stored-marker))
         (org-mem-updater-ensure-id-node-at-point-known)))
      (_ (error "EXTENT must be one of 'file or 'entry")))))

(defun mochar-utils--org-mem-title-full (entry)
  (cl-assert (org-mem-entry-p entry))
  (pcase-let* ((affix (funcall org-node-affixation-fn entry (org-mem-entry-title entry)))
               (`(,title ,prefix ,suffix) affix)
               (title (concat prefix title suffix)))
    title))

;;;; Web

(defun mochar-utils--get-page-title (url &optional slugify)
  "Get title of web page by URL, or `nil' if not found.

Uses various utilities from `url.el'."
  (let (web-title-str coding-charset)
    (with-current-buffer (url-retrieve-synchronously url)
      ;; find title by grep the html code
      (goto-char (point-min))
      (when (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
        (setq web-title-str (match-string 1)))
      ;; find charset by grep the html code
      (goto-char (point-min))
      ;; downcase the charaset. e.g, UTF-8 is not acceptible for emacs, while utf-8 is ok.
      (setq coding-charset
            (or (when (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
                  (downcase (match-string 1)))
                "utf-8"))
      ;; decode the string of title.
      (when web-title-str
        (setq web-title-str (string-trim (decode-coding-string web-title-str (intern coding-charset)))))
      (kill-buffer))
    (if slugify
        (mochar-utils--slugify-title web-title-str)
      web-title-str)))

(defun mochar-utils--get-website-as-org (url)
  "Download the HTML content from URL and convert to Org with Defuddle and Pandoc."
  (let ((default-directory (file-name-directory convtools-defuddle-path)))
    (with-temp-buffer
      (insert
       (with-output-to-string
         (call-process
          convtools-node-path
          nil standard-output nil
          convtools-defuddle-path
          "url" url "markdown")))
      (call-process-region (point-min) (point-max)
                           "pandoc" t t nil
                           "-f" "markdown" "-t" "org"
                           "--wrap=preserve")
      (buffer-string))))

;;;; Transient

(defun mochar-utils--transiet-set-target-value (target-key new-value)
  "Finds the target argument by its key and sets its internal value slot."
  (let* (;; Get the list of currently displayed suffix objects (internal variable)
         (all-suffixes transient--suffixes)
         ;; Find the target object by its key
         (target-obj (cl-find target-key all-suffixes
                             :key (lambda (s) (oref s key))
                             :test #'equal)))
    ;; Check if it's a valid object and manually set the 'value' slot (internal slot)
    (when (cl-typep target-obj 'transient-infix)
      (oset target-obj value new-value))))


;;;; Bibtex

(defun mochar-utils--format-bibtex-entry (entry &optional key)
  "Format a parsebib ENTRY alist or hash entry as a BibTeX string.
ENTRY may be an alist or association list with keys like \"=type=\", \"=key=\" and field names.
KEY may be the key to use instead of =key=."
  (let* ((key (or key (cdr (assoc "=key=" entry))))
         (type (cdr (assoc "=type=" entry)))
         (fields (cl-remove-if
                  (lambda (f) (string-prefix-p "=" (car f)))
                  entry))
         (pad 2))
    (concat "@"
            (or type "misc")
            "{"
            (or key "")
            ",\n"
            (mapconcat
             (lambda (pair)
               (format "%s%s = {%s}" 
                       (make-string pad ?\s)
                       (car pair) 
                       (cdr pair)))
             fields
             ",\n")
            "\n}\n")))

;;;; Footer

(provide 'mochar-utils)

;;; mochar-utils.el ends here

