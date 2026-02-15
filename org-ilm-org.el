;;; org-ilm-org.el --- Org attachments -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org-capture)
(require 'org-element)
(require 'embark)

(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-capture)
(require 'org-ilm-card)
(require 'org-ilm-media)
(require 'org-ilm-element)

;;;; Variables

(defvar org-ilm-target-value-regexp "\\(extr\\|card\\):\\(beg\\|end\\):\\([^>]+\\)"
  "Regexp to match values of targets enclosing extracts and clozes.")

(defvar org-ilm-target-regexp (format "<<%s>>" org-ilm-target-value-regexp)
  "Regexp to match targets enclosing extracts and clozes.")

(defvar org-ilm--targets-editable nil
  "Whether or not to allow editing/removing of target text.")

;;;; New

(defun org-ilm--org-new (type collection &optional parent)
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
                             ;; (let ((org-ilm-capture-show-menu t))
                             (let ((org-ilm-capture-show-menu nil))
                               (org-ilm--capture-capture
                                type
                                :capture-kwargs '(:immediate-finish nil)
                                :parent (-some-> parent org-ilm-element--id)
                                :collection collection
                                :file tmp-file
                                :method 'mv))))))
    (cl-letf (((symbol-value 'org-capture-templates)
               (list (list "n" "New" 'plain (list 'file tmp-file) ""
                           :after-finalize after-finalize))))
      (org-capture nil "n"))))

(defun org-ilm-org-new-material (collection &optional parent)
  (interactive)
  (org-ilm--org-new 'material collection parent))

(defun org-ilm-org-new-card (collection &optional parent)
  (interactive)
  (org-ilm--org-new 'card collection parent))

;;;; Functions

(cl-defun org-ilm--org-buffer-text-process (&key keep-clozes no-footnotes (source-buffer (current-buffer)) (begin (point-min)) (end (point-max)))
  "Process current buffer for extraction or cloze."
  ;; Remove capture targets
  (replace-regexp org-ilm-target-regexp "" nil begin end)
  ;; Undo cloze syntax
  (unless keep-clozes (org-ilm--card-uncloze-buffer begin end))
  ;; Append referenced footnotes
  (if no-footnotes
      (replace-regexp org-footnote-re "" nil begin end)
    ;; Copy over footnotes 
    (let ((footnotes '()))
      (goto-char (point-min))
      (while (re-search-forward org-footnote-re nil t)
        (when-let* ((fn (match-string 0))
                    (label (match-string 1))
                    (def (with-current-buffer source-buffer
                           (org-footnote-get-definition label))))
          (push (concat fn " " (nth 3 def)) footnotes)))
      (goto-char (point-max))
      (insert "\n\n* Footnotes\n")
      (dolist (fn (delete-dups (nreverse footnotes)))
        (insert fn "\n")))))

(defun org-ilm--org-buffer-text-prepare (&optional region-begin region-end keep-clozes remove-footnotes)
  "Return processed buffer text for new extract or card element."
  (let* ((text (buffer-substring-no-properties (or region-begin (point-min))
                                               (or region-end (point-max))))
         (text (string-trim text))
         (buffer (current-buffer)))
    (with-temp-buffer
      (insert text)
      (org-ilm--org-buffer-text-process
       :source-buffer buffer
       :keep-clozes keep-clozes
       :no-footnotes remove-footnotes)
      (buffer-string))))

;;;; Extract

(defvar org-ilm--org-mark-element nil)
(defvar org-ilm--org-mark-point nil)

(defun org-ilm-org-mark-element-cycle ()
  (interactive)
  (if-let ((_ org-ilm--org-mark-element)
           (parent (org-element-property :parent org-ilm--org-mark-element)))
          (setq org-ilm--org-mark-element parent)
    (setq org-ilm--org-mark-element (org-element-context)))
  (set-mark (org-element-end org-ilm--org-mark-element))
  (goto-char (org-element-begin org-ilm--org-mark-element))
  (activate-mark))

(defvar-keymap org-ilm-mark-map
  :doc "Keymap for org-ilm-mark-mode."
  :repeat (:enter (org-ilm-org-mark-element-cycle) :exit (org-ilm-org-extract))
  "x" #'org-ilm-org-mark-element-cycle
  "RET" (lambda ()
          (interactive)
          (org-ilm-mark-mode -1)
          (org-ilm-org-extract))
  "C-g" (lambda ()
          (interactive)
          (when org-ilm--org-mark-point
            (goto-char org-ilm--org-mark-point))
          (deactivate-mark)
          (org-ilm-mark-mode -1)))

(define-minor-mode org-ilm-mark-mode
  ""
  :keymap org-ilm-mark-map
  (if org-ilm-mark-mode
      (progn
        (setq org-ilm--org-mark-point (point))
        (org-ilm-org-mark-element-cycle))
    (setq org-ilm--org-mark-element nil
          org-ilm--org-mark-point nil)))

(cl-defmethod org-ilm--extract (&context (ilm-attachment org))
  "Extract text in Org mode attachments by cycling through regions."
  (if (region-active-p)
      (org-ilm-org-extract)
    (org-ilm-mark-mode 1)))

(defun org-ilm-org-extract (&optional title)
  "Extract region text in Org mode attachments."
  (interactive)
  (unless (region-active-p) (user-error "No region active"))
  (let* ((file-buf (current-buffer))
         (file-path buffer-file-name)
         (file-name (file-name-base file-path))
         (attach-org-id (car (org-ilm--attachment-data)))
         (attach-org-id-marker (org-id-find attach-org-id t))
         (file-org-id file-name)
         ;; TODO This doesnt work great, use what i do in other places, which
         ;; is: i forgot. i think there is a utility function
         (file-org-id-marker (org-id-find file-org-id t)))
    (unless attach-org-id-marker
      (error "Current file is not an attachment, or failed to parse org ID."))
    (unless file-org-id-marker
      (error "Current file name not org ID, or failed to find it."))
    
    (let* ((region-begin (region-beginning))
           (region-end (region-end))
           ;; If region end is beginning of line, reposition to end of previous
           ;; line. This avoids formatting problems.
           (region-end (if (save-excursion (goto-char region-end) (bolp))
                           (max (- region-end 1) 1)
                         region-end))
           (region-text (org-ilm--org-buffer-text-prepare region-begin region-end))
           (extract-org-id (org-id-new))
           (entry (org-mem-entry-by-id attach-org-id))
           props)

      ;; If the element has media, then extract time.
      (when (org-mem-entry-property-with-inheritance org-ilm-property-media entry)
        (when-let ((timer (org-ilm--media-extract-range region-text)))
          (setf (plist-get props :ILM_MEDIA+) timer)))
          
      (org-ilm--capture-capture
       'material
       :parent file-org-id
       :id extract-org-id
       :content region-text
       :title title
       :props props
       :on-success
       (lambda (&rest _)
         ;; Wrap region with targets.
         (with-current-buffer file-buf
           (save-excursion
             (let* ((target-template (format "<<extr:%s:%s>>" "%s" extract-org-id))
                    (target-begin (concat
                                   (format target-template "beg")
                                   ;; Region starts at beginning of line that
                                   ;; contains header or org element that starts
                                   ;; with #. These need to start at beginning
                                   ;; of line so we enter the target at the line
                                   ;; before it.
                                   (when (and (member (char-before region-begin) (list nil ?\n))
                                              (member (char-after region-begin) (list ?* ?#)))
                                     "\n")))
                    (target-end (format target-template "end")))
               ;; Insert target before region
               (goto-char region-begin)
               (insert target-begin)

               ;; Insert target after region, adjusting for start target length
               (goto-char (+ region-end (length target-begin)))
               (insert target-end)

               (save-buffer)
               (org-ilm-recreate-overlays region-begin (point))))))
       ))))

;;;; Cloze

(cl-defmethod org-ilm--cloze (&context (ilm-attachment org))
  (org-ilm-org-cloze))

(defun org-ilm-org-cloze ()
  "Create a cloze card.
A cloze is made automatically of the element at point or active region."
  (interactive)

  (let* ((attachment (org-ilm--attachment-data))
         (file-org-id (car attachment))
         (file-buf (current-buffer))
         (card-org-id (org-id-new))
         (cloze-bounds (org-ilm--card-cloze-bounds))
         (buffer-text (buffer-string))
         (point (point))
         snippet)

    (cl-assert cloze-bounds)

    ;; Prepare card buffer text
    (with-temp-buffer
      (insert buffer-text)
      (goto-char point)
      (let ((marker (point-marker))
            (content (buffer-substring (car cloze-bounds) (cdr cloze-bounds))))
        (delete-region (car cloze-bounds) (cdr cloze-bounds))
        (org-ilm--org-buffer-text-process :source-buffer file-buf)
        (setq snippet (org-ilm--generate-text-snippet (buffer-string)))
        (goto-char marker)
        (org-ilm--card-cloze-region (prog1 (point) (insert content)) (point))
        (setq buffer-text (buffer-string))))
        
    (org-ilm--capture-capture
     'card
     :parent file-org-id
     :id card-org-id
     :content buffer-text
     :title snippet
     :on-success
     (lambda (&rest _)
       (with-current-buffer file-buf
         (save-excursion
           (goto-char (cdr cloze-bounds))
           (insert "<<card:end:" card-org-id ">>")
           (goto-char (car cloze-bounds))
           (insert "<<card:beg:" card-org-id ">>"))
         (save-buffer)
         (org-ilm-recreate-overlays))))))

;;;; Split

(defun org-ilm-org-split (&optional level)
  "Split org document by heading level."
  (interactive)
  (cl-assert (and (eq (car (org-ilm--where-am-i)) 'attachment)
                  (eq major-mode 'org-mode))
             nil "Not in Org mode attachment")

  (unless level
    (setq level (read-number "Split by level: "  (max (org-outline-level) 1))))

  (let ((org-ilm-capture-show-menu nil))
    (save-excursion
      (goto-char (point-min))
      (let ((re (format "^\\*\\{%d\\} " level))
            title)
        (while (re-search-forward re nil t)
          (setq title (org-get-heading t t t t))
          (atomic-change-group
            (beginning-of-line)
            (insert "\n")
            (set-mark (point))
            (org-end-of-subtree t)
            (insert "\n")
            (org-ilm-org-extract title)))))))
;;;; Target overlays

;; Targets refer to the anchor points used to highlight extracted or clozed
;; sections of text in org attachments. It looks like this:
;; <<{type}:beg:{id}>>bla bla<<{type}:end:{id}>>
;; Type can be 'card' or 'extr'.
;; They are called targets because that's what Org mode calls them. Targets are
;; part of the Org mode spec and are therefore parse by 'org-element'.

;; When an Org mode file is loaded that is recognized as an ilm attachment, the
;; buffer is parsed for target pairs. For each pair, three overlays are created:
;; one for each target element to hide it, and one that encapsulates the whole
;; target region to visually indicate it with a color.

;;;;; Parsing

(defun org-ilm--target-parse-string (string &optional with-brackets)
  "Parse and return parts of target string as specified in `org-ilm-target-value-regexp'."
  (when-let* ((regexp (if with-brackets
                          org-ilm-target-regexp
                        org-ilm-target-value-regexp))
              (parts (s-match regexp string))
              (type (nth 1 parts))
              (pos (nth 2 parts))
              (id (nth 3 parts)))
    `(:string ,string :type ,type :pos ,pos :id ,id)))

(defun org-ilm--target-parse-element (element)
  "Parse and return parts of target element."
  (when-let* ((string (org-element-property :value element))
              (parts (org-ilm--target-parse-string string))
              (begin (org-element-property :begin element))
              (end (org-element-property :end element)))
    (let* (;; Parse target text to offset end position by spaces
           (target-text (buffer-substring-no-properties begin end))
           (trailing-spaces (length (progn
                                      (string-match " *\\'" target-text)
                                      (match-string 0 target-text))))
           (end-nowhite (- end trailing-spaces)))
      (setq parts (plist-put parts :begin begin))
      (setq parts (plist-put parts :end end-nowhite)))
    parts))

(defun org-ilm--target-parse-match ()
  "Parse and return target matched with `re-search-forward' and friends."
  (when-let ((target (org-ilm--target-parse-string (match-string-no-properties 0)))
             (begin (match-beginning 0))
             (end (match-end 0)))
    (setq target (plist-put target :begin begin))
    (setq target (plist-put target :end end))
    target))

(defun org-ilm--target-around-point (pos &optional id)
  "Returns start target or end target around point, depending if `POS' is 'begin or 'end."
  (cl-assert (memq pos '(begin end)))
  (let* ((find-begin (eq pos 'begin))
         (re-func (if find-begin #'re-search-backward #'re-search-forward))
         (pos-string (if find-begin "beg" "end"))
         found)
    (save-excursion
      (while (and (not found) (funcall re-func org-ilm-target-regexp nil t))
        (let ((target (org-ilm--target-parse-match)))
          (when (and (string-equal (plist-get target :pos) pos-string)
                     (if id (string-equal (plist-get target :id) id) t))
            (setq found target)))))
    found))

(defun org-ilm--targets-around-point ()
  "Returns cons with begin and targets around point, or nil if not in highlight."
  (when-let* ((target-begin (org-ilm--target-around-point 'begin))
              (target-end (org-ilm--target-around-point 'end (plist-get target-begin :id))))
    (cons target-begin target-end)))

(defun org-ilm-targets-remove-around-point ()
  "Removes targets around point if exists."
  (interactive)
  (when-let ((targets (org-ilm--targets-around-point)))
    (atomic-change-group
      (let ((org-ilm--targets-editable t))
        (delete-region (plist-get (cdr targets) :begin)
                       (plist-get (cdr targets) :end))
        (delete-region (plist-get (car targets) :begin)
                       (plist-get (car targets) :end)))
      (org-ilm-recreate-overlays))))

;;;;; Overlays

(defun org-ilm--target-ov-block-edit (ov after-change-p beg end &optional len-pre)
  ;; The hook functions are called both before and after each change. If the functions save the information they receive, and compare notes between calls, they can determine exactly what change has been made in the buffer text.
  ;; When called before a change, each function receives four arguments: the overlay, nil, and the beginning and end of the text range to be modified.
  ;; When called after a change, each function receives five arguments: the overlay, t, the beginning and end of the text range just modified, and the length of the pre-change text replaced by that range. (For an insertion, the pre-change length is zero; for a deletion, that length is the number of characters deleted, and the post-change beginning and end are equal.)
  (message "%s" ov)
  (unless (or after-change-p org-ilm--targets-editable)
    (user-error "Cannot modify this region")))

(defun org-ilm--overlay-edit-hook (ov after-change-p beg end &optional len-pre)
  )

(defun org-ilm--create-overlay (target-begin target-end &optional no-face)
  "Create an overlay to hide the target markers and highlight the target text."
  ;; Hide targets
  (dolist (target (list target-begin target-end))
    (let ((begin (plist-get target :begin))
          (end (plist-get target :end))
          (begin-p (string= "beg" (plist-get target :pos))))

      ;; Previously this overlay was invisible, but it ran into some
      ;; difficulties which i am too tired to recall now.
      (let ((ov (make-overlay
                 begin end nil ;; buffer
                 ;; FRONT-ADVANCE: if non-nil, makes the marker for the front of
                 ;; the overlay advance when text is inserted there (which means
                 ;; the text *is not* included in the overlay).
                 ;; t
                 ;; REAR-ADVANCE: if non-nil, makes the marker for the rear of
                 ;; the overlay advance when text is inserted there (which means
                 ;; the text *is* included in the overlay).
                 t
                 )))
        (overlay-put ov 'org-ilm-target t)
        ;; (overlay-put ov 'invisible t)
        (overlay-put ov 'display
                     (propertize (if begin-p "«" "»")
                                 'face '(:foreground "dim gray")))
        (overlay-put ov 'cursor-intangible t)
        (overlay-put ov 'modification-hooks '(org-ilm--target-ov-block-edit)))

      ;; If next character is newline, prevent it from being edited.  Otherwise
      ;; hitting backspace on header will move the header to the front of the
      ;; target, and then no longer is a valid header. The has to be a seperate
      ;; overlay, because the display and/or invisible property makes headlines
      ;; invalid if the newline before them is included.
      (when (eq (char-after end) ?\n)
        (let ((ov (make-overlay end (1+ end))))
          (overlay-put ov 'org-ilm-target t)
          (overlay-put ov 'modification-hooks '(org-ilm--target-ov-block-edit))))))
  
  ;; Highlight region
  (let* ((id (plist-get target-begin :id))
         (type (plist-get target-begin :type))
         (face (pcase type
                 ("extr" 'org-ilm-face-extract)
                 ("card" 'org-ilm-face-card)))
         (begin (plist-get target-begin :begin))
         (end (plist-get target-end :end))
         ;; (begin (plist-get target-begin :end))
         ;; (end (1+ (plist-get target-end :begin)))
         ov)
    (unless (= begin end)
      (setq ov (make-overlay begin end));; nil t t))
      (unless no-face
        (overlay-put ov 'face face))
      (overlay-put ov 'org-ilm-highlight t)
      (overlay-put ov 'org-ilm-type type)
      (overlay-put ov 'org-ilm-id id)
      (overlay-put ov 'modification-hooks '(org-ilm--overlay-edit-hook))
      (overlay-put ov 'help-echo (format "Highlight %s" id)))))

(defun org-ilm-remove-overlays (&optional point-min point-max)
  ""
  (interactive)
  (org-with-wide-buffer
   (remove-overlays point-min point-max 'org-ilm-highlight t)
   (remove-overlays point-min point-max 'org-ilm-target t)))

(defun org-ilm-recreate-overlays (&optional point-min point-max no-face)
  ""
  (interactive)
  (org-ilm-remove-overlays point-min point-max)
  (org-with-wide-buffer
   (goto-char (or point-min (point-min)))
   (let ((begin-targets (make-hash-table :test 'equal)))
     (while-let ((end (re-search-forward org-ilm-target-regexp point-max t))
                 (begin (match-beginning 0))
                 (string (match-string-no-properties 0))
                 (type (match-string-no-properties 1))
                 (pos (match-string-no-properties 2))
                 (id (match-string-no-properties 3))
                 (target (list :end end :begin begin :string string
                               :type type :pos pos :id id)))
       (when (member type '("extr" "card"))
         (pcase pos
           ("beg" (puthash id target begin-targets))
           ("end"
            (when-let ((begin-target (gethash id begin-targets)))
              (org-ilm--create-overlay begin-target target no-face)
              ;; TODO Do we need to remove it?
              (remhash id begin-targets)))))))))

(defun org-ilm--open-from-ov (ov)
  ""
  (when-let ((id (overlay-get ov 'org-ilm-id))
             (attachment (format "%s.org" id)))
    (when (or
           (file-exists-p attachment)
           (yes-or-no-p "Attachment does not exist. Open anyway?"))
        (find-file attachment))
    id))

;;;;; Embark

;; Integration with embark so that we can apply actions on elements without
;; opening them.

(defun org-ilm-element-actions-embark (target)
  (let ((id (get-text-property 0 :id target)))
    (org-ilm-element-actions nil id)))

(defun org-ilm-element-delete-embark (target)
  (let ((element (get-text-property 0 :element target))
        (ov (get-text-property 0 :ov target)))
    (org-ilm--element-with-point-at element
      (call-interactively #'org-ilm-element-delete element))
    (save-excursion
      (goto-char (ov-beg ov))
      (save-match-data
        (re-search-forward org-ilm-target-regexp)
        (org-ilm-targets-remove-around-point)))))

(defun org-ilm-element-enter-embark (target)
  (let ((ov (get-text-property 0 :ov target)))
    (org-ilm--open-from-ov ov)))

(defvar-keymap org-ilm-element-embark-map
  ;; :parent embark-general-map
  "e" #'org-ilm-element-actions-embark
  "k" #'org-ilm-element-delete-embark
  "RET" #'org-ilm-element-enter-embark)

(add-to-list 'embark-keymap-alist '(ilm-element . org-ilm-element-embark-map))

(defun org-ilm--target-embark-finder ()
  ;; We look at overlays rather than parsing with `org-ilm--target-around-point'
  ;; as it can deal with overlapping overlays faster, parsing already done.
  (when-let* ((ovs (seq-filter
                    (lambda (ov) (overlay-get ov 'org-ilm-id))
                    (overlays-at (point)))))
    (seq-keep
     (lambda (ov)
       (when-let* ((id (overlay-get ov 'org-ilm-id))
                   (element (ignore-errors (org-ilm--element-by-id id)))
                   (target (concat
                            (org-ilm--org-add-todo-face (org-ilm-element--state element))
                            ": "
                            (org-ilm-element--title element)))
                   (beg (ov-beg ov))
                   (end (ov-end ov)))
         (add-text-properties 0 (length target) (list :id id :ov ov :element element) target)
         `(ilm-element ,target ,beg . ,end)))
     ovs)))

(add-to-list 'embark-target-finders 'org-ilm--target-embark-finder)

;;; Footer

(provide 'org-ilm-org)

;;; org-ilm-org.el ends here
