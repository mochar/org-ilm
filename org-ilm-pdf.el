;;; org-ilm-pdf.el --- PDF attachments -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'pdf-view)
(require 'cond-let)
(require 'transient)
(require 'color)

(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-review)
(require 'org-ilm-capture)
(require 'org-ilm-attachment)
(require 'org-ilm-image)
(require 'org-ilm-element)
(require 'org-ilm-convert)

;;;; Variables

(defvar-keymap org-ilm-pdf-map
  :doc "Keymap for PDF attachments."
  "d" #'org-ilm-pdf-open-full-document
  "x" #'org-ilm-pdf-extract
  "z" #'org-ilm-pdf-cloze
  "c" #'org-ilm-pdf-convert
  "s" #'org-ilm-pdf-split
  "n" #'org-ilm-pdf-toggle-narrow
  "h" #'org-ilm-pdf-toggle-highlights)

(defcustom org-ilm-pdf-minimum-virtual-page-size '(1 . 1)
  "The minimum size of the area to create a virtual PDF extract."
  :type '(cons (symbol :tag "Width")
               (symbol :tag "Height"))
  :group 'org-ilm-pdf)

(defcustom org-ilm-pdf-convert-org-respect-area t
  "When virtual view is an area of a single page, convert just that area.

This is done by converting the area to an image first. Note that this will likely effect the quality of the conversion output, probably for the worse if it contains complex objects other than text."
  :type 'boolean
  :group 'org-ilm-pdf)

(defcustom org-ilm-pdf-highlight-alpha 0.35
  "The alpha value of the extract highlights on the PDF page."
  :type 'number
  :group 'org-ilm-pdf)

(defconst org-ilm--pdf-output-types
  '((virtual . "PDF (virtual)")
    (text . "Text")
    (image . "Image")
    (org . "Org (Marker)")))

(defconst org-ilm--pdf-extract-options
  '((page . "Page")
    (region . "Region")
    (section . "Section")))

(defconst org-ilm--pdf-extract-option-to-types
  '((page . (virtual text image org))
    (region . (virtual text image org))
    (section . (virtual text org))))

;;;; Utilities

(defun org-ilm--pdf-mode-p ()
  "Return non-nil if current major mode is `pdf-view-mode' or `pdf-virtual-view-mode'."
  (member major-mode '(pdf-view-mode pdf-virtual-view-mode)))

(defun org-ilm--pdf-mode-assert ()
  "Assert if not in pdf mode."
  (cl-assert (org-ilm--pdf-mode-p) nil "Not in a PDF document"))

(defun org-ilm--pdf-path ()
  "Return path to pdf file of currently open PDF buffer."
  (pcase major-mode
    ('pdf-view-mode
     buffer-file-name)
    ('pdf-virtual-view-mode
     (car (pdf-virtual-document-page 1)))
    (_ (error "Not in PDF buffer"))))

(defun org-ilm--pdf-region-below-min-size-p (size-or-page &optional region min-size)
  "Check if normalized coordinates REGION in SIZE is less than SIZE-MAX.
When REGION is nil, use active region.
When MIN-SIZE is nil, compare to `org-ilm-pdf-minimum-virtual-page-size'."
  (when-let* ((size (if (integerp size-or-page) (pdf-info-pagesize size-or-page) size-or-page))
              (min-size (or min-size org-ilm-pdf-minimum-virtual-page-size))
              (region (or region (org-ilm--pdf-region-normalized)))
              (region-width (* (car size) (- (nth 2 region) (nth 0 region))))
              (region-height (* (cdr size) (- (nth 3 region) (nth 1 region)))))
    (or (< region-width (car min-size))
        (< region-height (cdr min-size)))))

(defun org-ilm--pdf-data ()
  "Return list with some data about the current PDF attachment."
  (pcase (org-ilm--where-am-i)
    (`(attachment ,id ,collection ,colfile)
     (cl-assert (org-ilm--pdf-mode-p))
     (let* ((pdf-path (org-ilm--pdf-path))
            (attach-dir (file-name-directory pdf-path))
            (headline (org-ilm--org-headline-element-from-id id)))
       (cl-assert headline nil "Collection element not found")
       (list :id id :attach-dir attach-dir :pdf-path pdf-path
             :collection collection :headline headline)))
    (_ (error "Not in element attachment"))))

(defmacro org-ilm--pdf-with-point-on-collection-headline (of-document &rest body)
  "Place point on the headline belonging to this PDF attachment.
When OF-DOCUMENT non-nil, jump to headline which has the orginal document as attachment."
  (declare (indent 1))
  `(when-let ((headline (plist-get (org-ilm--pdf-data) :headline)))
     (org-with-point-at headline
       ,@body)))

(cl-defun org-ilm--pdf-image-export (filename &key region page dir)
  "Export current PDF buffer as an image.

REGION: Note that if in virtual view with region, already exports that region."
  (org-ilm--pdf-mode-assert)
  (let ((region (or region '(0 0 1 1)))
        (page (or page (pdf-view-current-page)))
        (img-buffer (get-buffer-create "*Org Ilm PDF Image*"))
        img-type img-ext img-path)
    ;; TODO Supports combining multiple regions as one image, might be nice for
    ;; region export
    (pdf-view-extract-region-image
     (list page region) page nil img-buffer 'no-display)
    (with-current-buffer img-buffer
      (setq img-type (image-type-from-buffer)
            img-ext (symbol-name img-type) ;; TODO ???
            img-path (expand-file-name
                      (concat filename "." img-ext)
                      (or dir temporary-file-directory)))
      (write-region (point-min) (point-max) img-path))
    (kill-buffer img-buffer)
    img-path))

(defun org-ilm-pdf-virtual-refresh ()
  "Refresh the virtual document."
  (interactive)
  (unless (eq major-mode 'pdf-virtual-view-mode)
    (error "This command can only be run in a pdf-view-mode buffer"))

  ;; The `pdf-view-have-image-mode-pixel-vscroll' constant checks
  ;; if the Emacs version supports pixel-based vertical scrolling,
  ;; which is necessary for `window-vscroll' to return the correct value.
  (let* ((pixel-scroll-p (bound-and-true-p pdf-view-have-image-mode-pixel-vscroll))
         (page (pdf-view-current-page))
         (size pdf-view-display-size)
         (hscroll (window-hscroll))
         (vscroll (window-vscroll nil pixel-scroll-p)))
    (pdf-virtual-edit-mode)
    (pdf-virtual-view-mode)
    
    ;; Set the display size (zoom level).
    ;; This needs to be done *before* going to the page so the
    ;; image is rendered at the correct size.
    (setq-local pdf-view-display-size size)

    ;; Go to the specified page. This will also redisplay the buffer.
    (pdf-view-goto-page page)

    ;; After redisplaying, set the scroll positions.
    ;; It's important to do this last, as the scrollable area
    ;; depends on the rendered image size.
    (set-window-hscroll (selected-window) hscroll)
    (set-window-vscroll (selected-window) vscroll pixel-scroll-p)))

(defun org-ilm--pdf-outline-get (&optional file-or-buffer full-p)
  "Parse outline of the PDF in FILE-OR-BUFFER or current buffer.
Same as `pdf-info-outline' but adds in each section info about the next
section at the same or shallower depth."
  ;; TODO Issues running this on virtual pdf buffer so set it to main pdf
  (setq file-or-buffer (or file-or-buffer (org-ilm--pdf-path)))
  (let ((outline (pdf-info-outline file-or-buffer))
        (num-pages (pdf-info-number-of-pages file-or-buffer)))
    (dotimes (index (length outline))
      (let* ((current-item (nth index outline))
             (title (alist-get 'title current-item))
             (page (alist-get 'page current-item))
             (top (alist-get 'top current-item))
             (depth (alist-get 'depth current-item))
             (begin (list page 0 top 1 1))
             next-item range spec-parts)

        ;; Find the next item at the same or a shallower depth
        (dolist (j (number-sequence (1+ index) (- (length outline) 1)))
          (let ((potential-next (nth j outline)))
            (when (and (not next-item) (<= (alist-get 'depth potential-next) depth))
              (setf (alist-get 'next current-item) j)
              (setq next-item potential-next))))

        (let* ((next-item-page (alist-get 'page next-item))
               (next-item-top (alist-get 'top next-item))
               (has-top (and top (not (= top 0))))
               last-page last-top)

          (if next-item-page
              (if (or (= page next-item-page) (not (= next-item-top 0)))
                  (setq last-page next-item-page
                        last-top next-item-top)
                (setq last-page (max page (1- next-item-page))))
            (setq last-page num-pages))

          (if (= last-page page)
              (progn
                (when (and last-top (< (- last-top (or top 0)) .02))
                  (setq last-top nil))
                (let ((area (when (or has-top last-top) (list 0 top 1 (or last-top 1))))) 
                  (push (make-org-ilm-pdf-page :number page :area area) spec-parts)))

            (when has-top
              (push (make-org-ilm-pdf-page :number page :area (list 0 top 1 1)) spec-parts))

            (cond
             ((> last-page (1+ page))
              (push (make-org-ilm-pdf-range
                     :begin (if has-top (1+ page) page)
                     :end (if last-top (1- last-page) last-page))
                    spec-parts))
             ((null has-top)
              (push (make-org-ilm-pdf-page :number page) spec-parts)))
            
            (when last-top
              (push (make-org-ilm-pdf-page
                     :number last-page
                     :area (list 0 0 1 last-top))
                    spec-parts)))
          
          (setf (alist-get 'next-page current-item) last-page
                (alist-get 'next-top current-item) last-top
                (alist-get 'spec current-item) (make-org-ilm-pdf-spec :parts (reverse spec-parts) :single-page-p (= last-page page))))

        (setf (nth index outline) current-item)))

    (if (and outline (not full-p)
             (eq major-mode 'pdf-virtual-view-mode)
             (not (= 1 (org-ilm--pdf-page-normalized 1))))
        (let ((first-page (org-ilm--pdf-page-normalized 1))
              (last-page (org-ilm--pdf-page-normalized (pdf-info-number-of-pages))))
          (seq-filter
           (lambda (section)
             (<= first-page (alist-get 'page section) last-page))
           outline))
      outline)))

(defun org-ilm--pdf-outline-page-sections (&optional page)
  "Find the outline section of PAGE. If page overlaps multiple sections,
 prompt user to select."
  (setq page (or page (org-ilm--pdf-page-normalized)))
  (let* ((outline (org-ilm--pdf-outline-get))
         (num-pages (pdf-info-number-of-pages))
         within-indices)

    (when outline
      (setq within-indices
            (seq-filter
             (lambda (i)
               (let ((page-start (alist-get 'page (nth i outline)))
                     (page-end (if (= i (1- (length outline)))
                                   num-pages
                                 (alist-get 'page (nth (1+ i) outline)))))
                 (and (>= page page-start)
                      (<= page page-end))))
             (number-sequence 0 (1- (length outline)))))
      
      (cond
       ((= 1 (length within-indices))
        (nth (car within-indices) outline))
       ((> (length within-indices) 1)
        (let* ((choices (mapcar (lambda (i)
                                  (cons (alist-get 'title (nth i outline)) i))
                                within-indices))
               (choice (cdr (assoc (completing-read "Section: " choices nil t) choices))))
          (nth choice outline)))))))

(defun org-ilm--pdf-page-normalized (&optional page)
  "If in virtual mode, maps virtual PAGE to document page, else return as is.
When not specified, PAGE is current page."
  (let ((page (or page (pdf-view-current-page))))
    (cond
     ((eq major-mode 'pdf-view-mode) page)
     ((eq major-mode 'pdf-virtual-view-mode)
      (nth 1 (pdf-virtual-document-page page)))
     (t (error "Not in a PDF buffer")))))

(defun org-ilm--pdf-active-region ()
  "If region select, return first region, if text select, return bounding box."
  (interactive)
  (if pdf-view--have-rectangle-region
      (cadr (pdf-view-active-region))
    (pcase-let* ((boxes (pdf-info-getselection
                         (pdf-view-current-page)
                         (cadr (pdf-view-active-region))))
                 (`(,min-l ,min-t ,max-r ,max-b) (car boxes)))
      
      (unless boxes
        (error "Selection does not contain any text"))
      
      (dolist (box (seq-subseq boxes 1))
        (cl-destructuring-bind (L T R B) box
          (setq min-l (min min-l L))
          (setq min-t (min min-t T))
          (setq max-r (max max-r R))
          (setq max-b (max max-b B))))
      
      (list min-l min-t max-r max-b))))

(defun org-ilm--pdf-region-normalized (&optional region virtual-page)
  "If in virtual mode, maps virtual REGION to document region, else return as is.
When not specified, REGION is active region."
  (let ((region (or region (org-ilm--pdf-active-region))))
    (cond
     ((eq major-mode 'pdf-view-mode) region)
     ((eq major-mode 'pdf-virtual-view-mode)
      (let* ((virtual-page (or virtual-page (pdf-view-current-page)))
             (page-region (nth 2 (pdf-virtual-document-page virtual-page))))
        (if page-region
            (pcase-let* ((`(,LE ,TO ,RI, BO) page-region)
                         (`(,le ,to ,ri ,bo) region)
                         (w (- RI LE))
                         (h (- BO TO)))
              (list (+ LE (* le w))
                    (+ TO (* to h))
                    (+ LE (* ri w))
                    (+ TO (* bo h))))
          ;; When page-region is nil, that means we are in a virtual page that
          ;; is not zoomed into a particular region, just shows the whole
          ;; page. In the case the virtual region is the same as the normal
          ;; document region.
          region)))
     (t (error "Not in a PDF buffer")))))

(defun org-ilm--pdf-region-denormalized (region &optional virtual-page)
  "Given a region with coords relative to full page, transform them to be
relative to virtual page.

If VIRTUAL-PAGE is omitted, use the current virtual page."
  (org-ilm--pdf-mode-assert)
  (if (eq major-mode 'pdf-view-mode)
      region

    (let* ((virtual-page (or virtual-page (pdf-view-current-page)))
           (page-region (nth 2 (pdf-virtual-document-page virtual-page))))
      (if page-region
          (pcase-let* ((`(,LE ,TO ,RI ,BO) page-region)
                       (`(,le ,to ,ri ,bo) region)
                       (w (- RI LE))
                       (h (- BO TO)))
            ;; Inverse of normalization:
            ;; full = LE + rel_virtual * w
            ;; rel_virtual = (full - LE) / w
            (list (/ (- le LE) w)
                  (/ (- to TO) h)
                  (/ (- ri LE) w)
                  (/ (- bo TO) h)))
        ;; No subregion (whole page shown)
        region))))

(defun org-ilm--pdf-mouse-position-relative ()
  "Return the relative coordinates (X . Y) of the mouse on the current
PDF page."
  (let* ((mouse-pos (mouse-pixel-position))
         (x (cadr mouse-pos))
         (y (cddr mouse-pos))
         (pos (posn-at-x-y x y))
         (image-pixel-xy (posn-object-x-y pos))
         (relative-xy (pdf-util-scale-pixel-to-relative 
                       image-pixel-xy)))
    ;; Correct for virtual pdf view area
    (let ((norm (org-ilm--pdf-region-normalized (list (car relative-xy) (cdr relative-xy) 1 1))))
      (setq relative-xy (cons (nth 0 norm) (nth 1 norm))))
    relative-xy))

(defun org-ilm--pdf-clip-rect (rect)
  (mapcar (lambda (x) (min (max x 0) 1)) rect))


;;;; Section spec

;; Defines a specification for selecting a set of pages, with optional areas and
;; selections.

(cl-defstruct (org-ilm-pdf-page
               (:conc-name org-ilm-pdf-page--))
  number area selections)

(cl-defstruct (org-ilm-pdf-range
               (:conc-name org-ilm-pdf-range--))
  begin end)

(cl-defstruct (org-ilm-pdf-spec
               (:conc-name org-ilm-pdf-spec--))
  path ;; path to pdf
  parts ;; list: org-ilm-pdf-page | org-ilm-pdf-range
  single-page-p
  )

(defun org-ilm-pdf-spec--contains (spec page)
  "Checks if PAGE is part of SPEC."
  (seq-find
   (lambda (part)
     (cl-etypecase part
       (org-ilm-pdf-page
        (= page (org-ilm-pdf-page--number part)))
       (org-ilm-pdf-range
        (with-slots (begin end) part
          (<= begin page end)))))
   (org-ilm-pdf-spec--parts spec)))

(defun org-ilm-pdf-spec--range (spec)
  "Returns cons of start and end page of pages contained in SPEC."
  (let (pages)
    (dolist (part (org-ilm-pdf-spec--parts spec))
      (cl-etypecase part
        (org-ilm-pdf-page
         (push (org-ilm-pdf-page--number part) pages))
        (org-ilm-pdf-range
         (with-slots (begin end) part
           (push begin pages)
           (push end pages)))))
    (let ((min (apply #'min pages))
          (max (apply #'max pages)))
      (if (= min max)
          (cons min nil)
        (cons min max)))))
      
(defun org-ilm--pdf-spec-is-valid-rect (rect)
  "Return non-nil if RECT is list of length 4, each values 0 <= x <= 1."
  (and (listp rect)
       (= (length rect) 4)
       (cl-every (lambda (x) (<= 0 x 1)) rect)))

(defun org-ilm--pdf-spec-parse-page (page-data)
  (cond
   ((numberp page-data)
    (make-org-ilm-pdf-page :number page-data))
   ((and (listp page-data)
         (numberp (car page-data))
         (<= (length page-data) 3)
         (listp (nth 1 page-data))
         (or (= 4 (length (nth 1 page-data))) ;; area
             (listp (car (nth 1 page-data))))) ;; selections
    (let* ((page (car page-data))
           (area (when (numberp (car (nth 1 page-data))) (nth 1 page-data)))
           (selections (nth (if area 2 1) page-data)))
      (when (and area (not (org-ilm--pdf-spec-is-valid-rect area)))
        (error "Area of page %s not valid: %s" page area))
      (when (and selections (not (cl-every #'org-ilm--pdf-spec-is-valid-rect selections)))
        (error "Selections of page %s not valid: %s" page selections))
      (make-org-ilm-pdf-page :number page :area area :selections selections)))))

(defun org-ilm--pdf-spec-parse (spec-data &optional pdf-path)
  (when (stringp spec-data)
    (setq spec-data (car (read-from-string spec-data))))
  
  (unless (or (numberp spec-data) (listp spec-data))
    (error "PDF spec invalid: %s" spec-data))
  
  (if-let ((page (org-ilm--pdf-spec-parse-page spec-data)))
      (make-org-ilm-pdf-spec :parts (list page) :single-page-p t :path pdf-path)
    ;; Must be a list of parts

    ;; Skip trailing "-":
    ;; Technically we could support the remainder of pages being specified with
    ;; a trailing "-", however we then need to first read the pdf to determine
    ;; the number of pages which is a pain.
    (when (eq (car (last spec-data)) '-)
      (error "PDF page spec cannot end with \"-\": %s" spec-data))

    (let ((i 0)
          (cur-page 1)
          range-begin
          parts)
      (while-let ((part (nth i spec-data)))
        (cond-let*
          ((eq part '-)
           (when range-begin (error "Cannot have double range (--): %s" spec-data))
           (if (= i (1- (length spec-data)))
               (let ((last-part (car parts))
                     beg)
                 (cl-etypecase last-part
                   (org-ilm-pdf-page
                    (setq beg (1+ (org-ilm-pdf-page--number last-part))))
                   (org-ilm-pdf-range
                    (setq beg (1+ (org-ilm-pdf-range--end last-part)))))
                 (push (make-org-ilm-pdf-range :begin beg) parts))
             (setq range-begin cur-page))
           (cl-incf i))
          
          ([page (org-ilm--pdf-spec-parse-page part)]
           (with-slots (number area selections) page
             (let ((has-parts (or area selections)))
               (if (and (not has-parts) (eq (nth (1+ i) spec-data) '-))
                   (progn
                     (unless range-begin
                       (setq range-begin (org-ilm-pdf-page--number page)))
                     (cl-incf i 2))
                 (cond 
                  ((null range-begin)
                   (push page parts))
                  ((= range-begin number)
                   (push page parts))
                  (has-parts
                   (let ((range (make-org-ilm-pdf-range
                                 :begin range-begin
                                 :end (1- number))))
                     (push range parts))
                   (push page parts))
                  (t
                   (let ((range (make-org-ilm-pdf-range :begin range-begin :end number)))
                     (push range parts))))
                 
                 (cl-incf i)
                 (setq cur-page (1+ number)
                       range-begin nil)))))
          
          (t
           (error "PDF spec contains invalid part: %s" part)))) ;; while
      (make-org-ilm-pdf-spec :parts (reverse parts) :path pdf-path))))

(defun org-ilm--pdf-spec-to-string (spec)
  (cl-assert (org-ilm-pdf-spec-p spec))
  (let ((i 0)
        spec-data)
    (with-slots (parts single-page-p) spec
      (dolist (part (org-ilm-pdf-spec--parts spec))
        (cl-etypecase part
          (org-ilm-pdf-page
           (with-slots (number area selections) part
             (if (not (or area selections))
                 (push number spec-data)
               (let ((part-data (list number)))
                 (when area (push area part-data))
                 (when selections (push selections part-data))
                 (push (reverse part-data) spec-data)))))
          (org-ilm-pdf-range
           (with-slots (begin end) part
             (unless (and (= 1 begin) (= 0 i))
               (push begin spec-data))
             (push '- spec-data)
             (push end spec-data))))
        (cl-incf i))

      (setq spec-data (if single-page-p
                          (car spec-data)
                        (reverse spec-data))))
    spec-data))

(when nil
  (-> (org-ilm--pdf-spec-parse 4)
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(4 (0.1 0.2 0.3 0.4)))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(4 ((.1 .1 .1 .1) (.2 .2 .2 .2))))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(4 (0.1 0.2 0.3 0.4) ((.1 .1 .1 .1) (.2 .2 .2 .2))))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(2 5))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(- 5))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(2 - 5))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(- 2 - 5))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(3 - 5))
      org-ilm--pdf-spec-to-string)
  (-> (org-ilm--pdf-spec-parse '(- 2 (4 (0.1 0.2 0.3 0.4) ((.1 .1 .1 .1) (.2 .2 .2 .2)))))
      org-ilm--pdf-spec-to-string)
  )

;;;; Commands

(defun org-ilm-pdf-open-full-document ()
  "Open the full PDF document from an extracted virtual view, jump to current page.

TODO Cute if we can use numeric prefix to jump to that page number"
  (interactive)
  (save-excursion
    (org-ilm--pdf-with-point-on-collection-headline 'of-document
     (org-ilm--attachment-open)
     (pdf-view-goto-page page))))

(defun org-ilm-pdf-toggle-narrow ()
  "When in virtual view that specifies an area, toggle between the area and whole page."
  (interactive)
  (save-excursion
    (org-ilm--pdf-with-point-on-collection-headline nil
     (org-ilm--attachment-open :pdf-no-region region)
     (pdf-view-goto-page virtual-page))))

(defun org-ilm-pdf-toggle-highlights ()
  "Toggle render of extract and cloze highlights."
  (interactive)
  (let ((state (setq org-ilm-pdf-highlight-highlights-p (not org-ilm-pdf-highlight-highlights-p))))
    (message "Highlights turned %s" (if state "on" "off")))
  (pdf-view-redisplay))

;;;; Capture highlights

;; Highlights of child captures shown in PDF file.

(cl-defstruct (org-ilm-pdf-highlight
               (:conc-name org-ilm-pdf-highlight--))
  el-id el-type
  type ;; selection, area
  interactive-p ;; user interactive capture selection
  page rect)

(defvar-local org-ilm-pdf-highlight-highlights-p t)
(defvar-local org-ilm--pdf-highlights nil)

(defun org-ilm--pdf-gather-highlights ()
  "Return list of `org-ilm-pdf-highlight' for all child elements of PDF element."
  (map-let (:id :headline) (org-ilm--pdf-data) 
    (let* ((element (org-ilm--element-by-id id))
           ;; (include-self (eq (org-ilm-element--type element) 'card))
           (include-self t)
           (headlines (org-ilm--element-query-children
                       element
                       :return-type 'headline
                       :include-self include-self
                       :more-query '((property "ILM_PDF"))))
           highlights)
      (dolist (headline headlines)
        (when-let* ((el-id (org-element-property :ID headline))
                    (type (org-ilm--element-type headline))
                    (range (org-element-property :ILM_PDF headline))
                    (spec (org-ilm--pdf-spec-parse range)))
          (dolist (part (org-ilm-pdf-spec--parts spec))
            ;; TODO Find some way to also highlight ranges without creating a
            ;; highlight object per page
            (when (org-ilm-pdf-page-p part)
              (cond-let*
                ([selections (org-ilm-pdf-page--selections part)]
                 (dolist (selection selections)
                   (push
                    (make-org-ilm-pdf-highlight
                     :el-id el-id
                     :el-type type
                     :type 'selection
                     :page (org-ilm-pdf-page--number part)
                     :rect selection)
                    highlights)))
                ([area (org-ilm-pdf-page--area part)]
                 (unless (string= el-id id)
                   (push
                    (make-org-ilm-pdf-highlight
                     :el-id el-id
                     :el-type type
                     :type 'area
                     :page (org-ilm-pdf-page--number part)
                     :rect area)
                    highlights)))
                (t
                 (unless (string= el-id id)
                   (push
                    (make-org-ilm-pdf-highlight
                     :el-id el-id
                     :el-type type
                     :type 'page
                     :page (org-ilm-pdf-page--number part)
                     :rect (list 0 0 1 1))
                    highlights)))
                )))))
      highlights)))

(defun org-ilm--pdf-highlights (&optional reparse-p)
  (org-ilm--pdf-mode-assert)
  (or (and (not reparse-p) (bound-and-true-p org-ilm--pdf-highlights))
      (setq-local org-ilm--pdf-highlights
                  (org-ilm--pdf-gather-highlights))))

(defun org-ilm--pdf-page-highlights (&optional page)
  "Return all specs in or part of PAGE."
  (setq page (or page (org-ilm--pdf-page-normalized)))
  (seq-filter
   (lambda (highlight)
     (= page (org-ilm-pdf-highlight--page highlight)))
   (org-ilm--pdf-highlights)))

(defun org-ilm--pdf-create-capture-context-menu (id)
  "Create a context menu for capture highlight ID."
  (let ((menu (make-sparse-keymap)))
    (define-key
     menu [ilm-open-collection]
     `(menu-item "Ilm: View in collection"
                 ,(lambda ()
                    (interactive)
                    (org-id-goto id))
                 :help "View this extract in the collection."))
    
    (define-key
     menu [ilm-open-attachment]
     `(menu-item "Ilm: Open attachment"
                 ,(lambda ()
                    (interactive)
                    (org-ilm--org-with-point-at id
                      (org-ilm--attachment-open)))
                 :help "Open the attachment of this extract."))
    menu))

(defun org-ilm--pdf-create-highlight-hotspots (highlights size)
  "Hotspots specify interactible regions in the image generated by pdf-view.

See :map in info node `(elisp) Image Descriptors'

See `pdf-annot-create-hotspots', `pdf-annot-hotspot-function', and
buffer-local `pdf-view--hotspot-functions'."
  (seq-keep
   (lambda (highlight)
     (with-slots (el-id el-type page rect) highlight
         (let* ((region (org-ilm--pdf-region-denormalized rect))
                (id-symb (intern (format "ilm-pdf-capture-%s" el-id)))
                ;; Scale relative coords to pixel coords
                (e (pdf-util-scale region size 'round)))

           ;; Bind the mouse click event for this ID to our handler
           (local-set-key
            (vector id-symb 'mouse-1)
            (lambda ()
              (interactive)
              (when-let ((el (org-ilm--element-by-id el-id)))
                (message "%s: %s" (org-ilm-element--type el)
                         (org-ilm-element--title el)))))

           (local-set-key
            (vector id-symb 'down-mouse-3)
            (lambda ()
              (interactive "@")
              (popup-menu (org-ilm--pdf-create-capture-context-menu el-id))))
           
           ;; Hotspot data ‘(AREA ID PLIST)’
           ;; See :map in (info "(elisp) Image Descriptors")
           (list
            `(rect . ((,(nth 0 e) . ,(nth 1 e))
                      . (,(nth 2 e) . ,(nth 3 e))))
            id-symb
            (list 'pointer 'hand
                  'help-echo (format "Capture: %s" el-id))))))
   highlights))

(defun org-ilm--pdf-info-renderpage-highlights (page width highlights &optional file-or-buffer)
  "Render PDF PAGE as image with HIGHLIGHTS highlighted."
  ;; To create the image we call `pdf-info-renderpage' at the end. It accepts a
  ;; list of commands that it goes through sequantilally and applies. The
  ;; important commands are setting the current style (:alpha :background
  ;; :foreground) and creating a highlight (:highlight-region).
  (let* ((this-id (car (org-ilm--attachment-data)))
         cmds
         (capture-cmds
          (lambda (highlights color &optional fg-color)
            (apply #'append
                   cmds
                   (list :background color :foreground (or fg-color color))
                   (seq-keep
                    (lambda (highlight)
                      (with-slots (rect) highlight
                        (list :highlight-region
                              (org-ilm--pdf-clip-rect
                               (org-ilm--pdf-region-denormalized rect)))))
                    highlights))))
         (extract-color (color-darken-name
                         (face-background 'org-ilm-face-extract) 10.))
         (cloze-color (color-darken-name
                       (face-background 'org-ilm-face-card) 10.))
         (interactive-color (color-darken-name
                             (face-background 'org-ilm-pdf-interactive-capture-face) 10.))
         this-highlight extracts clozes interactives)

    (dolist (highlight highlights)
      (with-slots (el-id el-type interactive-p) highlight
        (cond
         (interactive-p
          (push highlight interactives))
         ((string= el-id this-id)
          (setq this-highlight highlight))
         ((eq el-type 'material)
          (push highlight extracts))
         ((eq el-type 'card)
          (push highlight clozes)))))

    (setq cmds
          (append
           (list :alpha org-ilm-pdf-highlight-alpha)
           (funcall capture-cmds extracts extract-color)
           (funcall capture-cmds clozes cloze-color)
           (funcall capture-cmds interactives interactive-color)
           (pcase (-some-> this-highlight org-ilm-pdf-highlight--el-type)
             ('material
              (funcall capture-cmds (list this-highlight) extract-color))
             ('card
              (append
               (when (and (org-ilm-reviewing-p)
                          (not (plist-get org-ilm--review-data :card-revealed)))
                 (list :alpha 1))
               (funcall capture-cmds (list this-highlight) cloze-color "#000000"))))))

    (apply #'pdf-info-renderpage
           page width file-or-buffer cmds)))

;; Advice `pdf-view-create-page' to add highlights for our captures. The
;; function is responsible for generating an image of the PDF. During this
;; process, region rectangle and hotspots are created to generate the image,
;; which we intercept to add our capture highlights. Alternative was calling
;; `pdf-view-display-region' to create the regions but it gets overwritten as
;; soon as the image needs to be generated again.
(defun org-ilm--advice--pdf-view-create-page (page &optional window)
  (let* ((highlights (or (ensure-list
                          (org-ilm--pdf-interactive-capture-page-highlights page))
                         (and (org-ilm--attachment-data)
                              org-ilm-pdf-highlight-highlights-p
                              (org-ilm--pdf-page-highlights))))
         
         ;; Replicate logic from pdf-view-display-region
         (size (pdf-view-desired-image-size page window))
         (width (car size))
         (hotspots (append
                    (org-ilm--pdf-create-highlight-hotspots highlights size)
                    ;; Standard hotspots for eg PDF annotations
                    (pdf-view-apply-hotspot-functions window page size)))
         
         ;; Call the low-level renderer with highlight data
         (data (org-ilm--pdf-info-renderpage-highlights
                page width highlights)))

    ;; This is just a small wrapper around emacs' `create-image'.
    ;; The properties that are passed can be found in (C-x C-e):
    ;;      (info "(elisp) Image Descriptors")
    (pdf-view-create-image data
      :width width
      :rotation (or pdf-view--current-rotation 0)
      :map hotspots
      :pointer 'arrow)))

(defun org-ilm--ilm-hook-pdf ()
  (if org-ilm-global-minor-mode
      (advice-add #'pdf-view-create-page
                  :override #'org-ilm--advice--pdf-view-create-page)
    (advice-remove #'pdf-view-create-page
                   #'org-ilm--advice--pdf-view-create-page)))

(add-hook 'org-ilm-global-minor-mode-hook #'org-ilm--ilm-hook-pdf)
  

;;;; Virtual

(defun org-ilm--pdf-open-virtual (&optional no-region)
  "Open virtual page given by ILM_PDF spec, return buffer if found."
  (when-let* ((el-type (ignore-errors (org-ilm--element-type)))
              (pdf-range (org-entry-get nil "ILM_PDF"))
              (attachment (org-ilm--attachment-find-ancestor "pdf"))
              (attach-dir (file-name-directory attachment))
              (spec (org-ilm--pdf-spec-parse pdf-range attachment))
              (buffer-name (concat (org-id-get) ".pdf")))

    ;; For cards, the ILM_PDF property specifies the cloze, so it is not
    ;; desirable to zoom into just the cloze region. Instead infer from parent
    ;; what region should be, and `org-ilm--pdf-render-page-highlights' will
    ;; show the current cloze region with a black border. Further during review,
    ;; the alpha will be set to 1 to hide the cloze.
    ;; (when (eq el-type 'card)
    ;;   (save-excursion
    ;;     (org-up-heading 1)
    ;;     (if-let ((parent-pdf-range (org-entry-get nil "ILM_PDF")))
    ;;         (setq spec (org-ilm--pdf-parse-spec
    ;;                     (org-ilm--pdf-range-from-string
    ;;                      parent-pdf-range)
    ;;                     attachment))
    ;;       (setq spec (org-ilm--pdf-parse-spec
    ;;                   (plist-get spec :begin-page)
    ;;                   attachment)))))

    (org-ilm--pdf-open-specs (list spec) buffer-name attach-dir no-region)))

(defun org-ilm--pdf-open-specs (specs buffer-name attach-dir &optional no-region)
  "Open a virtual pdf buffer with the given SPECS.
With NO-REGION non-nil, view entire page instead of zooming to specified region.

TODO Handle two column layout"
  ;; We use pop-to-buffer instead of with-current-buffer -> pop-to-buffer
  ;; because the same buffer can be edited on demand when this function is
  ;; called. If done the other way, with-current-buffer will deem the original
  ;; buffer damaged.
  ;; TODO Maybe there is a way to make virtual-edit-mode invisible so it appears
  ;; less janky.
  (pop-to-buffer (get-buffer-create buffer-name))
  
  ;; Since buffer is not associated with a file, default directory is same as
  ;; whereever we opened the attachment from. Set it to attach dir of original
  ;; pdf instead.
  (setq-local default-directory attach-dir)
  
  (pdf-virtual-edit-mode)
  (erase-buffer)
  (insert ";; %VPDF 1.0\n\n")  
  (insert "(")
  (dolist (spec specs)
    (insert "(\"" (org-ilm-pdf-spec--path spec) "\"")
    (dolist (part (org-ilm-pdf-spec--parts spec))
      (cl-etypecase part
        (org-ilm-pdf-range
         (with-slots (begin end) part
           (insert (format "(%s . %s)" begin end))))
        (org-ilm-pdf-page
         (with-slots (number area) part
             (if (and area (not no-region))
                 (insert (format "(%s . %s)" number area))
               (insert (format " %s " number)))))))
    (insert ")"))
  (insert ")\n")
  
  ;; (pdf-virtual-edit-mode)
  (pdf-virtual-view-mode)
  (current-buffer))

(defun org-ilm--pdf-open-page (pdf-path page-number buffer-name)
  "Open a virtual pdf buffer with a single page."
  (with-current-buffer (generate-new-buffer buffer-name)
    (insert ";; %VPDF 1.0\n\n")
    (insert "((\"" pdf-path "\" " (number-to-string page-number) " ))\n")
    (pdf-virtual-view-mode)
    (pop-to-buffer (current-buffer))
    (current-buffer)))


;;;; Convert

;; TODO off-load pages handling to here so that this function can be more usefull
(cl-defun org-ilm--pdf-convert-attachment-to-org (pdf-path pages org-id &key on-success on-error)
  "Convert attachment PDF to Md using Marker, then to Org mode using Pandoc."
  (org-ilm-convert--convert-to-org-with-marker-pandoc
   :process-id org-id
   :input-path pdf-path
   :new-name org-id 
   :pdf-pages pages
   :on-success on-success
   :on-error on-error))

(defun org-ilm-pdf-convert ()
  "Convert PDF to another format within the same attachment.

See also `org-ilm-pdf-convert-org-respect-area'."
  (interactive)
  (org-ilm--pdf-mode-assert)
  (unless (org-ilm--attachment-data)
    (user-error "Not in an attachment buffer."))
  (org-ilm--pdf-convert-transient))

(defun org-ilm--pdf-convert-transient-format ()
  (when-let* ((args (or (transient-get-value) (transient-args transient-current-command)))
              (format (car (remove "main" args))))
    (intern format)))

(transient-define-prefix org-ilm--pdf-convert-transient ()
  :refresh-suffixes t
  :value '("image")
  
  ["PDF Convert"
   ("f" "Format" "%s"
    :class transient-switches
    :transient transient--do-call
    :allow-empty nil
    :always-read t
    :choices ("image" "org" "text")
    :argument-format "%s"
    :argument-regexp "\\(\\(image\\|org\\|text\\)\\)")
   (:info
    (lambda ()
      (propertize
       (pcase (org-ilm--pdf-convert-transient-format)
         ('org "Convert to Org file with Marker")
         ('text "Extract text to Org file")
         ('image "Convert this page to an image")
         (_ ""))
       'face 'transient-key-noop))
    :if org-ilm--pdf-convert-transient-format)

   ("m" "Main" "main" :transient transient--do-call)
   (:info (propertize "Use as main attachment of this element" 'face 'transient-key-noop))
   ]
  
  [
   ("RET" "Convert"
    (lambda ()
      (interactive)
      (let* ((args (transient-args transient-current-command))
             (main-p (member "main" args))
             (format (org-ilm--pdf-convert-transient-format)))
        (org-ilm--pdf-convert format main-p)))
    :inapt-if-not org-ilm--pdf-convert-transient-format)
   ]
  )

(defun org-ilm--pdf-convert (format &optional as-main-p)
  "Convert current PDF buffer into FORMAT."
  (cl-assert (member format '(org text image)))
  (pcase-let* ((pdf-buffer (current-buffer))
               (`(,org-id ,collection, collection-file) (org-ilm--attachment-data))
               (headline (org-ilm--org-headline-element-from-id org-id))
               (num-pages (pdf-info-number-of-pages))
               ;; This will only return if in virtual page
               (`(,pdf-path ,_ ,region) (ignore-errors (pdf-virtual-document-page 1)))
               (pdf-path (or pdf-path buffer-file-name))
               (attach-dir (file-name-directory pdf-path))
               (out-path-format (expand-file-name (concat org-id ".%s") attach-dir))
               (on-success
                (lambda (ext)
                  (when as-main-p
                    (org-with-point-at headline
                      (org-entry-put nil org-ilm-property-ext ext))))))

    (pcase format
      ('text
       (with-temp-buffer
         (dolist (page (number-sequence 1 num-pages))
           (insert (pdf-info-gettext page '(0 0 1 1) nil pdf-buffer)))
         (write-region (point-min) (point-max) (format out-path-format "org")))
       (funcall on-success "org"))
      ('image
       ;; Note: Will convert current page only
       (let ((img-path (org-ilm--pdf-image-export org-id :dir attach-dir)))
         (funcall on-success (file-name-extension img-path))))
      ('org
       ;; Decide on whether to convert just the virtual pdf region or the entire
       ;; page.
       (if (and region (= 1 num-pages) org-ilm-pdf-convert-org-respect-area)
           (org-ilm--image-convert-attachment-to-org
            (org-ilm--pdf-image-export org-id :dir attach-dir)
            org-id
            :on-success
            (lambda (proc buf id) (funcall on-success)))
         (when (or (<= num-pages 3)
                   (yes-or-no-p (format "Convert %s pages to Org using Marker?" num-pages)))
           (org-ilm--pdf-convert-attachment-to-org
            pdf-path
            (if (= 1 num-pages)
                (1- (org-ilm--pdf-page-normalized))
              (cons (1- (org-ilm--pdf-page-normalized 1))
                    (1- (org-ilm--pdf-page-normalized num-pages))))
            org-id
            :on-success
            (lambda (proc buf id) (funcall on-success "org")))))))))

;;;; Extract

(defun org-ilm--pdf-extract-prompt-for-output-type (extract-option)
  "Prompt user to select output type given the extract option."
  (let ((options (seq-filter
                  (lambda (option)
                    (member (cdr option)
                            (assoc extract-option org-ilm--pdf-extract-option-to-types)))
                  (org-ilm--invert-alist org-ilm--pdf-output-types))))
    (if (= 1 (length options))
        (cdr (car options))
      (cdr (assoc (completing-read "Extract as: " options nil t) options)))))

(defun org-ilm-pdf-extract (extent &optional output-type)
  "Extract PDF pages, sections, region, and text."
  (interactive
   (list
    (if (pdf-view-active-region-p)
        'region
      (let* ((options (org-ilm--invert-alist org-ilm--pdf-extract-options)))
        (cdr (assoc (completing-read "Extract: " options nil t) options))))
    current-prefix-arg))
  (cl-assert (or (eq major-mode 'pdf-view-mode) (eq major-mode 'pdf-virtual-view-mode)))
  (cl-assert (memq extent '(page outline region section)))
  
  ;; (when (and output-type (called-interactively-p))
  (unless output-type
    (setq output-type (org-ilm--pdf-extract-prompt-for-output-type extent)))

  (unless output-type (setq output-type 'virtual))
  
  (pcase extent
    ('page (org-ilm-pdf-page-extract output-type))
    ('region (org-ilm-pdf-region-extract output-type))
    ('section (org-ilm-pdf-section-extract output-type))))

(defun org-ilm-pdf-page-extract (output-type)
  "Turn PDF page into an extract."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'page)))
  (map-let (:id :attach-dir :pdf-path :collection :headline) (org-ilm--pdf-data)
    (let ((current-page (pdf-view-current-page))
          (current-page-real (org-ilm--pdf-page-normalized))
          (extract-id (org-id-new)))

      (pcase output-type
        ('virtual
         (let ((excerpt (org-ilm--generate-text-snippet
                         (pdf-info-gettext current-page '(0 0 1 1)))))
           (org-ilm--capture-capture
            'material
            :parent id
            :title (format "Page %s%s" current-page-real
                           (if excerpt (concat ": " excerpt) ""))
            :props (list :ILM_PDF current-page-real))))
        ('text
         (let* ((text (pdf-info-gettext current-page '(0 0 1 1) nil)))
           (org-ilm--capture-capture
            'material
            :parent id
            :content text
            :props (list :ILM_PDF current-page-real)
            :ext "org")))
        ('image
         (let* ((img-path (org-ilm--pdf-image-export extract-id)))
           (org-ilm--capture-capture
            'material
            :parent id
            :file img-path
            :title (format "Page %s" current-page-real)
            :id extract-id
            :method 'mv
            :ext t
            :props (list :ILM_PDF current-page-real))))
        ('org
         (org-ilm--capture-capture
          'material
          :parent id
          :title (format "Page %s" current-page-real)
          :id extract-id
          :ext "org"
          :props (list :ILM_PDF current-page-real)
          :on-success
          (lambda (&rest _)
            (org-ilm--pdf-convert-attachment-to-org
             pdf-path
             (1- current-page-real)
             extract-id
             :on-success
             (lambda (proc buf id) (message "Conversion finished."))))))))))

;; TODO When extracting text, use add-variable-watcher to watch for changes in
;; pdf-view-active-region as it has no hooks. it allow buffer local and set only
;; (not let).
(defun org-ilm-pdf-region-extract (output-type &optional card-p)
  "Turn selected PDF region into an extract."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'region)))
  (cl-assert (pdf-view-active-region-p) nil "No active region")
  (when (and card-p (not (eq output-type 'virtual)))
    (error "Can only create virtual PDF card extracts"))
  
  (map-let (:id :attach-dir :pdf-path :collection :headline) (org-ilm--pdf-data)
    (let* ((extract-id (org-id-new))
           (current-page (pdf-view-current-page))
           (current-page-real (org-ilm--pdf-page-normalized))
           (region (org-ilm--pdf-active-region))
           (region-normalized (org-ilm--pdf-region-normalized region))
           (region-text (car (pdf-view-active-region-text)))
           (pdf-buffer (current-buffer))
           (title
            (format "Page %s region%s"
                    current-page-real
                    (if region-text
                        (concat ": " (org-ilm--generate-text-snippet region-text))
                      "")))
           (capture-data
            (list
             :title title
             :parent id
             :id extract-id
             ;; Add PDF region so that it can be rendered in pdf page even
             ;; when extracting as eg image
             :props (list :ILM_PDF (list current-page-real region-normalized))))
           capture-on-success)

      (when (org-ilm--pdf-region-below-min-size-p current-page region-normalized)
        (user-error "Region smaller than minimum size. Try extracting as text or image."))

      (pcase output-type
        ('virtual)
        ('text
         (setf (plist-get capture-data :content) region-text
               (plist-get capture-data :ext) "org"))
        ('image
         (let ((file (org-ilm--pdf-image-export extract-id :region region)))
           (setf (plist-get capture-data :file) file
                 (plist-get capture-data :method) 'mv
                 (plist-get capture-data :ext) t)))
        ('org
         (setf (plist-get capture-data :ext) "org")
         (setq capture-on-success
               (lambda ()
                 (org-ilm--image-convert-attachment-to-org
                  ;; TODO this shouldnt be the normalized region i think
                  (org-ilm--pdf-image-export
                   extract-id :region region-normalized :dir attach-dir)
                  extract-id))))
        (_ (error "Unrecognized output type")))

      ;; Temporary disable `org-link-parameters' which is an overkill way to
      ;; deal with the following problem. When org-pdftools is used,
      ;; `org-pdftools-setup-link' is called, which adds a custom link in
      ;; `org-link-parameters' that detects an org capture in a pdf buffer, and
      ;; then creates a text underline highlight automatically. This is
      ;; undesired, so this turns it off. The backtrace is:
      ;;   org-pdftools-store-link()
      ;;   org-link--try-link-store-functions(nil)
      ;;   org-store-link(nil)
      (let ((org-link-parameters nil))
        (apply #'org-ilm--capture-capture
               (if card-p 'card 'material)
               :on-success
               (lambda (&rest _)
                 (when capture-on-success
                   (funcall capture-on-success))
                 (org-ilm--pdf-selections 'reparse)
                 (pdf-view-deactivate-region)
                 (pdf-view-redisplay)
                 (when (eq major-mode 'pdf-virtual-view-mode)
                   (org-ilm-pdf-virtual-refresh)))
               capture-data)))))

(defun org-ilm-pdf-section-extract (output-type)
  "Extract current section of outline."
  (interactive (list (org-ilm--pdf-extract-prompt-for-output-type 'section)))

  (map-let (:id :attach-dir :pdf-path :collection :headline) (org-ilm--pdf-data)
    (let* ((extract-id (org-id-new))
           (pdf-buffer (current-buffer))
           title spec)

      (cond-let*
        ((org-ilm--pdf-interactive-capture-p)
         (setq spec (org-ilm--pdf-interactive-capture-spec)
               title (read-string "Title: ")))
        ([section (org-ilm--pdf-outline-page-sections)]
         (setq spec (alist-get 'spec section)
               title (alist-get 'title section)))
        (t (user-error "No outline or section data available")))

      ;; TODO When no title use page range
      (setq title (concat "Section: " title))

      (pcase output-type
        ('virtual
         (org-ilm--capture-capture
          'material
          :parent id
          :title title
          :props (list :ILM_PDF (org-ilm--pdf-spec-to-string spec)))
         (when (org-ilm--pdf-interactive-capture-p)
           (org-ilm--pdf-highlights 'reparse)
           (org-ilm--pdf-interactive-capture-reset)))
        ;; ('text
        ;;  ;; TODO This currently extracts the actual PDF data lol
        ;;  (with-temp-buffer
        ;;    (if (= (length range) 2)
        ;;        (insert (pdf-info-gettext (car range) (cdr range) nil pdf-buffer) "\n")
        ;;      (let* ((range-begin (car range))
        ;;             (range-end (cdr range))
        ;;             (page-begin (car range-begin))
        ;;             (page-end (car range-end)))
        ;;        (dolist (page (number-sequence page-begin page-end))
        ;;          (insert (pdf-info-gettext
        ;;                   page
        ;;                   (cond
        ;;                    ((= page page-begin) (cdr range-begin))
        ;;                    ((= page page-end) (cdr range-end))
        ;;                    (t '(0 0 1 1)))
        ;;                   'word pdf-buffer)
        ;;                  "\n"))))

        ;;    (org-ilm--capture-capture
        ;;     'material
        ;;     :parent id :content (buffer-string) :ext "org"
        ;;     :title title)))
        ('org
         (org-ilm--capture-capture
          'material
          :parent id
          :id extract-id
          :ext "org"
          :title title
          :on-success
          (lambda (&rest _)
            (org-ilm--pdf-convert-attachment-to-org
             pdf-path
             (cons (1- (alist-get 'page section))
                   (1- (alist-get 'next-page section)))
             extract-id
             :on-success
             (lambda (proc buf id) (message "Conversion finished."))))))))))




;;;; Split

(defun org-ilm-pdf-split ()
  "Split PDF outline items into a extracts."
  (interactive)
  (map-let (:id :attach-dir :pdf-path :collection :headline) (org-ilm--pdf-data)
    (let ((outline (org-ilm--pdf-outline-get)))
      (unless outline (error "No outline found"))
      (let ((depth (cdr
                    (org-ilm--select-alist
                     (seq-map
                      (lambda (section)
                        (cons
                         (format "%s%s"
                                 (make-string (1- (alist-get 'depth section)) ?\s)
                                 (alist-get 'title section))
                         (alist-get 'depth section)))
                      outline)
                     "Level on which to split: "
                     nil 'ordered)))
            (org-ilm-capture-show-menu nil))
        (dolist (section outline)
          (when (= depth (alist-get 'depth section))
            (org-ilm--capture-capture
             'material
             :parent id
             :title (concat "Section: " (alist-get 'title section))
             :props (list :ILM_PDF (org-ilm--pdf-spec-to-string (alist-get 'spec section))))))))))

;;;; Cloze

(defun org-ilm-pdf-cloze ()
  (interactive)
  (org-ilm-pdf-region-extract 'virtual 'card-p))

;;;; Interactive capture

;; Interactive capture means user builds up interactively what page (regions)
;; they want to extract. In addtion, multiple selections can be added to this
;; capture.
;; Keybindings:
;; - '[' -> Select current page as begin page
;;          Region active: Limit to region area
;;          Prefix: Point location defines area upper bound
;; - ']' -> Same as "[" but for end page
;; - '"' -> Add current selection regions to active selections

(defface org-ilm-pdf-interactive-capture-face
  '((t :background "grey"))
  "Face used to highlight interactive capture areas.")

(defvar-local org-ilm-pdf-interactive-capture nil
  "State of interactive capture.")

(defun org-ilm--pdf-interactive-capture-reset ()
  "Undo interactive capture selection."
  (setq-local org-ilm-pdf-interactive-capture nil)
  (when (org-ilm--pdf-mode-p)
    (pdf-view-redisplay)))

(defun org-ilm--pdf-interactive-capture-p ()
  "Return t if something has been selected."
  (and org-ilm-pdf-interactive-capture
       (not (cl-every #'null (map-values org-ilm-pdf-interactive-capture)))))

(defun org-ilm--pdf-interactive-capture-print-state ()
  "Print what is currently being considered for capture."
  (map-let (:begin-page :begin-top :begin-bottom :end-page :end-bottom)
      org-ilm-pdf-interactive-capture
    (cond
     (end-page
      (message "Pages %s to %s selected. C-g to reset." (or begin-page 1) end-page))
     (begin-page
      (message "Page %s selected. C-g to reset." begin-page))
     (t
      (message "Nothing selected")))))

(defun org-ilm--pdf-interactive-capture-page-highlights (&optional page)
  "Return interactive capture state of PAGE as `org-ilm-pdf-highlight' objects."
  (setq page (org-ilm--pdf-page-normalized page))
  (when (org-ilm--pdf-interactive-capture-p)
    (map-let ((:id el-id) (:type el-type)) org-ilm--data
      (map-let (:begin-page :begin-top :begin-bottom :end-page :end-bottom :selections)
          org-ilm-pdf-interactive-capture
        (cond-let*
          ([hls (seq-keep
                 (lambda (s)
                   (when (= page (car s))
                     (make-org-ilm-pdf-highlight
                      :el-id el-id :el-type el-type
                      :type 'selection :interactive-p t
                      :page page :rect (cdr s))))
                 selections)]
           hls)
          ((and begin-page (= page begin-page))
           (make-org-ilm-pdf-highlight
            :el-id el-id :el-type el-type
            :type 'area :interactive-p t
            :page page
            :rect (list 0 (or begin-top 0) 1 (or begin-bottom 1))))
          ((and end-page (<= (or begin-page 1) page end-page))
           (make-org-ilm-pdf-highlight
            :el-id el-id :el-type el-type
            :type 'area :interactive-p t
            :page page
            :rect (list 0 0 1 (or (when (= end-page page) end-bottom) 1)))))))))

(defun org-ilm--pdf-interactive-capture-spec ()
  "Return interactive capture state as `org-ilm-pdf-spec'."
  (when (org-ilm--pdf-interactive-capture-p)
    (map-let (:begin-page :begin-top :begin-bottom :end-page :end-bottom :selections)
        org-ilm-pdf-interactive-capture
      (let (spec-data)
        (when begin-page
          (push (append
                 `(,begin-page)
                 (when (or begin-top begin-bottom)
                   (list (list 0 (or begin-top 0) 1 (or begin-bottom 1))))
                 (seq-keep
                  (lambda (s) (when (= (car s) begin-page) (cdr s)))
                  selections)
                 )
                spec-data))
        (when end-page
          (push '- spec-data)
          (push (append
                 `(,end-page)
                 (when end-bottom (list (list 0 0 1 (or end-bottom 1)))))
                spec-data))
        (org-ilm--pdf-spec-parse (reverse spec-data))))))

(defun org-ilm-pdf-select-begin-page (arg)
  "Select current page as start point of capture.
With prefix arg, use mouse position as top cutoff point."
  (interactive "P")
  (let ((page (org-ilm--pdf-page-normalized))
        (bottom (plist-get org-ilm-pdf-interactive-capture :begin-bottom))
        top)
    
    (if arg
      (let ((pos (org-ilm--pdf-mouse-position-relative)))
        (setq top (cdr pos))
        (when (and bottom (> top bottom))
          (setq bottom nil)))
      (setq bottom nil))

    (map-let (:end-page :end-bottom) org-ilm-pdf-interactive-capture
      (when (and end-page (<= end-page page))
        (when (and (= end-page page) end-bottom (> end-bottom (or top 0)))
          (setq bottom end-bottom))
        (dolist (k '(:end-page :end-bottom))
          (setf (plist-get org-ilm-pdf-interactive-capture k) nil))))
    
    (setf (plist-get org-ilm-pdf-interactive-capture :begin-page) page
          (plist-get org-ilm-pdf-interactive-capture :begin-top) top
          (plist-get org-ilm-pdf-interactive-capture :begin-bottom) bottom)
    (pdf-view-redisplay)
    (org-ilm--pdf-interactive-capture-print-state)))

(defun org-ilm-pdf-select-end-page (arg)
  "Select current page as end point of capture.
With prefix arg, use mouse position as bottom cutoff point."
  (interactive "P")
  (let ((page (org-ilm--pdf-page-normalized))
        bottom)
    
    (when arg
      (let ((pos (org-ilm--pdf-mouse-position-relative)))
        (setq bottom (cdr pos))))

    (map-let (:begin-page :begin-top :begin-bottom) org-ilm-pdf-interactive-capture
      (cond
       ((null begin-page)
        (setf (plist-get org-ilm-pdf-interactive-capture :begin-page) (org-ilm--pdf-page-normalized 1)))
       ((< begin-page page)
        (setf (plist-get org-ilm-pdf-interactive-capture :begin-bottom) nil))
       ((or (> begin-page page)
            (and (= begin-page page)
                 (or (null bottom)))
                     (and begin-top (< bottom begin-top)))
        (setf (plist-get org-ilm-pdf-interactive-capture :begin-page) (org-ilm--pdf-page-normalized 1)
              (plist-get org-ilm-pdf-interactive-capture :begin-top) nil
              (plist-get org-ilm-pdf-interactive-capture :begin-bottom) nil))
       ((= begin-page page)
        (when (and bottom (> bottom (or begin-top 0)))
          (setf (plist-get org-ilm-pdf-interactive-capture :begin-bottom) bottom))
        (setq page nil
              bottom nil)))

      (setf (plist-get org-ilm-pdf-interactive-capture :end-page) page 
            (plist-get org-ilm-pdf-interactive-capture :end-bottom) bottom))
    (pdf-view-redisplay)
    (org-ilm--pdf-interactive-capture-print-state)))

(defun org-ilm-pdf-select-region ()
  ""
  (interactive)
  (unless (pdf-view-active-region-p)
    (user-error "No region selected"))
  (let ((page (org-ilm--pdf-page-normalized))
        (area (org-ilm--pdf-region-normalized)))
    (if pdf-view--have-rectangle-region
        (push (cons page area)
              (plist-get org-ilm-pdf-interactive-capture :selections))
      (let* ((page-rel (car pdf-view-active-region))
             (area-rel (cadr pdf-view-active-region))
            (rects (pdf-info-getselection
                    page-rel area-rel pdf-view-selection-style)))
        (dolist (rect rects)
          (push (cons page (org-ilm--pdf-region-normalized rect page-rel))
                (plist-get org-ilm-pdf-interactive-capture :selections))))  
      )))

(defun org-ilm--pdf-interactive-capure--hook ()
  (cond
   (org-ilm-global-minor-mode
    (define-key pdf-view-mode-map (kbd "[") #'org-ilm-pdf-select-begin-page)
    (define-key pdf-view-mode-map (kbd "]") #'org-ilm-pdf-select-end-page)
    (define-key pdf-view-mode-map (kbd "\"") #'org-ilm-pdf-select-region)
    (advice-add 'keyboard-quit :before #'org-ilm--pdf-interactive-capture-reset)
    )
   (t
    (define-key pdf-view-mode-map (kbd "[") nil)
    (define-key pdf-view-mode-map (kbd "]") nil)
    (define-key pdf-view-mode-map (kbd "\"") nil)
    (advice-remove 'keyboard-quit #'org-ilm--pdf-interactive-capture-reset)
    )))

(add-hook 'org-ilm-global-minor-mode-hook #'org-ilm--pdf-interactive-capure--hook)
    
;;; Footer

(provide 'org-ilm-pdf)

;;; org-ilm-pdf.el ends here
