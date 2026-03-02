;;; org-ilm-capture.el --- Capture -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-attach)
(require 'org-node)
(require 'transient)
(require 'ts)

(require 'org-ilm-core)
(require 'org-ilm-utils)
(require 'org-ilm-pqueue)
(require 'org-ilm-element)
(require 'org-ilm-collection)
(require 'org-ilm-bqueue-view)
(require 'org-ilm-priority)
(require 'ost)

;;;; Capture

;; Logic related to creating child elements, i.e. extracts and cards.

(defcustom org-ilm-capture-show-menu t
  "When creating an extract or card, always show a menu to configure it
 first, rather than with a prefix argument."
  :type 'boolean
  :group 'org-ilm)

(defvar org-ilm-capture-hook nil
  "Hook run after element was captured.
Arguments are: type, id, collection, parent-id")

;;;;; Capture

(cl-defstruct (org-ilm-capture
               (:conc-name org-ilm-capture--))
  "Data for a capture (material or card)."
  (id (org-id-new))
  (title "")
  (type nil :documentation "Should be one of `org-ilm-element-types'.")
  (parent nil :documentation "Parent org-ilm element.
If specified this is a child element, otherwise this is 
and either COLLECTION or TARGET must be set.")
  (collection nil :documentation "Collection symbol.
When omitted and PARENT specified, use PARENT's collection.")
  (target nil :documentation "Target specifier as in org-capture.
Ignored when PARENT is specified.")
  (file nil :documentation "Path to file that will be used as main attachment.")
  (attach-method 'cp :documentation "How to import FILE. See `org-attach-method'.")
  (ext nil :documentation "The extension of the main attachment file.
This needs to be specified when an element has multiple attachments with
the same ID name, or when CONTENT is specified. When set to t, FILE is
used to infer the extension. When specified, the extension will be set
in the ILM_EXT property.")
  (content nil :documentation "String with attachment contents.
Assumes Org unless EXT is specified.")
  (bibtex nil :documentation "Bibtex string.")
  (concepts nil :documentation "Concepts of element.
For value see `org-ilm--concept-property-prepare'.")
  props
  priority
  scheduled
  template
  bqueue
  state
  jump-p
  on-success
  on-abort
  capture-kwargs)

(defun org-ilm-capture-ensure (&rest data)
  "Parse plist DATA as org-ilm-capture object, or return if it already is
one. Makes sure to correct and process and validate the contents.

TYPE should be one of `org-ilm-element-types'.

PARENT should be the org-id of the parent element if the capture
represents an extract or a card generated from a material. If omitted,
the capture is implied to be a new element (import) without parent
element. In that case, COLLECTION or TARGET must be set.

TARGET can optionally be set to be the path of the collection file in
which to store the element. If relative file path to the collection,
COLLECTION must be set. If absolute file path, collection can be
inferred.

DATA is a plist that contains info about the capture entry.

The callback ON-SUCCESS is called when capture is saved.

The callback ON-ABORT is called when capture is cancelled."
  ;; A bit of a hack: Want to do the validation regardless if passing data as
  ;; plist or a single org-ilm-capture object. So: start by making an invalid
  ;; org-ilm-capture object and immediately unpack it for processing.
  (with-slots
      (type parent title id ext content props file attach-method
            priority scheduled template collection state bibtex jump-p
            target bqueue concepts on-success on-abort capture-kwargs)

      (if (and (= 1 (length data)) (org-ilm-capture-p (car data)))
          (car data)
        (apply #'make-org-ilm-capture data))

    (cl-assert (member type '(material card)))
    (unless id (setq id (org-id-new)))

    ;; See `org-attach-method'
    (unless attach-method (setq attach-method 'cp))
    (cl-assert (member attach-method '(mv cp ln lns)))

    ;; Target, parent, and collection
    (cond
     ((org-ilm-element-p parent)
      ;; Determine collection. Note that TARGET is ignored as PARENT specifies
      ;; this.
      (unless collection (setq collection (oref parent collection)))

      ;; If no priority given, decide based on parent
      (unless priority
        (let* ((parent-p (org-ilm--pqueue-priority
                          (oref parent id) nil 'nil-if-absent collection))
               (p (org-ilm--priority-calculate-child-priority
                   parent-p collection type)))
          (when p (setq priority p)))))
     (parent (error "PARENT must be of type `org-ilm-element'"))
     (target
      (let* ((target-buf (org-ilm--org-capture-target-buffer target))
             (target-path (buffer-file-name target-buf)))
        (if collection
            (cl-assert (org-ilm--collection-file target-path collection)
                       nil "TARGET not in COLLECTION")
          (let ((collections (org-ilm--collection-file target-file)))
            (pcase (length collections)
              (0 (error "No collcetion specified or found for target %s" target))
              (1 (setq collection (car collections)))
              (_ (error "Target %s may belong to one of multiple collections: %s" collections)))))))
     (t
      (cl-assert (assoc collection (org-ilm-collections)) nil
                 "COLLECTION must be a valid if no PARENT or TARGET given, got %s"
                 collection)
      (setq target (org-ilm--collection-property collection :import))))

    ;; Make sure bqueue is valid
    (when bqueue
      (cl-assert (org-ilm-bqueue-p bqueue))
      (if collection
          (cl-assert (eq collection (oref bqueue collection)))
        (setq collection (oref bqueue collection))))

    ;; Make sure there is a collection
    (setq collection (or collection (org-ilm--active-collection)))

    ;; Priority. We always need a priority value.
    (let ((pqueue (org-ilm--pqueue collection)))
      (setq priority
            (if priority
                (ost-tree-position pqueue priority)
              (org-ilm--bqueue-select-read (org-ilm-pqueue--bqueue pqueue) nil nil 1))))

    ;; Schedule date. Always needed.
    (setq scheduled
          (cond
           ((ts-p scheduled)    scheduled)
           ((stringp scheduled) (ts-parse scheduled))
           (scheduled           (error "scheduled unrecognized"))
           (t (org-ilm--element-first-interval type collection priority))))

    ;; Generate title from content if no title provided
    (when (and (not title) content)
      (setq title (org-ilm--generate-text-snippet content)))

    ;; Set extension from file when set to t
    (when (eq ext t)
      (if file
          (setq ext (file-name-extension file))
        (error "Cannot infer extension when no file provided (:ext=t)")))

    ;; When no file but content, place content in tmp file for import
    (when (and (not file) content)
      (setq file (expand-file-name
                  (format "%s.%s" id (or ext "org"))
                  temporary-file-directory)
            attach-method 'cp))

    ;; TODO If concepts, already process them here into props using
    ;; org-ilm--concept-property-prepare so that if there is an error, it
    ;; happens already at this point.

    (make-org-ilm-capture
     :parent parent :target target :type type :title title :id id :ext ext
     :content content :props props :file file :attach-method attach-method
     :priority priority :scheduled scheduled :template template
     :concepts concepts :jump-p jump-p
     :state state :on-success on-success :on-abort on-abort :bqueue bqueue
     :bibtex bibtex :collection collection :capture-kwargs capture-kwargs)))

(defun org-ilm--capture (&rest data)
  "Make an org capture to make a new source heading, extract, or card.

For type of arguments DATA, see `org-ilm-capture-ensure'"
  (let* ((capture    (apply #'org-ilm-capture-ensure data))
         (id         (org-ilm-capture--id capture))
         (type       (org-ilm-capture--type capture))
         (collection (org-ilm-capture--collection capture))
         (scheduled  (org-ilm-capture--scheduled capture))
         (priority   (org-ilm-capture--priority capture))
         (template   (org-ilm-capture--template capture))
         (parent     (org-ilm-capture--parent capture))
         target
         capture-kwargs
         attach-dir) ; Will be set in :hook, and passed to on-success

    (cl-assert (org-ilm-capture-p capture))

    ;; Capture target. If parent is specified, target is replaced with parent
    ;; org id.
    (if parent
        ;; Originally used the built-in 'id target, however for
        ;; some reason it sometimes finds the wrong location which
        ;; messes everything up. I noticed this behavior also with
        ;; org-id-find and such. I should probably find the root
        ;; cause, but org-node finds location accurately so i rely
        ;; on it instead to find the location of a heading by id.
        ;; `(id ,parent)
        (let ((parent-id (copy-sequence (oref parent id))))
          (setq target (list 'function
                             (lambda ()
                               (org-node-goto-id parent-id)
                               (org-back-to-heading)))))
      (setq target (org-ilm-capture--target capture)))
    
    ;; Save content in a file if provided.
    (when-let ((content (org-ilm-capture--content capture)))
      (write-region content nil (org-ilm-capture--file capture)))

    ;; If no explicit template is set, build it from title and state
    (unless template
      (let ((title (org-ilm-capture--title capture))
            (state (org-ilm-capture--state capture)))
        (setq template
              (format "* %s%s%s"
                      (if state (concat state " ") "")
                      (or title "")
                      "%?"))))

    (setq capture-kwargs
          (list
           :hook
           (lambda ()
             ;; Set attach dir which will be passed to on-success
             ;; callback. Has to be done in the hook so that point is on
             ;; the headline, and respects file-local or .dir-locals
             ;; `org-attach-id-dir'.
             ;; TODO Should do a check that the top parent dir has DIR property,
             ;; otherwise this fails.
             (setq attach-dir (if parent
                                  (expand-file-name (org-attach-dir))
                                (org-ilm--collection-attachment-dir collection id)))
             
             ;; If this is an import header where the attachments will live, we
             ;; need to set the DIR property, otherwise for some reason
             ;; org-attach on children doesn't detect that there is a parent
             ;; attachment header, even with a non-nil
             ;; `org-attach-use-inheritance'.
             (unless parent
               (org-entry-put
                nil "DIR"
                (file-relative-name
                 attach-dir
                 (file-name-directory (org-ilm--collection-path collection)))))

             ;; Every element requires id and type properties.
             (org-entry-put nil "ID" id)
             (org-entry-put nil org-ilm-property-type (symbol-name type))
             ;; Also trigger org-mem to update cache
             ;; (save-buffer)
             ;; (org-mem-updater-ensure-id-node-at-point-known)

             ;; Set collection property if it does not match the inherited property.
             (unless (eq collection (-some-> (org-entry-get nil org-ilm-property-collection 'inherit) intern))
               (org-entry-put nil org-ilm-property-collection (symbol-name collection)))

             ;; Set concept property.
             (when-let ((concepts (oref capture concepts)))
               (org-entry-put nil "CONCEPTS+"
                              (org-ilm--concept-property-prepare concepts)))

             ;; Add id to priority queue. An abort is detected in
             ;; `after-finalize' to remove the id.
             (org-ilm--pqueue-insert id priority collection)

             ;; Attachment extension if specified
             (when-let ((ext (org-ilm-capture--ext capture)))
               (org-entry-put nil org-ilm-property-ext ext))

             ;; Additional properties
             (when-let ((props (org-ilm-capture--props capture)))
               (cl-loop for (p v) on props by #'cddr
                        do (org-entry-put
                            nil
                            (if (stringp p) p (substring (symbol-name p) 1))
                            (format "%s" v))))

             ;; Schedule in the org heading
             (org-schedule nil (org-ilm--element-format-scheduled type scheduled))

             ;; Log to drawer and other preparations
             (org-ilm--element-prepare-new type collection priority scheduled)

             ;; Mark headline title so it can be edited faster
             (goto-char (point-min))
             (if (re-search-forward org-complex-heading-regexp nil t)
                 (when-let ((beg (match-beginning 4))
                            (end (match-end 4)))
                   (set-mark beg)
                   (goto-char end))
               (end-of-line))

             ;; Save buffer to let org-mem detect node. This allows us org-ilm
             ;; actions like setting concepts. When aborted, org-capture undoes
             ;; the changes in the target file.
             (save-buffer)
             
             ) ; end :hook lambda

           ;; Hook that is run right before a capture process is finalized. The
           ;; capture buffer is still current when this hook runs and it is
           ;; widened to the entire buffer.
           :before-finalize
           (lambda ()
             ;; Attach the file in the org heading attach dir
             (when-let ((file   (org-ilm-capture--file capture))
                        (attach-method (org-ilm-capture--attach-method capture)))
               ;; Turn of auto tagging if not import source.
               (let ((org-attach-auto-tag (if parent nil org-attach-auto-tag)))
                 (org-attach-attach file nil attach-method)

                 ;; Make sure the file name is the org-id
                 (rename-file
                  (expand-file-name (file-name-nondirectory file) attach-dir)
                  (expand-file-name (concat id "." (file-name-extension file))
                                    attach-dir)
                  'ok-if-already-exists)))

             ) ; end :before-finalize lambda

           ;; Hook that is run right after a capture process is finalized.
           :after-finalize
           (lambda ()
             ;; Deal with success and aborted capture. This can be detected in
             ;; after-finalize hook with the `org-note-abort' flag set to t in
             ;; `org-capture-kill'.
             (if org-note-abort
                 (progn
                   (org-ilm--pqueue-remove id collection)
                   (when-let ((on-abort (org-ilm-capture--on-abort capture)))
                     (funcall on-abort)))

               ;; Store bibtex
               (when-let ((bibtex (org-ilm-capture--bibtex capture))
                          (bib-target (org-ilm--collection-property collection :bib)))
                 (org-ilm--org-capture-programmatic bib-target bibtex 'plain))
               
               (when org-ilm-update-org-mem-after-capture
                 (org-ilm--org-mem-update-cache-after-capture 'entry))
               (run-hook-with-args 'org-ilm-capture-hook
                                   type id collection parent)
               
               (when-let ((bqueue (org-ilm-capture--bqueue capture)))
                 ;; TODO Do this in seperate thread?
                 (org-ilm--org-mem-ensure)
                 (if-let ((element (org-ilm--element-by-id id)))
                     (org-ilm-bqueue--insert bqueue element)
                   (warn "Could not add captured element to queue, not found: %s" id))) 

               (when-let ((on-success (org-ilm-capture--on-success capture)))
                 (dolist (callback (ensure-list on-success))
                   (funcall callback id attach-dir collection)))

               (when (oref capture jump-p)
                 (org-ilm--org-goto-id id))
               )

             ) ; end :after-finalize lambda
           
           ;; By default the capture buffer is not shown, but this can be
           ;; overwritten in the `capture-kwargs' slot in DATA.
           :immediate-finish t))

    (cl-letf (((symbol-value 'org-capture-templates)
               (list
                (append
                 (list "i" "Import" 'entry target template)
                 (org-combine-plists
                  capture-kwargs
                  (org-ilm-capture--capture-kwargs capture))))))
      (org-capture nil "i"))))

(defun org-ilm--capture-capture (type &rest data)
  (let ((immediate-p (if org-ilm-capture-show-menu
                         current-prefix-arg
                       (not current-prefix-arg)))
        (capture (apply #'org-ilm-capture-ensure
                        (map-merge 'plist data (list :type type)))))
    (if immediate-p
        (org-ilm--capture capture)
      (org-ilm--capture-transient capture))))

;;; Footer

(provide 'org-ilm-capture)

;;; org-ilm-capture.el ends here
