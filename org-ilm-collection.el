;;; org-ilm-collection.el --- Collection -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'cond-let)
(require 'org-attach)
(require 'org-ql)

(require 'org-ilm-core)
;; (require 'org-ilm-pqueue)
(require 'org-ilm-utils)
;; (require 'org-ilm-element)

;;;; Variables

(defcustom org-ilm-collection-default-targets
  '((concept  . (file "concepts.org"))
    (registry . (file "registry.org"))
    (import   . (file "ilm.org"))
    (bib      . (file "refs.bib")))
  "Alist mapping ilm things to default targets."
  :type '(alist :key-type symbol :value-type alist)
  :group 'org-ilm-collection)

(defcustom org-ilm-collections
  (let ((targets org-ilm-collection-default-targets))
    `((ilm :path     "~/ilm/"
           :bib      ,(alist-get 'bib targets)
           :concept  ,(alist-get 'concept targets)
           :registry ,(alist-get 'registry targets)
           :import   ,(alist-get 'import targets)
           ))) 
  "Alist mapping collection name symbol to its configuration plist.

Configuration properties:

`path' (required)

  The path to the collection (file or folder). Note that multiple collections
  can share a folder, in which case the concepts and registry are shared.

The following specify default targets, for which see
`org-ilm-collection-default-targets'.

`import'

  The default target for imports.

`concept'

  The deafult target for concepts.

`registry'

  The default target for registry items.

`bib'

  The default target for bibtex entries."
  :type '(alist :key-type symbol :value-type alist)
  :group 'org-ilm-collection)

(defcustom org-ilm-custom-collections nil
  "Collections saved by org-ilm automatically."
  :type '(alist :key-type symbol :value-type alist)
  :group 'org-ilm-collection)

;;;; Collection

(defun org-ilm-collections (&optional kind)
  "Return alist of collection symbol -> data alist."
  (pcase kind
    ('set org-ilm-collections)
    ('custom org-ilm-custom-collections)
    (_ (append org-ilm-collections org-ilm-custom-collections))))

(defun org-ilm--collection-property (collection property &optional kind)
  "Return PROPERTY of COLLECTION."
  (when-let ((config (alist-get collection (org-ilm-collections kind))))
    (cond-let*
      ;; When the property points to a target, we need to convert it to absolute path.
      ([key (intern (substring (symbol-name property) 1))]
       [default-target (alist-get key org-ilm-collection-default-targets)]
       (let ((target (copy-sequence (or (plist-get config property) default-target)))
             (col-path (plist-get config :path)))
         (when (and (eq (car target) 'file) (not (f-absolute-p (nth 1 target))))
           (if (f-file-p col-path)
               (error "Collection \"%s\" is a single file, so target \"%s\" must be an absolute path" collection target)
             (setf (nth 1 target) (expand-file-name (nth 1 target) col-path))))
         target))
      ;; When property is path, convert to absolute path
      ((eq property :path)
       (expand-file-name (plist-get config :path)))
      (t 
       (plist-get config property)))))

(defun org-ilm--collection-path (collection)
  "Return path of COLLECTION."
  (org-ilm--collection-property collection :path))

(defun org-ilm-new-collection (name path)
  "Create a new collection."
  (interactive
   (list
    (intern (read-string "Name: "))
    (read-directory-name "Path: " (org-ilm--collection-property (org-ilm--active-collection) :path))))
  (cond
     ((alist-get name org-ilm-collections)
      (user-error "Collection already exists with this name in org-ilm-collections, which cannot be overwritten automatically."))
     ((alist-get name org-ilm-custom-collections)
      ;; TODO Option to overwrite. Need to delete old pqueue, maybe more?
      (user-error "Collection with name %s already exists." name))
     (t
      (setf (alist-get name org-ilm-custom-collections) `(:path ,path))
      (customize-save-variable 'org-ilm-custom-collections org-ilm-custom-collections))))

(defun org-ilm-delete-collection (collection)
  "Delete a custom collection."
  (interactive
   (list
    (intern (completing-read "Delete collection: " org-ilm-custom-collections))))
  (let ((data-dir (org-ilm--collection-data-dir collection)))
    (if data-dir
      (let ((delete-by-moving-to-trash t))
        (delete-directory data-dir 'recursive 'trash))
      (warn "Data directory not found for collection %s" collection))
    (setf (alist-get collection org-ilm-custom-collections nil 'remove) nil)
    (customize-save-variable 'org-ilm-custom-collections org-ilm-custom-collections)
    (setf (alist-get collection org-ilm--pqueues nil 'remove) nil)
    (message "Delete collection %s" collection)))

(defvar org-ilm--active-collection nil
  "The current collection that is active.")

(defun org-ilm--active-collection ()
  "Return or infer the active collection. If missing, prompt user."
  (cond-let*
    ([collection (org-ilm--collection-from-context)]
     (setq org-ilm--active-collection collection))
    ([collection org-ilm--active-collection]
     collection)
    ([collection (org-ilm--select-collection)]
     (setq org-ilm--active-collection collection))))

(defun org-ilm--collection-first-valid (&optional collections)
  "Goes up the ilm element hierarchy until it finds the first valid collection property."
  (let ((collections (or collections (mapcar #'car (org-ilm-collections)))))
    (org-element-lineage-map (org-element-at-point)
        (lambda (el)
          (-some--> (org-entry-get el org-ilm-property-collection)
            intern
            (and (member it collections) it)))
      '(headline org-data)
      'with-self 'first-match)))

(defun org-ilm--collection-from-context ()
  "Infer collection based on the current buffer."
  (let ((location (org-ilm--where-am-i)))
    (pcase (car location)
      ('collection (cdr location))
      ('attachment (nth 2 location))
      ('queue (cadr location)))))

(defun org-ilm--collection-data-dir (collection)
  "Return the directory path where the data of COLLECTION is stored."
  (when-let* ((file-or-dir (org-ilm--collection-path collection))
              (parent-dir (if (f-file-p file-or-dir)
                              org-ilm-data-dir
                            (expand-file-name ".ilm/collections/" file-or-dir))))
    (let ((dir (expand-file-name (format "%s/" collection) parent-dir)))
      (make-directory dir 'parents)
      dir)))

(defun org-ilm--collection-attachment-dir (collection &optional id create-p)
  "Directory of where the attachments of COLLECTION are stored.
With ID return the path of element with the ID."
  (when-let* ((file-or-dir (org-ilm--collection-path collection))
              (dir (if (f-file-p file-or-dir)
                       (expand-file-name (format "%s/" collection) org-ilm-data-dir)
                     (expand-file-name ".ilm/attach/" file-or-dir))))
    (make-directory dir 'parents) ;; always make parent dir
    (when id
      (setq dir (expand-file-name id dir))
      (when create-p (make-directory dir 'parents)))
    dir))

(defun org-ilm--collection-single-file-p (collection)
  "Return t if COLLECTION is a single file collection.
Raise error if neither file nor folder."
  (let ((path (org-ilm--collection-path collection)))
    (if path
        (f-file-p path)
      (error "No path found for collection %s" collection))))

(defun org-ilm--collection-registry-path (&optional collection)
  "Return path to registry of COLLECTION."
  (when-let* ((collection (or collection (org-ilm--active-collection)))
              (target (org-ilm--collection-property collection :registry))
              (buf (org-ilm--org-capture-target-buffer target))
              (path (buffer-file-name buf)))
    path))

(defun org-ilm-bibliography ()
  (interactive)
  (if-let* ((target (org-ilm--collection-property (org-ilm--active-collection) :bib))
            (buf (org-ilm--org-capture-target-buffer target))
            (path (buffer-file-name buf)))
      (let ((key (ignore-errors (car (org-ilm--org-mem-cite-refs)))))
        (with-current-buffer (find-file path)
          (when key 
            (goto-char (point-min))
            (when (re-search-forward key nil t)
              (beginning-of-line)))))
    (user-error "Path to bibliography not found")))

(defun org-ilm--collection-file (&optional file collection kind)
  "Return list of collections of which FILE belongs to.

A collection symbol COLLECTION can be passed to test if file belongs to that
 collection.
When FILE is nil, file of current buffer."
  (when-let ((file (or file (buffer-file-name (buffer-base-buffer))))
             (path (expand-file-name file))
             (test (lambda (f place)
                     (if (f-directory-p place)
                         (and
                          (file-in-directory-p f place)
                          (not (file-in-directory-p f org-attach-id-dir)))
                       (file-equal-p f place)))))
    (if collection
        (funcall test path (org-ilm--collection-path collection))
      (seq-keep
       (lambda (c)
         (when (funcall test path (plist-get (cdr c) :path)) (car c)))
       (org-ilm-collections kind)))))

(defun org-ilm--select-collection (&optional file kind)
  "Prompt user for collection to select from.
With FILE, limit collections to those valid in file (see :path property
in `org-ilm-collections')."
  (let ((collections (if file
                         (org-ilm--collection-file file nil kind)
                       (mapcar #'car (org-ilm-collections kind)))))
    (car 
     (org-ilm--select-alist
      (mapcar
       (lambda (collection)
         (cons collection (org-ilm--collection-property collection :path kind)))
       collections)
      "Collection: "))))

(defun org-ilm--collection-files (collection)
  "Return all collection org files that belong to COLLECTION."
  (let ((file-or-dir (org-ilm--collection-path collection)))
    (cond
     ((f-file-p file-or-dir)
      (list (expand-file-name file-or-dir)))
     ((f-dir-p file-or-dir)
      ;; TODO Skip registry.org
      (directory-files file-or-dir 'full "^[^.].*\\.org$"))
     (t (error "No files in collection %s" collection)))))

(defun org-ilm--select-collection-file (collection &optional relative-p prompt)
  "Prompt user to select a file from COLLECTION.
With RELATIVE-P non-nil, return path truncated relative to collection directory."
  (cl-assert (assoc collection (org-ilm-collections)))
  (let ((col-path (org-ilm--collection-path collection))
        (files (org-ilm--collection-files collection))
        file)
    (cond
     ((= 1 (length files))
      (setq file (car files)))
     ((> (length files) 1)
      ;; TODO Make it work with nested files
      (setq file (expand-file-name
                  (completing-read
                   (or prompt "Collection file: ")
                   (mapcar (lambda (f)
                             (file-relative-name f col-path))
                           files)
                   nil 'require-match)
                  col-path))))
    (when file
      (if relative-p (file-relative-name file col-path) file))))

(defun org-ilm--collection-entries (collection)
  "All org-mem entries in COLLECTION."
  (seq-filter
   (lambda (entry)
     (member (org-ilm--element-type entry) '(material card)))
   (org-mem-entries-in-files (org-ilm--collection-files collection))))

(defun org-ilm--collection-tags (collection)
  "List of tags used in COLLECTION."
  (cl-remove-duplicates
   (apply #'append
          (mapcar #'org-mem-entry-tags
                  (org-ilm--collection-entries collection)))
   :test #'string=))

;;;; Annotations

;; When in a collection org file, add "annotations" to element headlines:
;; - Eldoc doc in echo area when point is on element
;; - TODO Overlays next to element headlines

(defun org-ilm--collection-eldoc-element-info (callback)
  "Return info about the element at point."
  (when-let* ((element (ignore-errors (org-ilm--element-from-context)))
              (_ (member (org-ilm-element--type element) '(material card)))
              (doc ""))

    (setq doc (concat doc
                      "🚩 "
                      ;; "P: "
                      (org-ilm-element--priority-formatted element)))
    (when-let ((schedrel (org-ilm-element--schedrel element)))
      (setq doc (concat doc 
                        " 📅 "
                        ;; " S: "
                        (org-ilm--format-relative-date
                         (round schedrel)))))


    ;; Concepts
    (let ((ids (car (org-ilm-element--concepts element)))
          (n-direct (cdr (org-ilm-element--concepts element))))
      (when ids
        (setq doc (concat
                   doc
                   " 💡 "
                   ;; " C: "
                   (string-join
                    (seq-map-indexed 
                     (lambda (id i)
                       (when-let ((entry (org-mem-entry-by-id id)))
                         (org-ilm--org-mem-title-full entry)))
                     ids)
                    ", ")))))

    (funcall callback doc))
  t)

(defun org-ilm--collection-prepare-buffer ()
  (when (org-ilm--collection-file)
    ;; (add-hook 'eldoc-documentation-functions
    ;;           #'org-ilm--collection-eldoc-element-info
    ;;           nil 'local)
    ))

;;; Footer

(provide 'org-ilm-collection)

;;; org-ilm-collection.el ends here
