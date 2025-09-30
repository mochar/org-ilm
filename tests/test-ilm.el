;;; -*- lexical-binding: t; -*-
(require 'org-ilm)
(require 'org-id)
(require 'org-mem)

(setq org-ilm-test-dir (expand-file-name "org-ilm-tests/" temporary-file-directory))
(make-directory org-ilm-test-dir t)
(setq org-mem-watch-dirs (list org-ilm-test-dir (expand-file-name ".")))
(setq org-mem-do-sync-with-org-id t)
(org-mem-updater-mode)
(org-mem-reset 'takeover)

(describe
 "org-ilm"

 (it "dun"
     
     (with-current-buffer (find-file-noselect (file-name-concat org-ilm-test-dir "a.org"))
       (org-mode)
       (erase-buffer)
       
       (insert "
* INCR a
SCHEDULED: <2025-01-01 Wed>
:PROPERTIES:
:ID: a
:END:
")
       (save-buffer)

       (org-mem-reset 'takeover)
       (message "%s" (org-mem-all-ids))
       
       (goto-char (point-min))
       (org-next-visible-heading 1)
       (message "%s" (org-node-at-point))
       (expect (org-id-get) :to-equal "a")
       
       ;; (let ((el (org-ilm-element-at-point)))
         ;; (expect (org-ilm-element-id el) :to-equal "a"))

       )

     )
 )

