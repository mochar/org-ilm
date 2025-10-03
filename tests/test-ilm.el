;;; -*- lexical-binding: t; -*-
(require 'org-ilm)
(require 'org-id)
(require 'org-mem)

(setq org-ilm-tmp-dir (expand-file-name "org-ilm-tests/" temporary-file-directory))
(make-directory org-ilm-tmp-dir t)
(setq org-mem-watch-dirs (list org-ilm-tmp-dir (expand-file-name ".")))
(setq org-mem-do-sync-with-org-id t)
(org-mem-updater-mode)
(setq org-ilm-test-dir load-file-name)

(describe
 "org-ilm"

 (before-each
  (delete-directory org-ilm-tmp-dir t)
  (make-directory org-ilm-tmp-dir t)
  (copy-file (expand-file-name "data.org" (file-name-directory org-ilm-test-dir))
             (file-name-concat org-ilm-tmp-dir "data.org")))

 (it "dunno"     
     (with-current-buffer (find-file-noselect (file-name-concat org-ilm-tmp-dir "data.org"))
       (org-mem-reset 'takeover)
       (org-mem-await "" 10)
       
       (goto-char (point-min))
       (org-next-visible-heading 1)
       
       (let ((el (org-ilm-element-at-point)))
         (expect (org-ilm-element-id el) :to-equal "a"))

       )

     )
 )

