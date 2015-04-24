(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/school.org" 
                             "~/org/misc.org"))
