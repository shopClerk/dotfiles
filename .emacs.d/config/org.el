;; force UTF
;; (setq org-export-coding-system 'utf8)


;; files

(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

(setq org-directory "~/org")
(setq org-agenda-files (list "~/org/todo.org"
                             ;; "~/org/work.org"
                             ;; "~/org/work.org"
                             "~/org/school.org"
                             ;; "~/org/home.org"
                             ;; "~/org/tech.org"
                             ;; "~/org/personal.org"
                             ))

;; agenda configs
(setq org-agenda-include-diary t)

;; MobileOrg
(setq org-mobile-directory "~/Utilities/Backup/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))


;; todo config
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "HOLD(h!)" "|" "CANCELLED(c@/!)"))))

;; (setq org-todo-keyword-faces
;;       (quote (("TODO" :foreground "#fdf6e3" :background "#dc322f" :weight bold)
;;               ("NEXT" :foreground "#fdf6e3" :background "blue" :weight bold)
;;               ("DONE" :background "#fdf6e3" :foreground "forest green" :weight bold)
;;               ("WAITING" :foreground "#fdf6e3" :background "orange" :weight bold)
;;               ("HOLD" :background "#fdf6e3" :foreground "magenta" :weight bold)
;;               ("CANCELLED" :background "#fdf6e3" :foreground "forest green" :weight bold)
;;               )))

;; clock config
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;;;;;;;;;;;;;;;;;;; Refile config

;; Targets any file in the agenda with exactly one level
(customize-set-variable 'org-refile-targets (quote ((org-agenda-files :level . 1))))

;; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)

;; Targets complete
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; capture templates
(customize-set-variable 'org-capture-templates
                        (list
                         '("t" "todo" entry
                           (file "~/org/capture.org")
                           ;; "* TODO %?\n %U\n %a")
                           "* TODO %?\n")
                         '("j" "Journal" entry
                           (file+datetree "~/org/journal.org")
                           (file "~/.emacs.d/org-templates/journal_template.org"))
                         '("i" "Idea" entry
                           (file+datetree "~/org/ideas.org")
                           (file "~/.emacs.d/org-templates/buying_template.org"))
                         '("w" "Buying list" entry
                           (file+datetree "~/org/buying.org")
                           (file "~/.emacs.d/org-templates/buying_template.org"))
                         ))

;; (setq org-ditaa-jar-path "~/Utilities/gits/org-mode/contrib/scripts/ditaa.jar")
;; (setq org-ditaa-jar-path "/usr/share/ditaa.jar")

(org-babel-do-load-languages 'org-babel-load-languages
                             '((ditaa . t)
                               (ruby . t)
                               (sh . t)
                               ))
