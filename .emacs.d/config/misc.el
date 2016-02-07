;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Security stuff
(customize-set-variable 'tls-checktrust t)

;; Fonts
(custom-set-faces
  '(default ((t (:family "Hack" :foundry "bitstream" :slant normal :weight normal :height 96 :width normal)))))

;; Global key bindings

(define-key global-map (kbd "<f11>") 'calc)
;; (define-key global-map "\C-cl" 'org-store-link)
(define-key global-map (kbd "S-<f9>") 'org-pomodoro)
(define-key global-map (kbd "<f9>") 'org-clock-select-task)
(define-key global-map (kbd "S-<f12>") 'org-agenda)
(define-key global-map (kbd "<f12>") 'org-capture)

(global-set-key (kbd "<C-up>") 'enlarge-window)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)


;; Helps emacs to not break hard links
(setq backup-by-copying-when-linked t)

(column-number-mode)
(menu-bar-mode -1)


(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1))

;; Helps matching parenthesis (it highlights them)  ;; changed recently by corral
(show-paren-mode 1)

;; Mutes that goddamn bell
(setq visible-bell 1)

;; Selects the solarized light theme
;; there is something weird about the background, it loads a theme
;; then it loads solarized-light.  Otherwise, it is solarized-dark
;; (if window-system (load-theme 'wombat t) )
(setq calendar-latitude +21)
(setq calendar-longitude -101)
(require 'moe-theme)
(require 'moe-theme-switcher)
;; (if window-system (load-theme 'solarized-light t) )
;; (if window-system (load-theme 'moe-light t) )
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'solarized-dark t)

;; Tramp config files
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory "~/.tmp/tramp/")
(setq tramp-chunksize 2000)

;; Force w3m to show images
;; (setq w3m-default-display-inline-images t)

;; Disables toolbar-mode
;; (tool-bar-mode 1)

;; Show line numbers, I think
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f5>") 'linum-mode)

;; Highlight Columns
(global-set-key (kbd "C-<f6>") 'hl-line-mode)

;; Improve the buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; display “lambda” as “λ”
(global-prettify-symbols-mode 1)

;; disable common keybinding for closing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; put autosaves and backups in a better place
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; Something I modified for bookmarks - no idea what it does
(custom-set-variables '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"))

;; my position in the religious war
(setq-default indent-tabs-mode nil)

;; It makes helm easier to use
(global-set-key "\M- " 'hippie-expand)

;; Makes files with hasgbang executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Add autocompletition from company-mode
(company-mode)
(company-quickhelp-mode 1)
;; (define-key company-active-map (kbd "C-<f1>") 'company-show-doc-buffer)

;; Shows current time and date
;; (setq display-time-day-and-date t
;;       display-time-24hr-format t)
;; (display-time)

;; File extensions
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\rc\\'" . conf-unix-mode))
;; (add-to-list 'auto-mode-alist '("\.py\'" . python-mode))


;; Calendar stuff
(setq calendar-holidays holiday-mexican-holidays)

;; File size threshold
(setq large-file-warning-threshold 100000000)

;; Password cache threshold
(setq password-cache-expiry (* 60 60))
