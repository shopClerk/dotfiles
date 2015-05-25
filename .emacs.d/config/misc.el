;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helps emacs to not break hard links
(setq backup-by-copying-when-linked t)

(menu-bar-mode 1)

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1))

;; Helps matching parenthesis (it highlights them)
(show-paren-mode 1)

;; Mutes that goddamn bell
(setq visible-bell 1)

(custom-set-faces
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

;; Selects the solarized light theme
;; there is something weird about the background, it loads a theme
;; then it loads solarized-light.  Otherwise, it is solarized-dark
;; (if window-system (load-theme 'wombat t) )
(if window-system (load-theme 'solarized-light t) )
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

;; my position in the religious war
(setq-default indent-tabs-mode nil)

;; It makes helm easier to use
(global-set-key "\M- " 'hippie-expand)
