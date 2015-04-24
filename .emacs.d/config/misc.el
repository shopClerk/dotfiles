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
(if window-system (load-theme 'solarized t) )
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

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
(ido-mode 1)
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Usea the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;; Show line numbers, I think
(autoload 'linum-mode "linum" "toggle line numbers on/off" t) 
(global-set-key (kbd "C-<f5>") 'linum-mode)

;; Improve the buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
