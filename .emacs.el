;; UTF-8 support
;; (set-language-environment "UTF-8")
(setenv "LANG" "en_CA.UTF-8")
(setenv "LC_ALL" "en_CA.UTF-8")


;; Better remapping for some keys
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-? A v") 'apropos-variable)
(global-set-key (kbd "C-? A u") 'apropos-user-option)
(global-set-key (kbd "C-? A d") 'apropos-documentation)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)


;; Swap “C-t” and “C-x”, so it's easier to type on Dvorak layout
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

;; Fixes a bug with the keyboard
(require 'iso-transl)

;; Loads SLIME
(load (expand-file-name "~/.local/share/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; Dunno, does something so you can open files in an open session of emacs
(load "server")
;; require 'server)
(unless (server-running-p)
  (server-start))
;; (gnuserv-start)
;; (server-start)

;;; Function for loading all ".el" files under ~/.emacs.d/config directory.
(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))


;; Org-mode is weird and needs to be installed by hand
(add-to-list 'load-path "~/Utilities/gits/org-mode/lisp/")
(add-to-list 'load-path "~/Utilities/gits/org-mode/contrib/lisp" t)



;; Do package-managing stuff
(require 'package)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)


;; As says below

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Loads my config files
(load-directory "~/.emacs.d/config")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
