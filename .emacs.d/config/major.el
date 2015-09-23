;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Minor modes configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun special-if-mode-active (mode default-f special-f)
  `(lambda () (interactive)
     (if (bound-and-true-p ,mode)
         (call-interactively ',special-f)
       (call-interactively ',default-f))))

(defun bind-special-list (keybinds mode)
  (dolist (keybind keybinds)
    (let* ((stroke (car keybind))
           (new (cadr keybind))
           (old (caddr keybind)))
      (global-set-key (kbd stroke) (special-if-mode-active mode old new)))))

;; Expand region stuff

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Corral (parenthesis matching) stuff
;; (require 'corral)
;; (global-set-key (kbd "M-1") 'corral-parentheses-backward)
;; (global-set-key (kbd "M-2") 'corral-parentheses-forward)
;; (global-set-key (kbd "M-[") 'corral-brackets-backward)
;; (global-set-key (kbd "M-]") 'corral-brackets-forward)
;; (global-set-key (kbd "M-{") 'corral-braces-backward)
;; (global-set-key (kbd "M-}") 'corral-braces-forward)
;; (global-set-key (kbd "M-\"") 'corral-double-quotes-backward)

;; Gnus stuff

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5)
  )

(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda ()
            (local-set-key "o" 'my-gnus-group-list-subscribed-groups )
            (setq send-mail-function (quote smtpmail-send-it))
            ))

(customize-set-variable 'gnus-inhibit-startup-message t)

;; Auctex stuff
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(defun prefs-latex-mode ()
  (setq TeX-electric-sub-and-superscript t)
  ;; (TeX-add-symbols "foo")
  (LaTeX-add-environments "align" "align*" "equation*" "theorem")
  (customize-set-variable 'LaTeX-math-list
                          (list
                           '(?% LaTeX-math-frac "" nil)
                           '(?, LaTeX-math-ldots "" nil)
                           '(?\; LaTeX-math-cdots "" nil)
                           '("2" LaTeX-math-sqrt "" nil)
                           '("C-+" LaTeX-math-oplus "" nil)
                           '(?o LaTeX-math-overline "" nil)))
  (customize-set-variable 'TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"\"%o\"#src:%(default-dir)%(buffer-name):%n:0\"")))
  (customize-set-variable 'TeX-view-program-selection '((output-pdf "qpdfview")))
  (customize-set-variable 'TeX-newline-function (quote newline-and-indent))
  (setq LaTeX-font-list
        (quote
         ((1 "" "" "\\mathcal{" "}")
          (2 "\\textbf{" "}" "\\mathbf{" "}")
          (3 "\\textsc{" "}")
          (5 "\\emph{" "}")
          (6 "\\textsf{" "}" "\\mathsf{" "}")
          (9 "\\textit{" "}" "\\mathit{" "}")
          (13 "\\textmd{" "}")
          (14 "\\textnormal{" "}" "\\mathnormal{" "}")
          (18 "\\textrm{" "}" "\\mathrm{" "}")
          (19 "\\textsl{" "}" "\\mathbb{" "}")
          (20 "\\texttt{" "}" "\\mathtt{" "}")
          (11 "" "" "\\mathfrak{" "}")
          (21 "\\textup{" "}")
          (4 "" "" t))))
  )

(add-hook 'LaTeX-mode-hook 'prefs-latex-mode)

(defun setup-qpdfview()
  ;;(setq TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"\"%o\"#src:%(buffer-name):%n:0\"")))
  (setq TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"\"%o\"#src:%(default-dir)%(buffer-name):%n:0\"")))
  (setq TeX-view-program-selection '((output-pdf "qpdfview")))
  )
(add-hook 'LaTeX-mode-hook 'setup-qpdfview)     


;; C and C++ stuff
;; (defun my-c-mode-hook ()
;;   (setq c-basic-offset 4))
;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;; 
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4))

(defun c++-compile-key ()
  (local-set-key [f1] 'compile))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c++-mode-hook 'c++-compile-key)

(add-hook 'c++-mode-hook
  (lambda ()
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
	   (let ((file (file-name-nondirectory buffer-file-name)))
	     (concat "g++ -g -O2 -Wall -o " 
		     (file-name-sans-extension file)
		     " " file))))))

(add-hook 'c++-mode-hook
  (lambda ()
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
	   (let ((file (file-name-nondirectory buffer-file-name)))
	     (concat "gdb -i=mi " (file-name-sans-extension file)))))))
;; 
;; (defun my-java-mode-hook ()
;;   (setq c-basic-offset 6))
;; (add-hook 'java-mode-hook 'my-java-mode-hook)


;; RVM stuff
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;; Rainbow mode stuff
(require 'rainbow-mode)

;; scss mode stuff
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; web-mode stuff
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  ;; (setq web-mode-markup-indent-offset 2)  ;; for everything, i think
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-extra-auto-pairs
        '(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end")))
          ))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)


;; ruby documentation stuff
(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

(add-hook 'ruby-mode-hook 'ri-bind-key)

;; Rinari stuff
(require 'rinari)
(global-rinari-mode)

;; Paradox stuff
(setq paradox-github-token  sensible-github-token)
(customize-set-variable 'paradox-automatically-star 't)

;; magit stuff
(global-set-key "\C-ci" 'magit-status)

;; openwith stuff
(when (require 'openwith nil 'noerror)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv" "webm"))
               "mpv --force-window"
               '(file))
         ;;      (list (openwith-make-extension-regexp
         ;;             '("xbm" "pbm" "pgm" "ppm" "pnm"
         ;;               "png" "gif" "bmp" "tif" "jpeg" "jpg"))
         ;;            "geeqie"
         ;;            '(file))
         (list (openwith-make-extension-regexp
                '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
               "libreoffice"
               '(file))
         '("\\.lyx" "lyx" (file))
         '("\\.chm" "kchmviewer" (file))
         (list (openwith-make-extension-regexp
                '("pdf" "ps" "ps.gz" "dvi"))
               "qpdfview --unique"
               '(file))
         ))
  (openwith-mode 1))


;; Bundler (bundle for rails) stuff
(require 'bundler)


;; Dired stuff

(require 'dired+)

(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies '(always))
(setq dired-recursive-deletes '(top))

;; Allows navigating inside dir
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)

;; Image-viweer stuff
(eval-after-load 'image '(require 'image+))
(eval-after-load 'image-dired '(require 'image-dired+))
(eval-after-load 'image-dired+ '(image-diredx-async-mode 1))
(define-key image-dired-thumbnail-mode-map "\C-n" 'image-diredx-next-line)
(define-key image-dired-thumbnail-mode-map "\C-p" 'image-diredx-previous-line)
(define-key image-dired-thumbnail-mode-map "g" 'revert-buffer)

;; Helm stuff
(require 'helm)
(require 'helm-config)

(helm-mode 1)

(global-set-key (kbd "C-c h") 'helm-command-prefix)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(let ((keybinds '(("M-y"     helm-show-kill-ring kill-ring)
                  ("M-x"     helm-M-x            execute-extended-command)
                  ("C-x b"   helm-mini           switch-to-buffer)
                  ("C-x C-f" helm-find-files     find-file))))
  (bind-special-list keybinds 'helm-mode))

;; ESS mode
;; (require 'ess-site)

;; Python stuff
;; (require 'python-mode)
;; (add-to-list 'auto-mode-alist '("\.py\'" . python-mode))
;; ;; (require 'ipython)
;; (defun python-mode-preferences-hook ()
;;   ;; use IPython
;;   (setq-default py-shell-name "ipython")
;;   (setq-default py-which-bufname "IPython")
;;   ;; use the wx backend, for both mayavi and matplotlib
;;   (setq py-python-command-args
;;         '("-i" "--gui=wx" "--pylab=wx" "--colors" "Linux"))
;;   (setq py-force-py-shell-name-p t)
;;   ;; Improve windows splitting
;;   (setq-default py-split-windows-on-execute-function 'split-window-horizontally)
;;   ;; try to automagically figure out indentation
;;   (setq py-smart-indentation t)
;;   ;; execute without saving
;;   ;; (setq py-execute-no-temp t)
;;   )
;; 
;; (add-hook 'python-mode-hook 'python-mode-preferences-hook)
;; (add-hook 'python-mode-hook 'jedi:setup)
(elpy-enable)
(elpy-use-ipython)
(define-key elpy-mode-map (kbd "C-c C-s") nil)
(setq elpy-rpc-backend "jedi")

(defun python-prefs-hook ()
  ;; (local-set-key (kbd "C-c C-\\") 'elpy-rgrep-symbol)
  (local-set-key (kbd "C-c C-s") 'python-shell-send-region))

(defun elpy-prefs-hook ()
  (local-set-key (kbd "C-c C-\\") 'elpy-rgrep-symbol))

(add-hook 'python-mode-hook 'python-prefs-hook)
(add-hook 'elpy-mode-hook 'elpy-prefs-hook)
