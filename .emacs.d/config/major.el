;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Minor modes configs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expand region stuff

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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

;; Auctex stuff
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(defun prefs-latex-mode ()
  (setq TeX-electric-sub-and-superscript t)
  (setq LaTeX-math-list
	(quote
	 ((?% LaTeX-math-frac "" nil)
	  ("3" LaTeX-math-sqrt "" nil)
	  (o LaTeX-math-overline "" nil))))
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


;; Programming languages identation stuff
(defun my-c-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-java-mode-hook ()
  (setq c-basic-offset 6))
(add-hook 'java-mode-hook 'my-java-mode-hook)


;; Rinari stuff
(require 'rinari)
(global-rinari-mode)

;; RVM stuff
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;; Lambdada stuff
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

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

(defun ri-bind-key ()
  (local-set-key [f1] 'yari-helm))

;; Paradox stuff
(setq paradox-github-token  sensible-github-token)

;; magit stuff
(global-set-key "\C-ci" 'magit-status)

;; openwith stuff
(when (require 'openwith nil 'noerror)
  (setq openwith-associations
	(list
	 (list (openwith-make-extension-regexp
		'("mpg" "mpeg" "mp3" "mp4"
		  "avi" "wmv" "wav" "mov" "flv"
		  "ogm" "ogg" "mkv"))
	       "mpv"
	       '(file))
;;	 (list (openwith-make-extension-regexp
;;		'("xbm" "pbm" "pgm" "ppm" "pnm"
;;		  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
;;	       "geeqie"
;;	       '(file))
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

(add-to-list  'mm-inhibit-file-name-handlers 'openwith-file-handler)
