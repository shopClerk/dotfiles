(require 'cl)

(setq nnml-directory "~/.mail")
(setq message-directory "~/.mail")

(setq smtp-accounts
      `(,sensible-email-account-smtp-list-1
	,sensible-email-account-smtp-list-2
	,sensible-email-account-smtp-list-3))

(defun my-change-smtp ()
  (save-excursion
    (loop with from = (save-restriction
			(message-narrow-to-headers)
			(message-fetch-field "from"))
	  for (addr fname server port stmtype) in smtp-accounts
	  when (string-match addr from)
	  do (setq user-mail-address addr
		   user-full-name fname
		   smtpmail-smtp-user addr
		   smtpmail-smtp-server server
		   smtpmail-smtp-service port
		   smtpmail-stream-type stmtype))))

(defadvice smtpmail-via-smtp
    (before change-smtp-by-message-from-field (recipient buffer &optional ask) activate)
  (with-current-buffer buffer (my-change-smtp)))


(setq user-mail-address sensible-email-account-1-user
      user-full-name sensible-real-name
      smtpmail-smtp-server sensible-email-account-1-server
      smtpmail-smtp-service 587
      smtpmail-stream-type nil)

;; (setq smtpmail-auth-credentials "~/.authinfo.gpg")

(setq gnus-select-method
      `(nnimap ,sensible-email-account-1-name
	       ,`(nnimap-address ,sensible-email-account-1-server)
	       ,`(nnimap-server-imap-port ,sensible-email-account-1-imap-port)
	       ,`(nnimap-stream ssl)))

(setq gnus-secondary-select-methods
      `(,`(nnimap ,sensible-email-account-2-name
		  ,`(nnimap-address ,sensible-email-account-2-server)
		  ,`(nnimap-server-port ,sensible-email-account-2-imap-port)
		  ,`(nnimap-stream ssl))
	,`(nnimap ,sensible-email-account-3-name
		  ,`(nnimap-address ,sensible-email-account-3-server)
		  ,`(nnimap-server-port ,sensible-email-account-3-imap-port)
		  ,`(nnimap-stream ssl))
	,`(nnimap ,sensible-email-account-4-name
		  ,`(nnimap-address ,sensible-email-account-4-server)
		  ,`(nnimap-server-port ,sensible-email-account-4-imap-port)
		  ,`(nnimap-stream ssl))))



(setq-default
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 ;; gnus-summary-line-format "%U%R%z %(%&user-date <class="comment">;  %-15,15f  %B%s%)\n"
 gnus-summary-line-format "%U%R%&user-date; %-20,20n %B%-80,80S\n"
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")


;;;;; Not mine magic
;; (setq mm-automatic-display (remove "text/html" mm-automatic-display))
;; (setq mm-text-html-renderer 'nil)
;; (defun wicked/gnus-article-show-html ()
;;   "Show the current message as HTML mail."
;;   (interactive)
;;   (let ((mm-automatic-display (cons "text/html" mm-automatic-display)))
;;     (gnus-summary-show-article)))
;; (define-key gnus-article-mode-map "WH" 'wicked/gnus-article-show-html)
