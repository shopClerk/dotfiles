(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'smtpmail)

;; Make emacs use smtp for sending emails
(setq message-send-mail-function 'smtpmail-send-it)

(defvar my-mu4e-account-alist
  `(("Personal"
     (mu4e-sent-folder "/Personal/sent")
     (mu4e-drafts-folder "/Personal/drafts")
     (mu4e-trash-folder "/Personal/trash")
     (user-mail-address            ,sensible-email-account-1-user)
     (smtpmail-default-smtp-server ,sensible-email-account-1-smtp)
     (smtpmail-local-domain        ,sensible-email-account-1-domain)
     (smtpmail-smtp-user           ,sensible-email-account-1-user)
     (smtpmail-smtp-server         ,sensible-email-account-1-smtp)
     (smtpmail-stream-type         ,sensible-email-account-1-stream)
     (smtpmail-smtp-service        ,sensible-email-account-1-smtp-port))
    ("School"
     (mu4e-sent-folder "/School/sent")
     (mu4e-drafts-folder "/School/drafts")
     (mu4e-trash-folder "/School/trash")
     (user-mail-address            ,sensible-email-account-2-user)
     (smtpmail-default-smtp-server ,sensible-email-account-2-smtp)
     (smtpmail-local-domain        ,sensible-email-account-2-domain)
     (smtpmail-smtp-user           ,sensible-email-account-2-user)
     (smtpmail-smtp-server         ,sensible-email-account-2-smtp)
     (smtpmail-stream-type         ,sensible-email-account-2-stream)
     (smtpmail-smtp-service        ,sensible-email-account-2-smtp-port))
    ("Gmail"
     (mu4e-sent-folder "/MeGmail/sent")
     (mu4e-drafts-folder "/MeGmail/drafts")
     (mu4e-trash-folder "/MeGmail/trash")
     (user-mail-address            ,sensible-email-account-3-user)
     (smtpmail-default-smtp-server ,sensible-email-account-3-smtp)
     (smtpmail-local-domain        ,sensible-email-account-3-domain)
     (smtpmail-smtp-user           ,sensible-email-account-3-user)
     (smtpmail-smtp-server         ,sensible-email-account-3-smtp)
     (smtpmail-stream-type         ,sensible-email-account-3-stream)
     (smtpmail-smtp-service        ,sensible-email-account-3-smtp-port))
    ("Throw"
     (mu4e-sent-folder "/Throw/Saved Items")
     (mu4e-drafts-folder "/Throw/drafts")     
     (mu4e-trash-folder  "/Throw/trash")
     (user-mail-address            ,sensible-email-account-4-user)
     (smtpmail-default-smtp-server ,sensible-email-account-4-smtp)
     (smtpmail-local-domain        ,sensible-email-account-4-domain)
     (smtpmail-smtp-user           ,sensible-email-account-4-user)
     (smtpmail-smtp-server         ,sensible-email-account-4-smtp)
     (smtpmail-stream-type         ,sensible-email-account-4-stream)
     (smtpmail-smtp-service        ,sensible-email-account-4-smtp-port))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(setq mu4e-user-mail-address-list `(,sensible-email-account-1-user ,sensible-email-account-2-user ,sensible-email-account-3-user ,sensible-email-account-4-user ,sensible-default-email))

(require 'mu4e)

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(setq mu4e-maildir (expand-file-name "~/.Mail"))

(setq mu4e-trash-folder "/Personal/trash")
(setq mu4e-drafts-folder "/Personal/drafts")
(setq mu4e-sent-folder   "/Personal/sent")
;; (setq message-signature-file "~/.emacs.d/.signature") ; put your signature in this file

; get mail
(setq mu4e-get-mail-command "offlineimap"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval (* 15 60)
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-maildir-shortcuts
      '( ("/Personal/INBOX"               . ?i)
         ("/MeGmail/INBOX"               . ?w)
         ("/Personal/sent"   . ?s)
         ("/Personal/trash"       . ?t)
         ("/Personal/drafts"    . ?d)))

;; show images
(setq mu4e-show-images t)

;; set pdf viewer
(setq mu4e-msg2pdf "/usr/bin/msg2pdf")

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; don't save message to Sent Messages, IMAP takes care of this
; (setq mu4e-sent-messages-behavior 'delete)

;; spell check
(add-hook 'mu4e-compose-mode-hook?
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))

;; Don't reply to self
(setq mu4e-compose-dont-reply-to-self t)

(defun mu4e-action-view-in-browser (msg)
  (let* ((q mu4e-html2text-command)
     )
    (setq mu4e-html2text-command  "mu4e-showinbrowser.sh")
    (mu4e-message-body-text msg)
    (setq mu4e-html2text-command q)
    ))

(add-to-list 'mu4e-headers-actions
         '("in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
         '("in browser" . mu4e-action-view-in-browser) t)
