;;; mail.el --- Description -*- lexical-binding: t; -*-
(add-load-path! "/usr/local/share/emacs/site-lisp/mu4e")
(after! mu4e
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-maildir-shortcuts
        '(("/Inbox"            . ?i)
          ("/Archives/2021"    . ?a)
          ("/Drafts"           . ?d)
          ("/Deleted Messages" . ?t)
          ("/Sent Messages"    . ?s))
        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

(set-email-account! "johannes@sigabrt.de"
                    '((user-mail-address     . "johannes@sigabrt.de")
                      (user-full-name        . "Johannes Wolf"))
                    nil)
