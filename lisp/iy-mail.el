(add-hook 'mail-mode-hook 'flyspell-mode)

(global-set-key (kbd "<menu>") 'mu4e)

(autoload 'mu4e "mu4e" nil t)
(eval-after-load 'mu4e
  (progn
    (custom-set-variables
     '(mu4e-maildir (expand-file-name "~/Mail")))))

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
(setq message-sendmail-extra-arguments '("-a" "ianyme"))
(setq mail-host-address "iany.me")

(provide 'iy-mail)
