(add-hook 'mail-mode-hook 'flyspell-mode)

;; (when (require 'mu4e nil t)
;;   (global-set-key (kbd "<menu>") 'mu4e)
;;   (custom-set-variables
;;    '(mu4e-maildir (expand-file-name "~/Mail"))))

(provide 'iy-mail)
