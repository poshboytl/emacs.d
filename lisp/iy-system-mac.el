;;; iy-system-mac.el --- config for Mac OS X

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq ns-pop-up-frames nil)
  (define-key global-map (kbd "M-`") 'other-frame))

(provide 'iy-system-mac)
