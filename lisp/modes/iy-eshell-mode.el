(require 'iy-dep)

(defun iy-eshell-mode-init ()
  (local-set-key (kbd "M-s") iy-map))

(add-hook 'eshell-mode-hook 'iy-eshell-mode-init)

(provide 'iy-eshell-mode)
