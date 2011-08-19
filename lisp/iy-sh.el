(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(defun iy-sh-mode-init ()
  (setq sh-basic-offset 2)
  (autopair-mode))

(add-hook 'sh-mode-hook 'iy-sh-mode-init)

(provide 'iy-sh)