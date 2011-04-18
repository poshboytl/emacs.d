(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(defun iy-sh-mode-init ()
  (setq sh-basic-offset 2)
  (autopair-mode)

)

(provide 'iy-sh)