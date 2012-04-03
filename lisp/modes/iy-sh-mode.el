(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(defun iy-sh-mode-init ()
  (local-set-key (kbd "<return>") 'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j") 'newline)
  (setq sh-basic-offset 2)
  (autopair-mode))

(add-hook 'sh-mode-hook 'iy-sh-mode-init)

(provide 'iy-sh-mode)
