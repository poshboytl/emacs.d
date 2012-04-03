(defun iy-lisp-mode-init ()
  (when (fboundp 'paredit-mode) (paredit-mode))
  (local-set-key (kbd "<return>") 'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j") 'newline))

(add-hook 'emacs-lisp-mode-hook 'iy-lisp-mode-init)

(provide 'iy-lisp-mode)
