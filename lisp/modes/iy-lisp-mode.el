(defun iy-lisp-mode-init ()
  (autopair-mode)
  (local-set-key (kbd "<return>") 'newline-and-indent))

(add-hook 'emacs-lisp-mode-hook 'iy-lisp-mode-init)

(provide 'iy-lisp-mode)
