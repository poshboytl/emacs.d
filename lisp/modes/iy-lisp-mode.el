(defun iy-lisp-mode-init ()
  (autopair-mode))

(add-hook 'emacs-lisp-mode-hook 'iy-lisp-mode-init)

(provide 'iy-lisp-mode)
