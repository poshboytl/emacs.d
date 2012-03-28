(defun iy-lisp-mode-init ()
  (when (fboundp 'autopair-mode) (autopair-mode)))

(add-hook 'emacs-lisp-mode-hook 'iy-lisp-mode-init)

(provide 'iy-lisp-mode)
