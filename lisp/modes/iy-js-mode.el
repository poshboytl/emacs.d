(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

(defun iy-js-mode-init ()
  (local-set-key (kbd "M-.") 'find-tag))

(add-hook 'js-mode-hook 'iy-js-mode-init)

(custom-set-variables
 '(js-indent-level 2))

(provide 'iy-js-mode)
