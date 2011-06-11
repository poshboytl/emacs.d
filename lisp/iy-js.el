(defun iy-js-mode-init ()
  (local-set-key (kbd "M-.") 'find-tag))

(add-hook 'js-mode-hook 'iy-js-mode-init)

(provide 'iy-js)
