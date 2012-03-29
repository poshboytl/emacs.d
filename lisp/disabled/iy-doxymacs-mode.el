(custom-set-variables
 '(doxymacs-use-external-xml-parser t))

(push 'doxymacs el-get-packages)

(defun iy-doxymacs-font-lock-mode-init ()
  (if (memq major-mode '(c-mode c++-mode objc-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'iy-doxymacs-font-lock-mode-init)

(push 'doxymacs-yard el-get-packages)
(add-hook 'ruby-mode-hook 'doxymacs-yard)
(add-hook 'ruby-mode-hook 'doxymacs-yard-font-lock)

(provide 'iy-doxymacs-mode)
