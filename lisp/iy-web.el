(require 'iy-dep)

(push 'yaml-mode el-get-packages)
(push 'haml-mode el-get-packages)
(push 'sass-mode el-get-packages)
(push 'rainbow-mode el-get-packages)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(push 'coffee-mode el-get-packages)

(defun iy-coffee-mode-init ()
  (local-set-key (kbd "<return>") 'coffee-newline-and-indent))
(defadvice coffee-newline-and-indent (around fix-error-when-bob activate)
  (if (bobp) (newline) ad-do-it))
(add-hook 'coffee-mode-hook 'iy-coffee-mode-init)

(defun iy-html-mode-init ()
  (zencoding-mode t)
  (local-set-key (kbd "<C-return>") 'zencoding-expand-line))
(add-hook 'sgml-mode-hook 'iy-html-mode-init)

(custom-set-variables
 '(coffee-tab-width 2)
 '(zencoding-indentation 2))

(provide 'iy-web)
