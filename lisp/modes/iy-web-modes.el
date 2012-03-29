(require 'iy-dep)

(push 'yaml-mode el-get-packages)
(push 'haml-mode el-get-packages)
(push 'sass-mode el-get-packages)
(push 'rainbow-mode el-get-packages)
(push 'coffee-mode el-get-packages)

(defadvice coffee-newline-and-indent (around fix-error-when-bob activate)
  (if (bobp) (newline) ad-do-it))

(defun iy-coffee-mode-init ()
  (local-set-key (kbd "<return>") 'coffee-newline-and-indent)
  (autopair-mode)
  (setq autopair-handle-action-fns
        (list 'autopair-default-handle-action
              'autopair-python-triple-quote-action)))

(defun iy-html-mode-init ()
  (zencoding-mode t)
  (local-set-key (kbd "<C-return>") 'zencoding-expand-line))

(defun iy-js-mode-init ()
  (local-set-key (kbd "M-.") 'find-tag))

(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'coffee-mode-hook 'iy-coffee-mode-init)
(add-hook 'sgml-mode-hook 'iy-html-mode-init)
(add-hook 'js-mode-hook 'iy-js-mode-init)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

(custom-set-variables
 '(js-indent-level 2)
 '(coffee-tab-width 2)
 '(zencoding-indentation 2))

(provide 'iy-web-modes)