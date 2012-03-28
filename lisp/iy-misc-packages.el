(push 'fringe-helper el-get-packages)

(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(custom-set-variables
 '(woman-fontify t)
 '(woman-use-topic-at-point-default t))

(custom-set-variables
 '(iy-go-to-char-key-backward 58))
(require 'iy-go-to-char)

(push 'tumbl el-get-packages)

(push 'cheat el-get-packages)

(push 'yaml-mode el-get-packages)
(push 'haml-mode el-get-packages)
(push 'sass-mode el-get-packages)
(push 'rainbow-mode el-get-packages)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(custom-set-variables
 '(coffee-tab-width 2))
(push 'coffee-mode el-get-packages)

(defun iy-coffee-mode-init ()
  (local-set-key (kbd "<return>") 'coffee-newline-and-indent))
(defadvice coffee-newline-and-indent (around fix-error-when-bob activate)
  (if (bobp) (newline) ad-do-it))
(add-hook 'coffee-mode-hook 'iy-coffee-mode-init)

(unless (eq system-type 'darwin)
  (push 'haskell-mode el-get-packages))

(push 'erlware-mode el-get-packages)

(defun iy-diff-mode-init ()
  (local-set-key (kbd "M-o") 'other-window))
(add-hook 'diff-mode-hook 'iy-diff-mode-init)

(push 'sml-modeline el-get-packages)
(defun iy-el-get-after-sml-modeline ()
  (sml-modeline-mode))

(provide 'iy-misc-packages)
