(push 'fringe-helper el-get-packages)

(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(custom-set-variables
 '(woman-fontify t)
 '(woman-use-topic-at-point-default t))

(custom-set-variables
 '(iy-go-to-char-key-backward 58))
(require 'iy-go-to-char)

(push 'ace-jump-mode el-get-packages)

(push 'tumbl el-get-packages)

(push 'cheat el-get-packages)

(unless (eq system-type 'darwin)
  (push 'haskell-mode el-get-packages)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(push 'erlware-mode el-get-packages)

(defun iy-diff-mode-init ()
  (local-set-key (kbd "M-o") 'other-window))
(add-hook 'diff-mode-hook 'iy-diff-mode-init)

(push 'sml-modeline el-get-packages)
(defun iy-el-get-after-sml-modeline ()
  (sml-modeline-mode))

(push 'pos-tip el-get-packages)

(push 'multiple-cursors el-get-packages)

(push 'undo-tree el-get-packages)

(provide 'iy-misc-packages)
