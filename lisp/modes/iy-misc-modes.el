(push 'yaml-mode el-get-sources)
(push 'haml-mode el-get-sources)
(push 'sass-mode el-get-sources)
(push '(:name rainbow-mode
              :url "git://github.com/emacsmirror/rainbow-mode.git")
      el-get-sources)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(push 'coffee-mode el-get-sources)

(unless (eq system-type 'darwin)
  (push 'haskell-mode el-get-sources))

(provide 'iy-misc-modes)
