(push 'fringe-helper el-get-sources)

(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(custom-set-variables
 '(woman-fontify t)
 '(woman-use-topic-at-point-default t))

(custom-set-variables
 '(iy-go-to-char-key-backward 58))
(require 'iy-go-to-char)

(push '(:name
        gist
        :type git
        :url "git://github.com/baron/gist.el.git")
      el-get-sources)

(push '(:name
        tumble
        :type elpa)
      el-get-sources)

(push '(:name
        cheat
        :type http
        :features nil
        :url "https://github.com/defunkt/cheat.el/raw/master/cheat.el"
        :post-init (lambda () (autoload 'cheat "cheat" nil t)))
      el-get-sources)

(push 'yaml-mode el-get-sources)
(push '(:name haml-mode :features haml-mode) el-get-sources)
(push 'sass-mode el-get-sources)
(push '(:name rainbow-mode
              :url "git://github.com/emacsmirror/rainbow-mode.git")
      el-get-sources)
(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(custom-set-variables
 '(coffee-tab-width 2))
(push 'coffee-mode el-get-sources)

(defun iy-coffee-mode-init ()
  (local-set-key (kbd "<return>") 'coffee-newline-and-indent))
(add-hook 'coffee-mode-hook 'iy-coffee-mode-init)

(unless (eq system-type 'darwin)
  (push 'haskell-mode el-get-sources))

(push 'erlware-mode el-get-sources)

(defun iy-diff-mode-init ()
  (local-set-key (kbd "M-o") 'other-window))
(add-hook 'diff-mode-hook 'iy-diff-mode-init)

(push '(:name
        sml-modeline
        :after sml-modeline-mode)
      el-get-sources)

(provide 'iy-misc-packages)
