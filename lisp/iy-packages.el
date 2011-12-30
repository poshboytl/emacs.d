;;; Misc packages

;; (when (require 'lcomp nil t)
;;   (lcomp-activate-advices t)
;;   (lcomp-keys-mode 1))

;; (when (fboundp 'shell-command-completion-mode)
;;   (shell-command-completion-mode))

(require 'iy-go-to-char)

(recentf-mode)

(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(push '(:name hl-line+
              :type emacswiki
              :features hl-line+
              :after (lambda () (toggle-hl-line-when-idle 1))
              ) el-get-sources)

(push 'highlight-symbol el-get-sources)
(push '(:name highlight-parentheses
              :after (lambda ()
                       (add-hook 'c-mode-common-hook 'highlight-parentheses-mode)
                       (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
                       (add-hook 'ruby-mode-hook 'highlight-parentheses-mode))
              ) el-get-sources)

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

(push '(:name gist
              :type git
              :url "git://github.com/baron/gist.el.git")
      el-get-sources)

(push '(:name tumble
              :type elpa)
      el-get-sources)

(push 'browse-kill-ring el-get-sources)

(push '(:name kill-ring-search
              :type elpa)
      el-get-sources)

(global-set-key (kbd "C-M-y") 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-search-maybe (arg) activate)
  "If last action was not a yank, run `kill-ring-search' instead."
  (interactive "p")
  (if (not (eq last-command 'yank))
      (kill-ring-search)
    (barf-if-buffer-read-only)
    ad-do-it))

;; fix flyspell
(defadvice called-interactively-p (before iy-fix-interactively-p (&optional arg) activate))

(push 'fringe-helper el-get-sources)

(push '(:name dtrt-indent
              :features nil
              :url "https://github.com/emacsmirror/dtrt-indent.git"
              :post-init (lambda () (autoload 'dtrt-indent-mode "dtrt-indent" nil t)))
      el-get-sources)

(push 'autopair el-get-sources)
(setq autopair-blink nil)

(push 'folding el-get-sources)

(push 'erlware-mode el-get-sources)

(push '(:name cheat
              :type http
              :features nil
              :url "https://github.com/defunkt/cheat.el/raw/master/cheat.el"
              :post-init (lambda () (autoload 'cheat "cheat" nil t)))
      el-get-sources)

(push '(:name pick-backup :type elpa) el-get-sources)

(defun iy/diff-mode-init ()
  (local-set-key (kbd "M-o") 'other-window))
(add-hook 'diff-mode-hook 'iy/diff-mode-init)

(provide 'iy-packages)
