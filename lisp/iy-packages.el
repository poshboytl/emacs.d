;;; Misc packages

;; (when (require 'lcomp nil t)
;;   (lcomp-activate-advices t)
;;   (lcomp-keys-mode 1))

;; (when (fboundp 'shell-command-completion-mode)
;;   (shell-command-completion-mode))

(require 'iy-go-to-char)

(recentf-mode)

(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(push '(:name thing-cmds
              :type emacswiki) el-get-sources)
(autoload 'mark-thing "thing-cmds"
  "Set point at one end of THING and set mark ARG THINGs from point." t)
(autoload 'cycle-thing-region "thing-cmds"
  "Select a thing near point." t)

(push '(:name hl-line+
              :type emacswiki
              :features hl-line+
              :after (lambda () (toggle-hl-line-when-idle 1))
              ) el-get-sources)

(push 'highlight-symbol el-get-sources)
(push 'highlight-parentheses el-get-sources)

(push 'yaml-mode el-get-sources)
(push 'haml-mode el-get-sources)
(push 'sass-mode el-get-sources)

(unless (eq system-type 'darwin)
  (push 'haskell-mode el-get-sources))

(push 'gist el-get-sources)

(push '(:name pivotal-tracker
              :type git
              :url "git://github.com/jxa/pivotal-tracker.git"
              :after (lambda ()
                       (autoload 'pivotal "pivotal-tracker" nil t)))
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
              :after (lambda () (autoload 'dtrt-indent-mode "dtrt-indent" nil t)))
      el-get-sources)

(push 'autopair el-get-sources)
(setq autopair-blink nil)

(push 'folding el-get-sources)

(push 'erlware-mode el-get-sources)

(push '(:name pick-backup :type elpa) el-get-sources)

(provide 'iy-packages)
