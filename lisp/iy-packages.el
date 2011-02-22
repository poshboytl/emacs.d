;;; Misc packages

;; (when (require 'lcomp nil t)
;;   (lcomp-activate-advices t)
;;   (lcomp-keys-mode 1))

;; (when (fboundp 'shell-command-completion-mode)
;;   (shell-command-completion-mode))

(require 'iy-go-to-char)
(eval-when-compile (require 'el-get))

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

(push 'sass-mode el-get-sources)
(push 'haml-mode el-get-sources)
(push 'yaml-mode el-get-sources)

(push 'gist el-get-sources)

(provide 'iy-packages)
