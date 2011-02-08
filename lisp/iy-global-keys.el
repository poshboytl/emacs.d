;;; iy-global-keys.el --- Global shortcut keys

(require 'iy-dep)

(global-set-key (kbd "C-'") 'set-mark-command)
(global-set-key (kbd "C-x C-'") 'pop-global-mark)
(global-set-key (kbd "C-M-'") 'mark-sexp)
(global-set-key (kbd "M-'") 'mark-word)

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-M-l") 'iy/switch-to-previous-buffer)

(global-set-key (kbd "C-2") 'set-mark-command)
(global-set-key (kbd "C-x C-2") 'pop-global-mark)
(global-set-key (kbd "C-M-2") 'mark-sexp)

(global-set-key (kbd "C--") 'undo)

(global-set-key [end] 'end-of-line)
(global-set-key [home] 'beginning-of-line)

(global-set-key (kbd "C-`") 'next-error)
(global-set-key (kbd "C-~") 'previous-error)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)

(global-set-key (kbd "C-<f9>") 'mark-thing)
(global-set-key (kbd "<ESC> <f9>") 'mark-thing)
(global-set-key (kbd "<f9>") 'cycle-thing-region)
(global-set-key (kbd "<f10>") 'grep-find)
(global-set-key (kbd "<f11>") 'occur)
(global-set-key (kbd "<f12>") 'magit-status)
(global-set-key (kbd "C-<f12>") 'git-status)
(global-set-key (kbd "<ESC> <f12>") 'git-status)

(provide 'iy-global-keys)