(require 'iy-dep)
(setq custom-theme-directory (concat iy-lisp-dir "themes"))
(load (concat iy-lisp-dir "themes/zenburn-theme"))

(set-frame-font iy-frame-font)
(set-fontset-font "fontset-default" 'chinese-gbk iy-frame-font-chinese)

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

(setq next-error-highlight 'fringe-arrow)

(add-to-list
 'default-frame-alist
 (cons 'font iy-frame-font))

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(custom-set-variables
 '(blink-cursor-mode t)
 '(blink-cursor-delay 2)
 '(blink-cursor-interval 0.5)
 '(indicate-empty-lines nil)
 '(indicate-buffer-boundaries 'right)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(use-file-dialog nil)
 '(use-dialog-box nil))

(global-hl-line-mode)

(push 'diminish el-get-packages)
(defun iy-el-get-after-diminish ()
  (eval-after-load "whole-line-or-region" '(diminish 'whole-line-or-region-mode))
  (eval-after-load "eproject" '(diminish 'eproject-mode))
  (eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "abbrev" '(diminish 'abbrev-mode))
  (eval-after-load "whitespace" '(diminish 'global-whitespace-mode))
  (eval-after-load "hideshow" '(diminish 'hs-minor-mode))
  (eval-after-load "ruby-block" '(diminish 'ruby-block-mode))
  (eval-after-load "rinari" '(diminish 'rinari-minor-mode " R"))
  (eval-after-load "flyspell" '(diminish 'flyspell-mode " fs"))
  (eval-after-load "flymake" '(diminish 'flymake-mode " fm"))
  (eval-after-load "paredit" '(diminish 'paredit-mode " par"))
  (diminish 'auto-fill-function " F"))

(provide 'iy-theme)
