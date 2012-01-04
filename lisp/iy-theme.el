(setq custom-theme-directory (concat iy-lisp-dir "themes"))
(load (concat iy-lisp-dir "themes/zenburn-theme"))

(set-frame-font iy-frame-font)
(set-fontset-font "fontset-default" 'chinese-gbk iy-frame-font-chinese)

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

(custom-set-variables
 '(blink-cursor-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil))

(provide 'iy-theme)
