(require 'color-theme)
(require 'color-theme-solarized)
;; (require 'zenburn)

(unless window-system
  (setq solarized-colors-index 3))

(color-theme-solarized-dark)
;; (tool-bar-mode 0)
;; (scroll-bar-mode 0)
(set-frame-font iy-frame-font)
(set-fontset-font "fontset-default" 'chinese-gbk iy-frame-font-chinese)

(add-hook 'after-make-frame-functions
          (lambda
            (new-frame)
            (select-frame new-frame)
            (when (and window-system (eq solarized-colors-index 3))
              (setq solarized-colors-index 1)
              (create-solarized-theme dark)
              (color-theme-solarized-dark))))

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

(provide 'iy-theme)

