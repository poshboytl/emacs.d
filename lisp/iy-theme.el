(require 'color-theme)
(require 'color-theme-solarized)
;; (require 'zenburn)

(defun iy-theme-do-when-window-system ()
  (when window-system
    (color-theme-solarized-dark)
    ;; (tool-bar-mode 0)
    ;; (scroll-bar-mode 0)
    (set-frame-font iy-frame-font)
    (set-fontset-font "fontset-default" 'chinese-gbk iy-frame-font-chinese)))

(iy-theme-do-when-window-system)

(add-hook 'after-make-frame-functions
          (lambda
            (new-frame)
            (select-frame new-frame)
            ;; (tool-bar-mode 0)
            ;; (scroll-bar-mode 0)
            (iy-theme-do-when-window-system)))

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

(provide 'iy-theme)

