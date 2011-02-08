;;; iy-misc.el --- Miscellaneous configurations

;;; Encoding

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'iy-misc)
