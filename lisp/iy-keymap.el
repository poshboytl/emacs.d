;;; Global keymap

(define-prefix-command 'iy-map)

(global-set-key (kbd "M-s") 'iy-map)
(global-set-key (kbd "M-S") search-map)

(provide 'iy-keymap)
