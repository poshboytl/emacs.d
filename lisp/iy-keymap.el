;;; Global keymap

(define-prefix-command 'iy-map)

(global-set-key (kbd "M-s") 'iy-map)
(global-set-key (kbd "M-S") search-map)
(define-key minibuffer-local-map (kbd "M-s") 'iy-map)

(provide 'iy-keymap)