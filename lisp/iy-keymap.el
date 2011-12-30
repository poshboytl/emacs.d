;;; Global keymap

(define-prefix-command 'iy-map)

(global-set-key (kbd "M-s") 'iy-map)
(define-key iy-map (kbd "s") search-map)
(define-key minibuffer-local-map (kbd "M-s") 'iy-map)

(provide 'iy-keymap)