(require 'iy-keymap)

(push 'bookmark+ el-get-sources)

(add-hook 'bookmark-bmenu-mode-hook 'iy-bookmark-bmenu-mode-init)
(defun iy-bookmark-bmenu-mode-init ()
  (define-key bookmark-bmenu-mode-map (kbd "M-o") 'other-window))

(defun iy-bmkp-navigation ()
  "bookmark+ navigation"
  (interactive)
  (let ((done nil)
        (ev last-command-event)
        (echo-keystrokes nil))
    (while (not done)
      (cond ((or (eq ev ?,) (eq ev ?\M-,)) (bmkp-next-bookmark-this-buffer 1))
            ((or (eq ev ?.) (eq ev ?\M-.)) (bmkp-previous-bookmark-this-buffer 1))
            ((eq ev ?<) (bmkp-next-bookmark 1))
            ((eq ev ?>) (bmkp-previous-bookmark 1))
            ((eq (event-basic-type ev) ?/) (bookmark-bmenu-list))
            (t (setq done t)))
      (when (not done)
        (setq ev (read-event))))
    (push ev unread-command-events)))

(global-set-key (kbd "C-x j SPC") 'jump-to-register)

(define-key iy-map (kbd "m") 'bookmark-set)
(define-key iy-map (kbd "M-m") 'bookmark-set)
(define-key iy-map (kbd ".") 'iy-bmkp-navigation)
(define-key iy-map (kbd "M-.") 'iy-bmkp-navigation)
(define-key iy-map (kbd ">") 'iy-bmkp-navigation)
(define-key iy-map (kbd ",") 'iy-bmkp-navigation)
(define-key iy-map (kbd "M-,") 'iy-bmkp-navigation)
(define-key iy-map (kbd "<") 'iy-bmpk-navigation)
(define-key iy-map (kbd "/") 'bookmark-bmenu-list)
(define-key iy-map (kbd "M-/") 'bookmark-bmenu-list)

(provide 'iy-bookmark)
