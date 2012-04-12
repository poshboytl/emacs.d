(eval-when-compile (require 'cl))
(require 'iy-dep)

(custom-set-variables
 '(bookmark-default-file (concat iy-data-dir "bookmark"))
 '(bookmark-use-annotations nil))

(push 'bookmark+ el-get-packages)

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
      (ignore-errors
        (cond ((or (eq ev ?,) (eq ev ?\M-,)) (bmkp-next-bookmark-this-buffer 1))
              ((or (eq ev ?.) (eq ev ?\M-.)) (bmkp-previous-bookmark-this-buffer 1))
              ((eq ev ?<) (bmkp-next-bookmark 1))
              ((eq ev ?>) (bmkp-previous-bookmark 1))
              ((eq (event-basic-type ev) ?/) (bookmark-bmenu-list))
              (t (setq done t))))
      (when (not done)
        (setq ev (read-event))))
    (push ev unread-command-events)))

(provide 'iy-bookmark)
