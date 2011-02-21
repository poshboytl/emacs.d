;;; iy-global-keys.el --- Global shortcut keys

;;; Load libraries
(require 'iy-dep)
(require 'iy-packages)

;;; Move
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "M-F") 'forward-symbol)
(global-set-key (kbd "M-B") (lambda (arg)
                              (interactive "p")
                              (forward-symbol (- arg))))

(define-key iy-map (kbd "f") 'iy-go-to-char)
(global-set-key (kbd "C-z") 'iy-go-to-char)
(define-key iy-map (kbd "F") 'iy-go-to-char-backward)
(define-key iy-map (kbd ";") 'iy-go-to-char-continue)
(define-key iy-map (kbd ":") 'iy-go-to-char-continue-backward)

(define-key iy-map (kbd "*") 'isearch-forward-at-point)
(define-key iy-map (kbd "8") 'isearch-forward-at-point)
(define-key iy-map (kbd "C-s") 'isearch-forward-at-point)
(define-key iy-map (kbd "C-r") 'isearch-forward-at-point)

(global-set-key [end] 'end-of-line)
(global-set-key [home] 'beginning-of-line)

(global-set-key (kbd "C-`") 'next-error)
(global-set-key (kbd "C-~") 'previous-error)

(global-set-key (kbd "C->") 'scroll-left)
(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C-x >") 'scroll-left)
(global-set-key (kbd "C-x <") 'scroll-right)

(global-set-key (kbd "C-<left>") 'scroll-right)
(global-set-key (kbd "C-<right>") 'scroll-left)
(global-set-key (kbd "C-<up>") 'scroll-down)
(global-set-key (kbd "C-<down>") 'scroll-up)

(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)

;;; Delete

(define-key iy-map (kbd "d") 'zap-to-char)
(define-key iy-map (kbd "D") 'iy-zap-back-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'iy-zap-back-up-to-char)

;;; Insert
(define-key iy-map (kbd "q u") 'iy-insert-user)
(define-key iy-map (kbd "q t") 'iy-insert-time)
(define-key iy-map (kbd "q s") 'iy-insert-timestamp)
(define-key iy-map (kbd "q d") 'iy-insert-date)
(define-key iy-map (kbd "q f") 'iy-insert-file-name)
(define-key iy-map (kbd "q b") 'jared/insert-file-or-buffer-name)

;;; Mark
(global-set-key (kbd "C-'") 'set-mark-command)
(global-set-key (kbd "C-x C-'") 'pop-global-mark)
(global-set-key (kbd "C-M-'") 'mark-sexp)
(global-set-key (kbd "M-'") 'mark-word)

(global-set-key (kbd "C-2") 'set-mark-command)
(global-set-key (kbd "C-x C-2") 'pop-global-mark)
(global-set-key (kbd "C-M-2") 'mark-sexp)

;;; Highlight

(define-key iy-map (kbd "w") 'flash-line-highlight)

(define-key iy-map (kbd "9") 'iy-highlight-symbol-navigation)
(define-key iy-map (kbd "0") 'iy-highlight-symbol-navigation)
(define-key iy-map (kbd "+") 'highlight-symbol-query-replace)
(define-key iy-map (kbd "=") 'highlight-symbol-at-point)
(define-key iy-map (kbd "-") 'highlight-symbol-remove-all)
(define-key iy-map (kbd "_") 'highlight-symbol-mode)

(defun iy-highlight-symbol-navigation ()
  "highlighted symbol navigation"
  (interactive)
  (let ((done nil)
        (ev last-command-event)
        (echo-keystrokes nil))
    (while (not done)
      (cond ((eq ev ?9) (highlight-symbol-prev))
            ((eq ev ?0) (highlight-symbol-next))
            (t (setq done t)))
      (when (not done)
        (setq ev (read-event))))
    (push ev unread-command-events)))

(defun iy-highlight-symbol-navigation ()
  "highlighted symbol navigation"
  (interactive)
  (let ((done nil)
        (ev last-command-event)
        (echo-keystrokes nil))
    (while (not done)
      (cond ((eq ev ?9) (highlight-symbol-prev))
            ((eq ev ?0) (highlight-symbol-next))
            (t (setq done t)))
      (when (not done)
        (setq ev (read-event))))
    (push ev unread-command-events)))

;;; Buffer/File
(global-set-key (kbd "C-M-l") 'iy-switch-to-previous-buffer)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-frame)

;;; Fx
(global-set-key (kbd "C-<f7>") 'speedbar-get-focus)
(global-set-key (kbd "<ESC> <f7>") 'speedbar-get-focus)

(global-set-key (kbd "C-<f9>") 'mark-thing)
(global-set-key (kbd "<ESC> <f9>") 'mark-thing)
(global-set-key (kbd "<f9>") 'cycle-thing-region)
(global-set-key (kbd "<f10>") 'grep-find)
(global-set-key (kbd "<f11>") 'occur)
(global-set-key (kbd "<f12>") 'magit-status)
(global-set-key (kbd "C-<f12>") 'git-status)
(global-set-key (kbd "<ESC> <f12>") 'git-status)

;;; Misc
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-x SPC") 'point-to-register)

(provide 'iy-global-keys)