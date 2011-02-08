;;; iy-editing.el

(require 'iy-keymap)

;;; Move

(require 'iy-go-to-char)
(define-key iy-map (kbd "f") 'iy-go-to-char)
(global-set-key (kbd "C-z") 'iy-go-to-char)
(define-key iy-map (kbd "F") 'iy-go-to-char-backward)
(define-key iy-map (kbd ";") 'iy-go-to-char-continue)
(define-key iy-map (kbd ":") 'iy-go-to-char-continue-backward)

;;; Edit

(autoload 'copy-from-above-command "misc" "copy from previous nonblank line" t)
(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(defun iy-zap-back-to-char (arg char)
  (interactive "p\ncZap back to char: ")
  (zap-to-char (- arg) char))
(defun iy-zap-back-up-to-char (arg char)
  (interactive "p\ncZap back up to char: ")
  (zap-up-to-char (- arg) char))

(define-key iy-map (kbd "d") 'zap-to-char)
(define-key iy-map (kbd "D") 'iy-zap-back-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'iy-zap-back-up-to-char)


(provide 'iy-editing)