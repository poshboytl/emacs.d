(defun paredit-wrap-round-from-behind ()
  (interactive)

  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(defun iy-lisp-mode-init ()
  (when (fboundp 'paredit-mode)
    (paredit-mode)
    (define-key paredit-mode-map (kbd "M-)") 'paredit-wrap-round-from-behind))
  (local-set-key (kbd "<return>") 'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j") 'newline))

(add-hook 'emacs-lisp-mode-hook 'iy-lisp-mode-init)

(provide 'iy-lisp-mode)
