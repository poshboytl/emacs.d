(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md.hb\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md.erb\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(push 'markdown-mode el-get-packages)

(defun iy-markdown-mode-init ()
  (turn-on-auto-fill)
  (flyspell-mode 1)

  (local-set-key (kbd "C-c C-f") 'outline-forward-same-level)
  (local-set-key (kbd "C-c C-b") 'outline-backward-same-level)
  (local-set-key (kbd "C-c C-n") 'outline-next-visible-heading)
  (local-set-key (kbd "C-c C-p") 'outline-previous-visible-heading)
  (local-set-key (kbd "C-M-f") 'forward-sexp)
  (local-set-key (kbd "C-M-b") 'backward-sexp)

  (when (fboundp 'yas/markdown-noconflict)
    (yas/markdown-noconflict)))

(add-hook 'markdown-mode-hook 'iy-markdown-mode-init)

(provide 'iy-markdown-mode)
