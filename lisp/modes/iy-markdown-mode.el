(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(push 'markdown-mode el-get-packages)

(defun iy-markdown-mode-init ()
  (turn-on-auto-fill)
  (flyspell-mode 1)

  (when (fboundp 'yas/markdown-noconflict)
    (yas/markdown-noconflict)))

(add-hook 'markdown-mode-hook 'iy-markdown-mode-init)

(provide 'iy-markdown-mode)
