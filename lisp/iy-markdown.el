(push 'markdown-mode el-get-sources)

(add-hook 'markdown-mode-hook 'iy-markdown-mode-init)
(add-to-list 'auto-mode-alist '("\\.md\\.erb\\'" . markdown-mode))
(defun iy-markdown-mode-init ()
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (yas/fix-keybindings))

(provide 'iy-markdown)