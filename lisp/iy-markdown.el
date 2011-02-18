(push 'markdown-mode el-get-sources)

(add-hook 'markdown-mode-hook 'iy-markdown-mode-init)
(defun iy-markdown-mode-init ()
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (when (fboundp 'yas/fix-keybindings) (yas/fix-keybindings)))

(provide 'iy-markdown)