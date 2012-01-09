(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(push '(:name
        markdown-mode
        :url "git@github.com:doitian/markdown-mode.git")
      el-get-sources)

(defun iy-markdown-mode-init ()
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (when (fboundp 'yas/fix-keybindings)
    (yas/fix-keybindings)))

(provide 'iy-markdown-mode)