(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(push 'markdown-mode el-get-packages)

(defun iy-markdown-mode-init ()
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (autopair-mode 1)

  (setq autopair-extra-pairs
        '(:everywhere ((?` . ?`))))
  (setq autopair-handle-action-fns
        (list 'autopair-default-handle-action
              'autopair-python-triple-quote-action))

  (when (fboundp 'yas/markdown-noconflict)
    (yas/markdown-noconflict)))

(add-hook 'markdown-mode-hook 'iy-markdown-mode-init)
(modify-syntax-entry ?` "\"" markdown-mode-syntax-table)

(provide 'iy-markdown-mode)
