(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md.hb\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md.erb\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(push 'markdown-mode el-get-packages)
(defvar markdown-imenu-generic-expression
  '(("title"  "^\\(.*\\)[\n]=+$" 1)
    ("h2-"    "^\\(.*\\)[\n]-+$" 1)
    ("h1"   "^# \\(.*\\)$" 1)
    ("h2"   "^## \\(.*\\)$" 1)
    ("h3"   "^### \\(.*\\)$" 1)
    ("h4"   "^#### \\(.*\\)$" 1)
    ("h5"   "^##### \\(.*\\)$" 1)
    ("h6"   "^###### \\(.*\\)$" 1)
    ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

(defun iy-markdown-mode-init ()
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (autopair-mode 1)

  (setq imenu-generic-expression markdown-imenu-generic-expression)

  (setq autopair-extra-pairs
        '(:everywhere ((?` . ?`))))
  (setq autopair-handle-action-fns
        (list 'autopair-default-handle-action
              'autopair-python-triple-quote-action))
  (modify-syntax-entry ?` "\"" markdown-mode-syntax-table)

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
