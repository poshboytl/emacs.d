(require 'iy-dep)
(custom-set-variables
 '(mumamo-chunk-coloring 1)
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-skip-welcome t))

(push 'nxhtml el-get-packages)

(defun iy-html-mode-init ()
  (zencoding-mode t)
  (local-set-key (kbd "<C-return>") 'zencoding-expand-line))

(defun iy-el-get-after-nxhtml ()
  ;; revert some mode
  (add-to-list 'auto-mode-alist '("\\.md\\.erb\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.md\\.erb\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html.haml\\'" . haml-mode))
  (add-to-list 'auto-mode-alist '("\\.eco\\'" . html-mode))
  (add-hook 'html-mode-hook 'iy-html-mode-init))

(provide 'iy-web)
