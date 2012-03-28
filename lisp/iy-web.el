(require 'iy-dep)
(custom-set-variables
 '(mumamo-chunk-coloring 1)
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-skip-welcome t))

;; add nxhtml in the end, because it will load old org
(push
 '(:name nxhtml
         :after iy-el-get-after-nxhtml)
 el-get-sources)

(defun iy-el-get-after-nxhtml ()
  ;; revert some mode
  (add-to-list 'auto-mode-alist '("\\.md\\.erb\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.md\\.erb\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html.haml\\'" . haml-mode))
  (add-to-list 'auto-mode-alist '("\\.eco\\'" . html-mode)))

(defun iy-html-mode-init ()
  (zencoding-mode t)
  (local-set-key (kbd "<C-return>") 'zencoding-expand-line))
(add-hook 'html-mode-hook 'iy-html-mode-init)

(provide 'iy-web)
