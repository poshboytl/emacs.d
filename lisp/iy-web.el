(require 'iy-dep)
(require 'iy-keymap)

;; add nxhtml in the end, because it will load old org
(push
 '(:name nxhtml
         :after iy-el-get-after-nxhtml)
 el-get-sources)

(defun iy-el-get-after-nxhtml ()
  (defun iy/nxhtml-mode-init ()
    (zencoding-mode t))
  (add-hook 'nxhtml-mode-hook 'iy/nxhtml-mode-init)

  (require 'fold-dwim nil t)
  (when (fboundp 'sml-modeline-mode)
    (sml-modeline-mode t))
  (define-key iy-map (kbd "i") 'fold-dwim-toggle)
  (define-key iy-map (kbd "M-i") 'fold-dwim-toggle)
  (define-key iy-map (kbd "I") 'fold-dwim-hide-all)
  (define-key iy-map (kbd "C-i") 'fold-dwim-show-all)

  ;; revert some mode
  (add-to-list 'auto-mode-alist '("\\.md\\.erb\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.md\\.erb\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.html.haml" . haml-mode)))

(provide 'iy-web)