(require 'iy-dep)
(require 'iy-keymap)
(require 'iy-company)

;; add nxhtml in the end, because it will load old org
(push
 '(:name nxhtml
         :after iy-el-get-after-nxhtml)
 el-get-sources)

(defun iy-el-get-after-nxhtml ()
  (defun iy/nxhtml-mode-init ()
    (setq popcmp-completion-style 'anything)
    (zencoding-mode t))
  (add-hook 'nxhtml-mode-hook 'iy/nxhtml-mode-init)

  (require 'fold-dwim nil t)
  (define-key iy-map (kbd "i") 'fold-dwim-toggle)
  (define-key iy-map (kbd "M-i") 'fold-dwim-toggle)
  (define-key iy-map (kbd "I") 'fold-dwim-hide-all)
  (define-key iy-map (kbd "C-i") 'fold-dwim-show-all))

(provide 'iy-web)