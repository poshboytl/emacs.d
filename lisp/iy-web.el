(require 'iy-dep)
(require 'iy-keymap)
(require 'iy-company)

;; add nxhtml in the end, because it will load old org
(add-to-list
 'el-get-sources
 '(:name nxhtml
         :after iy-el-get-after-nxhtml)
 t)

(defun iy-el-get-after-nxhtml ()
  (defun iy-popcmp-popup-os ()
    "Use OS popup menu always"
    (interactive)
    (let ((popcmp-completion-style 'popcmp-popup))
      (nxml-complete)))
  (defun iy/nxhml-mode-init ()
    (local-set-key (kbd "M-S-<iso-lefttab>") 'iy-popcmp-popup-os)
    (local-set-key (kbd "M-S-<tab>") 'iy-popcmp-popup-os)
    (local-set-key (kbd "M-S-<backtab>") 'iy-popcmp-popup-os)
    (zencoding-mode t)
    (company-mode t)
    (setq popcmp-completion-style 'company-mode)
    (local-set-key (kbd "C-<return>") 'zencoding-expand-line))
  (add-hook 'nxhtml-mode-hook 'iy/nxhml-mode-init)

  (require 'fold-dwim nil t)
  (define-key iy-map (kbd "i") 'fold-dwim-toggle)
  (define-key iy-map (kbd "M-i") 'fold-dwim-toggle)
  (define-key iy-map (kbd "I") 'fold-dwim-hide-all)
  (define-key iy-map (kbd "C-i") 'fold-dwim-show-all))

(provide 'iy-web)