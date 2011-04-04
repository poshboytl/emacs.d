(push 'auctex el-get-sources)

(defun iy-tex-mode-init ()
  ;; (turn-on-cdlatex)
  (turn-on-reftex)
  (turn-on-auto-fill)
  (flyspell-mode 1)
  (TeX-PDF-mode 1)
  (iy-tex-style))

(add-hook 'LaTeX-mode-hook 'iy-tex-mode-init)
(add-hook 'ConTeXt-mode-hook 'iy-tex-mode-init)

(defun iy-tex-style ()
  (LaTeX-add-environments
   '("algorithm"))
  (TeX-add-symbols
   '("autoref" TeX-arg-label)
   )
  )

(defalias 'tm 'TeX-master-file-ask)

(provide 'iy-tex)