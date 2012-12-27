(push 'mmm-mode el-get-packages)


(custom-set-variables
 '(mmm-global-mode 'maybe)
 '(mmm-submode-decoration-level 2)
 '(mmm-parse-when-idle t))

(defun iy-el-get-after-mmm-mode ()
  (require 'mmm-auto)

  (mmm-add-classes
   '((yaml-header-matters
      :submode yaml-mode
      :face mmm-code-submode-face
      :front "\\`---"
      :back "^---")))

  (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
  (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
  (mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
  (mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)
  (mmm-add-mode-ext-class 'markdown-mode nil 'yaml-header-matters)

  (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
  (add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode)))

(provide 'iy-mmm-mode)
