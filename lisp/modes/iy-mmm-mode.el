(push 'mmm-mode el-get-packages)

(add-hook 'after-make-window-system-frame-hooks
          '(lambda ()
             (setq mmm-font-lock-available-p t)))

(defvar iy-mmm-fenced-code-block-mode-alist nil)
(setq iy-mmm-fenced-code-block-mode-alist
      '(("javascript" . js-mode)))

(defun iy-mmm-fenced-code-block-get-mode (string)
  (string-match "[a-zA-Z_-]+" string)
  (setq string (match-string 0 string))
  (or
   (mmm-ensure-modename (or (cdr (assoc string iy-mmm-fenced-code-block-mode-alist))
                            (intern (concat string "-mode"))))
   (signal 'mmm-no-matching-submode nil)))

(defun iy-el-get-after-mmm-mode ()
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 2)
  (setq mmm-parse-when-idle t)
  (require 'mmm-auto)

  (mmm-add-classes
   '((yaml-header-matters
      :submode yaml-mode
      :face mmm-code-submode-face
      :front "\\`---$"
      :back "^---$")))

  (mmm-add-classes
   '((fenced-code-block
      :front "^\\(```\\)\\(\\w+\\)$"
      :front-offset (end-of-line 1)
      :face mmm-code-submode-face
      :back "^```$"
      :delimiter-mode nil
      :match-submode iy-mmm-fenced-code-block-get-mode
      :insert ((?s fenced-code-block "Code Type: " @ "```" str "\n" _
                   @ "\n" @ "```\n" @)))))

  (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
  (mmm-add-mode-ext-class 'html-erb-mode "\\.rhtml\\'" 'erb)
  (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
  (mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
  (mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)
  (mmm-add-mode-ext-class 'markdown-mode nil 'yaml-header-matters)
  (mmm-add-mode-ext-class 'markdown-mode nil 'fenced-code-block)
  (mmm-add-mode-ext-class 'gfm-mode nil 'yaml-header-matters)
  (mmm-add-mode-ext-class 'gfm-mode nil 'fenced-code-block)

  (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . html-erb-mode))
  (add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode)))

(provide 'iy-mmm-mode)
