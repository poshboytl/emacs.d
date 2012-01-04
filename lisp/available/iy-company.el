(push '(:name company
              :type elpa
              :after iy-after-el-get-company)
      el-get-sources)

(defun iy-after-el-get-company ()
  (setq company-idle-delay nil)
  (setq company-show-numbers nil)
  (setq company-begin-commands '(self-insert-command))
  (setq company-backends  
        '(company-elisp
          company-nxml
          company-css
          company-semantic
          company-etags
          company-keywords))
  (require 'company-etags)
  (add-to-list 'company-etags-modes 'ruby-mode)
  (define-key iy-map " " 'company-complete-common)
  (define-key iy-map (kbd "M-<SPC>") 'company-complete-common)
  (defun iy/enable-company ()
    (ignore-errors
      (company-mode)
      (local-set-key (kbd "M-<tab>") 'company-complete-common)))

  (add-hook 'c-mode-common-hook 'iy/enable-company)
  (add-hook 'java-mode-hook 'iy/enable-company)
  (add-hook 'ruby-mode-hook 'iy/enable-company)
  (add-hook 'emacs-lisp-mode-hook 'iy/enable-company))

(provide 'iy-company)