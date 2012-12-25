(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))


(push 'ruby-mode el-get-packages)
;; (push 'Enhanced-Ruby-Mode el-get-packages)
(push 'ruby-end el-get-packages)

(setq rinari-minor-mode-prefixes '(";"))
(push 'rinari el-get-packages)
(defun iy-rinari-minor-mode-init ()
  (setq yas/extra-modes (cons 'rails-mode yas/extra-modes)))
(add-hook 'rinari-minor-mode-hook 'iy-rinari-minor-mode-init)

(push 'rhtml-mode el-get-packages)
(defun iy-el-get-after-rhtml-mode ()
  (add-to-list 'auto-mode-alist '("\\.eco$" . rhtml-mode))
  (eval-after-load "rhtml-mode"
    '(add-to-list
      'rhtml-in-erb-keywords
      '("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" . (1 font-lock-constant-face prepend)))))

(push 'yari el-get-packages)
(defalias 'ri 'yari)

(push 'ruby-block el-get-packages)
(make-variable-buffer-local 'ruby-block-mode)
(custom-set-variables
 '(ruby-block-highlight-toggle 'overlay)
 '(ruby-block-highlight-face 'show-paren-match))

(push 'cucumber el-get-packages)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(defun iy-ruby-mode-init ()
  (local-set-key (kbd "<return>") 'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j") 'newline)
  (local-set-key (kbd "C-h h") 'yari)
  ;; (local-set-key (kbd "M-s `") 'ruby-find-error)
  ;; (local-set-key (kbd "M-s M-`") 'ruby-find-error)

  (rinari-minor-mode t)
  (hs-minor-mode t)
  (flyspell-prog-mode)
  (autopair-mode t)
  (ruby-block-mode t)
  (ruby-end-mode t)
  (turn-on-auto-fill)
  (subword-mode)

  ;;(remove-hook 'before-save-hook 'ruby-mode-set-encoding)
  (setq autopair-extra-pairs '(:code ((?` . ?`)))))

(add-hook 'ruby-mode-hook 'iy-ruby-mode-init t)

(font-lock-add-keywords
 'ruby-mode
 '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))))


(provide 'iy-ruby-mode)
