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

(setq rinari-minor-mode-prefixes '(";"))
(push 'rinari el-get-packages)

(push 'ruby-electric el-get-packages)
(autoload 'ruby-electric-mode "ruby-electric")

(push 'yari el-get-packages)
(defalias 'ri 'yari)

(push 'ruby-block el-get-packages)
(make-variable-buffer-local 'ruby-block-mode)

(push 'cucumber el-get-packages)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(push 'rspec-mode el-get-packages)

(defun iy-ruby-mode-init ()
  (rinari-minor-mode t)
  (hs-minor-mode t)
  (flyspell-prog-mode)
  (ruby-electric-mode t)
  (turn-on-auto-fill)
  (local-set-key (kbd "C-j") 'newline)
  (local-set-key (kbd "C-h h") 'yari)
  (subword-mode)
  ;;(remove-hook 'before-save-hook 'ruby-mode-set-encoding)
  )

(add-hook 'ruby-mode-hook 'iy-ruby-mode-init t)

(font-lock-add-keywords
 'ruby-mode
 '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))))

(provide 'iy-ruby-mode)
