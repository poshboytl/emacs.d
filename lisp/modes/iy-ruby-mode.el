(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))

(setq rinari-minor-mode-prefixes '(";"))
(push 'rinari el-get-sources)

(push '(:name ruby-electric
              :type http
              :url "http://shylock.uw.hu/Emacs/ruby-electric.el")
      el-get-sources)
(autoload 'ruby-electric-mode "ruby-electric")

(push 'yari el-get-sources)
(defalias 'ri 'yari)

(push 'ruby-block el-get-sources)
(make-variable-buffer-local 'ruby-block-mode)

(push '(:name cucumber
              :type git
              :url "git://github.com/michaelklishin/cucumber.el.git"
              :after (lambda ()
                       (autoload 'feature-mode "feature-mode" nil t)
                       (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))))
      el-get-sources)
(push 'rspec-mode el-get-sources)

(defun iy/ruby-mode-init ()
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

(defun yas/advise-indent-function (function-symbol)
  (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
           ,(format
             "Try to expand a snippet before point, then call `%s' as usual"
             function-symbol)
           (let ((yas/fallback-behavior nil))
             (unless (yas/expand)
               ad-do-it)))))

(yas/advise-indent-function 'ruby-indent-line)

(add-hook 'ruby-mode-hook 'iy/ruby-mode-init t)

(font-lock-add-keywords
 'ruby-mode
 '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))))

(provide 'iy-ruby-mode)
