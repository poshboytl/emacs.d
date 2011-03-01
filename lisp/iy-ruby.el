(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))

(push '(:name rvm
              :type git
              :url "git://github.com/senny/rvm.el.git"
              :after (lambda () (rvm-use-default))
              :features rvm)
      el-get-sources)

(push '(:name ruby-electric
              :type http
              :url "http://shylock.uw.hu/Emacs/ruby-electric.el")
      el-get-sources)
(autoload 'ruby-electric-mode "ruby-electric")

(push 'yari el-get-sources)
(defalias 'ri 'yari)

(push 'ruby-block el-get-sources)
(make-variable-buffer-local 'ruby-block-mode)

;; Load rinari at last, so it is initialized before other packages.
;; Rinari bundled ruby-mode shoud be used instead of the system default.
(push 'rinari el-get-sources)

(push '(:name cucumber
              :type git
              :url "git://github.com/michaelklishin/cucumber.el.git"
              :after (lambda ()
                       (autoload 'feature-mode "feature-mode" nil t)
                       (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))))
      el-get-sources)
(push 'rspec-mode el-get-sources)
(setq rspec-use-rvm t)

(defun iy/ruby-mode-init ()
  (rinari-minor-mode t)
  (hs-minor-mode t)
  (flyspell-prog-mode)
  (ruby-electric-mode t)
  (turn-on-auto-fill)
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (local-set-key (kbd "C-h h") 'yari)
  ;; (local-set-key (kbd "C-h .") 'rct-complete-symbol)
  (remove-hook 'before-save-hook 'ruby-mode-set-encoding))

(add-hook 'ruby-mode-hook 'iy/ruby-mode-init t)

(provide 'iy-ruby)
