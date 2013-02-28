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

;; fix emacs-rails loading error
(defun decamelize (string)
  "Convert from CamelCaseString to camel_case_string."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2"
      (replace-regexp-in-string
       "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2"
       string)))))

;; Thank you, I'll handle it myself
(defvar rails-auto-mode-alist nil)

(push 'ruby-mode el-get-packages)
(push 'inf-ruby el-get-packages)
(push 'ruby-end el-get-packages)
(push 'emacs-rails el-get-packages)
(push 'rspec-mode el-get-packages)

(custom-set-variables
 '(rails-always-use-text-menus t)
 '(rails-browse-api-with-w3m t)
 ;; already handled by ~/bin/rspec
 '(rspec-use-bundler-when-possible nil)
 '(rspec-use-zeus-when-possible nil))

(defun rails-nav:goto-assets-stylesheets ()
  "Go to stylesheets."
  (interactive)
  (rails-nav:goto-file-with-menu "app/assets/stylesheets/" "Go to stylesheet.." "s?[ac]ss" t))

(defun rails-nav:goto-assets-javascripts ()
  "Go to JavaScripts."
  (interactive)
  (rails-nav:goto-file-with-menu "app/assets/javascripts/" "Go to javascripts.." "\\(js\\|coffee\\)" t))

(defun rails-find:assets-stylesheets ()
  "Run find-file in Rails \"assets/stylesheets\" dir"
  (interactive)
  (let ((default-directory (rails-core:file "app/assets/stylesheets")))
    (call-interactively 'ido-find-file)))

(defun rails-find:assets-javascripts ()
  "Run find-file in Rails \"assets/javascripts\" dir"
  (interactive)
  (let ((default-directory (rails-core:file "app/assets/javascripts")))
    (call-interactively 'ido-find-file)))

(defun iy-el-get-after-emacs-rails ()
  (define-key rails-minor-mode-map  (kbd "C-c C-c #")  'rails-spec:run-last)
  (define-key rails-minor-mode-map  (kbd "C-c C-c 3")  'rails-spec:run-last)
  (define-key rails-minor-mode-map  (kbd "C-c C-c .")  'rails-spec:run-current)
  (define-key rails-minor-mode-map  (kbd "C-c C-c /")  'rails-spec:run-this-spec)
  (define-key rails-minor-mode-map  (kbd "M-s SPC")  'rails-lib:run-primary-switch)
  (define-key rails-minor-mode-map  (kbd "M-s M-SPC")  'rails-lib:run-secondary-switch)
  (define-key rails-minor-mode-map  (kbd "C-c C-c g j")  'rails-nav:goto-assets-javascripts)
  (define-key rails-minor-mode-map  (kbd "C-c C-c g s")  'rails-nav:goto-assets-stylesheets)
  (define-key rails-minor-mode-map  (kbd "C-c C-c f j")  'rails-find:assets-javascripts)
  (define-key rails-minor-mode-map  (kbd "C-c C-c f s")  'rails-find:assets-stylesheets)
  (define-key rails-minor-mode-map  (kbd "<f9>") nil)
  (global-set-key (kbd "C-c C-c j") nil)
  (global-set-key (kbd "C-c C-c") nil))

(defun iy-turn-of-rails-minor-mode-for-modes ()
  (when (memq major-mode '(rebase-mode))
    (if rails-minor-mode
        (rails-minor-mode 0))))

(add-hook 'rails-minor-mode-hook 'iy-turn-of-rails-minor-mode-for-modes)

(push 'yari el-get-packages)
(defalias 'ri 'yari)

(push 'ruby-block el-get-packages)
(make-variable-buffer-local 'ruby-block-mode)
(custom-set-variables
 '(ruby-block-highlight-toggle 'overlay)
 '(ruby-block-highlight-face 'show-paren-match))

(push 'cucumber el-get-packages)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

(defvar gem-hist nil)
;; open gem interactively
(defun gem (&optional other-window)
  (interactive "P")
  (let ((file (ido-read-file-name
               "File: "
               (cadr
                (let ((choises
                       (mapcar
                        (lambda (line) (split-string line " "))
                        (butlast (split-string (with-output-to-string (shell-command "gem la" standard-output))
                                               "\n")))))
                  (assoc (ido-completing-read "Gem: " choises nil t nil gem-hist) choises))))))
    (if other-window
        (find-file-other-window file)
      (find-file file))))

(defun iy-ruby-mode-init ()
  (local-set-key (kbd "<return>") 'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-j") 'newline)
  (local-set-key (kbd "C-h h") 'yari)
  ;; (local-set-key (kbd "M-s `") 'ruby-find-error)
  ;; (local-set-key (kbd "M-s M-`") 'ruby-find-error)

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

(eval-after-load 'inf-ruby
  '(progn
     (add-to-list 'inf-ruby-implementations '("pry" . "gemset debug pry"))
     (setq inf-ruby-default-implementation "pry")
     (setq inf-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)> *")
     (setq inf-ruby-prompt-pattern "^\\[[0-9]+\\] pry\\((.*)\\)[>*\"'] *")))

(defalias 'irb 'inf-ruby)
(defalias 'pry 'inf-ruby)

(defadvice rails-core:prepare-command (around zeus-maybe (command) activate)
  (if (or (file-exists-p (rails-core:file ".zeus.sock"))
          (file-exists-p (rails-core:file "zeus.json")))
      (setq ad-return-value (concat "zeus-maybe " command))
    ad-do-it))

(provide 'iy-ruby-mode)
