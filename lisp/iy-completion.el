(require 'iy-dep)

(push 'pos-tip el-get-packages)
(push 'popup el-get-packages)


;;{{{ Tab
(defun iy-fix-tab-in-map (map)
  (let ((binding (assoc 'tab map)))
    (when binding
      (setcar binding 9))))

(eval-after-load "org"
  '(progn (iy-fix-tab-in-map org-mode-map)))
(eval-after-load "markdown-mode"
  '(progn (iy-fix-tab-in-map markdown-mode-map)))

;;{{{ Snippet

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     :isearch t
     )))

(custom-set-variables
 '(yas-trigger-key "TAB")
 '(yas-choose-keys-first nil)
 '(yas-prompt-functions (quote (yas-popup-isearch-prompt
                                yas-ido-prompt
                                yas-x-prompt
                                yas-no-prompt)))
 '(yas-use-menu nil))

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(defadvice yas--menu-keymap-get-create (around ignore (mode) activate))

(defun iy-el-get-after-yasnippet ()
  (require 'dropdown-list nil t)

  (setq yas-snippet-dirs (list (concat iy-config-dir "snippets")))
  (setq yas-use-menu nil)
  (yas-global-mode 1))

(push 'yasnippet el-get-packages)

(defun yas-safer-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (call-interactively 'yas-expand)))
  
(defun yas-ido-insert-snippets (&optional no-condition)
  (interactive "P")
  (let ((yas-prompt-functions '(yas-ido-prompt)))
    (yas-insert-snippet)))

(defun yas-buffer-name-stub ()
  (let ((name (or (buffer-file-name)
                  (buffer-name))))
    (replace-regexp-in-string
     "^t_\\|_test$\\|_spec$" ""
     (file-name-sans-extension (file-name-nondirectory name)))))

(defun yas-camel-to-underscore (string)
  (iy-string-camel-to-underscore string))
(defun yas-camel-to-lower-underscore (string)
  (downcase (iy-string-camel-to-underscore string)))

(defun yas-underscore-to-camel (string)
  (replace-regexp-in-string "_" "" (upcase-initials string)))

(define-key iy-map (kbd "M-/") 'yas-ido-insert-snippets)

;;}}}

;;{{{ Autocomplate

(push 'fuzzy el-get-packages)
(push 'auto-complete el-get-packages)

(eval-after-load 'popup
  '(progn
     (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
     (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
     (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
     (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
     (define-key popup-menu-keymap (kbd "M-p") 'popup-previous)))

(defun iy-el-get-after-auto-complete ()
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories (concat iy-el-get-dir "auto-complete/dict"))
  (add-to-list 'ac-dictionary-directories (concat iy-config-dir "ac-dict"))
  (require 'auto-complete-config)

  (push 'markdown-mode ac-modes)
  (push 'org-mode ac-modes)
  (push 'coffee-mode ac-modes)
  (push 'inferior-emacs-lisp-mode ac-modes)

  (define-key ac-complete-mode-map (kbd "RET") 'nil)
  (define-key ac-complete-mode-map (kbd "<return>") 'nil)
  (define-key ac-complete-mode-map (kbd "M-SPC") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-<tab>") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-g") 'ac-stop)

  (setq ac-trigger-commands
        (append
         '(backward-delete-char-untabify
           delete-backward-char
           autopair-backspace)
         ac-trigger-commands))

  (setq ac-trigger-on-stop-words-when-manually nil)

  (setq-default
   ac-sources '(ac-source-yasnippet
                ac-source-abbrev
                ac-source-dictionary
                ac-source-imenu
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers))

  (add-hook 'emacs-lisp-mode-hook 'iy-ac-emacs-lisp-mode-setup)
  (add-hook 'ruby-mode-hook 'iy-ac-ruby-mode-setup)
  (add-hook 'sh-mode-hook 'iy-ac-sh-mode-setup)
  (add-hook 'ielm-mode-hook 'iy-ac-ielm-mode-setup)

  (global-auto-complete-mode t)

  (defadvice ac-trigger-key-command (around disable-by-negative-prefix (&optional force) activate)
    (interactive "P")
    (let* ((ac-fuzzy-enable nil) ; cannot test whether ac expanded when fuzzy enabled, and it is slow
           (before-point (point))
           (auto-complete-mode nil)
           (keys (this-command-keys-vector))
           ;; hardcode to get the last key, works for single keystroke trigger key.
           (key (if keys (vector (elt keys (1- (length keys))))))
           (command (if key (key-binding key)))
           ac-expanded)
      ;; skip if prefix is negative
      (unless (< (prefix-numeric-value force) 0)
        (when (not force)
          (setq ac-expanded (yas-safer-expand)))
        (when (and (not ac-expanded)
                   (or force (ac-trigger-command-p last-command)))
          (let ((auto-complete-mode t))
            (auto-complete)
            ;; expanded if menu is live or point has moved
            (setq ac-expanded (or (ac-menu-live-p)
                                  (not (equal (point) before-point)))))))

      (when (and (not ac-expanded)
                 command
                 (not (eq command 'ac-trigger-key-command)))
        (call-interactively command))))
  )

(defun iy-ac-emacs-lisp-mode-setup ()
  (setq ac-sources
        '(
          ac-source-yasnippet
          ac-source-features
          ac-source-functions
          ac-source-variables
          ac-source-symbols
          ac-source-abbrev
          ac-source-dictionary
          ac-source-imenu
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers)))
(defun iy-ac-ruby-mode-setup ()
  (set (make-variable-buffer-local 'ac-stop-words) '("end")))

(defun iy-ac-sh-mode-setup ()
  (set (make-variable-buffer-local 'ac-stop-words) '("fi" "done" "esac")))

(defun iy-ac-ielm-mode-setup ()
 "Enables `auto-complete' support in \\[ielm]."
 (setq ac-sources '(ac-source-yasnippet
                    ac-source-features
                     ac-source-functions
                     ac-source-variables
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers)))

(custom-set-variables
 '(ac-use-fuzzy nil)
 '(ac-trigger-key "TAB")
 '(ac-auto-start nil)
 '(ac-use-menu-map t)
 '(ac-show-menu nil)
 '(ac-comphist-file (expand-file-name (concat iy-data-dir "ac-comphist.dat")))
 '(ac-use-quick-help nil))

;;}}}

;;{{{ Hippie Exapnd

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-list))

(global-set-key (kbd "M-/") 'hippie-expand)

;;}}}

;;{{{ Abbrev
(custom-set-variables
 '(abbrev-mode t)
 '(mail-abbrevs-mode t)
 '(abbrev-file-name (concat iy-data-dir "abbrev_defs")))
;;}}}

;;{{{ Spell

(custom-set-variables
 '(flyspell-use-meta-tab nil))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map [(control ?\,)] nil)
     (define-key flyspell-mode-map [(control ?\.)] nil)))

;;}}}

(provide 'iy-completion)
