(require 'iy-dep)

;;{{{ Autocomplate

(push 'pos-tip el-get-packages)
(push 'popup el-get-packages)
(push 'fuzzy el-get-packages)
(push 'auto-complete el-get-packages)

(global-set-key (kbd "M-/") 'iy-safe-ac-try-expand)

(defun iy-el-get-after-popup ()
  (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
  (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
  (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
  (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
  (define-key popup-menu-keymap (kbd "M-p") 'popup-previous))

(defun iy-safe-ac-try-expand ()
  (interactive)
  (when (ac-trigger-command-p last-command)
    (let ((before (point))
          (ac-use-fuzzy nil))
      (auto-complete)
      (or (ac-menu-live-p)
          (not (eq before (point)))))))

(defun iy-el-get-after-auto-complete ()
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories (concat iy-el-get-dir "auto-complete/dict"))
  (add-to-list 'ac-dictionary-directories (concat iy-config-dir "ac-dict"))
  (require 'auto-complete-config)

  (push 'markdown-mode ac-modes)
  (push 'org-mode ac-modes)
  (push 'coffee-mode ac-modes)

  (define-key ac-complete-mode-map (kbd "RET") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "<return>") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-<return>") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-SPC") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-g") 'ac-stop)

  (setq ac-trigger-commands
        (append
         '(backward-delete-char-untabify
           delete-backward-char
           autopair-backspace)
         ac-trigger-commands))

  (setq-default
   ac-sources '(ac-source-yasnippet
                ac-source-abbrev
                ac-source-dictionary
                ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'iy-ac-emacs-lisp-mode-setup)
  (global-auto-complete-mode t))

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
          ac-source-words-in-same-mode-buffers)))

(custom-set-variables
 '(ac-use-fuzzy t)
 '(ac-trigger-key "TAB")
 '(ac-auto-start nil)
 '(ac-use-menu-map t)
 '(ac-show-menu nil)
 '(ac-comphist-file (expand-file-name (concat iy-data-dir "ac-comphist.dat")))
 '(ac-use-quick-help nil))

;;}}}

;;{{{ Snippet

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
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
 '(yas/trigger-key "TAB")
 '(yas/choose-keys-first nil)
 '(yas/prompt-functions (quote (yas/popup-isearch-prompt
                                yas/ido-prompt
                                yas/x-prompt
                                yas/no-prompt)))
 '(yas/use-menu nil))

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(defun iy-el-get-after-yasnippet ()
  (require 'dropdown-list nil t)
  (setq yas/snippet-dirs (list (concat iy-config-dir "snippets")))
  (yas/initialize)
  (yas/load-snippet-dirs)
  (ad-activate 'ruby-indent-line)
  (ad-enable-advice 'ruby-indent-line 'around 'iy-tab-ruby-noconflict)
  (ad-activate 'markdown-cycle)
  (ad-enable-advice 'markdown-cycle 'around 'iy-tab-markdown-noconflict))

(push 'yasnippet el-get-packages)

(defun yas/very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(defun iy-tab-org-noconflict ()
  (set (make-variable-buffer-local 'yas/trigger-key) [tab])
  (add-to-list 'org-tab-first-hook 'iy-safe-ac-try-expand)
  (add-to-list 'org-tab-first-hook 'yas/very-safe-expand)
  (define-key yas/keymap [tab] 'yas/next-field))
(defadvice markdown-cycle (around iy-tab-markdown-noconflict)
  (or (yas/very-safe-expand)
      (iy-safe-ac-try-expand)
      ad-do-it))
(defadvice ruby-indent-line (around iy-tab-ruby-noconflict)
  (or (yas/very-safe-expand)
      (iy-safe-ac-try-expand)
      ad-do-it))

(defun yas/ido-insert-snippets (&optional no-condition)
  (interactive "P")
  (let ((yas/prompt-functions '(yas/ido-prompt)))
    (yas/insert-snippet)))

(defun yas/ido-choose-value (possibilities)
  "Prompt for a string in the list POSSIBILITIES and return it."
  (unless (or yas/moving-away-p
              yas/modified-p)
    (ido-completing-read "Choose: " possibilities)))

(defun yas/buffer-name-stub ()
  (let ((name (or (buffer-file-name)
                  (buffer-name))))
    (replace-regexp-in-string
     "^t_\\|_test$\\|_spec$" ""
     (file-name-sans-extension (file-name-nondirectory name)))))

(defun yas/camel-to-underscore (string)
  (iy-string-camel-to-underscore string))
(defun yas/camel-to-lower-underscore (string)
  (downcase (iy-string-camel-to-underscore string)))

(defun yas/underscore-to-camel (string)
  (replace-regexp-in-string "_" "" (upcase-initials string)))

(define-key iy-map (kbd "M-/") 'yas/ido-insert-snippets)

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

;; fix flyspell
(defadvice called-interactively-p (before iy-fix-interactively-p (&optional arg) activate))

(custom-set-variables
 '(flyspell-use-meta-tab nil))

;;}}}

(provide 'iy-completion)
