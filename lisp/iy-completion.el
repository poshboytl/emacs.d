(require 'iy-dep)

;;{{{ Autocomplate

(push 'pos-tip el-get-packages)
(push 'popup el-get-packages)
(push 'fuzzy el-get-packages)
(push 'auto-complete el-get-packages)

(defun iy-el-get-after-auto-complete ()
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories (concat iy-el-get-dir "auto-complete/dict"))
  (add-to-list 'ac-dictionary-directories (concat iy-config-dir "ac-dict"))
  (require 'auto-complete-config)

  (define-key ac-complete-mode-map (kbd "M-SPC") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-/") 'ac-expand)
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
  (ac-config-default))

(custom-set-variables
 '(ac-auto-start)
 '(ac-trigger-key "M-/")
 '(ac-use-menu-map t)
 '(ac-comphist-file (expand-file-name (concat iy-data-dir "ac-comphist.dat")))
 '(ac-use-quick-help nil))

;;}}}

;;{{{ Snippet

(custom-set-variables
 '(yas/choose-keys-first nil)
 '(yas/prompt-functions (quote (yas/dropdown-prompt yas/ido-prompt yas/x-prompt yas/no-prompt)))
 '(yas/use-menu (quote abbreviate)))

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))


(defun iy-el-get-after-yasnippet ()
  (add-to-list 'ac-modes 'markdown-mode)
  (add-to-list 'ac-modes 'org-mode)

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
  (add-to-list 'org-tab-first-hook 'yas/very-safe-expand)
  (define-key yas/keymap [tab] 'yas/next-field))
(defadvice markdown-cycle (around iy-tab-markdown-noconflict)
  (unless (yas/very-safe-expand) ad-do-it))
(defadvice ruby-indent-line (around iy-tab-ruby-noconflict)
  (unless (yas/very-safe-expand) ad-do-it))

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
        ;; senator-try-expand-semantic
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ;; try-expand-all-abbrevs
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        ;; try-expand-list
        ;; try-complete-lisp-symbol-partially
        ;; try-complete-lisp-symbol
        try-expand-whole-kill
        ))

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
