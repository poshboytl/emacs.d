(require 'iy-dep)

;;{{{ Autocomplate

(push '(:name
        auto-complete
        :type http-tar
        :options ("xjf")
        :url "http://cx4a.org/pub/auto-complete/auto-complete-1.3.1.tar.bz2"
        :post-init iy-el-get-after-auto-complete)
      el-get-sources)

(defun iy-el-get-after-auto-complete ()
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories (concat iy-el-get-dir "auto-complete/dict"))
  (add-to-list 'ac-dictionary-directories (concat iy-config-dir "ac-dict"))
  (require 'auto-complete-config)
  (ac-config-default))

(custom-set-variables
 '(ac-trigger-key "TAB")
 '(ac-auto-start nil))

;;}}}

;;{{{ Snippet

(custom-set-variables
 '(yas/choose-keys-first nil)
 '(yas/prompt-functions (quote (yas/dropdown-prompt yas/ido-prompt yas/x-prompt yas/no-prompt)))
 '(yas/use-menu (quote abbreviate)))

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(defun iy-el-get-after-yasnippet ()
  (require 'dropdown-list nil t)
  (setq yas/snippet-dirs (list (concat iy-config-dir "snippets")))
  (yas/initialize)
  (yas/load-snippet-dirs))

(push '(:name yasnippet
              :type git
              :url "git://github.com/capitaomorte/yasnippet.git"
              :features "yasnippet"
              :description "YASnippet is a template system for Emacs."
              :post-init iy-el-get-after-yasnippet)
      el-get-sources)

(defun yas/fix-keybindings ()
  (make-variable-buffer-local 'yas/trigger-key)
  (setq yas/trigger-key "<tab>")
  (define-key yas/keymap [tab] 'yas/next-field))

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
 '(mail-abbrevs-mode t))
;;}}}

;;{{{ Spell

;; fix flyspell
(defadvice called-interactively-p (before iy-fix-interactively-p (&optional arg) activate))

(custom-set-variables
 '(flyspell-use-meta-tab nil))

;;}}}

(provide 'iy-completion)
