(require 'iy-dep)

(push 'pos-tip el-get-packages)
(push 'popup el-get-packages)

;;{{{ Tab
(defun iy-tab-noconflict ()
  (let ((command (key-binding [tab])))
    (local-unset-key [tab])
    (local-set-key (kbd "TAB") command)))
(add-hook 'ruby-mode-hook 'iy-tab-noconflict)
(add-hook 'markdown-mode-hook 'iy-tab-noconflict)
(add-hook 'org-mode-hook 'iy-tab-noconflict)
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
  (yas/load-snippet-dirs))

(push 'yasnippet el-get-packages)

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

;;{{{ Autocomplate

(push 'fuzzy el-get-packages)
(push 'auto-complete el-get-packages)

(defun iy-el-get-after-popup ()
  (define-key popup-menu-keymap (kbd "M-n") 'popup-next)
  (define-key popup-menu-keymap (kbd "TAB") 'popup-next)
  (define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
  (define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
  (define-key popup-menu-keymap (kbd "M-p") 'popup-previous))

(defun iy-el-get-after-auto-complete ()
  (require 'auto-complete)
  (add-to-list 'ac-dictionary-directories (concat iy-el-get-dir "auto-complete/dict"))
  (add-to-list 'ac-dictionary-directories (concat iy-config-dir "ac-dict"))
  (require 'auto-complete-config)

  (push 'markdown-mode ac-modes)
  (push 'org-mode ac-modes)
  (push 'coffee-mode ac-modes)

  (define-key ac-complete-mode-map (kbd "M-<tab>") 'ac-expand)
  (define-key ac-complete-mode-map (kbd "RET") 'nil)
  (define-key ac-complete-mode-map (kbd "<return>") 'nil)
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
                ac-source-imenu
                ac-source-abbrev
                ac-source-dictionary
                ac-source-words-in-same-mode-buffers))

  (add-hook 'emacs-lisp-mode-hook 'iy-ac-emacs-lisp-mode-setup)
  (add-hook 'ruby-mode-hook 'iy-ac-ruby-mode-setup)
  (add-hook 'sh-mode-hook 'iy-ac-sh-mode-setup)

  (global-auto-complete-mode t)

  ;; temporarily disable ac when prefix is C-u, use C-u C-u to force ac
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
      ;; skip ac if prefix is C-u
      (if (eq (prefix-numeric-value force) 4)
          (setq ac-expanded nil)
        ;; no ac if last-command not in trigger commands,
        ;; but C-u C-u force ac
        (if (or force (ac-trigger-command-p last-command))
            (let ((auto-complete-mode t))
              (auto-complete)
              ;; expanded if menu is live or point has moved
              (setq ac-expanded (or (ac-menu-live-p)
                                    (not (equal (point) before-point)))))
          (setq ac-expanded nil)))

      (when (and (not ac-expanded)
                 command
                 (not (eq command 'ac-trigger-key-command)))
            (call-interactively command)))))

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
(defun iy-ac-ruby-mode-setup ()
  (set (make-variable-buffer-local 'ac-stop-words) '("end")))

(defun iy-ac-sh-mode-setup ()
  (set (make-variable-buffer-local 'ac-stop-words) '("fi" "done" "esac")))

(custom-set-variables
 '(ac-use-fuzzy t)
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

;;}}}

(provide 'iy-completion)
