(require 'iy-dep)

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))


(defun iy-el-get-after-yasnippet ()
  (require 'dropdown-list nil t)
  (yas/initialize)
  (setq yas/root-directory (concat iy-config-dir "snippets"))
  (yas/load-directory yas/root-directory))

(push '(:name yasnippet
              :after iy-el-get-after-yasnippet)
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
  (iy/string-camel-to-underscore string))
(defun yas/camel-to-lower-underscore (string)
  (downcase (iy/string-camel-to-underscore string)))

(defun yas/underscore-to-camel (string)
  (replace-regexp-in-string "_" "" (upcase-initials string)))

(define-key iy-map [tab] 'yas/ido-insert-snippets)

(provide 'iy-template)