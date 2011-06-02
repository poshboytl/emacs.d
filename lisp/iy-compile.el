(require 'iy-dep)
(require 'flymake)

(add-to-list 'compilation-error-regexp-alist-alist
             '(maven "^\\[\\w+\\] \\(.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\] \\(.*\\)$" 1 2 3 (4)))
(add-to-list 'compilation-error-regexp-alist 'maven)

(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '("\\.rb\\'" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)


(defun flymake-init-hook ()
  (if (and (not (null buffer-file-name))
           (file-writable-p buffer-file-name)
           (not (string-match "flymake" buffer-file-name)))
      (flymake-mode t)))

(add-hook 'ruby-mode-hook 'flymake-init-hook)

(defun iy/next-flymake-error ()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

(define-key iy-map (kbd "`") 'iy/next-flymake-error)
(define-key iy-map (kbd "M-`") 'iy/next-flymake-error)

(defvar flymake-fringe-overlays nil)
(make-variable-buffer-local 'flymake-fringe-overlays)

(when (iy-require-maybe 'iy-packages)
  (defadvice flymake-make-overlay (after add-to-fringe first
                                         (beg end tooltip-text face mouse-face)
                                         activate compile)
    (push (fringe-helper-insert-region
           beg end
           (fringe-lib-load (if (eq face 'flymake-errline)
                                (and (boundp 'fringe-lib-exclamation-mark) fringe-lib-exclamation-mark)
                              (and (boundp 'fringe-lib-question-mark) fringe-lib-question-mark)))
           'left-fringe 'font-lock-warning-face)
          flymake-fringe-overlays))

  (defadvice flymake-delete-own-overlays (after remove-from-fringe activate
                                                compile)
    (mapc 'fringe-helper-remove flymake-fringe-overlays)
    (setq flymake-fringe-overlays nil)))

(provide 'iy-compile)
