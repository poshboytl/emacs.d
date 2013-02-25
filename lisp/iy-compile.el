(require 'iy-dep)
(require 'flymake)

(custom-set-variables
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error)))

(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

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


(defun flymake-jslint-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "jslint" (list "--terse" local-file))))

(push '("\\.rb\\'" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(push '("\\.js\\'" flymake-jslint-init) flymake-allowed-file-name-masks)

(push '("^\\([^(]*\\)(\\([0-9]+\\)):\\(.+\\)$"
        1 2 nil 3)
      flymake-err-line-patterns)

(defun flymake-init-hook ()
  (if (and (not (null buffer-file-name))
           (file-writable-p buffer-file-name)
           (not (string-match "^<\\|flymake" buffer-file-name)))
      (flymake-mode 1)))

(add-hook 'ruby-mode-hook 'flymake-init-hook)
(add-hook 'js-mode-hook 'flymake-init-hook)

(defun iy-next-flymake-error ()
  (interactive)
  (flymake-goto-next-error)
  (flymake-err-echo))
(defun iy-prev-flymake-error ()
  (interactive)
  (flymake-goto-prev-error)
  (flymake-err-echo))

(defun flymake-err-at (pos)
  (let ((overlays (overlays-at pos)))
    (remove nil
            (mapcar (lambda (overlay)
                      (and (overlay-get overlay 'flymake-overlay)
                           (overlay-get overlay 'help-echo)))
                    overlays))))

(defun flymake-err-echo ()
  (interactive)
  (message "%s" (mapconcat 'identity (flymake-err-at (point)) "\n")))

(defadvice flymake-goto-next-error (after display-message activate compile)
  (my-flymake-err-echo))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  (my-flymake-err-echo))

(define-key iy-map (kbd "`") 'iy-next-flymake-error)
(define-key iy-map (kbd "~") 'iy-prev-flymake-error)
(define-key iy-map (kbd "M-`") 'iy-next-flymake-error)

(defvar flymake-fringe-overlays nil)
(make-variable-buffer-local 'flymake-fringe-overlays)

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
  (setq flymake-fringe-overlays nil))

(autoload 'smart-compile "smart-compile+" nil t)
(autoload 'smart-run "smart-compile+" nil t)
(eval-after-load 'smart-compile+
  '(progn
     (setq smart-run-alist
           (append
            (list
             (cons "_spec\\.rb\\'" '(compile (concat "cd " (eproject-root) "; rr rspec --no-color " (file-relative-name (buffer-file-name) (eproject-root)))))
             (cons "\\.rb\\'" "rr ruby %F"))
            smart-run-alist))
     (setq smart-compile-alist
           (append
            (list
             (cons "\\.coffee$" "coffee -c %f"))
            smart-compile-alist))
     (setq smart-executable-alist
           (append '("%n.rb") smart-executable-alist))))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'iy-compile)
