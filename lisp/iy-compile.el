(require 'iy-dep)
(require 'flymake)

(push 'flymake-easy el-get-packages)
(push 'flymake-sass el-get-packages)
(push 'flymake-ruby el-get-packages)
(push 'flymake-jslint el-get-packages)
(push 'flymake-coffee el-get-packages)

(custom-set-variables
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error)))

(add-to-list 'compilation-error-regexp-alist-alist
             '(maven "^\\[\\w+\\] \\(.*\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\] \\(.*\\)$" 1 2 3 (4)))
(add-to-list 'compilation-error-regexp-alist 'maven)

(defadvice flymake-easy-load (around disable-on-buffer-without-writable-file activate)
  (when (and (not (null buffer-file-name))
             (file-writable-p buffer-file-name)
             (not (string-match "^<\\|flymake" buffer-file-name)))
    ad-do-it))

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'js-mode-hook 'flymake-jslint-load)
(add-hook 'sass-mode-hook 'flymake-sass-load)
(add-hook 'css-mode-hook 'flymake-sass-load)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

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
  (flymake-err-echo))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  (flymake-err-echo))

(define-key iy-map (kbd "`") 'flymake-goto-next-error)
(define-key iy-map (kbd "~") 'flymake-goto-prev-error)
(define-key iy-map (kbd "M-`") 'flymake-goto-next-error)

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
