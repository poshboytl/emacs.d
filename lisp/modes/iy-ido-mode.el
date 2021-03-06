(require 'iy-dep)

;; customization

(custom-set-variables
 '(ido-hacks-mode t)
 '(ido-enable-regexp nil)
 '(ido-enable-flex-matching t)
 '(ido-save-directory-list-file (concat iy-data-dir "ido.last"))
 '(ido-everywhere t)
 '(ido-read-file-name-as-directory-commands nil)
 '(ido-use-filename-at-point nil))

(ido-mode t)
(ido-load-history)

(push 'ido-hacks el-get-packages)
(push 'ido-complete-space-or-hyphen el-get-packages)
(push 'smex el-get-packages)

(defun iy-ido-mode-init ()
  (define-key ido-completion-map (kbd "M-m") 'ido-merge-work-directories)
  (define-key ido-completion-map (kbd "M-l") 'iy-dwim-dash)
  (define-key ido-completion-map (kbd "M-s") iy-map)
  (define-key ido-completion-map (kbd "C-c") 'ido-toggle-case))

(defun iy-el-get-after-smex ()
  (global-set-key (kbd "M-x") 'smex)
  (define-key iy-map (kbd "M-x") 'smex-major-mode-commands))

(defun iy-el-get-after-ido-hacks ()
  (put 'dired-do-rename 'ido-hacks-fix-default nil)
  (put 'dired-do-copy 'ido-hacks-fix-default nil)

  (ido-hacks-mode t)
  (iy-el-get-after-smex)
  (ad-enable-advice 'ido-read-internal 'around 'ido-completing-read-use-initial-input-as-default)
  (ad-activate 'ido-read-internal))

(defvar ido-completing-read-use-initial-input-as-default-commands nil
  "Use initial input as default in list commands")

(defvar ido-completing-read-use-initial-input--running nil)

(setq ido-completing-read-use-initial-input-as-default-commands
      '(ibuffer-filter-by-mode
        ibuffer-filter-by-used-mode
        rails-lib:run-primary-switch))

(put 'diredp-do-bookmark-in-bookmark-file 'ido 'ignore)
(put 'diredp-set-bookmark-file-bookmark-for-marked 'ido 'ignore)

(defadvice ido-read-internal (around ido-completing-read-use-initial-input-as-default)
  ;;(defun ido-read-internal (item prompt history &optional default require-match initial)
  (if (and
       (not ido-completing-read-use-initial-input--running)
       (memq this-command ido-completing-read-use-initial-input-as-default-commands))
      (let ((ido-completing-read-use-initial-input--running t))
        (setq default (or default initial))
        (setq initial nil)
        ad-do-it)
    ad-do-it))

(add-hook 'ido-setup-hook 'iy-ido-mode-init)

;; func accepts buffer and must return a string or nil
(defun iy-make-buffer-alist-by (func)
  (let ((hash (make-hash-table :test 'equal))
        alist)
    (mapc
     (lambda (buf)
       (let ((key (funcall func buf)))
         (when key (puthash key (cons buf (gethash key hash)) hash))))
     (buffer-list 'current))
    (maphash (lambda (k v) (setq alist (cons (cons k v) alist))) hash)
    alist))

(defun iy-ido-switch-buffer-two-steps (prompt func &optional hist def)
  (let ((alist (iy-make-buffer-alist-by func)))
    (switch-to-buffer
     (ido-completing-read
      "Buffer: "
      (mapcar 'buffer-name
              (cdr (assoc (ido-completing-read prompt alist nil t nil hist def) alist)))))))

(defvar iy-ido-switch-buffer-by-major-mode-hist nil)
(defun iy-ido-switch-buffer-by-major-mode ()
  (interactive)
  (flet ((get-major-mode-name (&optional buf)
                              (replace-regexp-in-string "-mode$" ""
                                                        (symbol-name (if buf
                                                                         (with-current-buffer buf major-mode)
                                                                       major-mode)))))
    (iy-ido-switch-buffer-two-steps
     "Mode: "
     'get-major-mode-name
     iy-ido-switch-buffer-by-major-mode-hist
     (get-major-mode-name))))

(defvar iy-ido-switch-buffer-by-ext-name-hist nil)
(defun iy-ido-switch-buffer-by-ext-name ()
  (interactive)
  (iy-ido-switch-buffer-two-steps
   "Extension: "
   (lambda (buf)
     (file-name-extension (or (buffer-file-name buf) "")))
   iy-ido-switch-buffer-by-ext-name-hist
   (file-name-extension (or (buffer-file-name) ""))))

(defalias 'modb 'iy-ido-switch-buffer-by-major-mode)
(defalias 'extb 'iy-ido-switch-buffer-by-ext-name)

;; Or bind to key
(define-key iy-map (kbd "b") 'iy-ido-switch-buffer-by-major-mode)
(define-key iy-map (kbd "B") 'iy-ido-switch-buffer-by-ext-name)
(define-key iy-map (kbd "M-b") 'iy-ido-switch-buffer-by-ext-name)

;;}}}

(provide 'iy-ido-mode)
