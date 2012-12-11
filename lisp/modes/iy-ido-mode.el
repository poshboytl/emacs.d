(require 'iy-dep)

;; customization

(custom-set-variables
 '(ido-ubiquitous-command-exceptions '(execute-extended-command
                                       ;; describe-function
                                       ;; describe-variable
                                       ;; customize-variable
                                       ;; customize-variable-other-window
                                       ;; customize-option
                                       ;; customize-option-other-window
                                       ;; customize-face
                                       ;; customize-face-other-window
                                       ;; load-library
                                       w3m-goto-url-new-session
                                       w3m-goto-url
                                       where-is))
 '(ido-ubiquitous t)
 '(ido-enable-regexp nil)
 '(ido-enable-flex-matching t)
 '(ido-save-directory-list-file (concat iy-data-dir "ido.last"))
 '(ido-everywhere t)
 '(ido-read-file-name-as-directory-commands nil)
 '(ido-use-filename-at-point nil))

(ido-mode t)
(ido-load-history)

(push 'ido-ubiquitous el-get-packages)
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

(defvar ido-completing-read-use-initial-input-as-default-commands nil
  "Use initial input as default in list commands")

(defvar ido-completing-read-use-initial-input--running nil)

(setq ido-completing-read-use-initial-input-as-default-commands
      '(ibuffer-filter-by-mode
        ibuffer-filter-by-used-mode))

(defadvice ido-completing-read (around ido-completing-read-use-initial-input-as-default activate)
  (if (and
       (not ido-completing-read-use-initial-input--running)
       (memq this-command ido-completing-read-use-initial-input-as-default-commands))
      (let ((ido-completing-read-use-initial-input--running t))
        (setq ad-return-value (ido-completing-read prompt choices _predicate require-match nil hist
                                                   (or def initial-input)
                                                   _inherit-input-method)))
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

(defun iy-ido-switch-buffer-two-steps (prompt func &optional hist)
  (let ((alist (iy-make-buffer-alist-by func)))
    (switch-to-buffer
     (ido-completing-read
      "Buffer: "
      (mapcar 'buffer-name
              (cdr (assoc (ido-completing-read prompt alist nil t nil hist) alist)))))))

(defvar iy-ido-switch-buffer-by-major-mode-hist nil)
(defun iy-ido-switch-buffer-by-major-mode ()
  (interactive)
  (iy-ido-switch-buffer-two-steps
   "Mode: "
   (lambda (buf) (replace-regexp-in-string "-mode$" ""
                                           (symbol-name (with-current-buffer buf major-mode))))
   iy-ido-switch-buffer-by-major-mode-hist))

(defvar iy-ido-switch-buffer-by-ext-name-hist nil)
(defun iy-ido-switch-buffer-by-ext-name ()
  (interactive)
  (iy-ido-switch-buffer-two-steps
   "Extension: "
   (lambda (buf)
     (file-name-extension (or (buffer-file-name buf) "")))
   iy-ido-switch-buffer-by-ext-name-hist))

(defalias 'modb 'iy-ido-switch-buffer-by-major-mode)
(defalias 'extb 'iy-ido-switch-buffer-by-ext-name)

;; Or bind to key
;; (global-set-key (kbd "M-s b") 'iy-ido-switch-buffer-by-major-mode)
;; (global-set-key (kbd "M-s B") 'iy-ido-switch-buffer-by-ext-name)


;;}}}

(provide 'iy-ido-mode)
