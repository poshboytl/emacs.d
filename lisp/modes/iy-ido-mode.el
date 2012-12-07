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

(add-hook 'ido-setup-hook 'iy-ido-mode-init)

;;}}}

(provide 'iy-ido-mode)
