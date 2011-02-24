(require 'iy-dep)

;; customization
(setq ido-save-directory-list-file (concat iy-data-dir "ido.last"))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-read-file-name-as-directory-commands nil)
(setq ido-use-filename-at-point nil)

(ido-mode t)

(defun iy-ido-mode-init ()
  (define-key ido-completion-map (kbd "M-m") 'ido-merge-work-directories)
  (define-key ido-completion-map (kbd "M-s") 'iy-map)
  (define-key ido-completion-map (kbd "C-c") 'ido-toggle-case)
  (define-key ido-completion-map (kbd "C-'") 'ido-restrict-to-matches))

(add-hook 'ido-setup-hook 'iy-ido-mode-init)

(provide 'iy-ido)
