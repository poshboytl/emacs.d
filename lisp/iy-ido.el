(ido-mode t)

(defun iy-ido-mode-init ()
  (define-key ido-completion-map (kbd "M-m") 'ido-merge-work-directories)
  (define-key ido-completion-map (kbd "M-s") 'iy-map)
  (define-key ido-completion-map (kbd "C-c") 'ido-toggle-case)
  (define-key ido-completion-map (kbd "C-'") 'ido-restrict-to-matches)
  (define-key ido-completion-map (kbd "M-v") 'switch-to-completions)
  )
(add-hook 'ido-setup-hook 'iy-ido-mode-init)

(provide 'iy-ido)
