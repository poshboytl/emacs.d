;;{{{ General

(custom-set-variables
 '(vc-follow-symlinks t)
 '(revert-without-query '("COMMIT_EDITMSG\\'")))

;;}}}

;;{{{ GIT

(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.git/config\\'" . conf-mode))

(custom-set-variables
 '(git-state-modeline-decoration (quote git-state-decoration-large-dot))
 '(magit-process-popup-time 60)
 '(magit-repo-dirs (list iy-codebase-dir))
 '(magit-repo-dirs-depth 1))

(push 'git-emacs el-get-packages)
(push 'magit el-get-packages)

(defadvice git-describe-commit (around git-ll activate)
  (with-temp-buffer
    (call-process "git" nil t nil
                  "log" "-1" "--pretty=format:'%h... %an - %s %ar'"
                  hash)
    (setq ad-return-value (buffer-substring (point-min) (1- (point-max))))))

(defun iy-magit-mode-init ()
  (define-key magit-mode-map (kbd "M-s") iy-map)
  (local-set-key (kbd "<f12>") 'magit-quit-window))

(add-hook 'magit-mode-hook 'iy-magit-mode-init)

(defun iy-magit-log-edit-mode-init ()
  (flyspell-mode 1)
  (auto-fill-mode t)
  (setq fill-column 72))

(add-hook 'magit-log-edit-mode-hook 'iy-magit-log-edit-mode-init)

;;}}}

;;{{{ Mode Line

(defadvice vc-mode-line (after colorize-vc-mode-line activate)
  (when vc-mode
    ;; git is default
    (put-text-property 1 (length vc-mode) 'face 'font-lock-string-face vc-mode)))

;;}}}

(provide 'iy-scm)
