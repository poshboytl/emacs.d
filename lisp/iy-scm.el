;;{{{ General

(custom-set-variables
  '(vc-follow-symlinks t)
  '(revert-without-query '("COMMIT_EDITMSG\\'" "git-rebase-todo")))

(push 'git-gutter el-get-packages)
(push 'git-gutter-fringe el-get-packages)

(defadvice git-gutter:in-git-repository-p (around safe-call-process-shell-command () activate)
           (if (and default-directory (file-directory-p default-directory))
             ad-do-it
             (setq ad-return-value 1)))

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
  (define-key magit-mode-map (kbd "W") 'magit-toggle-whitespace)
  (local-set-key (kbd "<f12>") 'magit-quit-window))

(add-hook 'magit-mode-hook 'iy-magit-mode-init)

(defun iy-magit-log-edit-mode-init ()
  (flyspell-mode 1)
  (auto-fill-mode t)
  (setq fill-column 72))

(add-hook 'magit-log-edit-mode-hook 'iy-magit-log-edit-mode-init)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;;}}}

;;{{{ Mode Line

(defadvice vc-mode-line (after colorize-vc-mode-line activate)
  (when vc-mode
    ;; git is default
    (put-text-property 1 (length vc-mode) 'face 'font-lock-string-face vc-mode)))

;;}}}

(provide 'iy-scm)
