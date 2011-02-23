;;; GIT

(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.git/config\\'" . conf-mode))

(push '(:name magit :features nil) el-get-sources)
(push '(:name git-emacs
              :features git-emacs-autoloads)
      el-get-sources)

(autoload 'magit-status "magit" nil t)

(defadvice git-describe-commit (around git-ll activate)
  (with-temp-buffer
    (call-process "git" nil t nil
                  "log" "-1" "--pretty=format:'%h... %an - %s %ar'"
                  hash)
    (setq ad-return-value (buffer-substring (point-min) (1- (point-max))))))

(defun iy/magit-mode-init ()
  (define-key magit-mode-map (kbd "M-s") 'iy-map))
(add-hook 'magit-mode-hook 'iy/magit-mode-init)

(defun iy/magit-log-edit-mode-init ()
  (flyspell-mode 1)
  (auto-fill-mode t)
  (setq fill-column 72))

(add-hook 'magit-log-edit-mode-hook 'iy/magit-log-edit-mode-init)

(provide 'iy-scm)
