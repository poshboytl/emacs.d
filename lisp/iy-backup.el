(custom-set-variables
 '(auto-save-interval 50)
 '(auto-save-list-file-prefix (concat iy-data-dir "autosaves-"))
 '(auto-save-timeout 10)
 '(backup-directory-alist (list (cons "." (expand-file-name "~/.backup/emacs"))))
 '(delete-old-versions t)
 '(desktop-base-file-name ".emacs.desktop")
 '(desktop-path (list "." iy-data-dir))
 '(desktop-restore-eager 3)
 '(desktop-save (quote ask-if-new))
 '(kept-new-versions 20)
 '(kept-old-versions 5)
 '(recentf-arrange-rules (quote (("Elisp files (%d)" ".\\.el\\'") ("Java files (%d)" ".\\.java\\'") ("C/C++ files (%d)" ".\\.c\\(pp\\)?\\'" ".\\.h\\(pp\\)?\\'") ("Org files (%d)" ".\\.org\\'"))))
 '(recentf-exclude (quote ("semantic\\.cache" "COMMIT_EDITMSG" "git-emacs-tmp.*" "\\.breadcrumb" "\\.ido\\.last" "\\.projects.ede")))
 '(recentf-menu-filter (quote recentf-arrange-by-rule))
 '(recentf-save-file (concat iy-data-dir "recentf"))
 '(session-initialize (quote (de-saveplace session)))
 '(session-save-file (concat iy-data-dir "session"))
 '(vc-make-backup-files t)
 '(version-control t)
 )

(defcustom iy-session-enable-desktop
  nil
  "Whether to use desktop-save-mode"
  :group 'iy-config
  :type 'boolean)

(when (require 'session nil t)
  (add-hook 'after-init-hook 'session-initialize)
  (add-to-list 'session-globals-exclude 'org-mark-ring))

(if iy-session-enable-desktop
    (progn
      (desktop-save-mode 1)
      (setq history-length 250)
      (add-to-list 'desktop-globals-to-save 'file-name-history)
      (add-to-list 'desktop-modes-not-to-save 'Info-mode)
      (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
      (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)))

(recentf-mode)

(push '(:name pick-backup :type elpa) el-get-sources)

(provide 'iy-backup)
