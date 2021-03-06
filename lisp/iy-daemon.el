;;; iy-daemon.el --- Start daemon for emacsclient

(require 'iy-dep)

(defcustom iy-daemon-enable-daemon
  t
  "Enable Daemon Mode"
  :group 'iy-config
  :type 'boolean)

(defcustom iy-daemon-delete-frame-functions
  '(anything-c-adaptive-save-history
    bookmark-exit-hook-internal
    ac-comphist-save
    ido-kill-emacs-hook
    org-clock-save
    org-id-locations-save
    org-babel-remove-temporary-directory
    recentf-save-list
    semanticdb-kill-emacs-hook
    session-save-session
    w3m-arrived-shutdown
    w3m-cookie-shutdown
    tramp-dump-connection-properties)
  "List of functions that should be called when a OS window is closed"
  :group 'iy-config
  :type '(repeat symbol))

(setq edit-server-default-major-mode 'markdown-mode)
;; (setq edit-server-url-major-mode-alist
;;         '(("github\\.com" . markdown-mode)))

(if iy-daemon-enable-daemon
    (progn
      (defun iy-daemon-run-delete-frame-hooks (frame)
        (when (and (server-running-p)
                   (= 2 (length (frame-list))))
          (dolist (f iy-daemon-delete-frame-functions)
            (when (fboundp f)
              (message "Run delete frame hook %s" (prin1-to-string f))
              (funcall f)))))

      ;; do not run it in Mac OS X
      (unless (eq system-type 'darwin)
        (add-hook 'delete-frame-functions 'iy-daemon-run-delete-frame-hooks))

      (server-start)

      (push 'edit-server el-get-packages)
      (eval-after-load "edit-server"
        '(progn
           (edit-server-start)))))

(defun iy-server-visit-setup ()
  (let ((base (file-name-nondirectory (or (buffer-file-name) default-directory))))
    (cond ((member base '("MERGE_MSG" "COMMIT_EDITMSG"))
           (flyspell-mode 1)
           (auto-fill-mode t)
           (setq fill-column 72)
           (when (fboundp 'rails-minor-mode)
             (rails-minor-mode 0))
           (local-set-key (kbd "C-c C-c") 'server-edit))
          ((string-match-p "^mutt-" base)
           (mail-mode)
           (mail-text)
           (local-set-key (kbd "C-c C-c") 'server-edit)))))
(add-hook 'server-visit-hook 'iy-server-visit-setup)

(provide 'iy-daemon)
