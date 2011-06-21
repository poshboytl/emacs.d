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
    ido-kill-emacs-hook
    org-clock-save
    org-id-locations-save
    recentf-save-list
    semanticdb-kill-emacs-hook
    session-save-session
    w3m-arrived-shutdown
    w3m-cookie-shutdown
    tramp-dump-connection-properties)
  "List of functions that should be called when a OS window is closed"
  :group 'iy-config
  :type '(repeat symbol))

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

      (server-start)))

(provide 'iy-daemon)
