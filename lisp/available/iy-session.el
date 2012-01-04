;;; iy-session.el --- Store/Restore session while restarting Emacs

(require 'iy-dep)

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

(provide 'iy-session)
