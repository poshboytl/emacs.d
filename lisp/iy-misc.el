;;; iy-misc.el --- Miscellaneous configurations

(require 'iy-functions)

;;; Encoding

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; Helpers

(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'save-pwd 'xsteve-save-current-directory)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)
(defalias 'rb 'revert-buffer-no-confirm)
(defalias 'occ 'occur)
(defalias 'mocc 'multi-occur)
(defalias 'jabber 'jabber-connect-all)
(defalias 'aa 'anything-apropos)
(defalias 'sp 'speedbar-get-focus)
(defalias 'h 'flash-line-highlight)

(fset 'man 'woman)
(defun backup-dired ()
  (interactive)
  (find-file "~/.backup/emacs/")
  (dired-omit-mode 0))
(defalias 'loc 'iy/anything-locate)
(defalias 'pnotes 'iy/notes-publish)
(defalias 'pproject 'iy/project-publish)
(defalias 'csort-region 'iy/c-sort-header)
(defun csort ()
  (interactive)
  (iy/c-sort-header (point-min) (point-max)))
(defalias 'cupdate 'iy/c-update-header)
(defun Q ()
  (interactive)
  (desktop-save-in-desktop-dir)
  (session-save-session)
  (kill-emacs))
(defalias 'af 'auto-fill-mode)

(provide 'iy-misc)
