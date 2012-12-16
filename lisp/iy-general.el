(require 'iy-dep)

;;{{{ Basic

(custom-set-variables
 '(current-language-environment "UTF-8")
 '(default-major-mode (quote text-mode) t)
 '(user-full-name "Ian Yang")
 '(user-mail-address "me@iany.me")

 '(enable-recursive-minibuffers t)
 '(minibuffer-depth-indicate-mode t)

 '(delete-by-moving-to-trash t)

 '(mouse-yank-at-point t)
 '(x-select-enable-clipboard t)
 '(browse-url-browser-function (quote w3m-browse-url))
 '(tramp-default-method-alist (quote (("\\`localhost\\'" "\\`root\\'" "sudo")))))

;;}}}

;;{{{ Enable Commands
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;}}}

;;{{{ Shortcuts
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'save-pwd 'xsteve-save-current-directory)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)
(defalias 'rb 'revert-buffer-no-confirm)
(defalias 'occ 'occur)
(defalias 'mocc 'multi-occur)
(defalias 'moccr 'multi-occur-in-matching-buffers)
(defalias 'aa 'anything-apropos)
(defalias 'wc 'whitespace-cleanup)
(defalias 'flb 'add-file-local-variable)
(defalias 'fll 'add-file-local-variable-prop-line)
(defalias 'fl 'add-file-local-variable-prop-line)
(defalias 'dl 'add-dir-local-variable)

(fset 'man 'woman)
(defun backup-dired ()
  (interactive)
  (find-file "~/.backup/emacs/")
  (dired-omit-mode 0))
(defun Q ()
  (interactive)
  (desktop-save-in-desktop-dir)
  (when (fboundp 'session-save-session) (session-save-session))
  (kill-emacs))
(defalias 'af 'auto-fill-mode)

(defun blog ()
  (interactive)
  (ido-find-file-in-dir "~/CodeBase/iany.me/content/posts"))
(defun wiki ()
  (interactive)
  (ido-find-file-in-dir "~/CodeBase/iany.me/content/wiki"))

;;}}}

(provide 'iy-general)
