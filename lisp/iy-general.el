(require 'iy-dep)

;;{{{ Basic
(custom-set-variables
 '(current-language-environment "UTF-8")
 '(default-major-mode (quote text-mode) t)
 '(user-full-name "Ian Yang")
 '(user-mail-address "me@iany.me")

 '(enable-recursive-minibuffers t)
 '(minibuffer-depth-indicate-mode t)

 '(mouse-yank-at-point t)
 '(x-select-enable-clipboard t)
 '(browse-url-browser-function (quote w3m-browse-url))
 '(tramp-default-method-alist (quote (("\\`localhost\\'" "\\`root\\'" "sudo")))))
;;}}}

;;{{{ Shortcuts
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'save-pwd 'xsteve-save-current-directory)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)
(defalias 'rb 'revert-buffer-no-confirm)
(defalias 'occ 'occur)
(defalias 'mocc 'multi-occur)
(defalias 'aa 'anything-apropos)

(fset 'man 'woman)
(defun backup-dired ()
  (interactive)
  (find-file "~/.backup/emacs/")
  (dired-omit-mode 0))
(defun Q ()
  (interactive)
  (desktop-save-in-desktop-dir)
  (session-save-session)
  (kill-emacs))
(defalias 'af 'auto-fill-mode)
;;}}}

(provide 'iy-general)
