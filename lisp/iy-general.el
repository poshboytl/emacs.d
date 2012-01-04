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
 '(browse-url-browser-function (quote w3m-browse-url)))
;;}}}

;;{{{ Cleanup Buffers
(custom-set-variables
 '(clean-buffer-list-delay-special 3600)
 '(clean-buffer-list-kill-buffer-names (quote ("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "bbdb" "*RE-Builder*" "*Shell Command Output*" "*ESS*" "*WoMan-Log*" "*magit-process*" "*Dired log*" "*anything*" "*CEDET Global*" "*Pp Eval Output*" "*Completions*")))
 '(clean-buffer-list-kill-regexps (quote ("\\`\\*Customize Group:" "\\`\\*Man " "\\`\\*magit" "\\`\\*RNC Input")))
 '(midnight-mode t nil (midnight))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-strip-common-suffix t))
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
