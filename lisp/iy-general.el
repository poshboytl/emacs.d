(require 'iy-dep)

;; Basic
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

;; Backup, Bookmark, Recentf
(custom-set-variables
 '(auto-save-interval 50)
 '(auto-save-list-file-prefix (concat iy-data-dir "autosaves-"))
 '(auto-save-timeout 10)
 '(backup-directory-alist (list (cons "." (expand-file-name "~/.backup/emacs"))))
 '(bookmark-default-file (concat iy-data-dir "bookmark"))
 '(bookmark-use-annotations nil)
 '(delete-old-versions t)
 '(desktop-base-file-name ".emacs.desktop")
 '(desktop-path (list "." iy-data-dir))
 '(desktop-restore-eager 3)
 '(desktop-save (quote ask-if-new))
 '(kept-new-versions 20)
 '(kept-old-versions 5)
 '(kill-ring-max 150)
 '(recentf-arrange-rules (quote (("Elisp files (%d)" ".\\.el\\'") ("Java files (%d)" ".\\.java\\'") ("C/C++ files (%d)" ".\\.c\\(pp\\)?\\'" ".\\.h\\(pp\\)?\\'") ("Org files (%d)" ".\\.org\\'"))))
 '(recentf-exclude (quote ("semantic\\.cache" "COMMIT_EDITMSG" "git-emacs-tmp.*" "\\.breadcrumb" "\\.ido\\.last" "\\.projects.ede")))
 '(recentf-menu-filter (quote recentf-arrange-by-rule))
 '(recentf-save-file (concat iy-data-dir "recentf"))
 '(session-initialize (quote (de-saveplace session)))
 '(session-save-file (concat iy-data-dir "session"))
 '(vc-make-backup-files t)
 '(version-control t)
 )

;; Cleanup Buffers
(custom-set-variables
 '(clean-buffer-list-delay-special 3600)
 '(clean-buffer-list-kill-buffer-names (quote ("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "bbdb" "*RE-Builder*" "*Shell Command Output*" "*ESS*" "*WoMan-Log*" "*magit-process*" "*Dired log*" "*anything*" "*CEDET Global*" "*Pp Eval Output*" "*Completions*")))
 '(clean-buffer-list-kill-regexps (quote ("\\`\\*Customize Group:" "\\`\\*Man " "\\`\\*magit" "\\`\\*RNC Input")))
 '(midnight-mode t nil (midnight))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-strip-common-suffix t))

;; Editor
(custom-set-variables
 '(tab-width 2)
 '(indent-tabs-mode nil)
 '(fill-column 80))

(provide 'iy-general)
