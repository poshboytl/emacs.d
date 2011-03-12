;; I save some customization here for conviniant

(require 'iy-dep)

(custom-set-variables
 '(iy-frame-font "Inconsolata:pixelsize=18")
 '(iy-frame-font-chinese "Heiti SC:pixelsize=18")
 '(abbrev-mode t)
 '(ack-prompt-for-directory t)
 '(appt-display-format (quote window))
 '(appt-message-warning-time 10)
 '(auto-save-interval 50)
 '(auto-save-list-file-prefix (concat iy-data-dir "autosaves-"))
 '(auto-save-timeout 10)
 '(backup-directory-alist (list (cons "." (expand-file-name "~/.backup/emacs"))))
 '(bbdb-default-area-code 21)
 '(bbdb-default-country "China")
 '(bbdb-dwim-net-address-allow-redundancy t)
 '(bbdb-file (concat iy-data-dir "bbdb"))
 '(bbdb-north-american-phone-numbers-p nil)
 '(bbdb-pop-up-display-layout (quote one-line))
 '(bbdb-pop-up-target-lines 1)
 '(bibtex-align-at-equal-sign t)
 '(bibtex-autokey-additional-names ".etal")
 '(bibtex-autokey-expand-strings t)
 '(bibtex-autokey-name-separator ".")
 '(bibtex-autokey-name-year-separator ":")
 '(bibtex-autokey-titleword-length 5)
 '(bibtex-autokey-titleword-separator ".")
 '(bibtex-autokey-titlewords 1)
 '(bibtex-autokey-year-length 0)
 '(bibtex-autokey-year-title-separator ":")
 '(bibtex-entry-format (quote (opts-or-alts required-fields numerical-fields realign)))
 '(bibtex-include-OPTcrossref (quote ("InProceedings" "InCollection" "InBook")))
 '(bibtex-maintain-sorted-entries t)
 '(blink-cursor-mode nil)
 '(bookmark-default-file (concat iy-data-dir "bookmark"))
 '(bookmark-use-annotations nil)
 '(browse-url-browser-function (quote w3m-browse-url))
 '(calendar-week-start-day 1)
 '(clean-buffer-list-delay-special 3600)
 '(clean-buffer-list-kill-buffer-names (quote ("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "bbdb" "*RE-Builder*" "*Shell Command Output*" "*ESS*" "*WoMan-Log*" "*magit-process*" "*Dired log*" "*anything*" "*CEDET Global*" "*Pp Eval Output*" "*Completions*")))
 '(clean-buffer-list-kill-regexps (quote ("\\`\\*Customize Group:" "\\`\\*Man " "\\`\\*magit" "\\`\\*RNC Input")))
 '(comint-buffer-maximum-size 10240)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-context-lines 10)
 '(compilation-scroll-output (quote first-error))
 '(compose-mail-user-agent-warnings nil)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(current-language-environment "UTF-8")
 '(debug-on-error nil)
 '(default-major-mode (quote text-mode))
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-base-file-name ".emacs.desktop")
 '(desktop-path (list "." iy-data-dir))
 '(desktop-restore-eager 3)
 '(desktop-save (quote ask-if-new))
 '(diary-file (concat iy-dropbox-dir "diary"))
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(doxymacs-use-external-xml-parser t)
 '(ecb-analyse-face (quote ecb-default-highlight-face))
 '(ecb-compile-window-height nil)
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-directory-face (quote ecb-default-highlight-face))
 '(ecb-history-face (quote ecb-default-highlight-face))
 '(ecb-key-map (quote ("C-." (t "fh" ecb-history-filter) (t "fs" ecb-sources-filter) (t "fm" ecb-methods-filter) (t "fr" ecb-methods-filter-regexp) (t "ft" ecb-methods-filter-tagclass) (t "fc" ecb-methods-filter-current-type) (t "fp" ecb-methods-filter-protection) (t "fn" ecb-methods-filter-nofilter) (t "fl" ecb-methods-filter-delete-last) (t "ff" ecb-methods-filter-function) (t "p" ecb-nav-goto-previous) (t "n" ecb-nav-goto-next) (t "C-p" ecb-nav-goto-previous) (t "C-n" ecb-nav-goto-next) (t "lc" ecb-change-layout) (t "lr" ecb-redraw-layout) (t "lw" ecb-toggle-ecb-windows) (t "lt" ecb-toggle-layout) (t "s" ecb-window-sync) (t "r" ecb-rebuild-methods-buffer) (t "a" ecb-toggle-auto-expand-tag-tree) (t "x" ecb-expand-methods-nodes) (t "h" ecb-show-help) (t "gl" ecb-goto-window-edit-last) (t "g1" ecb-goto-window-edit1) (t "g2" ecb-goto-window-edit2) (t "gc" ecb-goto-window-compilation) (t "gd" ecb-goto-window-directories) (t "gs" ecb-goto-window-sources) (t "gm" ecb-goto-window-methods) (t "gh" ecb-goto-window-history) (t "ga" ecb-goto-window-analyse) (t "gb" ecb-goto-window-speedbar) (t "md" ecb-maximize-window-directories) (t "ms" ecb-maximize-window-sources) (t "mm" ecb-maximize-window-methods) (t "mh" ecb-maximize-window-history) (t "ma" ecb-maximize-window-analyse) (t "mb" ecb-maximize-window-speedbar) (t "e" eshell) (t "o" ecb-toggle-scroll-other-window-scrolls-compile) (t "\\" ecb-toggle-compile-window) (t "/" ecb-toggle-compile-window-height) (t "," ecb-cycle-maximized-ecb-buffers) (t "." ecb-cycle-through-compilation-buffers))))
 '(ecb-layout-name "left7")
 '(ecb-method-face (quote ecb-default-highlight-face))
 '(ecb-options-version "2.40")
 '(ecb-other-window-behavior (quote smart))
 '(ecb-tag-header-face (quote secondary-selection))
 '(ecb-tip-of-the-day nil)
 '(ecb-toggle-layout-sequence (quote ("left7" "left15" "left13")))
 '(ecb-tree-buffer-style (quote image))
 '(ecb-tree-incremental-search (quote substring))
 '(ecb-windows-width 40)
 '(ede-project-placeholder-cache-file (concat iy-data-dir "projects.ede"))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(emacsw32-eol-check-new-files t)
 '(emacsw32-eol-file-name-lf-list (quote (".*")))
 '(enable-recursive-minibuffers t)
 '(eproject-completing-read-function (quote eproject--ido-completing-read))
 '(fill-column 80)
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gdb-speedbar-auto-raise t)
 '(gdb-use-colon-colon-notation t)
 '(gdb-use-separate-io-buffer t)
 '(git-state-modeline-decoration (quote git-state-decoration-large-dot))
 '(global-hl-spotlight-mode t)
 '(global-whitespace-mode t)
 '(graphviz-dot-auto-indent-on-braces t)
 '(graphviz-dot-delete-completions t)
 '(graphviz-dot-indent-width 4)
 '(graphviz-dot-toggle-completions t)
 '(highlight-symbol-idle-delay 1)
 '(highlight-symbol-on-navigation-p t)
 '(hl-paren-colors (quote ("firebrick1" "IndianRed1" "IndianRed4" "grey")))
 '(home-end-enable t)
 '(htmlize-html-charset "UTF-8")
 '(htmlize-output-type (quote css))
 '(ibuffer-always-show-last-buffer :nomini)
 '(ibuffer-default-shrink-to-minimum-size t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 24 24 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-jump-offer-only-visible-buffers nil)
 '(ibuffer-show-empty-filter-groups nil)
 '(icomplete-prospects-height 3)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(install-elisp-repository-directory (concat iy-lisp-dir "3rdparty"))
 '(iy-go-to-char-key-backward 58)
 '(jabber-alert-presence-hooks nil)
 '(jabber-history-enabled t)
 '(jabber-roster-show-bindings nil)
 '(jabber-show-offline-contacts nil)
 '(javascript-expr-indent-offset 4)
 '(javascript-indent-level 2)
 '(js-enabled-frameworks nil)
 '(js-expr-indent-offset 4)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-indent-on-enter-key t)
 '(kept-new-versions 20)
 '(kept-old-versions 5)
 '(kill-ring-max 150)
 '(kill-whole-line t)
 '(longlines-show-hard-newlines t)
 '(magit-process-popup-time 60)
 '(magit-repo-dirs (list iy-codebase-dir))
 '(magit-repo-dirs-depth 1)
 '(mail-abbrevs-mode t)
 '(menu-bar-mode nil)
 '(midnight-mode t nil (midnight))
 '(minibuffer-depth-indicate-mode t)
 '(mode-require-final-newline nil)
 '(mouse-avoidance-mode (quote jump) nil (avoid))
 '(mouse-yank-at-point t)
 '(mumamo-chunk-coloring 1)
 '(next-line-add-newlines nil)
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-skip-welcome t)
 '(org-emphasis-regexp-components (quote (" 	('\"{`" "- 	.,:!?;'\")}\\" " 	
,\"" "." 1)))
 '(org-export-html-style-include-default nil)
 '(org-export-html-style-include-scripts nil)
 '(org-export-html-title-format "<h1 id=\"pagetitle\">%s</h1>
")
 '(org-export-section-number-format (quote ((("1" ".")) . ".")))
 '(pulse-delay 0.03)
 '(pulse-flag nil)
 '(pulse-iterations 5)
 '(recentf-arrange-rules (quote (("Elisp files (%d)" ".\\.el\\'") ("Java files (%d)" ".\\.java\\'") ("C/C++ files (%d)" ".\\.c\\(pp\\)?\\'" ".\\.h\\(pp\\)?\\'") ("Org files (%d)" ".\\.org\\'"))))
 '(recentf-exclude (quote ("semantic\\.cache" "COMMIT_EDITMSG" "git-emacs-tmp.*" "\\.breadcrumb" "\\.ido\\.last" "\\.projects.ede")))
 '(recentf-menu-filter (quote recentf-arrange-by-rule))
 '(recentf-save-file (concat iy-data-dir "recentf"))
 '(reftex-default-bibliography (quote ("biblio.bib" "my.bib")))
 '(ruby-block-highlight-face (quote highlight))
 '(require-final-newline nil)
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(semantic-idle-scheduler-work-idle-time 40)
 '(session-initialize (quote (de-saveplace session)))
 '(session-save-file (concat iy-data-dir "session"))
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(snipplr-user-key "2ecd47bacb8684698112")
 '(sql-sqlite-options (quote ("-line")))
 '(sql-sqlite-program "sqlite3")
 '(tab-width 4)
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob) ("C-s" . isearch-forward) ("C-r" . isearch-backward) ("C-m" . term-send-raw) ("M-f" . term-send-forward-word) ("M-b" . term-send-backward-word) ("M-o" . term-send-backspace) ("M-p" . term-send-up) ("M-n" . term-send-down) ("M-M" . term-send-forward-kill-word) ("M-N" . term-send-backward-kill-word) ("M-r" . term-send-reverse-search-history) ("M-," . term-send-input) ("M-x" . execute-extended-command) ("M-X" . anything-at-point) ("M-o" . other-window) ("M-s" . iy-map))))
 '(term-unbind-key-list (quote ("C-x" "C-g" "M-w" "C-h")))
 '(tool-bar-mode nil)
 '(tramp-default-method-alist (quote (("\\`localhost\\'" "\\`root\\'" "sudo"))))
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-strip-common-suffix t)
 '(user-full-name "Ian Yang")
 '(user-mail-address "me@iany.me")
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(w3m-coding-system (quote utf-8))
 '(w3m-default-coding-system (quote utf-8))
 '(w3m-enable-google-feeling-lucky t)
 '(w3m-file-coding-system (quote utf-8))
 '(w3m-file-name-coding-system (quote utf-8))
 '(w3m-redirect-with-get t)
 '(w3m-terminal-coding-system (quote utf-8))
 '(w3m-use-cookies t)
 '(whitespace-action nil)
 '(whitespace-global-modes (quote (c-mode c++-mode makefile-gmail-mode sh-mode cmake-mode emacs-lisp-mode)))
 '(whitespace-line-column 80)
 '(whitespace-style (quote (tabs trailing lines-tail space-before-tab newline indentation space-after-tab tab-mark)))
 '(winner-dont-bind-my-keys t)
 '(winring-show-names t)
 '(woman-fontify t)
 '(woman-use-own-frame nil)
 '(woman-use-topic-at-point-default t)
 '(x-select-enable-clipboard t)
 '(yas/choose-keys-first nil)
 '(yas/prompt-functions (quote (yas/dropdown-prompt yas/ido-prompt yas/x-prompt yas/no-prompt)))
 '(yas/use-menu (quote abbreviate)))
