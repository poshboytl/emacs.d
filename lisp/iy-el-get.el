(require 'iy-dep)

(defun iy-filter-el-get-sources (recipe)
  (let* ((name (symbol-name (plist-get recipe :name)))
         (func (intern (concat "iy-el-get-after-" name)))
         (url (plist-get recipe :url)))

    ;; use iy-el-get-after-* as after function if defined
    (when (fboundp func)
      (plist-put recipe :after (list func)))
    (when (eq url 'bundle)
      (plist-put recipe :url (concat iy-bundle-dir name)))
    recipe))

(setq
 el-get-sources
 (mapcar
  'iy-filter-el-get-sources

  `(
    (:name helm
           :website "https://github.com/emacs-helm/helm"
           :description "Emacs incremental completion and selection narrowing framework"
           :type git
           :url bundle
           :depends (org-mode emacs-w3m)
           :build `(,(concat "make LOADPATH='-L . -L ../org-mode/lisp' EMACS='" el-get-emacs " -Q -batch'"))
           :autoloads "helm-config")

    (:name pick-backup :type git :url bundle :lazy t)
    (:name deft :lazy t)
    (:name org-mode :url bundle)
    (:name eproject :type git :url bundle :features eproject)
    (:name bookmark+ :url bundle)
    (:name nxhtml :url bundle)
    (:name yasnippet
           :type git
           :url bundle
           :features "yasnippet")
    (:name auctex
           :type git
           :url bundle
           :lazy t
           :build `(,(concat "./configure --with-lispdir=`pwd` --with-emacs=" el-get-emacs)
                    "make MAKEINFO=: clean all")
           :build/darwin `(,(concat
                             "./configure --with-lispdir=`pwd` "
                             "--with-texmf-dir=/usr/local/texlive/texmf-local "
                             "--with-emacs=" el-get-emacs)
                           "make MAKEINFO=: clean all")
           :load-path ("." "preview")
           :load  ("tex-site.el" "preview/preview-latex.el")
           :info "doc")
    (:name markdown-mode :url bundle :lazy t)
    (:name doxymacs :url bundle :lazy t)
    (:name doxymacs-yard :type git :url bundle :lazy t)
    (:name dired+)
    (:name dired-details)
    (:name dired-details+ :features dired-details+ :type emacswiki)
    (:name rinari
           :type git
           :url bundle
           :lazy t
           :load-path ("." "util" "util/jump")
           :features rinari
           :compile ("rinari.el" "rinari-merb.el"
                     "util/cucumber-mode-compilation.el"
                     "util/ruby-compilation.el"
                     "util/ruby-compilation-rspec.el"
                     "util/inf-ruby"
                     "util/jump/findr.el"
                     "util/jump/jump.el"
                     "util/jump/which-func.el"))
    (:name yari :url bundle :lazy t)
    (:name ruby-block :features nil
           :post-init (autoload 'ruby-block-mode "ruby-block" nil t))
    (:name cucumber :type git :url bundle :lazy t)
    (:name rspec-mode :url bundle :compile nil :lazy t)
    ;; (:name fringe-helper)
    (:name tumbl :type emacswiki :lazy t)
    (:name cheat :type git :url bundle :lazy t)
    (:name yaml-mode :url bundle :lazy t)
    (:name haml-mode :url bundle :lazy t)
    (:name sass-mode :url bundle :lazy t
           :depends haml-mode
           :build `(,(concat el-get-emacs " -Q -L . -L ../haml-mode -batch --eval \"(require 'cl)\" -f batch-byte-compile sass-mode.el")))
    (:name rainbow-mode :url bundle :lazy t)
    (:name coffee-mode :url bundle :lazy t)
    (:name haskell-mode :url bundle :lazy t)
    (:name erlware-mode :lazy t)
    (:name sml-modeline)
    (:name whole-line-or-region :features whole-line-or-region)
    (:name folding :post-init (folding-mode-add-find-file-hook))
    (:name fold-dwim
           :type http
           :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el"
           :features fold-dwim)
    (:name browse-kill-ring :lazy t)
    (:name kill-ring-search
           :type http
           :url "http://nschum.de/src/emacs/kill-ring-search/kill-ring-search.el"
           :lazy t)
    (:name dtrt-indent :url bundle)
    (:name highlight-symbol :lazy t)
    (:name highlight-parentheses :features nil)
    (:name hide-comnt :type emacswiki :lazy t)
    (:name thingatpt+ :type emacswiki :lazy t)
    (:name thing-cmds :type emacswiki :lazy t)
    (:name expand-region :type git :url bundle :lazy t)
    (:name switch-window :url bundle)
    (:name winring :type git :url bundle :features winring)
    (:name emacs-w3m
           :type git
           :lazy t
           :url bundle
           :build `("autoconf" ,(concat "./configure " "--with-emacs=" el-get-emacs) "make")
           :build/windows-nt ("sh /usr/bin/autoconf" "sh ./configure" "make")
           :info "doc")
    (:name git-emacs :url bundle :features git-emacs-autoloads)
    (:name magit :url bundle :lazy t)
    (:name full-ack :url bundle :lazy t)
    (:name xcscope :url bundle :lazy t)
    (:name alternative-files
           :compile "alternative-files.el"
           :type git
           :url bundle
           :features alternative-files
           :lazy t)
    (:name zencoding-mode :url bundle :lazy t)
    (:name rhtml-mode :url bundle)
    (:name pos-tip :features pos-tip)
    (:name popup :url bundle)
    (:name fuzzy :type git :url bundle :lazy t)
    (:name auto-complete :url bundle
           :branch "topic/customize-trigger-on-stop-words"
           :post-init nil
           :build `(,(concat el-get-emacs " -Q -L . -L ../popup -batch -f batch-byte-compile auto-complete.el auto-complete-config.el")))
    (:name diminish)
    (:name sequential-command :type emacswiki :features sequential-command)
    (:name paredit :lazy t)
    (:name command-log-mode :type git :url bundle :lazy t)
    (:name flymake-coffee :type git :url bundle :lazy t)
    (:name flymake-coffeelint :type git :url bundle
           :build `(,(concat el-get-emacs " -Q -batch -f batch-byte-compile flymake-coffeelint.el")
                    ("npm")))
    (:name ido-ubiquitous :type git :url bundle :features ido-ubiquitous)
    (:name ace-jump-mode :url bundle :lazy t)
    (:name multiple-cursors :type git :url bundle :lazy t)
    (:name jade-mode :type git :url bundle :lazy t)
    (:name ido-complete-space-or-hyphen :type git :url bundle :features ido-complete-space-or-hyphen)
    (:name smex :url bundle)

    )))

(provide 'iy-el-get)
