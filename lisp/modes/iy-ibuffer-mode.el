(global-set-key (kbd "C-x C-b") 'ibuffer)

(custom-set-variables
 '(ibuffer-always-show-last-buffer :nomini)
 '(ibuffer-default-shrink-to-minimum-size t)
 '(ibuffer-jump-offer-only-visible-buffers nil)
 '(ibuffer-show-empty-filter-groups nil))

(defun iy-ibuffer-mode-init ()
  ;; add another sorting method for ibuffer (allow the grouping of
  ;; filenames and dired buffers
  (define-ibuffer-sorter filename-or-dired
    "Sort the buffers by their pathname."
    (:description "filenames plus dired")
    (string-lessp
     (with-current-buffer (car a)

       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~"))
     (with-current-buffer (car b)
       (or buffer-file-name
           (if (eq major-mode 'dired-mode)
               (expand-file-name dired-directory))
           ;; so that all non pathnames are at the end
           "~"))))
  (define-key ibuffer-mode-map (kbd "s p")     'ibuffer-do-sort-by-filename-or-dired)
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("source" (or
                      (mode . c-mode)
                      (mode . c++-mode)
                      (mode . objc-mode)
                      (mode . cperl-mode)
                      (mode . perl-mode)
                      (mode . java-mode)
                      (filename . "\\.rb\\'")
                      (filename . "\\.js\\'")
                      (filename . "\\.css\\'")
                      (mode . html-mode)
                      (mode . nxml-mode)
                      (mode . nxhtml-mode)
                      (mode . haml-mode)
                      (mode . sass-mode)
                      (filename . "\\.erb\\'")))
           ("doc" (or
                   (mode   . latex-mode)
                   (mode   . metapost-mode)
                   (mode   . doc-view-mode)
                   (mode   . markdown-mode)))
           ("build" (or
                     (mode . cmake-mode)
                     (mode . makefile-mode)
                     (mode . makefile-gmake-mode)
                     (filename . "[Rr]akefile\\'")))
           ("pim" (or
                   (name    . "^\\*Calendar\\*$")
                   (name    . "^diary$")
                   (mode    . org-mode)))
           ("system" (or
                      (mode       . help-mode)
                      (mode       . completion-list-mode)
                      (mode       . apropos-mode)
                      (name      . "^\\*.*\\*$")
                      (filename . "\.emacs\.d")
                      (mode     . custom-mode))))))
  (ibuffer-switch-to-saved-filter-groups "default")
  (hl-line-mode))

(add-hook 'ibuffer-mode-hook 'iy-ibuffer-mode-init)

(provide 'iy-ibuffer-mode)