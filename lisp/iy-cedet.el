(require 'iy-dep)
(require 'iy-eproject)

(push '(:name cedet
              :autoloads nil
              :build `("find . -name Makefile | xargs touch"
                       ,(concat "make EMACS=" el-get-emacs " MAKEINFO=echo"))
              :after iy-after-el-get-cedet)
      el-get-sources)

(defun iy-after-el-get-cedet ()
  (load (concat (el-get-package-directory "cedet") "common/cedet.el"))
  (defvar iy-cedet-ignore-projects '(".emacs.d"))
  (defun iy-semanticdb-cache-directory-p(directory)
    (let* ((project (eproject-detect-project directory))
           (project-root (and (car project) (directory-file-name (car project))))
           (project-name (and project-root (file-name-nondirectory project-root))))
      (cond
       ((string-match-p "^/usr/include\\|^/usr/local/include" directory) t)
       (project-name (not (member project-name iy-cedet-ignore-projects)))
       (t nil))))
  (add-hook 'semanticdb-project-predicate-functions 'iy-semanticdb-cache-directory-p)

  (defun iy-semanticdb-project-root-p(directory)
      (let* ((project (eproject-detect-project directory))
             (project-root (and (car project) (directory-file-name (car project))))
             (project-name (and project-root (file-name-nondirectory project-root))))
        (cond
         ((string-match-p "^/usr/include/c++" directory) "/usr/include/c++")
         ((string-match-p "^/usr/include" directory) "/usr/include")
         ((string-match-p "^/usr/local/include" directory) "/usr/local/include")
         ((and project (not (member project-name iy-cedet-ignore-projects))) project-root)
         (t nil))))
  (add-hook 'semanticdb-project-root-functions 'iy-semanticdb-project-root-p)
  (setq semantic-imenu-index-directory t
        semanticdb-default-save-directory (expand-file-name (concat iy-data-dir "sementicdb"))
        semanticdb-persistent-path '(project))

  (semantic-load-enable-code-helpers)
  (require 'semantic-gcc)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (semantic-load-enable-primary-exuberent-ctags-support)

  (remove-hook 'senator-minor-mode-hook 'senator-hippie-expand-hook)

  (when (fboundp 'semantic-add-system-include)
    (dolist (dir iy-header-dirs)
      (semantic-add-system-include dir 'c++-mode)))

  (defun iy-senator-navigation ()
    "senator navigation"
    (interactive)
    (let ((done nil)
          (ev last-command-event)
          (echo-keystrokes nil))
      (while (not done)
        (ignore-errors
          (run-hooks 'pre-command-hook)
          (cond ((eq ev ?n)
                 (senator-next-tag))
                ((eq ev ?p)
                 (senator-previous-tag))
                ((eq ev ?u)
                 (senator-go-to-up-reference))
                (t
                 (setq done t))))
        (when (not done)
          (setq ev (read-event))))
      (push ev unread-command-events)))

  (define-key senator-prefix-map "p" 'iy-senator-navigation)
  (define-key senator-prefix-map "n" 'iy-senator-navigation)
  (define-key senator-prefix-map "u" 'iy-senator-navigation)
  (define-key senator-prefix-map "\C-p" 'iy-senator-navigation)
  (define-key senator-prefix-map "\C-n" 'iy-senator-navigation)
  (define-key senator-prefix-map "\C-u" 'iy-senator-navigation))

(provide 'iy-cedet)
