(require 'iy-dep)
(require 'iy-eproject)

(custom-set-variables
 '(semantic-idle-scheduler-work-idle-time 40))

(semantic-mode 1)

;; (global-ede-mode t)

(setq semantic-default-submodes
      '(global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-summary-mode
        global-semantic-idle-completions-mode
        global-semantic-decoration-mode
        global-semantic-highlight-func-mode
        global-semantic-stickyfunc-mode
        global-semantic-mru-bookmark-mode))

(semantic-gcc-setup)

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'ruby-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)
(set-default 'semantic-case-fold nil)

;; (load (concat (el-get-package-directory "cedet") "common/cedet.el"))
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

;; (remove-hook 'senator-minor-mode-hook 'senator-hippie-expand-hook)

(when (fboundp 'semantic-add-system-include)
  (dolist (dir iy-header-dirs)
    (semantic-add-system-include dir 'c++-mode)))

;; (defun iy-senator-navigation ()
;;   "senator navigation"
;;   (interactive)
;;   (let ((done nil)
;;         (ev last-command-event)
;;         (echo-keystrokes nil))
;;     (while (not done)
;;       (ignore-errors
;;         (run-hooks 'pre-command-hook)
;;         (cond ((eq ev ?n)
;;                (senator-next-tag))
;;               ((eq ev ?p)
;;                (senator-previous-tag))
;;               ((eq ev ?u)
;;                (senator-go-to-up-reference))
;;               (t
;;                (setq done t))))
;;       (when (not done)
;;         (setq ev (read-event))))
;;     (push ev unread-command-events)))

;; (define-key senator-prefix-map "p" 'iy-senator-navigation)
;; (define-key senator-prefix-map "n" 'iy-senator-navigation)
;; (define-key senator-prefix-map "u" 'iy-senator-navigation)
;; (define-key senator-prefix-map "\C-p" 'iy-senator-navigation)
;; (define-key senator-prefix-map "\C-n" 'iy-senator-navigation)
;; (define-key senator-prefix-map "\C-u" 'iy-senator-navigation)

;; (global-set-key (kbd "C-,") senator-prefix-map))

(eval-after-load 'cedet (iy-after-el-get-cedet))

(provide 'iy-cedet)
