(require 'iy-functions)

(custom-set-variables
 '(eproject-completing-read-function (quote eproject--ido-completing-read)))

(push 'eproject el-get-packages)

(eval-when-compile (require 'cl))

(defface eproject-mode-line-project-name-face
  '((t (:inherit font-lock-string :bold t)))
  "Face for displaying the project name in the modeline." :group 'eproject)

(defun eproject-ctags-command (&optional root)
  (let ((ctags (eproject-attribute :ctags (or root (eproject-root)))))
    (cond ((eq ctags 'rails) "ctags -u -e -f TAGS --tag-relative -R app lib vendor")
          (t ctags))))

(defun eproject-maybe-create-tags-table ()
  (let* ((root (eproject-root))
         (ctags (eproject-ctags-command root))
         (default-directory root))
    (when (and root ctags (eproject-attribute :ctags-first root))
      (ignore-errors
        (shell-command ctags)
        (visit-tags-table (concat root "TAGS") t)))))
(add-hook 'eproject-first-buffer-hook 'eproject-maybe-create-tags-table)

(defun eproject-visit-tags-table (&optional create)
  (interactive "P")
  (let* ((root (eproject-root))
         (ctags (eproject-ctags-command root))
         (file (concat root "TAGS")))
    (when root
      (when (and create ctags)
        (let ((default-directory root))
          (shell-command ctags)))
      (when (file-exists-p file)
        (ignore-errors
          (visit-tags-table file t))))))
(add-hook 'eproject-mode-hook 'eproject-visit-tags-table)

(defadvice eproject--buffer-file-name (after guess-directory activate)
  (when (and (boundp 'org-src-mode) org-src-mode
             (boundp 'org-edit-src-beg-marker) org-edit-src-beg-marker)
    (setq ad-return-value (buffer-file-name (marker-buffer org-edit-src-beg-marker))))
  (unless ad-return-value
    (setq ad-return-value
          (cond ((or (eq major-mode 'term-mode)
                     (string-match-p "^magit-" (prin1-to-string major-mode)))
                 (expand-file-name default-directory))
                (t nil)))))

(defun* eproject-attributes (&optional (root (eproject-root)))
  "Get attributes for the eproject ROOT
ROOT defaults to the current buffer's project-root."
  (cdr (assoc root eproject-attributes-alist)))

(defun* eproject-set-attribute (key value &optional (root (eproject-root)))
  "Set attribute KEY to VALUE for the eproject ROOT
ROOT defaults to the current buffer's project-root."
  (setf (getf (cdr (assoc root eproject-attributes-alist)) key) value))

(defun iy-eproject-guess-compile-command (root)
  (let ((default-directory root))
    (cond 
     ((or (file-exists-p "Makefile") (file-exists-p "makefile"))
      (concat "cd " root "; make -k"))
     ((or (file-exists-p "Rakefile"))
      (concat "cd " root "; rake "))
     ((or (file-exists-p "pom.xml"))
      (concat "cd " root "; mvn test"))
     (t nil))))

(defadvice eproject-maybe-turn-on (around ignore-errors activate)
  (ignore-errors
    ad-do-it))

(defun iy-eproject-mode-init ()
  ;; setup mode line
  (make-local-variable 'mode-line-buffer-identification)
  (setq mode-line-buffer-identification
        (nconc
         (list
          (propertize
           (eproject-name)
           'face 'eproject-mode-line-project-name-face)
          " ")
         (copy-list (default-value 'mode-line-buffer-identification)))))

(defun iy-compile ()
  "My compile wrapper"
  (interactive)
  (let* ((compile-command (if eproject-mode (eproject-attribute :compile-command) compile-command))
         (compile-history (if eproject-mode
                              (or (eproject-attribute :compile-history)
                                  (list compile-command))
                            compile-history)))
    (call-interactively 'compile)
    (when eproject-mode
      (eproject-set-attribute :compile-history compile-history)
      (eproject-set-attribute :compile-command compile-command))))

(defun* iy-eproject-list-project-files-with-cache (&optional (root (eproject-root)) force)
  (let ((files (eproject-attribute :project-files-cache root)))
    (if (and files (not force)) files
      (eproject-set-attribute :project-files-cache (eproject-list-project-files) root))))

(defun iy-eproject-find-file-with-cache (&optional force)
  "Present the user with a list of files in the current project.
to select from, open file when selected."
  (interactive "P")
  (let* ((root (ignore-errors (eproject-root)))
         (default-directory root)
         (files (and root (iy-eproject-list-project-files-with-cache root force))))
    (when files
      (find-file
       (ido-completing-read
        "Project file: "
        (mapcar
         'file-relative-name
         files))))))

(defadvice eproject-list-project-files (around search-by-backend (&optional root) activate)
  (let* ((root (or root (eproject-root)))
         (relevant-files (eproject-attribute :relevant-files root)))
    (if (symbolp (eproject-attribute :relevant-files root))
        (setq ad-return-value
              (funcall
               (intern (concat "eproject-list-project-files-by-" (prin1-to-string relevant-files)))
               root))
      ad-do-it)))

(defun eproject-list-project-files-by-git (root)
  (let ((default-directory root))
    (with-temp-buffer
      (call-process "git" nil (list (current-buffer) nil) nil
                    "ls-files" "--full-name" "-c" "-o" "--exclude-standard" "-z")
      (mapcar 'expand-file-name
              (split-string (buffer-string) "\0")))))

(defun iy-eproject-open-session ()
  (interactive)
  (let ((dir (eproject-root-safe)))
    (when dir
      (desktop-change-dir dir)
      (dired dir))))

(defun iy-eproject-open-project (&optional force)
  (interactive "P")
  (let ((current-prefix-arg (if force 8 4)))
    (call-interactively 'magit-status)))

(defun iy-eproject-eshell-toggle ()
  (interactive)
  (iy-eshell-toggle (ignore-errors (concat "*eshell*<" (eproject-name) ">"))))
(defun iy-eproject-eshell-here ()
  (interactive)
  (iy-eshell-here (ignore-errors (concat "*eshell*<" (eproject-name) ">"))))

(defmacro iy-eproject-call-interactively-in-root (func)
  `(lambda ()
     (interactive)
     (let ((default-directory (eproject-root)))
       (call-interactively ,func))))

(defun eproject-root-safe ()
  (ignore-errors (eproject-root)))

(defun iy-el-get-after-eproject ()
  (defun eproject-detect-project (&optional file)
    (let ((file (or file (eproject--buffer-file-name)))
          bestroot besttype)
      (loop for type in (eproject--all-types)
            do (let ((root (eproject--run-project-selector type file)))
                 (when (and root
                            (or (not bestroot)
                                ;; longest filename == best match (XXX:
                                ;; need to canonicalize?)
                                (> (length root) (length bestroot))))
                   (setq bestroot root)
                   (setq besttype type))))
      (cons bestroot besttype)))

  ;; use git backend by default
  (plist-put (car (last (assoc 'generic-git eproject-project-types)))
             :relevant-files 'git)

  (add-hook 'org-src-mode-hook 'eproject-maybe-turn-on)
  (add-hook 'magit-mode-hook 'eproject-maybe-turn-on)
  (add-hook 'term-mode-hook 'eproject-maybe-turn-on)

  (setq
   eproject-extra-attributes
   '(
     ((lambda (root) t)
      (:compile-command iy-eproject-guess-compile-command
                        :project-files-cache nil))))

  (add-hook 'eproject-mode-hook 'iy-eproject-mode-init)
  (global-set-key (kbd "<f5>") 'iy-compile)

  (define-key iy-map (kbd "p P") 'iy-eproject-open-project)
  (define-key iy-map (kbd "p s") 'iy-eproject-open-session)
  (define-key iy-map (kbd "p p") 'eproject-revisit-project)
  (define-key iy-map (kbd "p c") 'iy-compile)
  (define-key iy-map (kbd "p b") 'eproject-ibuffer)
  (define-key iy-map (kbd "p f") 'iy-eproject-find-file-with-cache)
  (define-key iy-map (kbd "p o") 'iy-eproject-find-file-with-cache)
  (define-key iy-map (kbd "p e") 'iy-eproject-eshell-toggle)
  (define-key iy-map (kbd "p E") 'iy-eproject-eshell-here)
  (define-key iy-map (kbd "p r") (iy-eproject-call-interactively-in-root 'rgrep))
  (define-key iy-map (kbd "p a") 'ag-project-at-point)
  (define-key iy-map (kbd "p A") 'ag-regexp-project-at-point)
  (define-key iy-map (kbd "p g") (iy-eproject-call-interactively-in-root 'git-grep))
  (define-key iy-map (kbd "p !") (iy-eproject-call-interactively-in-root 'shell-command))
  
  ;; generate TAGS, C-u to update
  (define-key iy-map (kbd "p t") 'eproject-visit-tags-table)
  )

(provide 'iy-eproject)
