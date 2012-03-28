(custom-set-variables
 '(eproject-completing-read-function (quote eproject--ido-completing-read)))

(push 'eproject el-get-packages)

(eval-when-compile (require 'cl))

(defface eproject-mode-line-project-name-face
  '((t (:inherit font-lock-string :bold t)))
  "Face for displaying the project name in the modeline." :group 'eproject)

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

(defun eproject-maybe-create-tags-table ()
  (let* ((root (eproject-root))
         (default-directory root))
    (when (and root (eproject-attribute :create-etags root))
      (shell-command "ctags -e -R")
      (ignore-errors
        (visit-tags-table (concat root "TAGS") t)))))
(add-hook 'eproject-first-buffer-hook 'eproject-maybe-create-tags-table)

(defun eproject-visit-tags-table (&optional create)
  (interactive "P")
  (let* ((root (eproject-root))
         (file (concat root "TAGS")))
    (when root
      (when create
        (let ((default-directory root))
          (shell-command "ctags -e -R")))
      (when (file-exists-p file)
        (ignore-errors
          (visit-tags-table file t))))))
(add-hook 'eproject-mode-hook 'eproject-visit-tags-table)

(eval-when-compile
  (progn
    (defmacro define-project-type (type supertypes selector &rest metadata)
      `(progn
         (defvar ,(intern (format "%s-project-file-visit-hook" type)) nil
           ,(format "Hooks that will be run when a file in a %s project is opened." type))
         (setq eproject-project-types
               (nconc (assq-delete-all ',type eproject-project-types)
                      (list
                        (list ',type ',supertypes
                              (lambda (file) ,selector)
                              ',metadata))))))))

(defun iy-el-get-after-eproject ()
  ;; use git backend by default
  (plist-put (car (last (assoc 'generic-git eproject-project-types)))
             :relevant-files 'git)

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

  (defadvice eproject-maybe-turn-on (around ignore-errors activate)
    (ignore-errors
      ad-do-it))

  (add-hook 'org-src-mode-hook 'eproject-maybe-turn-on)
  (add-hook 'magit-mode-hook 'eproject-maybe-turn-on)
  (add-hook 'term-mode-hook 'eproject-maybe-turn-on)

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
        "rake ")
       ((or (file-exists-p "pom.xml"))
        (concat "cd " root "; mvn test"))
       (t nil))))

  (setq
   eproject-extra-attributes
   '(
     ((lambda (root) t)
      (:compile-command iy-eproject-guess-compile-command
                        :project-files-cache nil))))

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
  (add-hook 'eproject-mode-hook 'iy-eproject-mode-init)

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
  (global-set-key (kbd "<f5>") 'iy-compile)

  (defun* iy-eproject-list-project-files-with-cache (&optional (root (eproject-root)) force)
    (let ((files (eproject-attribute :project-files-cache root)))
      (if (and files (not force)) files
        (eproject-set-attribute :project-files-cache (eproject-list-project-files-relative) root))))

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
          files)))))

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
        (split-string (buffer-string) "\0"))))

  (define-key iy-map (kbd "p p") 'eproject-revisit-project)
  (define-key iy-map (kbd "p b") 'eproject-ibuffer)
  (define-key iy-map (kbd "p f") 'iy-eproject-find-file-with-cache)
  (define-key iy-map (kbd "p o") 'iy-eproject-find-file-with-cache)

  (defun eproject-root-safe ()
    (ignore-errors (eproject-root)))

  (setq ack-root-directory-functions '(eproject-root-safe))
  )

(provide 'iy-eproject)
