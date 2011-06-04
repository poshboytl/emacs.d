(push '(:name eproject
              :type git
              :url "git://github.com/jrockway/eproject.git"
              :features eproject
              :after iy-el-get-after-eproject)
      el-get-sources)

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
      (visit-tags-table (concat root "TAGS") t))))
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
        (visit-tags-table file t)))))
(add-hook 'eproject-mode-hook 'eproject-visit-tags-table)

(defun iy-el-get-after-eproject ()
  (defadvice eproject--buffer-file-name (after guess-directory activate)
    (when (and (boundp 'org-src-mode) org-src-mode
               (boundp 'org-edit-src-beg-marker) org-edit-src-beg-marker)
      (setq ad-return-value (buffer-file-name (marker-buffer org-edit-src-beg-marker))))
    (unless ad-return-value
      (setq ad-return-value
            (cond ((memq major-mode '(magit-mode term-mode)) (expand-file-name default-directory))
                  (t nil)))))

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

  (defun* iy-eproject-list-project-files-with-cache (&optional (root (eproject-root)))
    (let ((files (eproject-attribute :project-files-cache root)))
      (if files files
        (eproject-set-attribute :project-files-cache (eproject-list-project-files) root))))

  (defun iy-eproject-find-file-with-cache ()
    "Present the user with a list of files in the current project.
to select from, open file when selected."
    (interactive)
    (let* ((root (ignore-errors (eproject-root)))
           (default-directory root)
           (files (and root (iy-eproject-list-project-files-with-cache root))))
      (when files
        (find-file
         (ido-completing-read
          "Project file: "
          (mapcar (lambda (f) (file-relative-name f root)) files))))))

  (define-key iy-map (kbd "p p") 'eproject-revisit-project)
  (define-key iy-map (kbd "p b") 'eproject-ibuffer)
  (define-key iy-map (kbd "p f") 'iy-eproject-find-file-with-cache))

(provide 'iy-eproject)
