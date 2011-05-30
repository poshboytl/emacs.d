;; (eval-when-compile (require 'el-get))
;; 
(push '(:name eproject
              :type git
              :url "git://github.com/jrockway/eproject.git"
              :features eproject
              :after iy-el-get-after-eproject)
      el-get-sources)
;; 
;; (eval-when-compile
;;   (progn
;;     (el-get)
;;     (require 'eproject)))

(eval-when-compile (require 'cl))

(defface eproject-mode-line-project-name-face
  '((t (:inherit font-lock-string :bold t)))
  "Face for displaying the project name in the modeline." :group 'eproject)

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
      (:compile-command iy-eproject-guess-compile-command))))

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

  (defun eroot ()
    "Open root directory"
    (interactive)
    (dired (eproject-root)))
  )

(provide 'iy-eproject)
