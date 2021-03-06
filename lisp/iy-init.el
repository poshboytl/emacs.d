;;; iy-init.el --- Init function. The start entry.

;;; eval me to compile the dir
;; (byte-recompile-directory "~/.emacs.d/lisp" 0 nil)


;; locate this file

(eval-and-compile
  (progn
    (require 'cl)
    (load "cl-seq")))
(require 'iy-dep)

(add-to-list 'load-path (concat iy-lisp-dir "modes"))
(add-to-list 'load-path (concat iy-lisp-dir "3rdparty"))
(add-to-list 'load-path iy-el-get-dir)
(add-to-list 'load-path (concat iy-el-get-dir "el-get"))
(add-to-list 'load-path iy-lisp-dir)

;; remove system org
(delete-if (lambda (path) (string= "org" (file-name-nondirectory path))) load-path)

(setq custom-readonly-file (concat iy-config-dir "custom.readonly.el"))
(setq custom-file (concat iy-config-dir "custom.el"))
(setq secrets-file (concat iy-config-dir "secrets.el"))
(defvar el-get-packages nil)

;; Once user has customized and saved, use user custom file.
(when (file-exists-p custom-file)
  (load custom-file t t))
(when (file-exists-p custom-readonly-file)
  (load custom-readonly-file t t))
(when (file-exists-p secrets-file)
  (load secrets-file t t))

(defun iy-init-load-module (feature)
  (message "[iy-init] load %s" feature)
  (require feature))

(defun iy-init-load-modules (&optional before-modules after-modules)
  "Emacs load modules"

  (dolist (feature before-modules)
    (iy-init-load-module feature))

  ;; load modules in lisp directory
  (dolist (file (nconc (file-expand-wildcards (concat iy-lisp-dir "iy-*.el"))
                       (file-expand-wildcards (concat iy-lisp-dir "modes/iy-*.el"))))
    (let ((feature (file-name-nondirectory (file-name-sans-extension file)))
          (exclude (append '(iy-init) before-modules after-modules)))
      (if (memq (intern feature) iy-blacklist)
          (message "[iy-init] %s is in black list" feature)
        (unless (memq (intern feature) exclude)
          (iy-init-load-module (intern feature))))))
  (dolist (feature after-modules)
    (iy-init-load-module feature)))

(defun iy-init ()
  "Emacs start entry"

  (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

  (iy-init-load-modules '(iy-theme iy-org-mode) '(iy-el-get))

  (el-get 'sync (reverse el-get-packages)))

(if (require 'el-get nil t)
    (iy-init)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp)
     (iy-init))))

(provide 'iy-init)
