;;; iy-init.el --- Init function. The start entry.

;;; eval me to compile the dir
;; (byte-recompile-directory "~/.emacs.d/lisp" 0 nil)


;; locate this file

(eval-when-compile (require 'cl))
(require 'iy-dep)

(add-to-list 'load-path (concat iy-lisp-dir "3rdparty"))
(add-to-list 'load-path (concat iy-lisp-dir "modes"))
(add-to-list 'load-path (concat iy-el-get-dir "el-get"))
(add-to-list 'load-path iy-lisp-dir)

;; remove system org
(delete-if (lambda (path) (string= "org" (file-name-nondirectory path))) load-path)

(setq custom-file (concat iy-config-dir "custom.el"))
(setq secrets-file (concat iy-config-dir "secrets.el"))
(defvar el-get-packages nil)

;; Once user has customized and saved, use user custom file.
(when (file-exists-p custom-file)
  (load custom-file t t))

(when (file-exists-p secrets-file)
  (load secrets-file t t))

(defun iy-init-load-modules ()
  "Emacs load modules"
  ;; load modules in lisp directory
  (dolist (file (nconc (file-expand-wildcards (concat iy-lisp-dir "iy-*.el"))
                       (file-expand-wildcards (concat iy-lisp-dir "modes/iy-*.el"))))
    (let ((feature (file-name-nondirectory (file-name-sans-extension file)))
          (blacklist (append (list 'iy-init 'iy-dep) iy-blacklist)))
      (if (memq (intern feature) blacklist)
          (message "[iy-init] %s is in black list" feature)
        (message "[iy-init] %s is loading" feature)
        (require (intern feature))
        (message "[iy-init] %s has been loaded" feature)))))

(defun iy-init ()
  "Emacs start entry"
  (require 'iy-theme)
  (require 'iy-pim)
  (iy-init-load-modules)

  ;; reverse the list
  (setq el-get-sources (nreverse el-get-sources))

  ;; extract out names from el-get-sources
  (setq el-get-packages
        (mapcar
         (lambda (package)
           (if (listp package)
               (plist-get package :name)
             package))
         el-get-sources))

  (el-get 'sync el-get-packages))

(if (require 'el-get nil t)
    (iy-init)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp)
     (iy-init))))

(provide 'iy-init)
