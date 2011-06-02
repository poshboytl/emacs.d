;;; iy-init.el --- Init function. The start entry.

;;; eval me to compile the dir
;; (byte-recompile-directory "~/.emacs.d/lisp" 0 nil)


;; locate this file

(eval-when-compile (require 'cl))
(require 'iy-dep)

(add-to-list 'load-path (concat iy-lisp-dir "3rdparty"))
(add-to-list 'load-path (concat iy-el-get-dir "el-get"))
(add-to-list 'load-path iy-lisp-dir)

(setq custom-file (concat iy-config-dir "custom.el"))
(setq iy-custom-defaults-file (concat iy-config-dir "custom.defaults.el"))

;; Once user has customized and saved, use user custom file.
(if (file-exists-p custom-file)
    (load custom-file t t)
  (load iy-custom-defaults-file t t))

(defun iy-init-load-modules ()
  "Emacs load modules"
  ;; load modules in lisp directory
  (dolist (file (file-expand-wildcards (concat iy-lisp-dir "iy-*.el")))
    (let ((feature (file-name-nondirectory (file-name-sans-extension file)))
          (blacklist (append (list 'iy-init 'iy-dep) iy-blacklist)))
      (if (memq (intern feature) blacklist)
          (message "[iy-init] %s is in black list" feature)
        (message "[iy-init] %s is loading" feature)
        (require (intern feature))
        (message "[iy-init] %s has been loaded" feature)))))

(defun iy-init-install-el-get ()
  ;; So the idea is that you copy/paste this code into your *scratch* buffer,
  ;; hit C-j, and you have a working el-get.
  (if (require 'el-get nil t)
      (message "el-get is already installed, try M-x el-get-update")
    (url-retrieve
     "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
     (lambda (s)
       (end-of-buffer)
       (eval-print-last-sexp)))))

(defun iy-init ()
  "Emacs start entry"
  (iy-init-install-el-get)

  ;; load some libraries first
  (unless (memq 'iy-theme iy-blacklist) (require 'iy-theme))
  (unless (memq 'iy-org iy-blacklist) (require 'iy-org))
  (iy-init-load-modules)

  ;; reverse the list
  (setq
   el-get-sources
   (cons
    '(:name package
            :post-init (lambda ()
                         (setq package-user-dir 
                               (expand-file-name 
                                (convert-standard-filename 
                                 (concat (file-name-as-directory 
                                          (el-get-package-directory "package")) 
                                         "elpa")))
                               package-directory-list 
                               (list (file-name-as-directory package-user-dir) 
                                     "/usr/share/emacs/site-lisp/elpa/"))
                         (make-directory package-user-dir t)
                         (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                                                  ("gnu" . "http://elpa.gnu.org/packages/")))
                         (package-initialize)))
    (nreverse el-get-sources)))
  (el-get 'wait))

(iy-init)

(provide 'iy-init)
