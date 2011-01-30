;;; iy-init.el --- Init function. The start entry.

;; locate this file

(eval-when-compile (require 'cl))
(require 'iy-dep)

(setq iy-config-dir (file-name-as-directory user-emacs-directory))
(setq iy-lisp-dir (file-name-directory (locate-file "iy-init.el" load-path)))
(setq iy-el-get-dir (file-name-as-directory (concat iy-config-dir "el-get")))

(add-to-list 'load-path (concat iy-lisp-dir "3rdparty"))
(add-to-list 'load-path (concat iy-el-get-dir "el-get"))
(add-to-list 'load-path iy-lisp-dir)

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
  (if (memq 'iy-theme iy-blacklist) (require 'iy-theme))
  (iy-init-load-modules))

(iy-init)

(provide 'iy-init)