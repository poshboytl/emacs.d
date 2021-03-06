;;; Dependencies

;;; Declare Variables
(unless (boundp 'el-get-sources)
  (setq el-get-sources nil))

;;; list of el get packages to install. packages are installed in reverse order.
(defvar el-get-packages nil)

;;; Constants

(defconst iy-config-dir (file-name-as-directory user-emacs-directory))
(defconst iy-data-dir (concat iy-config-dir "data/"))
(defconst iy-bundle-dir (expand-file-name (concat iy-config-dir "bundle/")))
(defconst iy-lisp-dir (file-name-directory (locate-file "iy-init.el" load-path)))
(defconst iy-el-get-dir (concat iy-config-dir "el-get/"))

;;; helper functions
(defun iy-set-default-as-directory (sym val)
  (set-default sym (file-name-as-directory val)))

(defvar iy-original-exec-path exec-path)
(defvar iy-original-env-path (getenv "PATH"))
(defun iy-set-exec-path (sym val)
  (setq iy-exec-path val)
  (setq exec-path (append val iy-original-exec-path))
  (setenv "PATH" (mapconcat 'identity (cons iy-original-env-path val) ":")))
(defun iy-remove-exec-path (val)
  (setq exec-path (append val iy-original-exec-path))
  (setenv "PATH" (mapconcat 'identity (cons iy-original-env-path val) ":")))

(defun iy-require-maybe (feature &optional filename noerror)
  (unless (memq feature iy-blacklist)
    (require feature filename noerror)))

;;; Customizations

(defgroup iy-config nil "Ian Yang's config file customization")

(defcustom iy-frame-font "Mono:pixelsize=21"
  "Default font"
  :type 'string
  :group 'iy-config
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-frame-font value)))

(defcustom iy-frame-font-chinese "Mono:pixelsize=22"
  "Chinese font"
  :type 'string
  :group 'iy-config
  :set (lambda (symbol value)
         (set-default symbol value)
         (set-fontset-font "fontset-default" 'chinese-gbk value)))

(defcustom iy-codebase-dir (expand-file-name "~/codebase/")
  "Root directory of projects source"
  :group 'iy-config
  :type 'directory
  :set 'iy-set-default-as-directory)

(defcustom iy-dropbox-dir (expand-file-name "~/Dropbox/")
  "Dropbox root directory"
  :group 'iy-config
  :type 'directory
  :set 'iy-set-default-as-directory)

(defcustom iy-header-dirs (list "/usr/include")
  "C/C++ header directories"
  :group 'iy-config
  :type '(repeat directory))

;;; path to ditaa jar
(defcustom iy-ditaa-path (expand-file-name "~/Dropbox/java-libs/ditaa0_9.jar")
  "Path to ditaa jar"
  :group 'iy-config
  :type 'file)

(defcustom iy-blacklist
  nil
  "Files in black list are not loaded"
  :group 'iy-config
  :type '(repeat symbol))

(defcustom iy-exec-path
  (mapcar 'expand-file-name nil)
  "Files in black list are not loaded"
  :group 'iy-config
  :type '(repeat string)
  :set 'iy-set-exec-path)

(defvar iy-map (make-sparse-keymap))
(global-set-key (kbd "M-s") iy-map)

(provide 'iy-dep)
