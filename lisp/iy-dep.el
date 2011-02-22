;;; Dependencies

;;; Constants

(defconst iy-config-dir (file-name-as-directory user-emacs-directory))
(defconst iy-data-dir (concat iy-config-dir "data/"))
(defconst iy-lisp-dir (file-name-directory (locate-file "iy-init.el" load-path)))
(defconst iy-el-get-dir (concat iy-config-dir "el-get/"))

;;; helper functions
(defun iy-set-default-as-directory (sym val)
  (set-default sym (file-name-as-directory val)))

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

(defcustom iy-codebase-dir (expand-file-name "~/CodeBase/")
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

(provide 'iy-dep)
