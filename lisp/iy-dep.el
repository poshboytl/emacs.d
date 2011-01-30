;;; Dependencies

;;; Customizations

(defgroup iy-config nil "Ian Yang's config file customization")

(defcustom iy-frame-font "Droid Sans Mono:pixelsize=21"
  "Default font"
  :group 'iy-config)

(defcustom iy-frame-font-chinese "WenQuanYi Micro Hei Mono:pixelsize=22"
  "Chinese font"
  :group 'iy-config)

(defcustom iy-codebase-dir (expand-file-name "~/CodeBase")
  "Root directory of projects source"
  :group 'iy-config
  :type 'directory)

(defcustom iy-dropbox-dir (expand-file-name "~/Dropbox")
  "Dropbox root directory"
  :group 'iy-config
  :type 'directory)

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
