(require 'iy-dep)
(require 'iy-keymap)

(defcustom iy-gpicker-cmd (executable-find "gpicker")
  "Default font"
  :type 'file
  :group 'iy-config)

(defun iy-gpicker-find-file (&optional dir)
  (interactive)
  (let* ((dir (or dir
                  (if dired-directory (expand-file-name dired-directory) default-directory)
                  iy-codebase-dir)))
    (with-temp-buffer
      (call-process iy-gpicker-cmd nil (list (current-buffer) nil) nil
                    "-m" dir)
      (cd dir)
      (dolist (file (split-string (buffer-string) "\0"))
        (unless (string-equal file "")
          (find-file file))))))

(defun iy-gpicker-find-file-in-project ()
  (interactive)
  (iy-gpicker-find-file (or eproject-root
                            iy-codebase-dir)))

(define-key iy-map (kbd "T") 'iy-gpicker-find-file-in-project)
(define-key iy-map (kbd "t") 'iy-gpicker-find-file)

(provide 'iy-gpicker)
