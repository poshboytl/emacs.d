(require 'iy-dep)

(defcustom iy-gpicker-cmd (executable-find "gpicker")
  "Default font"
  :type 'file
  :group 'iy-config)

(defun iy-gpicker-find-file (dir)
  (interactive
   (list
    (or (and current-prefix-arg (ido-read-directory-name "gpicker: " nil nil t))
        (and (boundp 'eproject-root) eproject-root)
        (and (not current-prefix-arg) (ido-read-directory-name "gpicker: " nil nil t))
        (if dired-directory (expand-file-name dired-directory) default-directory))))
  (when dir
    (with-temp-buffer
      (call-process iy-gpicker-cmd nil (list (current-buffer) nil) nil
                    "-t" "guess" "-m" dir)
      (cd dir)
      (dolist (file (split-string (buffer-string) "\0"))
        (unless (string-equal file "")
          (find-file file))))))

(define-key iy-map (kbd "o") 'iy-gpicker-find-file)
(define-key iy-map (kbd "M-o") 'iy-gpicker-find-file)

(provide 'iy-gpicker)
