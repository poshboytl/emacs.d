(eval-when-compile (require 'cl))

(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun iy-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun iy-string-camel-to-underscore (string)
  "Convert camel string to upcase one which concat words using underscore"
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "\\([[:upper:]]\\)\\([[:upper:]][[:lower:]]\\)" "\\1_\\2"
     (replace-regexp-in-string
      "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1_\\2" string))))

(defun camel-to-underscore (start end)
  (interactive "r")
  (let ((origin (buffer-substring start end)))
    (delete-region start end)
    (insert (iy-string-camel-to-underscore origin))))

(defun xsteve-save-current-directory ()
  "Save the current directory to the file ~/.emacs.d/data/pwd"
  (interactive)
  (let ((dir default-directory))
    (with-current-buffer (find-file-noselect "~/.emacs.d/data/pwd")
      (delete-region (point-min) (point-max))
      (insert (concat dir "\n"))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun iy-multi-term-dedicated-toggle ()
  ""
  (interactive)
  (multi-term-dedicated-toggle)
  (if (multi-term-dedicated-exist-p)
      (select-window multi-term-dedicated-window)))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (flet ((yes-or-no-p (prompt) t)) (revert-buffer)))

(defun iy-set-folding-marks (b e &optional mode)
  (interactive "sBegin Mark: \nsEnd Mark: ")
  (let* ((mode (or mode major-mode))
         (ptr (assq mode folding-mode-marks-alist)))
    (setcdr ptr (list b e))))

(defun iy-toggle-newline-mark ()
  (interactive)
  (whitespace-toggle-options 'newline-mark))

(defun iy-close-help ()
  (interactive)
  (save-excursion
    (dolist (w (window-list))
      (set-buffer (window-buffer w))
      (when (eq major-mode 'help-mode)
        (delete-window w)))))

(defun iy-insert-user ()
  (interactive)
  (insert (user-full-name)))

(defun iy-insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun iy-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%s")))

(defun iy-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun iy-insert-file-name ()
  (interactive)
  (insert (file-name-nondirectory (buffer-file-name))))

;; http://curiousprogrammer.wordpress.com/2009/05/14/inserting-buffer-filename/
(defun jared/get-files-and-buffers ()
  (let ((res '()))
    (dolist (buffer (buffer-list) res)
      (let ((buffername (buffer-name buffer))
            (filename (buffer-file-name buffer)))
        (unless (string-match "^ *\\*.*\\*$" buffername)
          (push buffername res))
        (when filename (push filename res))))))


(defun jared/insert-file-or-buffer-name (&optional initial)
  (interactive)
  (let ((name (ido-completing-read "File/Buffer Name: "
                                   (jared/get-files-and-buffers)
                                   nil nil initial)))
    (when (and (stringp name) (> (length name) 0))
      (insert name))))

(defun iy-zap-back-to-char (arg char)
  (interactive "p\ncZap back to char: ")
  (zap-to-char (- arg) char))
(defun iy-zap-back-up-to-char (arg char)
  (interactive "p\ncZap back up to char: ")
  (zap-up-to-char (- arg) char))

(provide 'iy-functions)
