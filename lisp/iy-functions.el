(eval-when-compile (require 'cl))

;;; Scroll

(defvar slow-scroll-mode 0)

(defun toggle-slow-scroll-mode (&optional arg)
  "Toggle slow scroll mode at each call of this function.
If slow-scroll-mode is 1, the cursor stays on current line after each scroll; else not."
  (interactive "P")
  (if (= slow-scroll-mode 0)
      (setq slow-scroll-mode 1)
    (setq slow-scroll-mode 0)))

(defun scroll-up-slowly (&optional arg)
  "Scroll text of current window upward ARG lines; or one line if no ARG.
This depend of slow scroll mode (keeping cursor on current line or not).
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (progn (if arg
             (scroll-up arg)
           (scroll-up 1))
         (if (= slow-scroll-mode 1)
             (forward-line 1))))

(defun scroll-down-slowly (&optional arg)
  "Scroll text of current window downward ARG lines; or one line if no ARG.
This depend of slow scroll mode (keeping cursor on current line or not).
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (progn (if arg
             (scroll-down arg)
           (scroll-down 1))
         (if (= slow-scroll-mode 1)
             (forward-line -1))))

(set-variable 'slow-scroll-mode 1)

(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))

;;; Search

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

;;; Other
(defun iy-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

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

(defun xsteve/save-current-directory ()
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
      (select-window multi-term-dedicated-window))
  )

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

(defvar iy-calendar-copy-date-format-history '("%Y-%m-%d"))
(defun iy-calendar-copy-date (arg)
  "Copy date under the cursor      . Read format from minibuffer if ARG, 
  use recently used format if no ARG . See the function `format-time-string' 
  for the document of time format string"
  (interactive "P")
  (let ((date (calendar-cursor-to-date t))
        (format (if arg
                    (completing-read
                     "Date Format:"
                     iy-calendar-copy-date-format-history nil nil nil
                     'iy-calendar-copy-date-format-history nil nil)
                  (car iy-calendar-copy-date-format-history)))
        string)
    (setq date (encode-time 0 0 0 (cadr date) (car date) (nth 2 date)))
    (setq string (format-time-string format date))
    (if (eq last-command 'kill-region)
        (kill-append string nil)
      (kill-new string))))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (flet ((yes-or-no-p (prompt) t)) (revert-buffer)))

(defun forward-empty-line ()
  (interactive)
  (while (and (not (eobp)) (looking-at "[ \t]*$"))
    (forward-line)))

(defun iy-zap-back-to-char (arg char)
  (interactive "p\ncZap back to char: ")
  (zap-to-char (- arg) char))
(defun iy-zap-back-up-to-char (arg char)
  (interactive "p\ncZap back up to char: ")
  (zap-up-to-char (- arg) char))

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

(provide 'iy-functions)
