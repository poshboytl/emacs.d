(eval-when-compile (require 'cl))

;;{{{ Buffer
(defun iy-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
;;}}}

;;{{{ String functions
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
;;}}}

;;{{{ Utilities
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

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (flet ((yes-or-no-p (prompt) t)) (revert-buffer)))

(defun iy-set-folding-marks (b e &optional mode)
  (interactive "sBegin Mark: \nsEnd Mark: ")
  (let* ((mode (or mode major-mode))
         (ptr (assq mode folding-mode-marks-alist)))
    (setcdr ptr (list b e))))

(defun iy-close-help ()
  (interactive)
  (save-excursion
    (dolist (w (window-list))
      (set-buffer (window-buffer w))
      (when (eq major-mode 'help-mode)
        (delete-window w)))))
;;}}}

;;{{{ Insert
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
;;}}}

;;{{{ Editing

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun iy-zap-back-to-char (arg char)
  (interactive "p\ncZap back to char: ")
  (zap-to-char (- arg) char))
(defun iy-zap-back-up-to-char (arg char)
  (interactive "p\ncZap back up to char: ")
  (zap-up-to-char (- arg) char))

(defun iy-next-line-and-open-it-if-not-empty ()
  (interactive)
  (forward-line)
  (unless (looking-at "[ 	]*$")
    (open-line 1))
  (indent-according-to-mode))

(defvar iy-last-is-case-transformation nil)
(defvar iy-case-tranformation-functions
  '(iy-dwim-dash iy-dwim-underscore))

(defun iy-dwim-dash (arg)
  (interactive "P")
  (when (consp arg) (setq arg 1))
  (if (or (region-active-p)
          arg
          (and iy-last-is-case-transformation
               (memq last-command iy-case-tranformation-functions)))
      (progn
        (iy-dwim-downcase (prefix-numeric-value arg))
        (setq iy-last-is-case-transformation t))
    (insert "-")
    (setq iy-last-is-case-transformation nil)))

(defun iy-dwim-downcase (arg)
  (interactive "p")
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word arg)))

(defun iy-dwim-underscore (arg)
  (interactive "P")
  (when (consp arg) (setq arg 1))
  (if (or (region-active-p)
          arg
          (and iy-last-is-case-transformation
               (memq last-command iy-case-tranformation-functions)))
      (progn
        (iy-dwim-upcase (prefix-numeric-value arg))
        (setq iy-last-is-case-transformation t))
    (insert "_")
    (setq iy-last-is-case-transformation nil)))

(defun iy-dwim-upcase (arg)
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word arg)))

(defun iy-dwim-capitalize (arg)
  (interactive "P")
  (when (consp arg) (setq arg 1))
  (if (region-active-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word (prefix-numeric-value arg))))

(defun shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line does not contain non-white space chars, then remove blank lines to just one.
If current line contains non-white space chars, then shrink any whitespace char surrounding cursor to just one space.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let (
        cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))

      (setq spaceTabNeighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)) t nil) )
      (goto-char cursor-point)

      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char cursor-point)      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point))
      )

    (if line-has-meat-p
        (let (deleted-text)
          (when spaceTabNeighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " ") ) ) )

      (progn
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n")
        (delete-blank-lines)
        )
      ;; todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )
    )
  )

;;}}}

;;{{{ Mark

(defvar things-map
  '((?w . word)
    (?W . symbol)
    (?s . sexp)
    (?d . defun)
    (?l . list)
    (?n . line)
    (?. . sentence)
    (?p . paragraph)
    (?P . page)
    (?f . filename)
    (?u . url)
    (?c . comment)))

(defun iy-ido-mark-thing ()
  (interactive)
  (thing-region
   (ido-completing-read "thing: " (mapcar (lambda (th) (prin1-to-string (cdr th))) things-map) nil t)))

(defun iy-mark-thing (arg)
  (interactive "P")
  (condition-case e
      (let* ((echo-keystrokes nil)
             (ev last-command-event)
             (thing (cdr (assq ev things-map))))
        (while thing
          (condition-case e
              (progn (mark-thing thing arg t))
            (error (message (cadr e))))
          (setq ev (read-event))
          (setq thing (cdr (assq ev things-map))))
        (push ev unread-command-events))
    ('quit (call-interactively 'keyboard-quit))))

(defun iy-mark-surround-thing ()
  (interactive)
  (let ((thing (cdr (assq last-command-event things-map))))
    (when thing
      (thing-region (prin1-to-string thing)))))

(defun iy-forward-thing (arg)
  (interactive "P")
  (let* ((echo-keystrokes nil)
         (ev last-command-event)
         (thing (cdr (assq ev things-map))))
    (while thing
      (condition-case e
          (progn (forward-thing thing (prefix-numeric-value arg)))
        (error (message (cadr e))))
      (setq ev (read-event))
      (setq thing (cdr (assq ev things-map))))
    (push ev unread-command-events)))

(defun iy-backward-thing (arg)
  (interactive "P")
  (iy-forward-thing (- (prefix-numeric-value arg))))

(defun iy-begining-of-thing ()
  (interactive)
  (let ((thing (cdr (assq last-command-event things-map))))
    (when thing
      (beginning-of-thing thing))))

(defun iy-end-of-thing ()
  (interactive)
  (let ((thing (cdr (assq last-command-event things-map))))
    (when thing
      (end-of-thing thing))))

;;}}}

;;{{{ Move
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
;;}}}

;;{{{ eshell

;; hide -> show -> full screen -> hide
;; inactive -> switch -> full screen -> hide
(defun iy-eshell-toggle ()
  (interactive)
  (let ((eshell-buffer (get-buffer "*eshell*")))
    (if (eq (current-buffer) eshell-buffer)
        (if (eq (length (window-list)) 1)
            ;; full screen
            (switch-to-buffer (other-buffer))
          ;; active, go to full screen
          (delete-other-windows))

      (unless eshell-buffer
        (save-window-excursion
          (setq eshll-buffer (eshell))))
      (switch-to-buffer-other-window eshell-buffer))))

(defun iy-eshell-here ()
  (interactive)
  (let ((dir default-directory)
        (eshell-buffer
         (or
          (get-buffer "*eshell*")
          (save-window-excursion (eshell)))))
    (unless (eq (current-buffer) eshell-buffer)
      (switch-to-buffer-other-window eshell-buffer)
      (goto-char (point-max))
      (insert (format "cd '%s'" dir))
      (eshell-send-input))))

;;}}}

(provide 'iy-functions)
