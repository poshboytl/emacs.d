;;; iy-dired.el --- config for Dired

(require 'iy-dep)

(setq
 el-get-packages
 (append
  '(dired+ dired-details dired-details+) el-get-packages))

(custom-set-variables
 '(dired-dwim-target t)
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top)))

(defun iy-el-get-after-dired+ ()
  (defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer)))
      ad-do-it
      (kill-buffer orig))))

(defun dired-launch-command ()
  (interactive)
  (dired-do-async-shell-command
   (if (eq system-type 'darwin) "open" "xopen")
   nil
   (dired-get-marked-files t current-prefix-arg)))

(defadvice dired-run-shell-command (around kid-dired-run-shell-command (command))
  "run a shell command COMMAND .
  If the COMMAND ends with `&' then run it in background and *discard* the
  output, otherwise simply let the original `dired-run-shell-command' run it."
  (if (string-match "&[[:blank:]]*$" command)
      (let ((proc (start-process "kid-shell" nil shell-file-name
                                 shell-command-switch
                                 (substring command 0 (match-beginning 0)))))
        (set-process-sentinel proc 'shell-command-sentinel))
    ad-do-it))
(ad-activate 'dired-run-shell-command)

(defadvice dired-advertised-find-file (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let* ((orig (current-buffer))
         (filename (dired-get-filename t t))
         (bye-p (file-directory-p filename)))
    ad-do-it
    (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
      (kill-buffer orig))))

(autoload 'wdired-change-to-wdired-mode "wdired")

(eval-after-load 'dired
  '(progn
     (require 'dired-x)
     (setq dired-omit-files
           (rx (or (seq bol "#")
                   (seq bol ".")
                   (seq "~" eol)
                   (seq bol "svn" eol)
                   (seq bol "_region_")
                   (seq bol "prv" (* anything) ".log" eol)
                   (seq bol "cscope.files" eol)
                   (seq bol "GPATH" eol)
                   (seq bol "GRTAGS" eol)
                   (seq bol "GSYMS" eol)
                   (seq bol "GTAGS" eol)
                   )))
     (setq dired-omit-extensions
           (append dired-omit-extensions
                   (list
                    ".auxbbl.make"
                    ".auxdvi.make"
                    ".aux.make"
                    ".fls"
                    ".ilg"
                    ".ind"
                    ".out"
                    ".out.make"
                    ".prv"
                    ".temp"
                    ".toc.make"
                    ".gpi.log"
                    ".ps.log"
                    ".pdf.log"
                    ".bak"
                    ".mp.log"
                    ".mp.make"
                    ".mpx"
                    ".sdb"
                    ".nav"
                    ".snm"
                    ".fdb_latexmk"
                    )))

     (define-key dired-mode-map "E" 'wdired-change-to-wdired-mode)
     (define-key dired-mode-map
       [menu-bar immediate wdired-change-to-wdired-mode]
       '("Edit File Names" . wdired-change-to-wdired-mode))
     (define-key dired-mode-map (kbd "`") 'dired-clean-directory)
     (define-key dired-mode-map (kbd ".") 'dired-omit-mode)
     (define-key dired-mode-map (kbd "M-o") 'other-window)
     (define-key dired-mode-map (kbd "/") 'diredp-omit-marked)
     (define-key dired-mode-map "(" 'dired-details-toggle)
     (define-key dired-mode-map ")" 'dired-details-toggle)
     (define-key dired-mode-map (kbd "M-<return>") 'dired-launch-command)))

(defun iy-dired-mode-init ()
  (hl-line-mode)
  (dired-omit-mode 1))

(add-hook 'dired-mode-hook 'iy-dired-mode-init)

(provide 'iy-dired-mode)
