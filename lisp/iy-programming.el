;;{{{ TAGS

(custom-set-variables
 '(tags-add-tables nil))

;;}}}

;;{{{ ACK

(custom-set-variables
 '(ack-root-directory-functions nil)
 '(ack-prompt-for-directory t))

(push 'full-ack el-get-packages)
(push 'xcscope el-get-packages)

(defun ack-root (&optional force)
  (interactive "P")
  (let ((ack-prompt-for-directory force)
        (ack-root-directory-functions
         '(eproject-root-safe ack-guess-project-root)))
    (call-interactively 'ack)))

;;}}}

;;{{{ cscope
(defun iy-el-get-after-xcscope ()
  (defcustom cscope-ignore-case t
    "*Whether to ignore case while searching."
    :group 'cscope
    :type 'boolean)

  (defun cscope-toggle-case ()
    (interactive)
    (setq cscope-ignore-case (not cscope-ignore-case))
    (cscope-tell-ignore-case))
  (defun cscope-tell-ignore-case ()
    (interactive)
    (message "Cscope Ignore Case (%s)"
              (if cscope-ignore-case "Enable" "Disable")))

  (define-key cscope:map "\C-csv" 'cscope-toggle-case)
  (define-key cscope:map "\C-csV" 'cscope-tell-ignore-case)
  (define-key cscope-list-entry-keymap "v" 'cscope-toggle-case)
  (define-key cscope-list-entry-keymap "V" 'cscope-tell-ignore-case)

  (defadvice cscope-call (before ignore-case activate)
    "ignore case in cscope search"
     (when cscope-ignore-case
       (ad-set-arg 1 (cons "-C" (ad-get-arg 1))))))
;;}}}

;;{{{ Alternative File

(push 'alternative-files el-get-packages)
(define-key iy-map "a" 'alternative-files-find-file)
(define-key iy-map (kbd "M-a") 'alternative-files-find-file)
(define-key iy-map (kbd "A") 'alternative-files-create-file)

;;}}}

;;{{{ recoll

(defvar recoll-error-regexp-alist-alist
  '((recoll "\\[file://\\(.*?\\)\\]" 1)))
(defvar recoll-error-regexp-alist '(recoll))
(eval-when-compile (require 'compile))

(define-compilation-mode recoll-mode "recoll" "recoll search" nil)
(define-key recoll-mode-map "n" 'compilation-next-error)
(define-key recoll-mode-map "p" 'compilation-previous-error)

(defun recoll-parse-results ()
  "Count the matches printed by `recoll' in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\(.*\\) results$" (point-max) t)
        (string-to-number (match-string 1))
      0)))

(defun recoll-sentinel (proc result)
  (when (eq (process-status proc) 'exit)
    (with-current-buffer (process-buffer proc)
      (let ((c (recoll-parse-results)))
        (when (= c 0)
          (kill-buffer (current-buffer)))
        (message "recoll finished with %d result%s" c (if (eq c 1) "" "s"))))))

(defun recoll-filter (proc output)
  (let ((buffer (process-buffer proc))
        (inhibit-read-only t)
        beg)
    (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (setq beg (point-max)))
          (insert output)
          ;; Error properties are done by font-lock.
          (font-lock-fontify-region beg (point-max))))
      (when (processp recoll-process)
        (delete-process recoll-process)))))

(defvar recoll-history nil)
(defvar recoll-process nil)
(defconst recoll-buffer-name "*recoll*")
(defun recoll (query)
  (interactive (list
                (read-string "recoll: " nil recoll-history)))
  (when query
    (let ((buffer (get-buffer-create recoll-buffer-name))
          (inhibit-read-only t))
      (setq next-error-last-buffer buffer)
      (with-current-buffer buffer
        (erase-buffer)
        (recoll-mode)
        (setq buffer-read-only t)
        (font-lock-fontify-buffer)
        (display-buffer (current-buffer)))
      (setq recoll-process
            (start-process "recoll" buffer "recoll" "-t" "-q" query))
      (set-process-sentinel recoll-process 'recoll-sentinel)
      (set-process-query-on-exit-flag recoll-process nil)
      (set-process-filter recoll-process 'recoll-filter))))

;;}}}

;;{{{ git grep

;; http://stackoverflow.com/a/2567637/667158
;; There's something similar (but fancier) in vc-git.el: vc-git-grep
;; -I means don't search through binary files
(defcustom git-grep-switches "--extended-regexp -I -n --ignore-case"
  "Switches to pass to `git grep'."
  :type 'string)

(defun git-grep (command-args)
  (interactive
   (list (read-shell-command "Run git-grep (like this): "
                             (format "git grep %s -e "
                                     git-grep-switches)
                             'git-grep-history)))
  (let ((grep-use-null-device nil))
    (grep command-args)))

;;}}}

(provide 'iy-programming)
