;;{{{ TAGS

(custom-set-variables
 '(tags-add-tables nil))

;;}}}

;;{{{ ACK

(custom-set-variables
 '(ack-root-directory-functions nil)
 '(ack-prompt-for-directory t))

(push 'xcscope el-get-packages)

(push 'ag el-get-packages)

(autoload 'ag/search "ag" nil nil)
(autoload 'ag "ag" nil t)
(autoload 'ag-regexp "ag" nil t)
(autoload 'ag-project "ag" nil t)
(autoload 'ag-project-regexp "ag" nil t)
(autoload 'ag-project-at-point "ag" nil t)
(autoload 'ag-regexp-project-at-point "ag" nil t)

(defun agap (string dir is-regexp)
  "Search string at point inside current directory. Use prefix arg to search by regexp."
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
                     (ido-read-directory-name default-directory)
                     current-prefix-arg))
  (ag/search string dir is-regexp))

(defun agap-regexp (string dir)
  "Search string at point inside current directory."
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
                     (ido-read-directory-name default-directory)))
  (ag/search string dir t))

(eval-after-load 'ag
  '(progn
     (fset 'ag (function agap))
     (fset 'ag-regexp (function agap-regexp))))

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

(defun alternative-files-factories-finder (&optional file)
  (let ((file (or file (alternative-files--detect-file-name))))
    (cond
     ((string-match "^\\(.*\\)/app/models/\\(.+\\)\\.rb$" file)
      (let ((root (match-string 1 file))
            (name (match-string 2 file)))
        (list
         (concat root "/spec/factories/" (alternative-files--pluralize-string name) ".rb"))))

     ((string-match "^\\(.*\\)/spec/factories/\\(.+\\).rb$" file)
      (let ((root (match-string 1 file))
            (name (match-string 2 file)))
        (list
         (concat root "/app/models/" (alternative-files--singularize-string name) ".rb")))))))

(defun iy-el-get-after-alternative-files ()
  (push 'alternative-files-factories-finder alternative-files-functions))

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
