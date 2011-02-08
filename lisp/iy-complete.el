;;; iy-complete.el -- Completion

;;; Hippie Expand
(setq hippie-expand-try-functions-list
      '(
        ;; senator-try-expand-semantic
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ;; try-expand-all-abbrevs
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        ;; try-expand-list
        ;; try-complete-lisp-symbol-partially
        ;; try-complete-lisp-symbol
        try-expand-whole-kill
        ))

(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'iy-complete)