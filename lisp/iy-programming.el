;;{{{ TAGS
(custom-set-variables
 '(tags-add-tables nil))
;;}}}

;;{{{ ACK
(custom-set-variables
 '(ack-prompt-for-directory t))

(push 'full-ack el-get-sources)
(push '(:name xcscope :after iy-el-get-after-xcscope :localname "xcscope.el")
      el-get-sources)

(defun ack-here ()
  (interactive)
  (let ((ack-root-directory-functions nil)
        (ack-prompt-for-directory t))
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
(push '(:name alternative-files
              :compile "alternative-files.el"
              :type git
              :url "git://github.com/doitian/alternative-files-el.git"
              :features alternative-files
              :after (lambda ()
                       (define-key iy-map "a" 'alternative-files-find-file)
                       (define-key iy-map (kbd "M-a") 'alternative-files-find-file)
                       (define-key iy-map (kbd "A") 'alternative-files-create-file)))
      el-get-sources)
;;}}}

(provide 'iy-programming)
