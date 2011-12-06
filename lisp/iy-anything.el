(require 'iy-dep)
(require 'iy-keymap)
(require 'iy-eproject)

;;; Libraries
(push
 '(:name anything
         :after iy-el-get-after-anything)
 el-get-sources)

(defun iy-el-get-after-anything ()
  (require 'anything-config)
  (require 'anything-match-plugin)

  ;;; Sources
  (setq anything-sources
        (list
         'anything-c-source-ffap-line
         'anything-c-source-ffap-guesser
         'anything-c-source-buffers-list
         'anything-c-source-file-cache
         'anything-c-source-files-in-current-dir+
         'anything-c-source-recentf
         'anything-c-source-file-name-history
         'anything-c-source-bookmarks
         'anything-c-source-w3m-bookmarks
         'anything-c-source-locate))

  ;;; Shortcuts
  (global-set-key (kbd "M-X") 'anything-at-point)
  (define-key iy-map (kbd "M-s") 'anything-at-point)
  (define-key iy-map (kbd "M-x") 'anything-M-x)

  (setq anything-enable-digit-shortcuts t)
  (define-key anything-map (kbd "M-1") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-2") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-3") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-4") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-5") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-6") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-7") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-8") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-9") 'anything-select-with-digit-shortcut)
  (define-key anything-map "\C-u" 'anything-delete-minibuffer-contents)
  (define-key anything-map "\M-N" 'anything-next-source)
  (define-key anything-map "\M-P" 'anything-previous-source)
  (define-key anything-map "\C-\M-n" 'anything-next-source)
  (define-key anything-map "\C-\M-p" 'anything-previous-source)
  (define-key anything-map (kbd "C-'") 'anything-toggle-visible-mark)

  (define-key minibuffer-local-map "\C-r" 'anything-minibuffer-history)
  (define-key minibuffer-local-completion-map "\C-r" 'anything-minibuffer-history)

  (define-key anything-command-map (kbd "g") 'anything-do-grep)
  (define-key anything-command-map (kbd "o") 'anything-occur)
  (define-key anything-command-map (kbd "r") 'anything-register)
  (define-key anything-command-map (kbd "R") 'anything-regexp)
  (define-key anything-command-map (kbd "b") 'anything-c-pp-bookmarks)
  (define-key anything-command-map (kbd "'") 'anything-all-mark-rings))

;; Customization
(setq anything-input-idle-delay 0)
(setq anything-idle-delay 0.3)
(setq anything-quick-update t)
(setq anything-c-use-standard-keys t)

;;; Bindings
(defun anything-insert-buffer-base-name ()
  "Insert buffer name stub."
  (interactive)
  (anything-insert-string
   (with-current-buffer anything-current-buffer
     (buffer-stub-name))))

;; 1. Quote the string
;; 2. If we didn't input any typically regexp characters, convert spaces to .*,
;;    however, it is still order related.
(defun anything-pattern-to-regexp (string)
  (prin1-to-string
   (if (string-match-p "[\\[\\]*+$^]" string) string
     (let ((parts (split-string string "[ \t]+" t)))
       (if (eq 2 (length parts))
           ;; for two parts a,b we make a.*b\|b.*a
           (concat
            (mapconcat 'regexp-quote parts ".*")
            "\\|"
            (mapconcat 'regexp-quote (reverse parts) ".*"))
         ;; only 1 part or more than 2 parts, fine, just combine them using .*,
         ;; thus it will slow down locate a lot. This means you have to type in order
         (mapconcat 'regexp-quote parts ".*"))))))

;; Hack
;; Convert anything pattern to regexp for locate
(defadvice anything-c-locate-init (around anything-pattern-to-regexp () activate)
  (let ((anything-pattern (anything-pattern-to-regexp anything-pattern)))
    ad-do-it))

(provide 'iy-anything)
