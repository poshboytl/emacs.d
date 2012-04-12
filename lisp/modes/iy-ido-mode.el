(require 'iy-dep)

;; customization
(setq ido-save-directory-list-file (concat iy-data-dir "ido.last"))
(setq ido-everywhere t)
(setq ido-read-file-name-as-directory-commands nil)
(setq ido-use-filename-at-point nil)

(ido-mode t)
(ido-load-history)

(push 'ido-ubiquitous el-get-packages)
(custom-set-variables
 '(ido-ubiquitous-command-exceptions '(execute-extended-command))
 '(ido-ubiquitous t))

(defun iy-ido-mode-init ()
  (define-key ido-completion-map (kbd "M-m") 'ido-merge-work-directories)
  (define-key ido-completion-map (kbd "M-l") 'iy-dwim-dash)
  (define-key ido-completion-map (kbd "M-s") iy-map)
  (define-key ido-completion-map (kbd "C-c") 'ido-toggle-case))

(add-hook 'ido-setup-hook 'iy-ido-mode-init)

;;{{{ flex matching

; http://scottfrazersblog.blogspot.com/2009/12/emacs-better-ido-flex-matching.html
(setq ido-enable-flex-matching nil)

(defun my-ido-fuzzy-match (str items)
  "Better ido fuzzy matching"
  (let ((str-len (length str)))
    (if (= str-len 0)
        (reverse items)
      (let ((char-lookup (make-hash-table :test 'equal)))
        ;; Make hash table of all characters with their corresponding indexes
        (let ((chars (split-string (if ido-case-fold (downcase str) str) "" t))
              (idx 0)
              elt)
          (dolist (char chars)
            (setq elt (gethash char char-lookup))
            (if elt
                (push idx elt) ;; It's important that the indexes are in descending order
              (setq elt (list idx)))
            (puthash char elt char-lookup)
            (setq idx (1+ idx))))
        ;; Go through all the items
        (let (corr matches)
          (dolist (item items)
            (setq corr (my-ido-match-get-correlation str-len char-lookup (ido-name item)))
            (when corr
              (push (cons item corr) matches)))
          ;; Sort matches and return
          (mapcar 'car (if ido-rotate
                           matches
                         (sort matches (lambda (x y) (> (cdr x) (cdr y)))))))))))

(defun my-ido-match-get-correlation (str-len char-lookup item)
  "Get the correlation for this item"
  (let ((partial-matches (make-vector str-len nil))
        (chars (split-string (if ido-case-fold (downcase item) item) "" t))
        (char-idx 0)
        elt-idxs corr prev-partial-match curr-partial-match)
    (dolist (char chars)
      (setq elt-idxs (gethash char char-lookup))
      (when elt-idxs
        (dolist (elt-idx elt-idxs)
          ;; Current and previous partial matches
          (setq curr-partial-match (aref partial-matches elt-idx))
          (setq prev-partial-match (and (> elt-idx 0)
                                        (aref partial-matches (1- elt-idx))))
          ;; Create a new partial match if necessary
          (when (and (not curr-partial-match)
                     (or prev-partial-match (= elt-idx 0)))
            (setq curr-partial-match
                  (aset partial-matches elt-idx
                        (cons char-idx (if (and (= elt-idx 0) (= char-idx 0)) 1 0)))))
          ;; Set (match-position . correlation)
          (when curr-partial-match
            (setcar curr-partial-match char-idx)
            (when prev-partial-match
              (setcdr curr-partial-match
                      (if (= char-idx (1+ (car prev-partial-match)))
                          (1+ (cdr prev-partial-match))
                        (cdr prev-partial-match))))
            ;; Update final correlation
            (when (= elt-idx (1- str-len))
              (if corr
                  (setq corr (max corr (cdr curr-partial-match)))
                (setq corr (cdr curr-partial-match)))))))
      (setq char-idx (1+ char-idx)))
    corr))

(defvar my-ido-use-fuzzy-match t
  "*Use my-ido-fuzzy-match for ido matching")

(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  "Choose between the regular ido-set-matches-1 and my-ido-fuzzy-match"
  (if (and my-ido-use-fuzzy-match
           (not ido-enable-regexp)
           (not ido-enable-prefix))
      (setq ad-return-value (my-ido-fuzzy-match ido-text (ad-get-arg 0)))
    ad-do-it))

;;}}}

(provide 'iy-ido-mode)
