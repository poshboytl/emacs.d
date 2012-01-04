(eval-when-compile (require 'cl))
(eval-when-compile (require 'ido))
(require 'iy-dep)

(custom-set-variables
 '(bookmark-default-file (concat iy-data-dir "bookmark"))
 '(bookmark-use-annotations nil))

(defun iy-el-get-after-bookmark+ ()
  ;; Redifine `bmkp-completing-read-1'.
  ;;
  ;; I have tried to use `flet' in `advice' to redefine `completing-read' but
  ;; failed because of `max-specpdl-size'.
  ;;
  ;; The redefined version use `ido-completing-read' to read user input.
  (defun bmkp-completing-read-1 (prompt default alist pred hist laxp)
    "Helper for `bookmark-completing-read(-lax)'.
LAXP non-nil means use lax completion."
    (bookmark-maybe-load-default-file)
    (setq alist  (or alist bookmark-alist))
    (if (and (not laxp)
             (listp last-nonmenu-event)
             (or (eq t bmkp-menu-popup-max-length)
                 (and (integerp bmkp-menu-popup-max-length)
                      (< (length alist) bmkp-menu-popup-max-length))))
        (bookmark-menu-popup-paned-menu
         t prompt
         (if bmkp-sort-comparer           ; Test whether to sort, but always use `string-lessp'.
             (sort (bookmark-all-names alist) 'string-lessp)
           (bookmark-all-names alist)))
      (let* ((icicle-delete-candidate-object  (lambda (cand) ; For `S-delete' in Icicles.
                                                (bookmark-delete
                                                 (icicle-transform-multi-completion cand))))
             (completion-ignore-case          bookmark-completion-ignore-case)
             (default                         default)
             (prompt                          (if default
                                                  (concat prompt (format " (%s): " default))
                                                (concat prompt ": ")))
             (str                             (ido-completing-read
                                               prompt
                                               (mapcar
                                                (lambda (e)
                                                  (if (listp e) (car e) e))
                                                alist)
                                               pred (not laxp) nil
                                               (or hist 'bookmark-history) default)))
        (if (and (string-equal "" str) default) default str)))))

(push '(:name bookmark+ :url "https://github.com/doitian/bookmark-plus.git" :after iy-el-get-after-bookmark+) el-get-sources)

(add-hook 'bookmark-bmenu-mode-hook 'iy-bookmark-bmenu-mode-init)
(defun iy-bookmark-bmenu-mode-init ()
  (define-key bookmark-bmenu-mode-map (kbd "M-o") 'other-window))

(defun iy-bmkp-navigation ()
  "bookmark+ navigation"
  (interactive)
  (let ((done nil)
        (ev last-command-event)
        (echo-keystrokes nil))
    (while (not done)
      (ignore-errors
        (cond ((or (eq ev ?,) (eq ev ?\M-,)) (bmkp-next-bookmark-this-buffer 1))
              ((or (eq ev ?.) (eq ev ?\M-.)) (bmkp-previous-bookmark-this-buffer 1))
              ((eq ev ?<) (bmkp-next-bookmark 1))
              ((eq ev ?>) (bmkp-previous-bookmark 1))
              ((eq (event-basic-type ev) ?/) (bookmark-bmenu-list))
              (t (setq done t))))
      (when (not done)
        (setq ev (read-event))))
    (push ev unread-command-events)))

(provide 'iy-bookmark)
