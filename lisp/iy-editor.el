;;{{{ General

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(custom-set-variables
 '(tab-width 2)
 '(indent-tabs-mode nil)
 '(show-paren-mode t)
 '(fill-column 78)

 ;; '(delete-active-region 'kill)
 ;; '(delete-selection-mode t)

 '(set-mark-command-repeat-pop t))

(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      time-stamp-start "[Uu]pdated\\(_at\\)?[ \t]*:?[ \t]+<"
      time-stamp-end ">")

(push 'whole-line-or-region el-get-packages)
(defun iy-el-get-after-whole-line-or-region ()
  (whole-line-or-region-mode))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;}}}

;;{{{ Folding

(defvar fringe-face 'fringe)
(defface collapsed-face '((t (:background "#2e3330" :foreground "#57705c"))) "Collapsed Overlay")
(defvar collapsed-face 'collapsed-face)

(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'ruby-mode-hook 'hs-minor-mode)

(push 'fold-dwim el-get-packages)

;; http://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(require 'hideshow)

;; http://code.google.com/p/bamanzi-misc/source/browse/trunk/_emacs.d/site-lisp/common/fold_/hideshow-fringe.el?r=122&spec=svn448
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
(defun display-code-line-counts (ov)
  (when (or (eq 'code (overlay-get ov 'hs))
            (eq 'outline (overlay-get ov 'invisible)))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format " (%d)..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "Hiddent text. M-s <SPC> to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 1 (length display-string) 'face 'collapsed-face display-string)
      (overlay-put ov 'display display-string)
      (overlay-put ov 'evaporate t))))
(defun hide-code-line-counts (ov)
  (when (or (eq 'code (overlay-get ov 'hs))
            (eq 'outline (overlay-get ov 'invisible)))
    (overlay-put ov 'display nil)))

(setq hs-set-up-overlay 'display-code-line-counts)

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\|{\\)" "\\(end\\|}\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))

(defadvice outline-flag-region (after display-code-line-counts (from to flag) activate)
  (let ((os (overlays-in from to)))
    (if flag
        (progn
          (mapc 'hide-code-line-counts os)
          (when os (display-code-line-counts (car os))))
      (mapc 'display-code-line-counts os))))

(defvar iy-forward-comment-stop-at-outline-header t)

(defun iy-forward-one-comment ()
  (let ((iy-forward-comment-stop-at-outline-header nil))
    ;; stop if at the begining of outline heading
    (when (not (and (bolp) (outline-on-heading-p)))
      (when (forward-comment 1)
        ;; forward-comment should not skip outline heading
        (let ((pos (point)))
          (if (and (forward-comment -1)
                   (outline-on-heading-p))
              nil
            (goto-char pos)
            t))))))

(defun iy-backward-one-comment ()
  (let ((iy-forward-comment-stop-at-outline-header nil))
    (when (forward-comment -1)
      (if (outline-on-heading-p)
          (progn
            (end-of-line)
            nil)
        t))))

;; Fix outline-minor-mode conflicts with fold-dwim
(defadvice forward-comment (around stop-at-outline-header (count) activate)
  (if (and (or outline-minor-mode (eq major-mode 'outline-mode))
           iy-forward-comment-stop-at-outline-header)
      (progn
        (while (and (> count 0) (iy-forward-one-comment))
          (setq count (1- count)))
        (while (and (< count 0) (iy-backward-one-comment))
          (setq count (1+ count)))
        (setq ad-return-value (zerop count)))
    ad-do-it))

;;}}}

;;{{{ Whitespace

(custom-set-variables
 '(whitespace-action '(cleanup))
 '(whitespace-global-modes '(emacs-lisp-mode ruby-mode coffee-mode sass-mode css-mode haml-mode))
 '(whitespace-line-column fill-column)
 '(whitespace-style (quote (face tabs trailing newline indentation space-before-tab tab-mark newline-mark)))
 '(coffee-cleanup-whitespace nil))
(global-whitespace-mode)

;;}}}

;;{{{ Kill ring

(custom-set-variables
 '(kill-ring-max 500)
 '(kill-whole-line t))

(push 'browse-kill-ring el-get-packages)
(push 'kill-ring-search el-get-packages)

(global-set-key (kbd "C-M-y") 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-search-maybe (arg) activate)
  "If last action was not a yank, run `kill-ring-search' instead."
  (interactive "p")
  (if (not (eq last-command 'yank))
      (kill-ring-search)
    (barf-if-buffer-read-only)
    ad-do-it))

;;}}}

;;{{{ Indention

(push 'dtrt-indent el-get-packages)

;;}}}

;;{{{ Electric

(push 'autopair el-get-packages)
(setq autopair-blink nil)

(push 'paredit el-get-packages)

;; Using local-set-key in a mode-hook is a better idea.
(defun iy-el-get-after-paredit ()
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-S") nil)
  (define-key paredit-mode-map [C-left] nil)
  (define-key paredit-mode-map [C-right] nil)
  (define-key paredit-mode-map [C-up] nil)
  (define-key paredit-mode-map [C-down] nil)
  (define-key paredit-mode-map (kbd "M-p") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "M-n") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-N") 'paredit-split-sexp))

;;}}}

;;{{{ Hilight
(custom-set-variables
 '(highlight-symbol-idle-delay 1)
 '(highlight-symbol-on-navigation-p t)
 '(hl-paren-colors (quote ("firebrick1" "IndianRed1" "IndianRed4" "grey")))
 '(pulse-delay 0.03)
 '(pulse-flag nil)
 '(pulse-iterations 5))

(push 'highlight-symbol el-get-packages)
(push 'highlight-parentheses el-get-packages)

(defun iy-el-get-after-highlight-parentheses ()
  (add-hook 'c-mode-common-hook 'highlight-parentheses-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'ruby-mode-hook 'highlight-parentheses-mode))
;;}}}

;;{{{ Mark

(push 'hide-comnt el-get-packages)
(push 'thingatpt+ el-get-packages)
(push 'thing-cmds el-get-packages)
(push 'expand-region el-get-packages)

;;}}}

;;{{{ Misc

;; diactivate mark after narrow

(defadvice narrow-to-region (after deactivate-mark (start end) activate)
  (deactivate-mark))

;;}}}

(provide 'iy-editor)
