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

 '(delete-active-region 'kill)

 '(set-mark-command-repeat-pop t))

(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      time-stamp-start "[Uu]pdated\\(_at\\)?[ \t]*:?[ \t]+<"
      time-stamp-end ">")

(custom-set-variables
 '(whole-line-or-region-mode t))

(push 'whole-line-or-region el-get-sources)

;;}}}

;;{{{ Folding

(defvar fringe-face 'fringe)
(custom-set-variables
 '(folding-font-lock-begin-mark 'fringe-face)
 '(folding-font-lock-end-mark 'fringe-face))
(defface collapsed-face '((t (:background "#e0cf9f" :foreground "#5f5f5f"))) "Collapsed Overlay")
(defvar collapsed-face 'collapsed-face)

(push '(:name
        folding
        :after (lambda ()
                 (setq folding-check-folded-file-function 'iy-folding-check-folded)
                 (folding-add-to-marks-list 'ruby-mode "# {{{" "# }}}" nil t)
                 (define-key iy-map (kbd "i") folding-mode-prefix-map)
                 (define-key folding-mode-prefix-map (kbd "i") 'folding-shift-in)
                 (define-key folding-mode-prefix-map (kbd "o") 'folding-shift-out)
                 (define-key folding-mode-prefix-map (kbd "<SPC>") 'folding-context-next-action)
                 (define-key folding-mode-prefix-map (kbd "j") 'folding-next-visible-heading)
                 (define-key folding-mode-prefix-map (kbd "n") 'folding-next-visible-heading)
                 (define-key folding-mode-prefix-map (kbd "k") 'folding-previous-visible-heading)
                 (define-key folding-mode-prefix-map (kbd "p") 'folding-previous-visible-heading)

                 (defun folding-shift-in (&optional noerror)
                   (interactive)
                   (labels
                       ((open-fold nil
                                   (let ((data (folding-show-current-entry noerror t)))
                                     (and data
                                          (progn
                                            (when folding-narrow-by-default
                                              (setq folding-stack
                                                    (if folding-stack
                                                        (cons (cons (point-min-marker)
                                                                    (point-max-marker))
                                                              folding-stack)
                                                      '(folded)))
                                              (folding-set-mode-line))
                                            (folding-narrow-to-region (car data) (nth 1 data)))))))
                     (let ((goal (point)))
                       (while (folding-skip-ellipsis-backward)
                         (beginning-of-line)
                         (open-fold)
                         (goto-char goal))
                       (if folding-narrow-by-default
                           (open-fold)
                         (widen)))))

                 (defun folding-font-lock-support ()
                   "Add font lock support."
                   (ignore-errors
                     (font-lock-add-keywords nil (folding-font-lock-keywords major-mode))))))
      el-get-sources)

(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(push '(:name
        fold-dwim
        :type http
        :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el"
        :features fold-dwim)
        ;; :after (lambda ()
        ;;          (folding-mode-add-find-file-hook)))
      el-get-sources)

(defun iy-folding-check-folded ()
  "Function to determine if this file is in folded form."
  (let ((folding-re1 "^.?.?.?{{{")
        (folding-re2 "[\r\n].*}}}"))
    (save-excursion
      (goto-char (point-min))
      ;;  If we found both, we assume file is folded
      (and (assq major-mode folding-mode-marks-alist)
           (< (point-max) 10000)
           (re-search-forward folding-re1 nil t)
           ;; if file is folded, there are \r's
           (re-search-forward "[\r\n]" nil t)
           (re-search-forward folding-re2 nil t)))))

(defun folding-marker-p (&optional pos)
  (eq (get-char-property (or pos (point)) 'face) 'fringe))

(defadvice fold-dwim-toggle (around toggle-folding-on-folding-marker activate)
  (if (folding-marker-p)
      (folding-toggle-show-hide)
    ad-do-it))

(defadvice forward-comment (around stop-at-folding-header (count) activate)
  (if (= 0 (ad-get-arg 0))
      (progn ad-do-it)
    (if (folding-marker-p)
        (setq ad-return-value nil)
      (let ((loop-times (abs count))
            (direction (/ count (abs count))))
        (ad-set-arg 0 direction)
        (setq ad-return-value t)
        (while (and (> loop-times 0) ad-return-value)
          ad-do-it
          (when ad-return-value
            (if (> direction 0)
              (if (folding-marker-p)
                  (setq ad-return-value nil)
                (when (folding-marker-p (- (point) 2))
                  (setq ad-return-value nil)
                  (forward-char -2)
                  (beginning-of-line)))
              (when (folding-marker-p)
                (end-of-line)
                (setq ad-return-value nil)))
            (setq loop-times (1- loop-times))))))))

(defadvice fold-dwim-hide-all (around folding-open-first activate)
  (if (and (boundp 'folding-mode) folding-mode)
      (progn
        (folding-uninstall)
        (let ((hs-hide-comments-when-hiding-all nil))
          ad-do-it)
        (folding-mode))
    ad-do-it))

(require 'hideshow)

;; http://code.google.com/p/bamanzi-misc/source/browse/trunk/_emacs.d/site-lisp/common/fold_/hideshow-fringe.el?r=122&spec=svn448
(define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
           (marker-length (length marker-string))
           (display-string (format " (%d)..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "Hiddent text. M-s <SPC> to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 1 (length display-string) 'face 'collapsed-face display-string)
      (overlay-put ov 'display display-string)
      (overlay-put ov 'evaporate t))))
(setq hs-set-up-overlay 'display-code-line-counts)

(defadvice folding-subst-regions (around toggle-fringe (list find replace) activate)
  ad-do-it
  (save-excursion
    (while list
      (let* ((begin (car list))
             (end (cadr list))
             bol eol
             (marker-string "*fringe-dummy*")
             (marker-length (length marker-string)))
        (dolist (ov (overlays-in begin end))
          (when (overlay-get ov 'fringe-folding-p)
            (delete-overlay ov)))
        (when (and (eq find ?\n) (eq replace ?\r))
          ;; \\n -> \\r add fringe
          (goto-char begin)
          (search-forward "\r")
          (forward-char -1)
          (let* ((ov (make-overlay (point) end nil 'front-advance))
                 (display-string (format " (%d)..." (count-lines begin end))))
            (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
            (overlay-put ov 'before-string marker-string)
            (put-text-property 1 (length display-string) 'face 'collapsed-face display-string)
            (overlay-put ov 'display display-string)
            (overlay-put ov 'priority 9999)
            (overlay-put ov 'fringe-folding-p t)
            (overlay-put ov 'evaporate t))))
      (setq list (cdr (cdr list))))))

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

(push 'browse-kill-ring el-get-sources)

(push '(:name kill-ring-search
              :type elpa)
      el-get-sources)

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

(push '(:name dtrt-indent
              :features nil
              :url "https://github.com/emacsmirror/dtrt-indent.git"
              :post-init (lambda () (autoload 'dtrt-indent-mode "dtrt-indent" nil t)))
      el-get-sources)

;;}}}

;;{{{ Hilight
(custom-set-variables
 '(highlight-symbol-idle-delay 1)
 '(highlight-symbol-on-navigation-p t)
 '(hl-paren-colors (quote ("firebrick1" "IndianRed1" "IndianRed4" "grey")))
 '(pulse-delay 0.03)
 '(pulse-flag nil)
 '(pulse-iterations 5))

(push 'highlight-symbol el-get-sources)
(push '(:name highlight-parentheses
              :after (lambda ()
                       (add-hook 'c-mode-common-hook 'highlight-parentheses-mode)
                       (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
                       (add-hook 'ruby-mode-hook 'highlight-parentheses-mode))
              ) el-get-sources)

(push 'autopair el-get-sources)
(setq autopair-blink nil)

;;}}}

;;{{{ Mark

(push '(:name
        hide-comnt
        :type emacswiki) el-get-sources)

(push '(:name
        thingatpt+
        :type emacswiki) el-get-sources)

(push '(:name
        thing-cmds
        :type emacswiki) el-get-sources)

;;}}}

(provide 'iy-editor)
