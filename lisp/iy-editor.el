;;{{{ General

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(custom-set-variables
 '(tab-width 2)
 '(indent-tabs-mode nil)
 '(fill-column 78)
 '(kill-ring-max 500)
 '(kill-whole-line t)

 '(delete-active-region 'kill))

(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      time-stamp-start "[Uu]pdated\\(_at\\)?[ \t]*:?[ \t]+<"
      time-stamp-end ">")

;;}}}

;;{{{ Folding

(defvar fringe-face 'fringe)
(defvar mode-line-inactive-face 'mode-line-inactive)
(custom-set-variables
 '(folding-font-lock-begin-mark 'fringe-face)
 '(folding-font-lock-end-mark 'fringe-face))

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
                   (font-lock-add-keywords nil (folding-font-lock-keywords major-mode)))))
      el-get-sources)

(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(push '(:name
        fold-dwim
        :type http
        :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el"
        :features fold-dwim
        :after (lambda ()
                 (folding-mode-add-find-file-hook)))
      el-get-sources)

(defun iy-folding-check-folded ()
  "Function to determine if this file is in folded form."
  (let ((folding-re1 "^.?.?.?{{{")
        (folding-re2 "[\r\n].*}}}"))
    (save-excursion
      (goto-char (point-min))
      ;;  If we found both, we assume file is folded
      (and (< (point-max) 10000)
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
            (if (folding-marker-p)
                (progn
                  (end-of-line)
                  (setq ad-return-value nil))
              (setq loop-times (1- loop-times)))))))))

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
           (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo "Hiddent text. M-s <SPC> to show")
      (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'mode-line-inactive display-string)
      (overlay-put ov 'display display-string)
      )))
(setq hs-set-up-overlay 'display-code-line-counts)

(provide 'hideshow-fringe)

;; \\r -> \\n  remove fringe
(defadvice folding-subst-regions (around toggle-fringe (list find replace) activate)
  ad-do-it
  (save-excursion
    (while list
      (let* ((begin (car list))
             (end (cadr list))
             (marker-string "*fringe-dummy*")
             (marker-length (length marker-string)))
        (when (and (eq find ?\n) (eq replace ?\r))
          (goto-char begin)
          (search-forward "\r")
          (forward-char -1)
          (let* ((ov (make-overlay begin (point)))
                 (display-string (format "%s (%d)" (buffer-substring begin (point)) (count-lines begin end))))
            (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'fringe-face) marker-string)
            (overlay-put ov 'before-string marker-string)
            (put-text-property 0 (length display-string) 'face 'mode-line-inactive display-string)
            (overlay-put ov 'display display-string)
            (overlay-put ov 'fringe-folding-p t)))
        (when (and (eq find ?\r) (eq replace ?\n))
          ;; \\r -> \\n  remove fringe
          (dolist (ov (overlays-in begin end))
            (when (overlay-get ov 'fringe-folding-p)
              (delete-overlay ov)))))
      (setq list (cdr (cdr list))))))

;;}}}

(provide 'iy-editor)
