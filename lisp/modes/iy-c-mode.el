(defconst my-c-style
  '((c-basic-offset             . 2)
    (c-comment-only-line-offset . (0 . 0))
    (c-ignore-auto-fill         . nil)
    (c-tab-always-indent        . t)
    (c-hanging-braces-alist     . ((defun-open after)
                                   (defun-close before)
                                   (class-open after)
                                   (class-close before)
                                   (block-open after)
                                   ;; (block-close . c-snug-do-while)
                                   (block-close before)
                                   (topmost-intro)
                                   (brace-list-open)
                                   (brace-list-close)
                                   (do-while-closure after)
                                   (substatement-open after)
                                   (else-clause after)
                                   (access-label after)
                                   (catch-clause  after)
                                   (inline-open after)
                                   (namespace-open)))
    (c-hanging-colons-alist     . ((inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)
                                   (member-init-intro before)))
    (c-cleanup-list             . (scope-operator
                                   defun-close-semi
                                   list-close-comma
                                   comment-close-slash))
    (c-offsets-alist            . ((block-open . 0)
                                   (block-close . 0)
                                   (statement-block-intro . +)
                                   (substatement . +)
                                   (substatement-open . 0)
                                   (substatement-label . 0)
                                   (label . 0)
                                   (statement-cont c-lineup-string-cont c-lineup-assignments +)
                                   ;; (template-args-cont iy-c-lineup-template-args-cont c-lineup-template-args +)
                                   (case-label . 0)
                                   (do-while-closure . 0)
                                   (else-clause . 0)
                                   (catch-clause . 0)
                                   (case-label . 0)
                                   (access-label . -)
                                   (arglist-close . c-lineup-close-paren)
                                   (namespace-open . 0)
                                   (class-open . 0)
                                   (class-close . 0)
                                   (innamespace . 0)
                                   (inline-open . 0)
                                   (member-init-intro . 0)
                                   (inher-intro . 0)
                                   )))
  "My C/C++ Programming Style")

;; Customizations for all modes in CC Mode.
(defun iy-c-mode-common-init ()
  (c-add-style "cust" my-c-style t)
  (setq tab-width 2
        indent-tabs-mode nil)
  (dtrt-indent-mode t)
  (c-toggle-auto-hungry-state 1))

(defun iy-c-mode-init ()
  (c-set-style "cust")
  (local-set-key (kbd "C-M-a") 'c-beginning-of-defun)
  (local-set-key (kbd "C-M-e") 'c-end-of-defun)
  (hs-minor-mode t)
  (if (fboundp 'c-subword-mode)
      (c-subword-mode t)
    (subword-mode t))
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill)
  (flyspell-prog-mode)
  (when (fboundp 'doxymacs-mode)
    (doxymacs-mode))
  (autopair-mode)
  (setq autopair-handle-action-fns
        '(
          autopair-default-handle-action
          iy-autopair-open-braces))
  (add-hook 'pre-command-hook 'wcy-cancel-auto-new-line nil t))

(defun iy-java-mode-init ()
  (c-set-style "cust")
  (setq c-basic-offset 2)
  (hs-minor-mode t)
  (if (fboundp 'c-subword-mode)
      (c-subword-mode t)
    (subword-mode t))
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (turn-on-auto-fill)
  (flyspell-prog-mode)
  (autopair-mode)
  (setq autopair-handle-action-fns
        '(
          autopair-default-handle-action
          iy-autopair-open-braces))
  (add-hook 'pre-command-hook 'wcy-cancel-auto-new-line nil t))

;; see http://ann77.stu.cdut.edu.cn/EmacsAutoNewLineImpv.html
;; auto-newline refinement
(defvar wcy-cancel-auto-new-line-command-list
  '(next-line previous-line)
  "a list of command which will trigger the cancel.")

(defun wcy-cancel-auto-new-line ()
  (interactive)
  (save-excursion
    (if (and (eq last-command 'c-electric-semi&comma)
             (memq this-command wcy-cancel-auto-new-line-command-list))
        (progn
          (if (and (boundp c-auto-newline) c-auto-newline)
              (progn
                (delete-blank-lines)))))))

(defun iy-autopair-open-braces (action pair pos-before)
  (when (and (eq 'opening action)
             (eq ?\} pair)
             (looking-back "^[ 	]*"))
    (save-excursion
      (newline)
      (indent-according-to-mode))))

(defun iy-c++-mode-init ()
  (iy-c-mode-init))
(defun iy-objc-mode-init ()
  (iy-c-mode-init))

(add-hook 'c-mode-common-hook 'iy-c-mode-common-init)
(add-hook 'c-mode-hook 'iy-c-mode-init)
(add-hook 'c++-mode-hook 'iy-c++-mode-init)
(add-hook 'objc-mode-hook 'iy-objc-mode-init)
(add-hook 'java-mode-hook 'iy-java-mode-init)

(push 'cmake-mode el-get-packages)

(provide 'iy-c-mode)
