(require 'iy-dep)

;;{{{ Cleanup Buffers

(custom-set-variables
 '(clean-buffer-list-delay-special 3600)
 '(clean-buffer-list-kill-buffer-names (quote ("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "bbdb" "*RE-Builder*" "*Shell Command Output*" "*ESS*" "*WoMan-Log*" "*magit-process*" "*Dired log*" "*anything*" "*CEDET Global*" "*Pp Eval Output*" "*Completions*")))
 '(clean-buffer-list-kill-regexps (quote ("\\`\\*Customize Group:" "\\`\\*Man " "\\`\\*magit" "\\`\\*RNC Input")))
 '(midnight-mode t nil (midnight))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-strip-common-suffix nil))

;;}}}

;;{{{ winring

(custom-set-variables
 '(winring-show-names t))

;;; winring for ediff
(defun iy-ediff-before-setup-winring-jump ()
  (iy-winring-jump-or-create "*ediff*"))
(defun iy-ediff-after-setup-save-register ()
  (set-register ?e (list (current-window-configuration) (point-marker))))
(defun iy-ediff-quit-winring-delete ()
  (when (string= (winring-name-of-current) "*ediff*")
    (let ((prev (ring-remove (winring-get-ring) 0)))
      (winring-restore-configuration prev))))

;;; winring for w3m
(defadvice w3m-close-window (before iy-w3m-change-winring activate)
  (when (string= (winring-name-of-current) "*w3m*")
    (let ((prev (ring-remove (winring-get-ring) 0)))
      (winring-restore-configuration prev))))
(defadvice w3m-quit (before iy-w3m-change-winring activate)
  (when (string= (winring-name-of-current) "*w3m*")
    (let ((prev (ring-remove (winring-get-ring) 0)))
      (winring-restore-configuration prev))))

(defun wicked-toggle-w3m ()
  "Switch to a w3m buffer or return to the previous buffer.

Changed to use winring
"
  (interactive)
  (if (derived-mode-p 'w3m-mode)
      (w3m-close-window)
    ;; Not in w3m
    ;; Find the first w3m buffer
    (iy-winring-jump-or-create "*w3m*")
    (let ((list (window-list)))
      (while list
        (if (with-current-buffer (window-buffer (car list))
              (derived-mode-p 'w3m-mode))
            (progn
              (select-window (car list))
              (setq list nil))
          (setq list (cdr list)))))
    (unless (derived-mode-p 'w3m-mode)
      (let ((list (buffer-list)))
        (while list
          (if (with-current-buffer (car list)
                (derived-mode-p 'w3m-mode))
              (progn
                (switch-to-buffer (car list))
                (setq list nil))
            (setq list (cdr list))))
        (unless (derived-mode-p 'w3m-mode)
          (call-interactively 'w3m))))))

(global-set-key (kbd "<f8>") 'wicked-toggle-w3m)
(define-key iy-map (kbd "M-g") 'wicked-toggle-w3m)

;; winring for magit
(defun iy-magit-status ()
  (interactive)
  "Start magit in winring configuration"
  (let ((buffer (current-buffer)))
    (iy-winring-jump-or-create "*magit*")
    (with-current-buffer buffer
        (call-interactively 'magit-status))
    (delete-other-windows)))

(defadvice magit-quit-window (after iy-kill-magit-winring activate)
  (when (string= (winring-name-of-current) "*magit*")
    (let ((prev (ring-remove (winring-get-ring) 0)))
      (winring-restore-configuration prev))))

(push 'switch-window el-get-packages)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-window)

(push 'winring el-get-packages)

(defun iy-el-get-after-winring ()
  (setq winring-keymap-prefix (kbd "M-s w"))
  (winner-mode 1)
  (define-key winring-map (kbd ",") 'winner-undo)
  (define-key winring-map (kbd ".") 'winner-redo)

  (defun winring-create-frame-hook (frame)
    (winring-set-name "W" frame))
  (define-key winring-map (kbd "w") 'iy-winring-jump-or-create)
  (define-key winring-map (kbd "n") 'winring-next-configuration)
  (define-key winring-map (kbd "C-n") 'winring-prev-configuration)
  (define-key winring-map (kbd "C-p") 'winring-prev-configuration)
  (winring-initialize)
  (add-hook 'ediff-before-setup-hook 'iy-ediff-before-setup-winring-jump)
  (add-hook 'ediff-after-setup-windows-hook 'iy-ediff-after-setup-save-register
            'append)
  (add-hook 'ediff-quit-hook 'iy-ediff-quit-winring-delete))

(defun iy-winring-jump-or-create (&optional name)
  "Jump to or create configuration by name"
  (interactive)
  (let* ((ring (winring-get-ring))
         (n (1- (ring-length ring)))
         (current (winring-name-of-current))
         (lst (list (cons current -1)))
         (def (if (>= n 0) (winring-name-of (ring-ref ring 0)) current))
         index item)
    (while (<= 0 n)
      (push (cons (winring-name-of (ring-ref ring n)) n) lst)
      (setq n (1- n)))
    (setq name
          (or name
              (ido-completing-read
               (format "Window configuration name (%s): " current)
               (mapcar 'car lst) nil 'confirm nil 'winring-name-history def)))
    (setq index (cdr (assoc name lst)))
    (if (eq nil index)
        (progn
          (winring-save-current-configuration)
          (delete-other-windows)
          (switch-to-buffer winring-new-config-buffer-name)
          (winring-set-name name))
      (when (<= 0 index)
        (setq item (ring-remove ring index))
        (winring-save-current-configuration)
        (winring-restore-configuration item)))))

;;}}}

;;{{{ win resize
(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
   (this-window-y-min (nth 1 win-edges))
   (this-window-y-max (nth 3 win-edges))
   (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
   (this-window-x-min (nth 0 win-edges))
   (this-window-x-max (nth 2 win-edges))
   (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((<= fr-width this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz (arg)
  (interactive "p")
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window (- arg)))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window arg))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window (- arg)))
   (t (message "nil"))))

(defun win-resize-minimize-horiz (arg)
  (interactive "p")
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window arg))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window (- arg)))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window arg))
   (t (message "nil"))))

(defun win-resize-enlarge-vert (arg)
  (interactive "p")
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally (- arg)))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally arg))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally (- arg)))))

(defun win-resize-minimize-vert (arg)
  (interactive "p")
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally arg))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally (- arg)))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally (arg)))))

(global-set-key [S-up] 'win-resize-enlarge-horiz)
(global-set-key [S-down] 'win-resize-minimize-horiz)
(global-set-key [S-left] 'win-resize-enlarge-vert)
(global-set-key [S-right] 'win-resize-minimize-vert)
;;}}}

;;{{{ buffer-move
(push 'buffer-move el-get-packages)
(global-set-key [C-up] 'buf-move-up)
(global-set-key [C-down] 'buf-move-down)
(global-set-key [C-left] 'buf-move-left)
(global-set-key [C-right] 'buf-move-right)
;;}}}

;;{{{ windmove

(windmove-default-keybindings 'meta)

(defun iy-wind-move-resize (arg)
  "wind move and resze"
  (interactive "p")
  (let ((done nil)
        (ev last-command-event)
        (echo-keystrokes nil))
    (while (not done)
      (condition-case e
          (cond ((or (eq ev ?h) (eq ev ?\M-h)) (windmove-left))
              ((or (eq ev ?j) (eq ev ?\M-j)) (windmove-down))
              ((or (eq ev ?k) (eq ev ?\M-k)) (windmove-up))
              ((or (eq ev ?l) (eq ev ?\M-l)) (windmove-right))
              ((or (eq ev 'backspace) (eq ev ?\C-h)) (buf-move-left))
              ((eq ev ?\C-j) (buf-move-down))
              ((eq ev ?\C-k) (buf-move-up))
              ((eq ev ?\C-l) (buf-move-right))
              ((eq ev ?H) (win-resize-enlarge-vert arg))
              ((eq ev ?J) (win-resize-minimize-horiz arg))
              ((eq ev ?K) (win-resize-enlarge-horiz arg))
              ((eq ev ?L) (win-resize-minimize-vert arg))
              (t (setq done t)))
        (error (message (apply 'concat (cdr e)))))
        (when (not done)
          (setq ev (read-event))))
    (push ev unread-command-events)))

(define-key iy-map "h" 'iy-wind-move-resize)
(define-key iy-map "j" 'iy-wind-move-resize)
(define-key iy-map "k" 'iy-wind-move-resize)
(define-key iy-map "l" 'iy-wind-move-resize)
(define-key iy-map (kbd "M-h") 'iy-wind-move-resize)
(define-key iy-map (kbd "M-j") 'iy-wind-move-resize)
(define-key iy-map (kbd "M-k") 'iy-wind-move-resize)
(define-key iy-map (kbd "M-l") 'iy-wind-move-resize)
(define-key iy-map (kbd "C-h") 'iy-wind-move-resize)
;; translated
(define-key iy-map (kbd "<backspace>") 'iy-wind-move-resize)
(define-key iy-map (kbd "C-j") 'iy-wind-move-resize)
(define-key iy-map (kbd "C-k") 'iy-wind-move-resize)
(define-key iy-map (kbd "C-l") 'iy-wind-move-resize)
(define-key iy-map "H" 'iy-wind-move-resize)
(define-key iy-map "J" 'iy-wind-move-resize)
(define-key iy-map "K" 'iy-wind-move-resize)
(define-key iy-map "L" 'iy-wind-move-resize)

;;}}}

(provide 'iy-win-buffer)
