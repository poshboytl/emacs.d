; Buffer Window staff

(defun iy-ediff-before-setup-winring-jump ()
  (iy-winring-jump-or-create "*ediff*"))
(defun iy-ediff-after-setup-save-register ()
  (set-register ?e (list (current-window-configuration) (point-marker))))
(defun iy-ediff-quit-winring-delete ()
  (when (string= (winring-name-of-current) "*ediff*")
    (let ((prev (ring-remove (winring-get-ring) 0)))
      (winring-restore-configuration prev))))

(push 'switch-window el-get-sources)
(push '(:name winring
              :type bzr
              :url "lp:winring"
              :features winring
              :after (lambda () 
                       (setq winring-keymap-prefix (kbd "M-s w"))
                       (defun winring-create-frame-hook (frame)
                         (winring-set-name "default" frame))
                       (define-key winring-map (kbd "w") 'iy-winring-jump-or-create)
                       (define-key winring-map (kbd "n") 'winring-next-configuration)
                       (define-key winring-map (kbd "C-n") 'winring-prev-configuration)
                       (define-key winring-map (kbd "C-p") 'winring-prev-configuration)
                       (winring-initialize)
                       (add-hook 'ediff-before-setup-hook 'iy-ediff-before-setup-winring-jump)
                       (add-hook 'ediff-after-setup-windows-hook 'iy-ediff-after-setup-save-register
                                 'append)
                       (add-hook 'ediff-quit-hook 'iy-ediff-quit-winring-delete)))
      el-get-sources)

(defun iy-winring-jump-or-create (&optional name)
  "Jump to or create configuration by name"
  (interactive)
  (let* ((ring (winring-get-ring))
         (n (1- (ring-length ring)))
         (current (winring-name-of-current))
         (lst (list (cons current -1)))
         index item)
    (while (<= 0 n)
      (push (cons (winring-name-of (ring-ref ring n)) n) lst)
      (setq n (1- n)))
    (setq name
          (or name
              (ido-completing-read
               (format "Window configuration name (%s): " current)
               (mapcar 'car lst) nil 'confirm nil 'winring-name-history current)))
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
     ((eq (+ fr-width 2) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -1))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 1))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -1))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 1))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -1))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 1))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -1))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 1))))

(global-set-key [S-up] 'win-resize-enlarge-horiz)
(global-set-key [S-down] 'win-resize-minimize-horiz)
(global-set-key [S-left] 'win-resize-enlarge-vert)
(global-set-key [S-right] 'win-resize-minimize-vert)

(windmove-default-keybindings 'control)

(provide 'iy-win-buffer)
