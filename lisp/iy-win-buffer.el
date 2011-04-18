; Buffer Window staff

(push 'switch-window el-get-sources)
(push '(:name winring
              :type bzr
              :url "lp:winring"
              :features winring
              :after (lambda () 
                       (setq winring-keymap-prefix (kbd "C-z"))
                       (define-key winring-map (kbd "C-z") 'iy-winring-jump-or-create)
                       (define-key winring-map (kbd "n") 'winring-next-configuration)
                       (define-key winring-map (kbd "C-n") 'winring-prev-configuration)
                       (define-key winring-map (kbd "C-p") 'winring-prev-configuration)
                       (winring-initialize)))
      el-get-sources)

(completing-read PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH
INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)

(defun iy-winring-jump-or-create (&optional name)
  "Jump to or create configuration by name"
  (interactive)
  (let* ((ring (winring-get-ring))
         (n (1- (ring-length ring)))
         (current (winring-name-of-current))
         lst index item)
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

;; EDiff windows
(define-key iy-map (kbd "e")
  (lambda ()
    (interactive)
    (let ((ediff-window-display-p (lambda () nil)))
      (ediff-show-registry))))

(defun iy-ediff-before-setup-winring-jump ()
  (iy-winring-jump-or-create "*ediff*"))
(defun iy-ediff-after-setup-save-register ()
  (set-register ?e (list (current-window-configuration) (point-marker))))
(defun iy-ediff-quit-winring-delete ()
  (when (string= (winring-name-of-current) "*ediff*")
    (let ((prev (ring-remove (winring-get-ring) 0)))
      (winring-restore-configuration prev))))

(add-hook 'ediff-before-setup-hook 'iy-ediff-before-setup-winring-jump)
(add-hook 'ediff-after-setup-windows-hook 'iy-ediff-after-setup-save-register
          'append)
(add-hook 'ediff-quit-hook 'iy-ediff-quit-winring-delete)

(provide 'iy-win-buffer)
