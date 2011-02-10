;;; Scroll

(defvar slow-scroll-mode 0)

(defun toggle-slow-scroll-mode (&optional arg)
  "Toggle slow scroll mode at each call of this function.
If slow-scroll-mode is 1, the cursor stays on current line after each scroll; else not."
  (interactive "P")
  (if (= slow-scroll-mode 0)
      (setq slow-scroll-mode 1)
    (setq slow-scroll-mode 0)))

(defun scroll-up-slowly (&optional arg)
  "Scroll text of current window upward ARG lines; or one line if no ARG.
This depend of slow scroll mode (keeping cursor on current line or not).
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (progn (if arg
             (scroll-up arg)
           (scroll-up 1))
         (if (= slow-scroll-mode 1)
             (forward-line 1))))

(defun scroll-down-slowly (&optional arg)
  "Scroll text of current window downward ARG lines; or one line if no ARG.
This depend of slow scroll mode (keeping cursor on current line or not).
When calling from a program, supply a number as argument or nil."
  (interactive "P")
  (progn (if arg
             (scroll-down arg)
           (scroll-down 1))
         (if (= slow-scroll-mode 1)
             (forward-line -1))))

(set-variable 'slow-scroll-mode 1)

(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))

(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

(provide 'iy-mouse)