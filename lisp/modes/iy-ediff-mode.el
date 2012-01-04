(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)))

(defun ediff-main ()
  "Show ediff registry"
  (interactive)
  (let ((ediff-window-display-p (lambda () nil)))
    (ediff-show-registry)))

(provide 'iy-ediff-mode)
