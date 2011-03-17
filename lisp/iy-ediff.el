(defun ediff-main ()
  "Show ediff registry"
  (interactive)
  (let ((ediff-window-display-p (lambda () nil)))
    (ediff-show-registry)))

(provide 'iy-ediff)