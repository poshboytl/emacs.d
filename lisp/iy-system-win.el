;;; iy-system-win.el --- config for Windows

(when (eq system-type 'windows-nt)
  (setq w32shell-msys-bin "D:/Ruby187/bin")
  (w32-register-hot-key [M-tab]))

(provide 'iy-system-win)