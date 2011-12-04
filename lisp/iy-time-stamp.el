;;; iy-time-stamp.el --- Time stamp auto update

(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      time-stamp-start "[Uu]pdated\\(_at\\)?[ \t]*:?[ \t]+<"
      time-stamp-end ">")

(provide 'iy-time-stamp)