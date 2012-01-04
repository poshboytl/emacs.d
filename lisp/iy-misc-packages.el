(push 'fringe-helper el-get-sources)

(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(custom-set-variables
 '(iy-go-to-char-key-backward 58))

(require 'iy-go-to-char)

(provide 'iy-misc-packages)
