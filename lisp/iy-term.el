(push 'multi-term el-get-sources)

(setq
 term-bind-key-alist
 '(
   ("C-c C-c" . term-interrupt-subjob)
   ("M-p" . term-send-up)
   ("M-n" . term-send-down)
   ("M-r" . term-send-reverse-search-history)))

(setq
 term-unbind-key-list
 '("C-x" "C-g" "M-w" "C-h" "M-s" "M-o" "M-x" "M-X"))


(provide 'iy-term)