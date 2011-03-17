(push '(:name espressso
              :type http
              :url "http://download.savannah.gnu.org/releases-noredirect/espresso/espresso.el")
      el-get-sources)

(autoload 'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(defalias 'js-mode 'espresso-mode)
(defalias 'javascript-mode 'espresso-mode)

(provide 'iy-js)
