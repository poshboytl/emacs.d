(push 'full-ack el-get-sources)
(push '(:name etags-select :type elpa) el-get-sources)

(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

(provide 'iy-code-browser)