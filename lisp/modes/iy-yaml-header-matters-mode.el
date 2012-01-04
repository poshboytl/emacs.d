(defconst iy-yaml-header-matters-mode-keywords
  '(("\\`---\n\\(.\\|\n\\)*?\n---\n" 0 font-lock-comment-face t)))

(font-lock-add-keywords 'markdown-mode iy-yaml-header-matters-mode-keywords)
(font-lock-add-keywords 'haml-mode iy-yaml-header-matters-mode-keywords)
(font-lock-add-keywords 'erb-mode iy-yaml-header-matters-mode-keywords)

(provide 'iy-yaml-header-matters-mode)
