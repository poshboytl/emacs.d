(defadvice yas/load-directory-1 (around compile (directory mode-sym parents &optional no-compiled-snippets) activate)
  (setq mode-sym (car major-mode-and-parents))
  (setq parents (cdr major-mode-and-parents))
  ad-do-it)

(defun batch-bundle-snippets ()
  "Bundle snippets"
  (dolist (top-level-dir command-line-args-left)
    (with-temp-file (concat top-level-dir "/.yas-bundled-snippets.el")
     (dolist (dir (yas/subdirs (expand-file-name top-level-dir)))
       (let ((major-mode-and-parents (yas/compute-major-mode-and-parents
                                      (concat dir "/dummy"))))
         (yas/compile-snippets dir)
         (insert-file-contents-literally (concat dir "/.yas-compiled-snippets.el")))))))
