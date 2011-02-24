(push '(:name eproject
              :type git
              :url "git://github.com/doitian/eproject.git"
              :features eproject
              :after iy-el-get-after-eproject)
      el-get-sources)

(defun iy-el-get-after-eproject ()
  (defadvice eproject--buffer-file-name (after guess-directory activate)
    (when (and (boundp 'org-src-mode) org-src-mode
               (boundp 'org-edit-src-beg-marker) org-edit-src-beg-marker)
      (setq ad-return-value (buffer-file-name (marker-buffer org-edit-src-beg-marker))))
    (unless ad-return-value
      (setq ad-return-value
            (cond ((memq major-mode '(magit-mode term-mode)) (expand-file-name default-directory))
                  (t nil)))))

  (define-project-type xcode (generic)
    (look-for "*.xcodeproj" :glob)
    :local-variables (lambda (root)
                       (lambda (root file)
                         (list 'compile-command (concat "cd " root "; xcodebuild -activeconfiguration"))))
    :cc-header-mode :objc
    :irrelevant-files ("^[.]" "^[#]" ".git/"))

  (add-hook 'org-src-mode-hook 'eproject-maybe-turn-on)
  (add-hook 'magit-mode-hook 'eproject-maybe-turn-on)
  (add-hook 'term-mode-hook 'eproject-maybe-turn-on)

  (require 'eproject-extras))

(provide 'iy-eproject)