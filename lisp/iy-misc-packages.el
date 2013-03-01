(push 'fringe-helper el-get-packages)

(autoload 'zap-up-to-char "misc" "kill up to but not including char" t)

(custom-set-variables
 '(woman-fontify t)
 '(woman-use-topic-at-point-default t))

(custom-set-variables
 '(iy-go-to-char-key-backward 58))
(require 'iy-go-to-char)

(push 'ace-jump-mode el-get-packages)

(push 'tumbl el-get-packages)

(push 'cheat el-get-packages)

(unless (eq system-type 'darwin)
  (push 'haskell-mode el-get-packages)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))

(push 'erlware-mode el-get-packages)

(defun iy-diff-mode-init ()
  (local-set-key (kbd "M-o") 'other-window))
(add-hook 'diff-mode-hook 'iy-diff-mode-init)

(push 'sml-modeline el-get-packages)
(defun iy-el-get-after-sml-modeline ()
  (sml-modeline-mode))

(push 'pos-tip el-get-packages)

(push 'multiple-cursors el-get-packages)
(defadvice set-rectangular-region-anchor (around edit-lines-when-region-is-active activate)
  (if (region-active-p)
      (call-interactively 'mc/edit-lines)
    ad-do-it))

(push 'undo-tree el-get-packages)
(defun iy-el-get-after-undo-tree ()
  (global-undo-tree-mode)
  (define-key undo-tree-map (kbd "C-x r u") nil)
  (define-key undo-tree-map (kbd "C-x r U") nil)
  (define-key undo-tree-map (kbd "C-x r") nil))

(push 'deft el-get-packages)
(defun org-drill-deft ()
  (interactive)
  (with-current-buffer (find-file-noselect (concat iy-dropbox-dir "g/cards/inbox.org"))
    (org-drill 'directory)))

(defun iy-el-get-after-deft ()
  (setq
   deft-extension "org"
   deft-directory (concat iy-dropbox-dir "g/cards")
   deft-use-filename-as-title t
   deft-text-mode 'org-mode)

  (define-key deft-mode-map (kbd "C-c SPC") 'org-drill-deft))

(provide 'iy-misc-packages)
