(require 'iy-dep)

(add-hook 'mail-mode-hook 'flyspell-mode)

(global-set-key (kbd "<menu>") 'mu4e)
(define-key iy-map (kbd "M") 'mu4e)

(autoload 'mu4e "mu4e" nil t)
(custom-set-variables
 '(mu4e-maildir (expand-file-name "~/Mail"))
 '(mu4e-drafts-folder "/drafts")
 '(mu4e-use-fancy-chars nil)
 '(mu4e-maildir-shortcuts
   '(("/INBOX" . ?i)))
 ;; (lambda (msg)
 ;;   (if (mu4e-message-contact-field-matches msg :to "ian@xxx")
 ;;      "/xxx/archive" "/archive"))
 '(mu4e-refile-folder "/archive")
 '(mu4e-view-show-images t)
 '(mu4e-view-image-max-width 800)
 '(mu4e-confirm-quit nil)
 '(mu4e-headers-date-format "%d/%b/%Y %H:%M")
 '(mu4e-html2text-command "w3m -T text/html")
 '(mu4e-msg2pdf "/usr/bin/msg2pdf")
 '(mu4e-org-contacts-file (concat iy-dropbox-dir "g/org/contacts.org")))

(setq mu4e-view-actions '(("capture message" . mu4e-action-capture-message)
                          ("view as pdf" . mu4e-action-view-as-pdf)
                          ("open in browser" . mu4e-action-view-in-browser)
                          ("save contact" . mu4e-action-add-org-contact)))

(defun iy-mu4e-main-mode-init ()
  (local-set-key (kbd "<menu>") 'mu4e-quit)
  (local-set-key (kbd "M") 'mu4e-quit))
(add-hook 'mu4e-main-mode-hook 'iy-mu4e-main-mode-init)

(setq message-kill-buffer-on-exit t)
(setq message-sendmail-envelope-from 'header)

(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "msmtp")
(setq message-sendmail-extra-arguments '("-a" "ianyme"))
(setq mail-host-address "iany.me")

;; http://zmalltalker.com/linux/mu.html
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(defadvice mu4e (around iy-mu4e-winring activate)
  (iy-winring-jump-or-create "*mu4e*")
  ad-do-it
  (delete-other-windows))

(defadvice mu4e-quit (after iy-mu4e-winring activate)
  (when (string= (winring-name-of-current) "*mu4e*")
    (let ((prev (ring-remove (winring-get-ring) 0)))
      (winring-restore-configuration prev))))

(provide 'iy-mail)
