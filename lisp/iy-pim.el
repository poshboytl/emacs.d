(require 'iy-dep)
(require 'iy-daemon)

(push 'deft el-get-packages)
(defun iy-el-get-after-deft ()
  (setq
   deft-extension "md"
   deft-directory (concat iy-dropbox-dir "g/notes")
   deft-use-filename-as-title t
   deft-text-mode 'markdown-mode))

(push 'org-mode el-get-packages)

;;; Modules
(setq org-modules
      '(
        org-bibtex
        org-bookmark
        org-expiry
        org-habit
        org-id
        org-info
        org-inlinetask
        org-man
        org-w3m
        org-clock
        org-timer
        ))

;;; Customization
(setq org-clock-persist-file
      (concat iy-data-dir "org-clock-save.el"))
(setq org-id-locations-file
      (concat iy-data-dir "org-id-locations.el"))

(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        "----------------"
        (930 1000 1200 1400 1600 1800 2000 2200 2400 2500)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "GOING(g)" "PAUSE(p)" "WAITING(w@)" "LATER(l)"
                  "|" "DONE(d!/@)" "SOMEDAY(s)" "CANCELED(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "coral3" :weight bold)
        ("GOING" :foreground "green" :weight bold)
        ("PAUSE" :foreground "yellow" :weight bold)))
(setq org-priority-faces
      '((?A :foreground "red" :weight bold)
        (?B :foreground "#94bff3" :weight bold)
        (?C :foreground "#6f6f6f")))

;; example:
;;   (:startgroup . nil)
;;   ("@" . ?w) ("@home" . ?h)
;;   ("@tennisclub" . ?t)
;;   (:endgroup . nil)
(setq org-tag-alist '((:startgroup . nil)
                      ("@home" . ?h)
                      ("@errands" . ?e)
                      ("@computer" . ?c)
                      ("@reading" . ?r)
                      ("@phone" . ?p)
                      ("@message" . ?m)
                      (:endgroup . nil)
                      ("project" . ?x)
                      ("event" . ?v)
                      ("idea" . ?i)
                      ("next" . ?n)))

(setq org-todo-state-tags-triggers
      '(("WAITING" ("next"))
        ("LATER" ("next"))
        ("DONE" ("next"))
        ("SOMEDAY" ("next"))
        ("CANCELED" ("next"))
        ("GOING" ("next" . t))))

(setq org-stuck-projects
      '("project/-DONE-CANCELED"
        ("GOING") ("next") ""))

(setq org-global-properties
      '(("STYLE_ALL" . "habit")))

(setq org-tags-exclude-from-inheritance '("project"))
(setq org-columns-default-format
      "%42ITEM %TODO %3Effort(E){:} %3CLOCKSUM_T(R) %SCHEDULED")
(setq org-read-date-prefer-future 'time)
(setq org-completion-use-ido t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)
                           (nil :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-clock-history-length 35)
(setq org-clock-in-resume nil)
(setq org-clock-in-switch-to-state "GOING")
(setq org-clock-out-switch-to-state
      (function iy-clock-out-switch-to-pause-if-going))
(setq org-clock-idle-time 30)
(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))
(setq org-clock-into-drawer "CLOCK")
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist (quote history))
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-span 7)
(setq org-tags-column -80)
(setq org-agenda-tags-column -80)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-cycle-separator-lines 2)
(setq org-agenda-todo-list-sublevels t)
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings nil)
(setq org-log-into-drawer t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees nil)
(setq org-use-fast-todo-selection t)
(setq org-directory (concat iy-dropbox-dir "g/org"))
(setq org-mobile-directory (concat iy-dropbox-dir "MobileOrg"))
(setq org-mobile-inbox-for-pull (concat iy-dropbox-dir "g/org/mobile-inbox.org"))

(setq org-agenda-files (list (concat iy-dropbox-dir "g/org") (concat iy-dropbox-dir "g/org/projects")))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-mobile-directory (concat iy-dropbox-dir "MobileOrg"))
(setq org-mobile-inbox-for-pull (concat iy-dropbox-dir "g/org/from_mobile.org"))
(setq org-ditaa-jar-path iy-ditaa-path)
(setq org-extend-today-until 2)

(setq org-file-apps
      '((t . emacs)
        (system . "xopen %s")))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)
(global-set-key (kbd "C-c a") 'org-agenda)

(define-key iy-map (kbd "<return>") 'org-clock-goto)
(define-key iy-map (kbd "<DEL>") 'org-clock-out)
(define-key iy-map (kbd "r") 'org-capture)
(define-key iy-map (kbd "M-r") 'org-capture)
(autoload 'org-footnote-action "org-footnote" nil t)
(define-key iy-map (kbd "t") 'org-footnote-action)
(define-key iy-map (kbd "'") 'org-pomodoro-record-interuptions)

(add-hook 'org-mode-hook 'iy-org-mode-init)
(defun iy-org-mode-init ()
  (define-key org-mode-map (kbd "C-c ,") 'org-cycle-agenda-files)
  (define-key org-agenda-mode-map "#" 'org-agenda-3-days-view)
  (define-key org-agenda-mode-map "M" 'org-agenda-month-view)
  (flyspell-mode 1)
  (org-pomodoro-on-org-load))

(defun wl-org-column-view-uses-fixed-width-face ()
  ;; copy from org-faces.el
  (when (fboundp 'set-face-attribute)
    ;; Make sure that a fixed-width face is used when we have a column table.
    (set-face-attribute 'org-column nil
                        :height (face-attribute 'default :height)
                        :family (face-attribute 'default :family))))

(when iy-daemon-enable-daemon
  (add-hook 'org-mode-hook 'wl-org-column-view-uses-fixed-width-face))

;;; Clock
(defun sacha-org-clock-in-if-starting ()
  "Clock in when the task is marked GOING."
  (when (and (string= org-state "GOING")
             (not (string= org-last-state org-state))
             (not org-clock-current-task))
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook
          'sacha-org-clock-in-if-starting)

(defun iy-org-clock-out-if-pause ()
  "Clock out when the task is marked PAUSE."
  (when (and (string= org-state "PAUSE")
             (not (string= org-last-state org-state))
             (org-clock-is-active))
    (org-clock-out t)))
(add-hook 'org-after-todo-state-change-hook
          'iy-org-clock-out-if-pause)
(defun iy-clock-out-switch-to-pause-if-going (state)
  "Switch to PAUSE if clock out a task marked GOING"
  (if (string= state "GOING") "PAUSE" state))

;;; Template
(setq
 org-capture-templates
 '(("r" "Notes" entry (file+headline (concat org-directory "/inbox.org") "Notes")
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i"
    :prepend t)
   ("t" "TODO" entry (file+headline (concat org-directory "/inbox.org") "Tasks")
    "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i")
   ("j" "Journal" plain (file+datetree (concat org-directory "/journal.org"))
    "\n%?\n" :empty-lines 1)
   ("s" "SOMEDAY" entry (file+headline (concat org-directory "/inbox.org") "Someday")
    "* SOMEDAY %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i")
   ("x" "Clipboard" entry (file+headline (concat org-directory "/inbox.org") "Notes")
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %x"
    :prepend t :empty-lines 1)
   ("i" "Idea" entry (file (concat org-directory "/spark.org") "")
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i")

   ("c" "Code snippet" entry (file (concat iy-dropbox-dir "g/snippets/inbox.org"))
    "* %^{title} %^g\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n\n#+BEGIN_SRC %^{lang}\n  %i%?\n#+END_SRC\n")))

;;; Custom Agenda
(setq org-agenda-custom-commands
      '(("l" . "Context List")
        ("lh" "Home"
         ((tags-todo "@home/GOING|PAUSE|TODO")))
        ("le" "Errands"
         ((tags-todo "@errands/GOING|PAUSE|TODO")))
        ("lc" "Computer"
         ((tags-todo "@computer/GOING|PAUSE|TODO")))
        ("lp" "Phone"
         ((tags-todo "@phone/GOING|PAUSE|TODO")))
        ("lm" "Message"
         ((tags-todo "@message/GOING|PAUSE|TODO")))
        ("lr" "Reading"
         ((tags-todo "@reading/GOING|PAUSE|TODO")))
        ("T" "TODO List"
         ((todo "GOING|PAUSE|TODO"))
         ((org-agenda-todo-ignore-with-date nil)))
        ("M" "Maybe"
         ((todo "WAITING|LATER"))
         ((org-agenda-todo-ignore-with-date nil)))
        ("i" "Inbox" tags "inbox-CONTAINER=\"true\"")

        ("d" "Daily Action List"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)))))

        ("r" "Review"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      (org-agenda-skip-deadline-if-done nil)
                      (org-agenda-skip-timestamp-if-done nil)
                      (org-agenda-skip-scheduled-if-done nil)))))

        ("p" "Projects" ((tags "project/-DONE-CANCELED") (stuck "")))

        ("x" "Archive tags search" tags "" 
         ((org-agenda-files (file-expand-wildcards (concat org-directory "/*.org_archive" )))))
        ("X" "Archive search" search ""
         ((org-agenda-files (file-expand-wildcards (concat org-directory "/*.org_archive" )))))

        ("c" "Code snippets tags search" tags ""
         ((org-agenda-files (append (file-expand-wildcards (concat iy-dropbox-dir "g/snippets/*.org" ))
                                    (file-expand-wildcards (concat iy-dropbox-dir "g/snippets/*/*.org"))))))
        ("C" "Code snippets search" search ""
         ((org-agenda-files (append (file-expand-wildcards (concat iy-dropbox-dir "g/snippets/*.org" ))
                                    (file-expand-wildcards (concat iy-dropbox-dir "g/snippets/*/*.org"))))))
        ))

;;; Appt
(appt-activate 1)
;; (ignore-errors (org-agenda-to-appt))
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(defun iy-appt-display (min-to-app new-time msg)
  (let ((title (format "Appointment in %d minute(s)" min-to-app)))
    (message (concat title ": " msg))
    (if (fboundp 'dbus-call-method)
        (dbus-call-method
         :session "org.freedesktop.Notifications"
         "/org/freedesktop/Notifications"
         "org.freedesktop.Notifications" "Notify"
         "Emacs Appt"
         0
         "appointment-soon"
         (format "Appointment in %d minute(s)" min-to-app)
         msg
         '(:array)
         '(:array :signature "{sv}")
         ':int32 -1)
      (el-get-notify title msg))))

(defun iy-org-clock-display (msg)
  (message "Org Notification: %s" msg)
  (if (fboundp 'dbus-call-method)
      (dbus-call-method
       :session "org.freedesktop.Notifications"
       "/org/freedesktop/Notifications"
       "org.freedesktop.Notifications" "Notify"
       "Emacs Org"
       0
       "appointment-missed"
       "Org Notification"
       msg
       '(:array)
       '(:array :signature "{sv}")
       ':int32 -1)
    (el-get-notify "Org Notification" msg)))

(setq appt-disp-window-function (function iy-appt-display))
(setq org-show-notification-handler (function iy-org-clock-display))

;; pomodoro

(defvar org-pomodoro-minutes 25)
(defvar org-pomodoro-cancelling nil)
(defvar org-pomodoro-process nil)
(defvar org-pomodoro-command (locate-file "pomodoro" exec-path))

(defun org-pomodoro-on-org-load ()
  (push (cons "p" org-pomodoro-minutes) org-effort-durations)
  (push '("Effort_ALL" . "1p 2p 3p 4p 5p 6p 7p 8p") org-global-properties)
  (define-key org-mode-map "\C-c\C-x'" 'org-pomodoro-columns)
  (define-key org-agenda-mode-map "\C-c\C-x'" 'org-pomodoro-agenda-columns))

(defadvice org-minutes-to-hh:mm-string (around org-pomodoro-minutes-to-pomodoros activate)
  (setq ad-return-value (format "%dp" (round (/ m (float org-pomodoro-minutes))))))
(defadvice org-columns-number-to-string (around org-pomodoro-minutes-to-pomodoros activate)
  (if (memq fmt '(add_times max_times min_times mean_times))
      (setq ad-return-value (format "%dp" (round (/ (* n 60) org-pomodoro-minutes))))
    ad-do-it))

(defun org-pomodoro-after-clock-in ()
  (when (not org-timer-current-timer)
    (org-timer-set-timer org-pomodoro-minutes)
    (when (and org-pomodoro-process
               (eq 'run (process-status org-pomodoro-process)))
      (interrupt-process org-pomodoro-process))
    (when org-pomodoro-command
      (setq org-pomodoro-process
            (let ((process-connection-type nil))
              (start-process "pomodoro" "*pomodoro*" "pomodoro" "-l" (number-to-string org-pomodoro-minutes)))))))

(defun org-pomodoro-after-clock-out ()
  (org-pomodoro-stop-process)
  (unless org-pomodoro-cancelling
    (org-pomodoro-cancel-timer-safe)))

(defun org-pomodoro-stop-process ()
  (when (and org-pomodoro-process
             (eq 'run (process-status org-pomodoro-process)))
    (interrupt-process org-pomodoro-process)
    (setq org-pomodoro-process nil)))

(defun org-pomodoro-is-indivisible! ()
  (let ((org-clock-out-remove-zero-time-clocks nil)
        (org-pomodoro-cancelling t)
        (buffer (current-buffer))
        (point (point)))
    (when (org-clock-is-active)
      (setq buffer (marker-buffer org-clock-marker)
            point (marker-position org-clock-marker))
      (org-clock-out))
    (save-excursion
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (goto-char point)
          (beginning-of-line 1)
          (when (and (not remove)
                     (looking-at (concat "[ \t]*" org-keyword-time-regexp))
                     (equal (match-string 1) org-clock-string))
            (goto-char (match-end 0))
            (beginning-of-line 1)
            (kill-line 1)
            (message "Pomodoro is cancelled")))))))

(defun org-pomodoro-done ()
  (when (org-clock-is-active) (org-clock-out)))

(defun org-pomodoro-cancel-timer-safe ()
  (when org-timer-current-timer (org-timer-cancel-timer)))

(add-hook 'org-timer-pause-hook 'org-pomodoro-cancel-timer-safe)
(add-hook 'org-timer-continue-hook 'org-pomodoro-cancel-timer-safe)
(add-hook 'org-timer-stop-hook 'org-pomodoro-cancel-timer-safe)
(add-hook 'org-timer-cancel-hook 'org-pomodoro-is-indivisible!)
(add-hook 'org-timer-done-hook 'org-pomodoro-done)
(add-hook 'org-clock-in-hook 'org-pomodoro-after-clock-in)
(add-hook 'org-clock-out-hook 'org-pomodoro-after-clock-out)

(defvar org-pomodoro-columns-format
  "%22SCHEDULED %CATEGORY %42ITEM %3Effort(E){:} %3CLOCKSUM_T(R) %POMODORO_INTERRUPTIONS(I){+}")

(defun org-pomodoro-columns ()
  (interactive)
  (org-columns org-pomodoro-columns-format))

(defun org-pomodoro-agenda-columns ()
  (interactive)
  (let ((org-agenda-overriding-columns-format org-pomodoro-columns-format))
   (org-agenda-columns)))

(defun org-pomodoro-record-interuptions (char)
  (interactive (list
                (if (string=
                     "internal"
                     (completing-read "type: " '("internal" "external") nil t nil nil "internal"))
                    ?' ?-)))
  (if (and org-clock-current-task org-clock-marker)
      (save-excursion
        (with-current-buffer (marker-buffer org-clock-marker)
          (goto-char org-clock-marker)
          (let ((value (concat (sort (cons char (string-to-list (org-entry-get nil "POMODORO_INTERUPTIONS"))) '<))))
            (org-entry-put nil "POMODORO_INTERUPTIONS" value)
            (org-entry-put nil "POMODORO_INTERUPTIONS_COUNT" (number-to-string (length value)))
            (message "Interuptions: %s" value))))
    (error "no active pomodoro")))

(defun org-agenda-3-days-view (&optional day-of-year)
  "Switch to 3-days (yesterday, today, tomorrow) view for agenda."
  (interactive "P")
  (org-agenda-check-type t 'agenda)
  (if (and (not day-of-year) (equal org-agenda-current-span 3))
      (error "Viewing span is already \"%s\"" 3))
  (let* ((sd (or day-of-year 
                 (org-get-at-bol 'day)
                 (time-to-days (current-time))))
         (sd (and sd (1- sd)))
         (org-agenda-overriding-arguments
          (or org-agenda-overriding-arguments
              (list (car (get-text-property (point) 'org-last-args)) sd 3 t))))
    (org-agenda-redo)
    (org-agenda-find-same-or-today-or-agenda))
  (org-agenda-set-mode-name)
  (message "Switched to %s view" 3))

(defun org ()
  (interactive)
  (ido-find-file-in-dir org-directory))
(defun orgb ()
  (interactive)
  (ido-find-file-in-dir org-directory))
(defun snippet ()
  (interactive)
  (ido-find-file-in-dir (concat iy-dropbox-dir "g/snippets" )))

(custom-set-variables
 '(appt-display-format (quote window))
 '(appt-message-warning-time 10)
 '(calendar-week-start-day 1)
 '(diary-file (concat iy-dropbox-dir "diary")))

(provide 'iy-pim)
