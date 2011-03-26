;;; iy-org --- config for Orgmode

(require 'iy-dep)
(require 'iy-daemon)
(require 'iy-keymap)

(push 'org-mode el-get-sources)

;; enable org-mac-protocol for Mac OS X
(when (eq system-type 'darwin)
  (push
   '(:name org-mac-protocol
           :type git
           :url "git://github.com/claviclaws/org-mac-protocol.git"
           :compile "org-mac-protocol.el"
           :build '("mkdir -p ~/Library/Scripts/"
                    "mkdir -p ~/Library/Scripts/orgQSLib"
                    "mv *.scpt ~/Library/Scripts/"
                    "mv orgQSLib/*.scpt orgQSLib/escape.rb ~/Library/Scripts/orgQSLib")
           :features org-mac-protocol)
   el-get-sources))

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
        org-jsinfo
        org-man
        org-protocol
        org-w3m
        org-gnus
        org-clock
        ))

;;; Customization
(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        "----------------"
        (900 1100 1300 1400 1600 1800 2000 2200 2400)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "GOING(g)" "PAUSE(p)" "WAITING(w@)" "LATER(l)"
                  "|" "DONE(d!/@)" "SOMEDAY(s)" "CANCELED(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("GOING" :foreground "green" :weight bold)
        ("PAUSE" :foreground "yellow" :weight bold)))

;; example:
;;   (:startgroup . nil)
;;   ("@" . ?w) ("@home" . ?h)
;;   ("@tennisclub" . ?t)
;;   (:endgroup . nil)
(setq org-tag-alist '((:startgroup . nil)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      ("@downtown" . ?d)
                      ("@gym" . ?g)
                      ("@reading" . ?r)
                      ("@computer" . ?c)
                      ("@call" . ?l)
                      ("@mail" . ?m)
                      (:endgroup . nil)
                      ("project" . ?p)
                      ("server" . ?s)
                      ("blog" . ?b)
                      ("event" . ?e)
                      ("intridea" . ?i)
                      ("family" . ?f)
                      ("note" . ?n)
                      ("next" . ?x)
                      (:startgroup . nil)
                      ("plan" . ?1)
                      ("research" . ?2)
                      ("design" . ?3)
                      ("implement" . ?4)
                      ("test" . ?5)
                      ("review" . ?6)
                      ("experiment" . ?7)
                      ("documenting" . ?8)
                      (:endgroup . nil)))

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
      '(("STYLE_ALL" . "habit")
        ("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 6:00 8:00 12:00 16:00 24:00")))

(setq org-tags-exclude-from-inheritance '("project"))
(setq org-columns-default-format "%42ITEM %TAGS %TODO{T} %5Effort(Time){:} %6CLOCKSUM{Total} %SCHEDULED")
(setq org-read-date-prefer-future 'time)
(setq org-completion-use-ido t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)
                           (nil :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-clock-history-length 35)
(setq org-clock-in-resume t)
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
(setq org-agenda-files (list (concat iy-dropbox-dir "g/org") (concat iy-dropbox-dir "g/org/projects")))
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-mobile-directory (concat iy-dropbox-dir "MobileOrg"))
(setq org-mobile-inbox-for-pull (concat iy-dropbox-dir "g/org/from_mobile.org"))
(setq org-ditaa-jar-path iy-ditaa-path)

;;; Startup
(eval-after-load "remember"
  '(progn
     (org-remember-insinuate)
     (org-clock-persistence-insinuate)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-insert-link-global)
(global-set-key (kbd "C-c o") 'org-open-at-point-global)
(global-set-key (kbd "C-c a") 'org-agenda)

(define-key iy-map (kbd "<return>") 'org-clock-goto)
(define-key iy-map (kbd "<DEL>") 'org-clock-out)
(define-key iy-map (kbd "r") 'org-capture)
(define-key iy-map (kbd "M-r") 'org-capture)
(autoload 'org-in-regexp "org" "for footnote")
(define-key iy-map (kbd "t") 'org-footnote-action)

(add-hook 'org-mode-hook 'iy-org-mode-init)
(defun iy-org-mode-init ()
  (define-key org-mode-map (kbd "C-'") 'set-mark-command)
  (define-key org-mode-map (kbd "C-x C-'") 'pop-global-mark)
  (define-key org-mode-map (kbd "C-M-'") 'mark-sexp)
  (define-key org-mode-map (kbd "M-'") 'mark-word)
  (define-key org-mode-map (kbd "C-c ,") 'org-cycle-agenda-files)
  (define-key org-agenda-mode-map "M" 'org-agenda-month-view)
  (flyspell-mode 1)
  (when (fboundp 'filladapt-mode)
    (filladapt-mode)
    (setq filladapt-token-table iy-default-filladapt-token-table))
  (when (fboundp 'yas/fix-keybindings) (yas/fix-keybindings)))

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
  (message last-state)
  (when (and (string= state "GOING")
             (not (string= last-state state)))
    (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook
          'sacha-org-clock-in-if-starting)

(defun iy-org-clock-out-if-pause ()
  "Clock out when the task is marked PAUSE."
  (when (and (string= state "PAUSE")
             (not (string= last-state state)))
    (org-clock-out t)))
(add-hook 'org-after-todo-state-change-hook
          'iy-org-clock-out-if-pause)
(defun iy-clock-out-switch-to-pause-if-going (state)
  "Switch to PAUSE if clock out a task marked GOING"
  (if (string= state "GOING") "PAUSE" state))

(add-hook 'remember-mode-hook 'iy-start-clock-if-needed 'append)
(defun iy-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *CLOCK-IN *" nil t)
      (replace-match "")
      (org-clock-in))))


;;; Report
(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports."
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (start (time-to-seconds
                 (apply 'encode-time (org-parse-time-string ts))))
         (end (time-to-seconds
               (apply 'encode-time (org-parse-time-string te))))
         day-numbers)
    (setq params (plist-put params :tstart nil))
    (setq params (plist-put params :end nil))
    (while (<= start end)
      (save-excursion
        (insert "\n\n"
                (format-time-string (car org-time-stamp-formats)
                                    (seconds-to-time start))
                "----------------\n")
        (org-dblock-write:clocktable
         (plist-put
          (plist-put
           params
           :tstart
           (format-time-string (car org-time-stamp-formats)
                               (seconds-to-time start)))
          :tend
          (format-time-string (car org-time-stamp-formats)
                              (seconds-to-time end))))
        (setq start (+ 86400 start))))))

;;; Agenda Load
(defun sacha-org-agenda-load (&optional match)
  "Can be included in `org-agenda-custom-commands'."
  (let ((inhibit-read-only t)
        (time (sacha-org-calculate-free-time
               ;; today
               (calendar-gregorian-from-absolute org-starting-day)
               ;; now if today AND after starting time, else start of day
               (if (= org-starting-day
                      (time-to-days (current-time)))
                   (max
                    (let* ((now (decode-time))
                           (cur-hour (nth 2 now))
                           (cur-min (nth 1 now)))
                      (+ (* cur-hour 60) cur-min))
                    (let ((start (car (elt org-agenda-time-grid 2))))
                      (+ (* (/ start 100) 60) (% start 100))))
                 (let ((start (car (elt org-agenda-time-grid 2))))
                   (+ (* (/ start 100) 60) (% start 100))))
               ;; until the last time in my time grid
               (let ((last (car (last (elt org-agenda-time-grid 2)))))
                 (+ (* (/ last 100) 60) (% last 100))))))
    (goto-char (point-max))
    (insert (format
             "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap"
             (or (cdr time) (/ (car time) (* .01 (cdr time))))
             (car time)
             (cdr time)
             (- (cdr time) (car time))))))

;; I do not use appt, just copy function appt-convert-time here
(defun iy-convert-time (time2conv)
  "Convert hour:min[am/pm] format TIME2CONV to minutes from midnight.
A period (.) can be used instead of a colon (:) to separate the
hour and minute parts."
  ;; Formats that should be accepted:
  ;;   10:00 10.00 10h00 10h 10am 10:00am 10.00am
  (let ((min (if (string-match "[h:.]\\([0-9][0-9]\\)" time2conv)
                 (string-to-number (match-string 1 time2conv))
               0))
        (hr (if (string-match "[0-9]*[0-9]" time2conv)
                (string-to-number (match-string 0 time2conv))
              0)))
    ;; Convert the time appointment time into 24 hour time.
    (cond ((and (string-match "pm" time2conv) (< hr 12))
           (setq hr (+ 12 hr)))
          ((and (string-match "am" time2conv) (= hr 12))
           (setq hr 0)))
    ;; Convert the actual time into minutes.
    (+ (* hr 60) min)))

(defun iy-org-get-entries (files date)
  (let (entry entries)
    (dolist (file files)
      (catch 'nextfile
        (org-check-agenda-file file)
        (setq entry (org-agenda-get-day-entries
                     file date :scheduled :timestamp))
        (setq entries (append entry entries))))
    entries))

(defun sacha-org-calculate-free-time (date start-time end-of-day)
  "Return a cons cell of the form (TASK-TIME . FREE-TIME) for DATE, given START-TIME and END-OF-DAY.
DATE is a list of the form (MONTH DAY YEAR).
START-TIME and END-OF-DAY are the number of minutes past midnight."
  (save-window-excursion
    (let* ((entries (iy-org-get-entries (org-agenda-files) date))
           (total-unscheduled 0)
           (total-gap 0)
           (last-timestamp start-time)
           scheduled-entries)
      ;; For each item on the list
      (dolist (entry entries)
        (let ((time (get-text-property 1 'time entry))
              (effort (org-get-effort (get-text-property 1 'org-hd-marker entry))))
          (cond
           ((and time
                 (string-match "\\([^-]+\\)-\\([^-]+\\)" time))
            (push (cons
                   (save-match-data (iy-convert-time (match-string 1 time)))
                   (save-match-data (iy-convert-time (match-string 2 time))))
                  scheduled-entries))
           ((and time
                 (string-match "\\([^-]+\\)\\.+" time)
                 (not (eq effort nil)))
            (let ((start (save-match-data (iy-convert-time (match-string 1 time))))
                  (effort (save-match-data (iy-convert-time effort))))
              (push (cons start (+ start effort)) scheduled-entries)))
           ((not (eq effort nil))
            (setq total-unscheduled (+ (iy-convert-time effort)
                                       total-unscheduled))))))
      ;; Sort the scheduled entries by time
      (setq scheduled-entries (sort scheduled-entries (lambda (a b) (< (car a) (car b)))))
      (while scheduled-entries
        (let ((start (car (car scheduled-entries)))
              (end (cdr (car scheduled-entries))))
          (cond
           ;; are we in the middle of this timeslot?
           ((and (>= last-timestamp start)
                 (<= last-timestamp end))
            ;; move timestamp later, no change to time
            (setq last-timestamp end))
           ;; are we completely before this timeslot?
           ((< last-timestamp start)
            ;; add gap to total, skip to the end
            (setq total-gap (+ (- start last-timestamp) total-gap))
            (setq last-timestamp end)))
          (setq scheduled-entries (cdr scheduled-entries))))
      (if (< last-timestamp end-of-day)
          (setq total-gap (+ (- end-of-day last-timestamp) total-gap)))
      (cons total-unscheduled total-gap))))

;;; Template
(setq
 org-remember-templates
 '(
   ("Applescript Remember" ?y
    "* %:shortdesc\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %c\n  %i\n %?"
    "inbox.org" "Bookmarks")
   ("AppleScript note" ?z
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i"
    "inbox.org" "Notes")))

(setq
 org-capture-templates
 '(("r" "Notes" entry (file+headline "inbox.org" "Notes")
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i"
    :prepend t)
   ("i" "Ideas" entry (file+headline "inbox.org" "Ideas")
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i"
    :prepend t)
   ("t" "TODO" entry (file+headline "inbox.org" "Tasks")
    "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i")
   ("s" "SOMEDAY" entry (file+headline "inbox.org" "Someday")
    "* SOMEDAY %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i")
   ("j" "Journal" entry (file+datetree "journal.org")
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a\n  %i")
   ("x" "Clipboard" entry (file+headline "inbox.org" "Clipboard")
    "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %x"
    :prepend t :empty-lines 1)
   ("b" "Default template" entry (file+headline "inbox.org" "Bookmarks")
    "* %:description\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %c\n  %i"
    :prepend t :empty-lines 1 :immediate-finish t)
   ("w" "Default template" entry (file+headline "inbox.org" "Bookmarks")
    "* %:description\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %c\n  %i"
    :prepend t :empty-lines 1 :immediate-finish t)))

;;; Custom Agenda
(setq org-agenda-custom-commands
      '(("l" . "Context List")
        ("lt" "TODO List"
         ((todo "GOING|PAUSE")
          (todo "TODO")
          (todo "WAITING|LATER"))
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))
        ("lh" "Home Lists"
         ((tags-todo "@home/TODO|GOING|PAUSE")
          (tags-todo "@reading/TODO|GOING|PAUSE")
          (tags-todo "@computer/TODO|GOING|PAUSE")
          (tags-todo "@mail/TODO|GOING|PAUSE")
          (tags-todo "@call/TODO|GOING|PAUSE"))
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))
        ("lo" "Office Lists"
         ((tags-todo "@office/TODO|GOING|PAUSE")
          (tags-todo "@reading/TODO|GOING|PAUSE")
          (tags-todo "@computer/TODO|GOING|PAUSE")
          (tags-todo "@mail/TODO|GOING|PAUSE")
          (tags-todo "@call/TODO|GOING|PAUSE"))
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))
        ("ld" "Downtown Lists"
         ((tags-todo "@downtown/TODO|GOING|PAUSE")
          (tags-todo "@mail/TODO|GOING|PAUSE")
          (tags-todo "@call/TODO|GOING|PAUSE"))
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))
        ("lm" "Maybe"
         ((tags-todo "WAITING")
          (tags-todo "LATER")
          (tags "inbox-CONTAINER=\"true\""))
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)))

        ("d" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)))
          (sacha-org-agenda-load)
          (todo "GOING|PAUSE")
          (tags "project/-DONE-CANCELED")
          (todo "TODO")
          (todo "WAITING|LATER")
          (tags "inbox-CONTAINER=\"true\"")))

        ("p" "Projects" ((tags "project/-DONE-CANCELED")))

        ("r" "Weekly Review"
         ((todo)
          (todo "WARNING")
          (todo "SOMEDAY")
          (tags "project/-DONE-CANCELED")
          (stuck "")
          (tags "inbox")))

        ("q" . "Custom queries")
        ("qa" "Archive search" search ""
         ((org-agenda-files (file-expand-wildcards (concat org-directory "/*.org_archive" )))))
        ("qA" "Archive tags search" org-tags-view "" 
         ((org-agenda-files (file-expand-wildcards (concat org-directory "/*.org_archive" )))))

        ("i" "Inbox" tags "inbox")))

;;; Appt
(appt-activate 1)
;; (ignore-errors (org-agenda-to-appt))
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
;; our little façade-function for djcb-popup

(defun iy/appt-display (min-to-app new-time msg)
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

(defun iy/org-clock-display (msg)
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

(setq appt-disp-window-function (function iy/appt-display))
(setq org-show-notification-handler (function iy/org-clock-display))

;; TODO: org-toodledo
(autoload 'org-toodledo-initialize-org "org-toodledo"
  "toodledo" t)

(provide 'iy-org)
