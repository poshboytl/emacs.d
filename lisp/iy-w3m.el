;;; iy-w3m.el --- w3m

(push '(:name emacs-w3m
              :after iy-el-get-after-emacs-w3m)
      el-get-sources)

(setq browse-url-generic-program
      (if (eq system-type 'darwin)
          "open"
        (or (executable-find "x-www-browser")
            (executable-find "firefox-4.0")
            (executable-find "firefox")
            (executable-find "google-chrome"))))

(defun wicked-w3m-open-current-page-in-default-browser ()
  "Open the current URL in GUI."
  (interactive)
  (browse-url-generic w3m-current-url))

(defun wicked-w3m-open-link-or-image-in-default-browser ()
  "Open the current link or image in GUI."
  (interactive)
  (browse-url-generic (or (w3m-anchor)
                          (w3m-image))))

(defun wicked-toggle-w3m ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (if (derived-mode-p 'w3m-mode)
      ;; Currently in a w3m buffer
      ;; Bury buffers until you reach a non-w3m one
      (while (derived-mode-p 'w3m-mode)
        (bury-buffer))
    ;; Not in w3m
    ;; Find the first w3m buffer
    (let ((list (buffer-list)))
      (while list
        (if (with-current-buffer (car list)
              (derived-mode-p 'w3m-mode))
            (progn
              (switch-to-buffer (car list))
              (setq list nil))
          (setq list (cdr list))))
      (unless (derived-mode-p 'w3m-mode)
        (call-interactively 'w3m)))))

(setq wicked-quick-search-alist
      '(("^g?:? +\\(.*\\)" . ;; Google Web
         "http://www.google.com/search?q=\\1")

        ("^g!:? +\\(.*\\)" . ;; Google Lucky
         "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=\\1")

        ("^dict:? +\\(.*\\)" . ;; Dictionary
         "http://dictionary.reference.com/search?q=\\1")))

(defadvice w3m-goto-url (before wicked activate)
  "Use the quick searches defined in `wicked-quick-search-alist'."
  (let* ((my-url (replace-regexp-in-string
                  "^ *\\| *$" ""
                  (replace-regexp-in-string "[ \t\n]+" " " (ad-get-arg 0))))
         (match (assoc-if
                 (lambda (a) (string-match a my-url))
                 wicked-quick-search-alist)))
    (if match
        (ad-set-arg 0 (replace-regexp-in-string
                       (car match) (cdr match) my-url)))))

(defadvice browse-url (before wicked activate)
  "Use the quick searches defined in `wicked-quick-search-alist'."
  (let* ((my-url (replace-regexp-in-string
                  "^ *\\| *$" ""
                  (replace-regexp-in-string "[ \t\n]+" " " (ad-get-arg 0))))
         (match (assoc-if
                 (lambda (a) (string-match a my-url))
                 wicked-quick-search-alist)))
    (if match
        (ad-set-arg 0 (replace-regexp-in-string
                       (car match) (cdr match) my-url)))))

(defun iy-el-get-after-emacs-w3m ()
  (require 'w3m-lnum)
  (w3m-lnum-mode)
  (define-key w3m-mode-map "o" 'wicked-w3m-open-current-page-in-default-browser)
  (define-key w3m-mode-map "O" 'wicked-w3m-open-link-or-image-in-default-browser)
  (define-key w3m-mode-map "," 'w3m-previous-buffer)
  (define-key w3m-mode-map "." 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buffer)
  (define-key w3m-mode-map (kbd "C-c M-s") 'w3m-session-select)
  (define-key w3m-mode-map (kbd "M-s") 'iy-map))

(global-set-key (kbd "<f8>") 'wicked-toggle-w3m)

;; Webjump
(global-set-key (kbd "C-<f8>") 'webjump)
(global-set-key (kbd "<ESC> <f8>") 'webjump)

(eval-after-load "webjump"
  '(progn
     (setq
      webjump-sites
      (append
       '(
         ("STL" .
          [simple-query
           "http://www.sgi.com/tech/stl/index.html"
           "http://www.google.com/search?q="
           "+site%3Awww.sgi.com%2Ftech%2Fstl"])
         ("Boost" .
          [simple-query
           "http://www.boost.org/doc/libs/1_39_0/libs/libraries.htm"
           "http://www.google.com/search?domains=www.boost.org%3Blists.boost.org&hq=site%3Awww.boost.org+OR+site%3Alists.boost.org&q="
           ""
           ])
         ("CPP" .
          [simple-query
           "http://www.cplusplus.com/reference/"
           "http://www.cplusplus.com/query/search.cgi?q=" ""])
         )
       webjump-sample-sites))))

(provide 'iy-w3m)
