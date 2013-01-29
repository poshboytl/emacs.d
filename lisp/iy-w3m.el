;;; iy-w3m.el --- w3m

(require 'iy-dep)
(custom-set-variables
 '(w3m-coding-system (quote utf-8))
 '(w3m-default-coding-system (quote utf-8))
 '(w3m-enable-google-feeling-lucky t)
 '(w3m-file-coding-system (quote utf-8))
 '(w3m-file-name-coding-system (quote utf-8))
 '(w3m-redirect-with-get t)
 '(w3m-terminal-coding-system (quote utf-8))
 '(w3m-use-cookies t)
 '(w3m-enable-google-feeling-lucky nil)
 '(w3m-search-engine-alist '(("duckduckgo" "http://duckduckgo.com/?q=%s" utf-8)
                             ("google" "http://www.google.com/search?q=%s" utf-8)))
 '(w3m-search-default-engine "duckduckgo")
 '(w3m-uri-replace-alist
   '(("\\`\\([-._a-zA-Z0-9]\+\\.\\)\\(com\\|me\\|org\\|ly\\)\\($\\|/.*\\)"
      w3m-pattern-uri-replace
      "http://\\1\\2\\3")
     ("\\`g " w3m-search-uri-replace "google")
     ("\\`d " w3m-search-uri-replace "duckduckgo"))))

(push 'emacs-w3m el-get-packages)

(setq browse-url-generic-program
      (if (eq system-type 'darwin)
          "open"
        (or (executable-find "x-www-browser")
            (executable-find "chromium")
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
  (browse-url-generic (or (w3m-anchor (point))
                          (w3m-image (point)))))

;; (setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-browser-function
      '(("ticket\\.opower\\.com\\|bitbucket\\.org\\|docs.local/\\(?:jquery\\|rails\\|ruby\\)\\|gems.dev" . browse-url-generic)
        ("." . w3m-browse-url)))
(defun iy-el-get-after-emacs-w3m ()
  (require 'w3m-lnum))

(defun w3m-anchor-markdown (&optional pos)
  (setq pos (or pos (point)))
  (unless (or (w3m-image pos) (w3m-anchor-sequence pos))
    (setq pos (w3m-goto-next-anchor-or-image)))
  (if (w3m-image pos)
      (format "![%s](%s)"
              (or (w3m-image-alt pos)
                  (w3m-image pos))
              (w3m-image pos))
    (format "[%s](%s)"
            (or
             (w3m-anchor-title pos)
             (buffer-substring-no-properties
              (let ((pre (previous-single-property-change pos 'w3m-anchor-sequence)))
                (if (and pre (w3m-anchor-sequence pre) (= (w3m-anchor-sequence pre) (w3m-anchor-sequence pos)))
                    pre pos))
              (next-single-property-change pos 'w3m-anchor-sequence)))
            (w3m-anchor pos))))

(defun w3m-lnum-markdown-this-url ()
  "Display the url under point in the echo area and put markdown format of it into `kill-ring'.
If no url under point, activate numbering and select one."
  (interactive)
  (if (or (w3m-anchor) (w3m-image))
      (w3m-print-this-url t)
    (let ((link (w3m-lnum-get-action "Select URL to copy: " 1)))
      (if link
          (let* ((pos (cadr link))
                 (markdown (w3m-anchor-markdown pos)))
            (message "%s" ())
            (kill-new markdown)
            (w3m-message "%s%s" (let ((im-alt (nth 3 link)))
                                  (if (zerop (length im-alt)) ""
                                    (concat im-alt ": ")))
                         markdown))
        (w3m-message "No URL selected")))))

(defun iy-el-w3m-init ()
  (w3m-lnum-mode)
  (define-key w3m-mode-map ";" w3m-lnum-mode-map)
  (define-key w3m-mode-map "f" 'w3m-lnum-goto)
  (define-key w3m-lnum-mode-map "m" 'w3m-lnum-markdown-this-url)
  (define-key w3m-mode-map "m" 'w3m-lnum-markdown-this-url)
  (define-key w3m-mode-map "o" 'wicked-w3m-open-current-page-in-default-browser)
  (define-key w3m-mode-map "O" 'wicked-w3m-open-link-or-image-in-default-browser)
  (define-key w3m-mode-map "," 'w3m-previous-buffer)
  (define-key w3m-mode-map "." 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buffer)
  (define-key w3m-mode-map (kbd "C-c s") 'w3m-session-select)
  (define-key w3m-mode-map (kbd "C-<tab>") 'w3m-select-buffer)
  (define-key w3m-mode-map (kbd "M-s") iy-map))

(add-hook 'w3m-mode-hook 'iy-el-w3m-init)

;; Webjump
(global-set-key (kbd "C-<f8>") 'webjump)
(global-set-key (kbd "<ESC> <f8>") 'webjump)
(define-key iy-map (kbd "g") 'webjump)

(eval-after-load "webjump"
  '(progn
     (setq
      webjump-sites
      '(
        ("duckduckgo" .
         [simple-query
          "http://duckduckgo.com/"
          "http://duckduckgo.com/?q=" ""])
        ("stl" .
         [simple-query
          "http://www.sgi.com/tech/stl/index.html"
          "http://www.google.com/search?q="
          "+site%3Awww.sgi.com%2Ftech%2Fstl"])
        ("boost" .
         [simple-query
          "http://www.boost.org/doc/libs/1_39_0/libs/libraries.htm"
          "http://www.google.com/search?domains=www.boost.org%3Blists.boost.org&hq=site%3Awww.boost.org+OR+site%3Alists.boost.org&q="
          ""
          ])
        ("cpp" .
         [simple-query
          "http://www.cplusplus.com/reference/"
          "http://www.cplusplus.com/query/search.cgi?q=" ""])
        ("youdao" .
         [simple-query
          "http://dict.youdao.com/"
          "http://dict.youdao.com/search?q=" ""])
        ("gems" . "http://gems.dev")
        ("jquery" . "http://docs.local/jquery")
        ("rails" . "http://docs.local/rails")
        ("ruby" . "http://docs.local/ruby")
        ))))

(provide 'iy-w3m)
