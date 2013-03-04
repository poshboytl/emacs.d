(setq auto-insert-alist
      '(
        (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
         (upcase (concat (file-name-nondirectory
                          (file-name-sans-extension buffer-file-name))
                         "_"
                         (file-name-extension buffer-file-name)))
         "#ifndef " str \n
         "#define " str "\n\n"
         _ "\n\n#endif")
        (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
         (upcase (concat (file-name-nondirectory
                          (file-name-sans-extension buffer-file-name))
                         "_"
                         (file-name-extension buffer-file-name)))
         "#ifndef " str \n
         "#define " str "\n\n"
         _ "\n\n#endif")

        (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
         nil
         "#include \""
         (let ((stem (file-name-sans-extension buffer-file-name)))
           (cond ((file-exists-p (concat stem ".h"))
                  (file-name-nondirectory (concat stem ".h")))
                 ((file-exists-p (concat stem ".hh"))
                  (file-name-nondirectory (concat stem ".hh")))))
         & ?\" | -10)

        (("[Mm]akefile\\'" . "Makefile") . "makefile.inc")

        (html-mode . (lambda () (sgml-tag "html")))

        (plain-tex-mode . "tex-insert.tex")
        (bibtex-mode . "tex-insert.tex")
        (latex-mode
         ;; should try to offer completing read for these
         "options, RET: "
         "\\documentclass[" str & ?\] | -1
         ?{ (read-string "class: ") "}\n"
         ("package, %s: "
          "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
         _ "\n\\begin{document}\n" _
         "\n\\end{document}")

        (("/bin/.*[^/]\\'" . "Shell-Script mode magic number") .
         (lambda ()
           (if (eq major-mode (default-value 'major-mode))
               (sh-mode))))

        (ada-mode . ada-header)

        (("\\.[1-9]\\'" . "Man page skeleton")
         "Short description: "
         ".\\\" Copyright (C), " (substring (current-time-string) -4) "  "
         (getenv "ORGANIZATION") | (progn user-full-name)
         "
.\\\" You may distribute this file under the terms of the GNU Free
.\\\" Documentation License.
.TH " (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
" " (file-name-extension (buffer-file-name))
" " (format-time-string "%Y-%m-%d ")
"\n.SH NAME\n"
(file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
" \\- " str
"\n.SH SYNOPSIS
.B " (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
"\n"
_
"
.SH DESCRIPTION
.SH OPTIONS
.SH FILES
.SH \"SEE ALSO\"
.SH BUGS
.SH AUTHOR
" (user-full-name)
'(if (search-backward "&" (line-beginning-position) t)
     (replace-match (capitalize (user-login-name)) t t))
'(end-of-line 1) " <" (progn user-mail-address) ">\n")

(("\\.el\\'" . "Emacs Lisp header")
 "Short description: "
 ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "

;; Copyright (C) " (substring (current-time-string) -4) "  "
                                                        (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
             '(if (search-backward "&" (line-beginning-position) t)
                  (replace-match (capitalize (user-login-name)) t t))
             '(end-of-line 1) " <" (progn user-mail-address) ">
;; Keywords: "
             '(require 'finder)
             ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
             '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
                               finder-known-keywords)
                    v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
                                  finder-known-keywords
                                  "\n"))
             ((let ((minibuffer-help-form v2))
                (completing-read "Keyword, C-h: " v1 nil t))
              str ", ") & -2 "

\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

\;;; Commentary:

\;; " _ "

\;;; Code:



\(provide '"
(file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
")
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")
(("\\.texi\\(nfo\\)?\\'" . "Texinfo file skeleton")
 "Title: "
 "\\input texinfo   @c -*-texinfo-*-
@c %**start of header
@setfilename "
 (file-name-sans-extension
  (file-name-nondirectory (buffer-file-name))) ".info\n"
  "@settitle " str "
@c %**end of header
@copying\n"
  (setq short-description (read-string "Short description: "))
  ".\n\n"
  "Copyright @copyright{} " (substring (current-time-string) -4) "  "
  (getenv "ORGANIZATION") | (progn user-full-name) "

@quotation
Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
A copy of the license is included in the section entitled ``GNU
Free Documentation License''.

A copy of the license is also available from the Free Software
Foundation Web site at @url{http://www.gnu.org/licenses/fdl.html}.

@end quotation

The document was typeset with
@uref{http://www.texinfo.org/, GNU Texinfo}.

@end copying

@titlepage
@title " str "
@subtitle " short-description "
@author " (getenv "ORGANIZATION") | (progn user-full-name)
" <" (progn user-mail-address) ">
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@c Output the table of the contents at the beginning.
@contents

@ifnottex
@node Top
@top " str "

@insertcopying
@end ifnottex

@c Generate the nodes for this menu with `C-c C-u C-m'.
@menu
@end menu

@c Update all node entries with `C-c C-u C-n'.
@c Insert new nodes with `C-c C-c n'.
@node Chapter One
@chapter Chapter One

" _ "

@node Copying This Manual
@appendix Copying This Manual

@menu
* GNU Free Documentation License::  License for copying this manual.
@end menu

@c Get fdl.texi from http://www.gnu.org/licenses/fdl.html
@include fdl.texi

@node Index
@unnumbered Index

@printindex cp

@bye

@c " (file-name-nondirectory (buffer-file-name)) " ends here\n")))

(provide 'iy-auto-insert)
