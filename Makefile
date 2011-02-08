EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

BATCH=$(EMACS) -batch -q -no-site-file -eval\
  "(setq load-path (cons (expand-file-name \"./lisp/\") (cons (expand-file-name \"./lisp/3rdparty/\") (cons (expand-file-name \"./el-get/el-get/\") load-path))))"

ELC= $(BATCH) -f batch-byte-compile

ELFILES=$(wildcard lisp/*.el) $(wildcard lisp/3rdparty/*.el)
ELCFILES=$(ELFILES:%.el=%.elc)

.SUFFIXES: .el .elc

compile: $(ELCFILES)

.el.elc:
	$(ELC) $<

info:
	echo $(ELCFILES)

clean:
	rm -rf $(ELCFILES)