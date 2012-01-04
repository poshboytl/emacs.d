OSTYPE=$(shell echo $$OSTYPE)
ifeq (,$(findstring darwin,$(OSTYPE)))
  EMACS=emacs
else
  EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
endif

BATCH=$(EMACS) -batch -q -no-site-file -eval\
  "(setq load-path (cons (expand-file-name \"./lisp/\") (cons (expand-file-name \"./lisp/modes/\") (cons (expand-file-name \"./lisp/3rdparty/\") (cons (expand-file-name \"./el-get/el-get/\") load-path)))))"\
  -eval "(setq max-specpdl-size 2000)" \
  -eval "(setq max-lisp-eval-depth 1000)"

ELC= $(BATCH) -f batch-byte-compile

ELFILES=$(wildcard lisp/*.el) $(wildcard lisp/3rdparty/*.el) $(wildcard lisp/modes/*.el)
ELCFILES=$(ELFILES:%.el=%.elc)

.SUFFIXES: .el .elc

compile: $(ELCFILES)

.el.elc:
	$(ELC) $<

info:
	echo $(ELCFILES)

clean:
	rm -rf $(ELCFILES)
