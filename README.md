Known problems
--------------

1. Remove `el-get/rinari/util/jump/inflections.elc`
2. Remove system orgmode:

       $ rm -rf /usr/share/emacs/23.3/lisp/org


3. Remove system ruby-mode:

       $ rm /usr/share/emacs/23.3/lisp/progmodes/ruby-mode.el


4. Manually compile auctex in Mac. `textmf` have to be manually specified

5. If a package failed to install, `el-get-remove` it. Or manually remove it
   from `el-get/.status.el` and remove the directory in `el-get`.

6. el-get is installed when Emacs starts the first time. Quit and start again to
   install all packages.
