Dependencies
------------

-   I'm using Emacs 24, may fail on 23.
-   `iy-tex-mode` depends on TeX distribution.
-   `iy-w3m` depends on `w3m` browser.

Or add modules to `iy-blacklist` to skip them.

Install
-------

1.  Initialize submodules

        git submodule init
        git submodule update

2.  To filter loaded files, create `custom.readonly.el`, and set `iy-blacklist`, e.g.,
disable tex mode:

        (custom-set-variables
          '(iy-blacklist (list 'iy-tex-mode)))

    Mac user can copy the file `custom.readonly.sample.mac.el` as template.

3.  Secret settings can be added in `secrets.el`, e.g.

        echo '(setq tumble-password "xx")' >> secrets.el

4.  Start emacs

5.  Restart If a package failed to install. Or eval:

        (el-get 'sync (reverse el-get-packages))

6.  Remove this compiled file:

        rm el-get/emacs-rails/inflections.elc

7.  Run <key>M-x customize-group iy-config</key> to see customizable options.

8.  `C-h` is mapped to delete char backward. Find following line in `lisp/iy-keybindings.el` to disable it:
        
        (define-key key-translation-map [?\C-h] [?\C-?])

9.  `C-r` is mapped to the default `C-x C-r` keymap, use `C-r C-r` to isearch backward, or delete following line in `lisp/iy-keybindings.el` to disable it:

        (global-set-key (kbd "C-r") ctl-x-r-map)
