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

2.  To filter loaded files, create `custom.el`, and set `iy-blacklist`, e.g.,
disable tex mode:

        (custom-set-variables
          '(iy-blacklist (list 'iy-tex-mode)))

3.  Secret settings can be added in `secrets.el`, e.g.

        echo '(setq tumble-password "xx")' >> secrets.el

4.  Start emacs

5.  Restart If a package failed to install. Or eval:

        (el-get 'sync (reverse el-get-packages))
