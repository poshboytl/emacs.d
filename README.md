Install
-------

1. Initialize submodules

       git submodule init
       git submodule update

2. To filter loaded files, create `custom.el`, and set iy-blacklist, e.g., disable tex mode:

       (custom-set-variables
         '(iy-blacklist (list 'iy-tex-mode)))

3. Secret settings can be added in `secrets.el`, e.g.

       echo '(setq tumble-password "4E&$&2U9TKfTQ&ob")' >> secrets.el

4. Start emacs

Known problems
--------------

1. Manually compile auctex in Mac. `textmf` have to be manually specified

2. If a package failed to install, `el-get-remove` it. Or manually remove it
   from `el-get/.status.el` and remove the directory in `el-get`. Then
   reinstall it using `el-get-install`
