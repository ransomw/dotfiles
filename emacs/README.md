# emacs config

this directory contains two emacs configurations.

The legacy configuration consists of `sys-config.el`, `defuns.el`, and `keys.el`.  It is meant to be used with a `~/.emacs` files.  See the documentation in `doc/emacs.rst` for the legacy configuration.

The new configuration consists of `init.el` and `lisp/`.  To install it,

```shell
ln -s /.../emacs/init.el ~/emacs.d/init.el
ln -s /.../emacs/lisp ~/.emacs.d/lisp
```

and ensure that `~/.emacs` doesn't exist.

### todo

there are some old ideas in in `investigate.el`

- [ ] document all functions in `lisp/my-defuns.el`
