# dotfiles

### timeline

This all started (_2017_) as typical dotfiles, a place to store configuation files with a few shell scripts included.

In addition to the usual collection of configuration files, startup scripts, etc.,
this repo is became (_2021_) a personal Python monorepo.

In the winter of this year (_2025_), these files are being revisited, at least for their original purpose of configuration.

### doc

Install [Invoke](https://www.pyinvoke.org/) and run `invoke build-docs` in the project root.  Install necessary libraries using `pip` until `invoke build-docs` succeeds (at time of writing, _Feb 1, 2026_, the pip packages are `fabric` and `sphinx`).  Then the documentation is available at `$root/doc/_build/index.html`, where `$root` is the project root directory.

### install config

There is also an `invoke` task to install some of these dotfiles: `invoke install-conf-files`.  It currently (_Feb 1, 2026_) installs configurations for `zellij`, `fish`, and `emacs`.

### python monorepo

The Python monorepo started in 2021 consists of

* `pyutils` - a package of random scripts and utilities.
* `pythonrc.py` - a startup script for the interpreter.  see the docstring in `pythonrc.py` for notes on how to install.
* `python.conf` is a configuration file used to keep track of what pypi package is associated with with python module.

Functionality in `pyutils` is exposed through the configuration in `pythonrc.py`.  As of _Feb 1, 2026_, there is no documentation other than the source.

