zellij
======

`zellij` is a a "terminal workspace" and is similar to `tmux` and `screen`.  `zellij` is newer than these other two.  the ui in `zellij` includes reminders for most keybindings.

this config unbinds `M-f`, which is used to create floating windows, and allows the terminal workspace to pass the keystroke on to the shell to move the cursor forward by a word.

install
-------
```
mv ~/.config/zellij/config.kdl ~/.config/zellij/config.kdl.bk
ln -s .../dotfiles/zellij_config.kdl ~/.config/zellij/config.kdl
```
