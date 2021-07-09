`screen`
========

start
-----
`screen` (again, no args or config)

prefix key: `C-a`
-----------------

Same with `tmux`,
multiplexer
hot keys/keybindings
prefixed.

`[keypresses]`
in the following, then, denotes
`<prefix-key> [keypresses]`.


create and select current
-------------------------

* `c` [again, once, also `C-a c`] : Create terminal
* `[0-9]` : jump to numbered terminal

see final, display line listing terminals.
something like

```
[sysname][        0$ zsh  2-$ zsh  3$ zsh  (4*$ zsh)        ][07/08/21  4:35 AM]
```

so `C-a 0` - `4` are available

* `<space>` : next terminal in order

* `C-a` : previously selected terminal

motion in terminal buffer
-------------------------

**todo** `tmux` `C-b [`-like


