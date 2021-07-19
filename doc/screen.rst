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

copy mode ..or
--------------

**scrollback** mode

allows cursor motion in terminal buffer


* `[` `C-[` and `<esc>` : enter copy mode
* `q` and `<esc><esc>` (Esc key double-press) : exit from copy mode, returning to terminal prompt

is about the same as `tmux`.

* `h` `j` `k` `l` : movement `vim`-style.
  `0``$``C-u``C-d``b``w`... also

_???_ no numerical prefixes on `vim`-alikes


**yank**/**copy** _todo_



Input Method
------------

allow screen to send input (other than pass-through)
to the terminal

* `C-v` : enter digraph (minibuffer prompt)

a digraph is a character indexed by a pair of characters

**todo** find out where the digraph table is stored, how to modify

_eg_ -- `o"` at the prompt results in `ö`
