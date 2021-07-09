Vim -- `vi`-mprov-`ed`
======================

"Mode-based" defn. _todo_

basic modes
* "cmd"
* "insert"

advanced modes
* "visual"
* "visual block"

> `esc`*+ exits any (_todo: anyany?_) mode
and lands in "cmd" mode
(*) `esc`

`:help` óR Fishing Lessons
--------------------------

First and foremost^


move cursor on page
* 'C-d', 'C-u' page up/down
* 'w', 'b' forward/backward word

around pages
* 'C-]' follow link
  - link text is a different color
    FOT: "**Use a terminal with colors!**"
    `urxvt`, `st`, `xterm`, etc.
  - move cursor on top of link to follow


^
----

_todo_ `:<tutorial>` command 
   (**still exists?**) << signs point to "yes"
   
----


           _unsure_how_to_

        make footnotes (in .rst)
                                                                                          history backward
                                                                                            the tutorial, again



edit one file
-------------

`vim file`

* move 'i', 'j', 'k', 'l'
* begin text entry 'i'nsert
* write changes `:w`
* undo `:u` 

comment a bunch of lines
i. '0', presuming ye be wantin
      th comment char(s) @ Beginnin O'Lyen
ii. 'C-v' enter ("Visual Block")[] (mode)[]
iii. 'j'okers 'n 'k'lowns up 'n down, the`*` lines
iv. **hurried?** -- 'C-u'p and 'C-d'own
     side-to-side ('h', 'l', ...) stuff
     is meaningless, (_afaict_?), and
     the **first column offset** of any
     selected line will be effected,
     not anywhere else.
v.  optional numeric (or any other "insert"
    mode prefix _arethere_?)
vi.  'I', make sure it's capitalized, `I`
vii. type comment char(s)
     (e.g. `#` for python or `//` C) 
     **and nuthin else**
     you'll only see one line (start
     or end line) change.. can confuse.
viii. `esc`-ape outta there 
      (e.g. press "Esc" and **blink**)
ix. One or more contiguous (i.e. "han-
    -ging together as a <adjective> w-
    -hole," to quote W.B.'s luv missi-
    -ves -- or, we mite sey "adjacnt")
    lines now begin with a line-comme-
    -nt character or few.
     


`*`

----
[!image(ZZ)](mg/zig-zag.png)
> and also that C-like lang
  w/ a similar-sounding name.

----


edit many files
---------------

`vim files`


many windows on screen at oncet
* `C-w C-w` change window
* `C-w =` equalize all window sizes
* `C-w _` maximize current window


set buffer in current window
* `:buffers :ls`
* `:buffer <num-idx>`



Questions
---------
* multi-lingual Input Methods?
* spaces instead of tabs?
  in shell scripts, specifically.
  ...obligatory
  [srs-seasoned dude (spaces) <> neo-esque dude (tabs)]
  (!omg/tabs_spaces.jpg)

and how to include images in Sphinx rst, obv



