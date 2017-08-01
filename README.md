# dotfiles
## or, how i learned to stop worrying and write documenation

so-called "dotfiles" are config files for various programs,
particularly, in this case, those running on a
commodity hardware (desktop or laptop).

on [POSIX](http://en.wikipedia.org/wiki/POSIX) Operating Systems,
which includes OS X, the modern
[Apple Computers](http://www.ebay.com/sch/i.html?_nkw=Apple%20II&_sacat=0)
laptop OS, "hidden" files are prefixed by a `.`,
so the hidden files used to configure programs are sometimes
called dotfiles.

the usual pattern with many of these files and directories
is to open a textual user interface
(Terminal, etc.)
```shell
cat <name> | less # or sometimes <name>rc
# read, edit, read, read, read, edit
cp -r <name> ~/.<name>
```
all of these files are intended to supplement their
accompanying programs' official documentation, either
from the web or via `info <name>` when
[GNU](http://savannah.gnu.org/) is present,
or `man <name>` otherwise.

## editors

when `emacs`, `vim`, and `vi` aren't around,
i like `nano` quite a lot:  no config files to get started,
and it says what it does right at the bottom of the screen:

> ^G Get Help    ^O Write Out   ^W Where Is    ^K Cut Text    ^J Justify     ^C Cur Pos     ^Y Prev Page   M-\ First Line
> ^X Exit        ^R Read File   ^\ Replace     ^U Uncut Text  ^T To Spell    ^_ Go To Line  ^V Next Page   M-/ Last Line

for instance, on an apple keyboard,
`^G` means, "hold the `control` key and press the `g` key."

if you have a **three button** mouse,
[acme](http://acme.cat-v.org/mouse)
is worth trying.  (thumbs up!)

while i often use `emacs`, the `emacs/` directory in this
repository is in a sort-of half-broken, half-upgraded state.

## email

### local storage

since i often want to view my email on my computer without an
internet connection, i typically run a program to backup messages
to local storage (my laptop's harddrive).

previously, i'd used
[getmail](http://pyropus.ca/software/getmail/)
but i'm currently trying out `mbsync`, and it appears
pretty great:  one of the linux kernel maintainers
has contributed to it, so it likely works very well
(in terms of "perf" or speed) with the linux filesystem.
using this program, i hope to think about my emails more
[simply](https://en.wikipedia.org/wiki/Directed_graph)
and
[beautifully](https://en.wikipedia.org/wiki/Algebraic_graph_theory),
because (and not to be
[snarky](https://en.wikipedia.org/wiki/Snark_(graph_theory))),
mail will always be there.

anyways, i setup a folder to store the mail in,
linked `mbsyncrc` in this repository to `~/.mbsyncrc`,
and initialized the backup

```shell
mkdir -p ~/mail/mbsync
ln -s /absolute/path/to/this/repo/mbsync ~/.mbsyncrc
mbsync init
```

the first run took a while, and now i can run
`mbsync get` to quickly backup my email.

### mutt

once emails are stored locally,
i use [(neo)?mutt](https://www.neomutt.org) to make sense of them.
on my system, the `muttrc` file in this directory is linked to
`~/.mutt/muttrc`.

##### deleting

i find the group feature helpful for deleting
emails that i don't want.
after opening `mutt` from the shell,
i type `l` (for "limit") and enter the
_limit string_ `%L nope` at the prompt
(`nope` is defined in `muttrc`).
after reviewing the list of messages to be deleted,
pressing `D` opens a prompt with the current limit string,
and by pressing return, one may confirm deletion of the messages.
when mutt is exited with `q`, i confirm the "purge deleted messages".

note that the `mbsync` configuration does not propegate these
deletions back to the server, but the mail will remain deleted locally.
