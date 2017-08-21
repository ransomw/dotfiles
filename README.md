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

## shell

it turns out that the textual user interface
(likely the contents of an OS X Terminal window)
that we've been using thus far is, in fact, a (somewhat clunky)
programming language called a "shell".  depending on your system,
it's probably something very much like
[bash](http://tldp.org/LDP/abs/html/), and there's
likely a dotfile already in your home directory written in the
shell language that gets run whenever a new shell is opened.

that preexisting dotfile is probably called `.bashrc` or `.profile`.

rather than replacing the preexisting dotfile, shell languages
include a mechanism for referencing one file from another:

```shell
. <filename>
```

is like saying "run all the commands the file named
`<filename>`, then continue running the commands in this file."
(although `source <filename>` may have the same meaning
as `. <filename>` within some shells, the latter is more portable —
i.e. more likely to be understood in a variety of different
shell dialects).

### aliases

aliases are a convenient way to create abbreviations
and avoid typing so much in the shell.
there are some examples in `bash_aliases.sh`.
for example, i use single-letter command, `m` (for "mail"),
to get mail from the server and view it with `mutt` as
described above.

this is definitely a dotfile to extend and customize:
adding a new alias usually needs less than a minute of setup,
and appropriately chosen aliases can save _lots_ of typing.

##### not getting confused

as demonstrated by aliases, a single-word command to the shell
is not always a the name of a program.
in fact, it's fairly common practice to write an alias with the
same name as a program.
for example, i pretty much always want to be able to crank up
the volume whilst listening to tunes, so i've created an alias
to my existing media player, `mpv`, with an the appropriate option.

here are a few tricks to make sense of what the shell is doing
when you enter any given command:

* to list all the current aliases in a shell run
  `alias` all by itself.
* if you're uncertain whether a particular command is an alias,
  a program on disk, a shell builtin (like `alias` itself),
  or what, run
  ```
  type <command>
  ```
* if you want to know where a program is, run
  ```
  which <program>
  ```
  for example, `which mpv` prints the location of my media player,
  and if, for some reason, i want to run `mpv` without
  the volume turned up, it's ok to type type in the full location of
  the program — or equivalently, to run
  ```
  $(which mpv)
  ```
  in order to bypass the alias.

* notice, by the same token, that `which m` doesn't print anything
  at all.  that's because `m` is not a program; it's an alias only.
  if you get confused and don't know what an alias is doing,
  the only way to find out is by running the alias command, as in
  ```
  alias m
  ```

### shell scripts

since the shell is a general purpose programming language,
it's possible to write programs for the shell.
these programs are called "scripts" — or more specifically
"shell scripts".  the exact meaning of "script" is somewhat
technical and also the subject of semantic drift.
generally (but not definitively), a script refers to a smallish
program written in a language that can be passed as a string
to another program, the script's "interpreter" in order to
have the computer's hardware follow the script.

scripts typically begin with the name of the interpreter
in a line formatted as

```
#! /path/to/interpreter
```

where `#!` is prounounced "shebang" (optionally in capitals
and with a variable number of exclamation points).
this special line is understood by the shell as a queue to pass
rest of the file (the script proper) to the interpreter.

the shell scripts in the repository are currently located in
`shutils/`

##### `bkup.sh`

this script expects two (possibly empty) files
placed alongside it in the same directory,
where is a newline-delimited list

* `paths_unison_dirs` — paths relative to your home directory
  to synchronize with removable media with
  [`unison`](http://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html)
* `paths_git_repos` — absolute (beginning with `/`) paths to
  [`git`](https://git-scm.com/book/en/v2) repositories

i use it backup files from my laptop to an external harddrive.
