# dotfiles

![strange](doc/img/strange.gif)

## or, how i learned to stop worrying and write documenation

so-called "dotfiles" are config files for various programs,
particularly, in this case, those running on a
commodity hardware (desktop or laptop).

in many operating systems,
"hidden" file names are prefixed by a `.`,
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

## shell

it turns out that the textual user interface
that we've been using thus far is, in fact, a (somewhat clunky)
programming language called a "shell".  depending on your system,
it's probably something very much like
[bash](http://tldp.org/LDP/abs/html/), and there's
likely a dotfile already in your home directory written in the
shell language that gets run whenever a new shell is opened.

that preexisting dotfile is probably called `.bashrc` or `.profile`,
albeit possibly something else, like
[`.zshrc`](http://zsh.sourceforge.net/Guide/zshguide.html).

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

#### FreeBSD

#### Debian GNU/Linux _circa 2018_

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

##### `gifmaker.sh`

as used to create the image at the top of this readme,

```shell
./gifmaker.sh -p 50 -s 1:32:55 -e 17 -v ~/video/strange.mp4 ~/tmp/strange.gif
```

creates a clip of
[Dr. Strangelove](https://en.wikipedia.org/wiki/Dr._Strangelove)
scaled to 50% of the original video size,
starting 1 hour 32 min 55 seconds in,
with verbose script output,
[seventeen seconds](https://en.wikipedia.org/wiki/Seventeen_Seconds)
in length.

##### `add_text.sh`

originally written to create the stimuli placeholders for
[emo-mcg](http://github.com/ransomw/emo-mcg),
this script draws a text caption on top of a video clip.

for example,

```shell
./shutils/add_text.sh -s 12 mor-iced-tea.mp4 "mor tea?" another.mp4
```

adds the caption "mor tea?" with font-size `12` to
[the sea captain commerical](https://raw.githubusercontent.com/ransomw/clj-demos/master/setsail/resources/assets/demo/vid/mor-iced-tea.mp4).

##### `latex_cts_build.sh`

this script builds and displays a
[LaTeX](https://en.wikipedia.org/wiki/LaTeX)
pdf document on whenever the `.tex` source files are saved.
it's a wrapper around
[`entr`](http://entrproject.org/)
and
[`lualatex`](http://luatex.org/),
the successor to `pdflatex`,
so both of these programs need to be installed.

for example, while editing `folland.tex`
and other `.tex` files in the `folland/` directory of
[the samizdat](http://github.com/ransomw/samizdat),
i run

```shell
samizdat$ PDFVIEWER=zathura latex_cts_build.sh folland
```

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
is [worth trying](https://9fans.github.io/plan9port/).

while i often use `emacs`, the `emacs/` directory in this
repository is in a sort-of half-broken, half-upgraded state.
to try it out anyway, one way is to add something like

```lisp
(add-hook
 'after-init-hook
 (progn
   (load-file "~/.dotfiles/emacs/sys-config.el") ;; global settings
   (load-file "~/.dotfiles/emacs/defuns.el") ;; custom functions via M-x
   (load-file "~/.dotfiles/emacs/keys.el"))) ;; keybindings
```

to the midsection of `~/.emacs`.

## midnight commander (`mc`)

before there were graphical user interfaces
(ok, actually
[well after](https://en.wikipedia.org/wiki/Xerox_Alto)),
there were
[orthodox file managers](https://en.wikipedia.org/wiki/File_manager#Orthodox_file_managers)
such as this, the "GNU Midnight Commander".

##### the sell

i think of `mc` like a command prompt with a keyboard-controlled GUI.
it does allow using a mouse as well.  however, i've found that
once i'm familiar with once keyboard macros (aka shortcuts),
that using the keyboard actually requires less rather than more
cognitive overhead than maintaining the hand/eye coordination
necessary to navigate with a mouse cursor, at least for simpler,
more piecemeal tasks.

whether you choose to use the keyboard exclusively or if you'd
prefer to use `mc` along with a mouse, it's still worth trying out
`mc` if you in a happen to be in a terminal window, anyway.
indeed, the "third window" within `mc`,
a single-line prompt toward the bottom of the interface at startup,
can be toggled between a maximized state and minimized state with
`^o` (or `C-o`, that is "hold down the `control` key on an Apple
keyboard and press `o`).
when the third window is maximized, you'll see that, inside `mc`,
you've got your plain old command prompt back again.

so even if the rest of `mc` only turns out to be good for listing
files and changing directories (it does _much_ more than this,
btw), it's still often well-worth a two-letter command after
opening up a new terminal window.

finally, note that `mc` has a help system built right into the
program, so there's no need to read a wall-of-text `man` page
or run `info info` (although the latter is at least worth considering).
instead, everything is presented in hypertext for today's
(or the 90's, whatever) modern attention-span.  press `F1` or
`m-1` (that is, hold down `alt` and press `1`) or click `Help`
at the bottom left of the `mc` screen to get started!

### the dotfiles

all the `mc` dotfiles are in `mc/`, which correponds to
`~/.local/share/mc/` on your local filesystem,
where `~` is an abbreviation for your home directory,
usually an absolute path like `/home/yourusername`.
run

```shell
cd # ~ is the default parameter passed to the change directory command
pwd # print working directory
```

to be certain.

----

## aside on links

the `cp`-`cat` dance described above works well
for use cases where it's desirable to grab only one or two dotfiles
from this repo or to heavily modify lots of files.
[symbolic links](https://en.wikipedia.org/wiki/Symbolic_link)
are a better option for keeping lots of files scattered throughout
one's home directory synchronized with the state of this repository,

[symbolic] links on the filesystem are somewhat like
links in the web browser:  they're small snippets of information
placed in some particular location that're used to refernce a
large piece of information stored elsewhere.
so rather than having to copy each of the files you'd like to
use out of this repository and into each of the myriad places
that various programs look for their respective dotfiles
every time the repository is updated, it's possible to link
each location (file or directory) you'd like to automatically
update once to a location in the directory tree of the repository
as in

```shell
ln -s /path/from/root/directory/to/.dotfiles ~/path/to/new/link
```

note that the first path, the path to a file in this repository
probably ought to be "absolute", which i mean it begins with a
`/`, to indicate that the path walks all the way up to the
directory containing all other directories on your local POSIX
system.  the second path is where a given program expects its
dotfiles to be.

----

##### skins

> The creator of main users of Acme find syntax highlighting unhelpful and distracting.

as it may, i picked up the wild and far-out idea somewhere in
the wwworld that conveying semantic information via coloring
can be as helpful as it is entertaining.

to specify a skin, use the "Options" menu, or run

```shell
mc -S <skin_name>
```

where `<skin_name>.ini` is the name of a file in `...mc/skins/`

## mimetypes

[MIME](https://wiki.debian.org/MIME) types are
[used](https://specifications.freedesktop.org/mime-apps-spec/latest/)
by my [system](https://www.debian.org/)
to associate file types (like `*.mp3` or `*.pdf`) with programs,
where i have created a link

```shell
ln -s /path/to/.dotfiles/mimeapps.list ~/.local/share/applications/mimeapps.list
```

_Please create a merge request to this document,
replacing these lines, if there is
[more current](http://www.rubicode.com/Software/RCDefaultApp/)
information on associating files with applications on OS X._

this association is achieved by means of a two-step process
(because it turns out this idea of Multipurpose Internet Mail
Extensions is useful for lots of things in addition to this
newfangled "Internet Mail" thing all the kids are talking about
these days :-)

1. a file (like `*.mp3`) is associated with a mimetype
   (such as `audio/mpeg`)
   * this can happen just by checking some text
     files (including `/etc/mime.types`, perhaps)
     that list a bunch of mimetypes
     and the file extension(s) each mimetype corresponds to.

2. a mimetype is associated with a program.  on my system,
   using the `xdg-open` command from freedesktop, what happens
   here is
   * that `mimeapps.list` that was created is checked for the
     mimetype, or, if it's not found in `mimeapps.list`,
     it's definitely going to appear in
     `/usr/share/applications/mimeinfo.cache`,
     albeit perhaps not as the default program for the mimetype.
     the raison d'etre for `mimeapps.list` is to allow each
     individual user to specify their perferred programs
   * the corresponding `<program>.desktop` file
     (installed for system programs in `/usr/share/applications/`
     here) allows for some additional information about how to
     run the program within the desktop environment

... yea, it seems ~~indirect and highly decoupled~~ _i mean_,
complicated to me, too.  perhaps i could just edit
`.../mc/mc.ext` in order to have `mc` open files on a keypress
with my preferred programs, but then i'd have to consider editing
program-specific dotfiles for every program (webbrowsers, email
clients, and whoknowwhatnext) that needs to associate files
with programs other than itself.

or maybe the whole "mimetype"
idea isn't really necessary at all and file extensions could go
directly to programs that ought to open them.  this doesn't
sound like a terrible idea to me.  i suppose for more complex
sequences of programs handing a file to one another, it could
be somewhat constraining, however.
if there's a web browser that can play audio but not video,
for instance, and all it knows about a program is that a generic
media player _can_ play the file, whatever it is, then perhaps an
audio file will get opened in the media player rather than the
browser.

in any event, back to practical matters:
rather than modifying `mimeapps.list` in a text editor, i run

```shell
xdg-mime default <program>.desktop <mimetype>
```

to ensure all the dots line up when associate files with programs,
consulting

* `/etc/mime.types` for mimetype names

and

* `/usr/share/applications/mimeinfo.cache` or
  the files in `/usr/share/applications/` for available programs

_... however,_ `xdg-mime` will overwrite symlink such that a
`cp`-`cat` dance is still necessary

## email

_Status Fall 2020_

Gmail fails to authenticate IMAP with
`mbsync` (and `sylpheed` for control).

Currently accessing email via Chrome only, considering
[fastmail](http://www.fastmail.com) account.

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

##### SSL certificate

it's possible to use an SSL certificate
(specified by the `CertificateFile` line in `mbsyncrc`)
shipped with one's system
in order to establish a secure connection with an remote imap server.
however, in case the handshake between the imap server (gmail in this
case) and `mbsync` fails with a system certificate, it could also be
possible to grab a certificate directly from the server.

this did, in fact, turn out to be the case at one time.
so i ran
```shell
shutils$ ./get_gmail_cert.sh > ~/.gmail.crt
```
_once_ and made the corresponding update to `mbsyncrc`
to start using a valid certificate.

### mutt

once emails are stored locally,
i use [(neo)?mutt](https://www.neomutt.org) to make sense of them.
on my system, the `muttrc` file in this directory is linked to
`~/.mutt/muttrc`.

note in particular that a few [newline-delimited]
address and regular-expression lists
are expected to exist in `~/.config/email/`.

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

## tmux && screen

##### it really does make perfect sense

now that we have some flavor for how useful a shell language can be
for automating the use of other programs, it's worth considering the
program that allows us to use the shell:  the terminal.

[back in the day](https://en.wikipedia.org/wiki/VT100),
there were actually physical terminals —
rather than getting a computer that could be attached to a
screen, mouse, keyboard, usb game controller, bluetooth headset,
or whathaveyou, there would be a box that plugged into the computer
with a screen a keyboard and that'd provide the entire user interface.

today, the program that runs a shell in a modern windowing enviroment
on a POSIX system still behaves like one of these terminal boxes.
the system provides an abstraction called a _pseudoterminal_ that
mimics the wire that used to run between real terminal hardware and
the computer.  when you open up a terminal program, it creates one
of these "virtual wires" and attaches to it.
even though communication happens entirely in software,
there's still a wire protocol sending 1's and 0's between
the system and the terminal (virtual or otherwise).

terminal multiplexers like `tmux` and `screen` further leverage
pseudoterminals to decouple terminal sessions from the windowing
environment entirely.  instead of connecting the pseudoterminal
directly to a particular window, they're separate programs that
help organize your bundle of pseudoterminal wires.  with `tmux`,
there's no need to open up tons of terminal windows, because `tmux`
can communicate with the operating system via lots of different
pseudoterminals, and one terminal window (with its single corresponding
pseudoterminal) is sufficient to communicate with `tmux`.

also, `tmux` can run in the background a keep a bunch of shells
(and their corresponding processes) open without any terminal
windows open at all.

### the conf file

by default, `tmux` looks for its local configuration file at
`~/.tmux.conf`.  alternately, you can pass a path to the
`source-file` command by typing

```
[prefix] :source-file <path> [Return]
```

where `[prefix]` is the prefix key, the single key that preceeds
any input recognized by `tmux`.  by default it's `C-b` (`Ctrl`+`b`),
and the config file in this repo rebinds it to an `emacs`-like
`C-x`.

----

## Odds 'n ends

The remainder of this readme consists of
programs that may or may not continue to function
in Debian GNU/Linux but are either non-functional
or unnecessary in current FreeBSD desktop usage.

## Devil's Pie

[`devilspie2`](http://www.gusnan.se/devilspie2/)
is specific to Xlib, a layer near the bottom of most
GNU/linux windowing environments, so this § can be
skipped if you're running OS X.

my main use-case for my `devilspie2` is repositioning
windows to different desktops:
new windows pretty much always open on the current desktop,
and sometimes i'd like to shuffle certain windows off to
a different desktop.
this is useful with the `latex_cts_build.sh`
[shell script](#shell-scripts),
for example,
because although i usually want to check the PDF viewer
every once in a while,
i don't want it continually stealing focus from the editor.
`devilspie2` can automatically move the PDF viewer to a
particular desktop each time it opens.

### the dotfiles

`devilspie2` is configured via an embedded
[Lua](http://www.lua.org)
interpreter.
(check out
[`lleaves`](https://github.com/ransomw/exercises/tree/master/lleaves)
for some examples of this scripting language.)
specifically, it's given the path of a directory as a parameter,
and it runs every `.lua` script in that directory in [lexicographic]
order.

every `.lua` script in `devilspie/scripts` is prefixed by a
"priority", a two-digit number between `00` and `99`,
so that for instance, `00_debug.lua` runs before any other
Lua script.

additionally, the `devilspie/run_devilspie.sh` wrapper in this
repository may be used to omit Lua scripts above a given priority.
it creates a temporary directory,
copies over the desired Lua scripts,
and starts up `devilspie2`.  for instance,

```shell
run_devilspie.sh -d -p 5
```

will run every script with a priority of `05` or lower
and additionally print debug information.

### notes

i view the notion of "priority" as something like
[SysV init](https://en.wikipedia.org/wiki/Init#SysV-style)
`rc`-levels.
although i'm not sure if it's an appropriate metaphor
for this situtation,
it's the hack i'm currently running with 💔.

`devilspie2` depends on `glib`, which i'm not too happy about.
[`kpie`](https://github.com/skx/kpie)
is an alternative, but i haven't looked into it in any depth.

## Python utilities

when shell scripting is not enough, sensible people turn to Perl.
i've gravitated toward Python, even though it's heavier,
because i find myself writing in it a lot for applications
like web-development where the ecosystem of libraries is helpful.
so it's more convenient for me to transition between working on
projects and making "quick" updates to these scripts without a
Burroughesian भाषा 换.

to use these, i add

```shell
export PYTHONPATH=$PYTHONPATH:/path/to.../.dotfiles/pyutils
```

to `.bashrc`, `.profile`, or the equivalent
(see the above [§](#shell) on using the shell language).
then, the `-m`-for-module flag to the python interpreter
can be used to run any of the given utilities
from the shell.  for example,

```shell
python -m s_wifi --help
```

i'm currently running these scripts with Python `3.5.3`.
on some systems, that interperter might be named `python3`,
where the unadorned `python` program refers to Python `2.7.x`.
these scripts are _not_ compatible with Python 2 — onwards!

##### `s_wifi` — [it was supposed to be] simple wifi

these scripts are specific to GNU/Linux.

i used the `wicd` for some time to connect to wireless networks.
however, i consistently had
[difficulties](https://gist.github.com/ransomw/57bc3891194db89996bf73335ab17be4)
with open networks that require a web-based login after connect
(aka capture ports).
since i would wind up stopping `wicd` and running several C programs
(`iw`, `dhclient`, `wpa_supplicant`)
for situations where `wicd` had difficulty connecting,
i eventually stopped using the program altogether, even though
the functionality was great when it ran smoothly.

`s`-for-simple`_wifi` is a wrapper around some typical "workflows"
of these programs, complicated enough to automate out the tedium,
easier to reason about that a split-process model, and with the
arguable virtue of not bringing any sort of GUI into the

> what networks are around?

> what's the password?

> am i online yet?

QA.

since there's not a separate daemonized process to run under a user
account with appropriate permissions,
i **run `s_wifi` as root**
(and nothing has caught on fire — yet).
