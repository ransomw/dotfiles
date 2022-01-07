how i learned to stop worrying and write documenation
====

dotfiles
----

.. image:: img/strange.gif


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

.. code:: shell

  cat <name> | less # or sometimes <name>rc
  # read, edit, read, read, read, edit
  cp -r <name> ~/.<name>


all of these files are intended to supplement their
accompanying programs' official documentation, either
from the web or via ``info <name>`` when
`GNU <http://savannah.gnu.org/>`_ is present,
or ``man <name>`` otherwise.


shell
----

it turns out that the textual user interface
that we've been using thus far is, in fact, a (somewhat clunky)
programming language called a "shell".  depending on your system,
it's probably something very much like
`bash <http://tldp.org/LDP/abs/html/>`_, and there's
likely a dotfile already in your home directory written in the
shell language that gets run whenever a new shell is opened.

that preexisting dotfile is probably called ``.bashrc`` or ``.profile``,
albeit possibly something else, like
`.zshrc <http://zsh.sourceforge.net/Guide/zshguide.html>`_.

rather than replacing the preexisting dotfile, shell languages
include a mechanism for referencing one file from another:

.. code:: shell

  . <filename>


is like saying "run all the commands the file named
``<filename>``, then continue running the commands in this file."
(although ``source <filename>`` may have the same meaning
as ``. <filename>`` within some shells, the latter is more portable —
i.e. more likely to be understood in a variety of different
shell dialects).

editors
----

when ``emacs``, ``vim``, and ``vi`` aren't around,
i like ``nano`` quite a lot:  no config files to get started,
and it says what it does right at the bottom of the screen:

| ^G Get Help    ^O Write Out   ^W Where Is    ^K Cut Text    ^J Justify     ^C Cur Pos     ^Y Prev Page   M-\ First Line
| ^X Exit        ^R Read File   ^\ Replace     ^U Uncut Text  ^T To Spell    ^_ Go To Line  ^V Next Page   M-/ Last Line

for instance, on an apple keyboard,
``^G`` means, "hold the ``control`` key and press the ``g`` key."

if you have a **three button** mouse,
`acme <http://acme.cat-v.org/mouse>`_
is `worth trying <https://9fans.github.io/plan9port/>`_.

while i often use ``emacs``, the ``emacs/`` directory in this
repository is in a sort-of half-broken, half-upgraded state.
to try it out anyway, one way is to add something like

.. code: lisp

  (add-hook
   'after-init-hook
   (progn
     (load-file "~/.dotfiles/emacs/sys-config.el") ;; global settings
     (load-file "~/.dotfiles/emacs/defuns.el") ;; custom functions via M-x
     (load-file "~/.dotfiles/emacs/keys.el"))) ;; keybindings


to the midsection of ``~/.emacs``.





tmux && screen
----

**it really does make perfect sense**

now that we have some flavor for how useful a shell language can be
for automating the use of other programs, it's worth considering the
program that allows us to use the shell:  the terminal.

`back in the day <https://en.wikipedia.org/wiki/VT100>`_,
there were actually physical terminals —
rather than getting a computer that could be attached to a
screen, mouse, keyboard, usb game controller, bluetooth headset,
or whathaveyou, there would be a box that plugged into the computer
with a screen a keyboard and that'd provide the entire user interface.

today, the program that runs a shell in a modern windowing enviroment
on a POSIX system still behaves like one of these terminal boxes.
the system provides an abstraction called a *pseudoterminal* that
mimics the wire that used to run between real terminal hardware and
the computer.  when you open up a terminal program, it creates one
of these "virtual wires" and attaches to it.
even though communication happens entirely in software,
there's still a wire protocol sending 1's and 0's between
the system and the terminal (virtual or otherwise).

terminal multiplexers like ``tmux`` and ``screen`` further leverage
pseudoterminals to decouple terminal sessions from the windowing
environment entirely.  instead of connecting the pseudoterminal
directly to a particular window, they're separate programs that
help organize your bundle of pseudoterminal wires.  with ``tmux``,
there's no need to open up tons of terminal windows, because ``tmux``
can communicate with the operating system via lots of different
pseudoterminals, and one terminal window (with its single corresponding
pseudoterminal) is sufficient to communicate with ``tmux``.

also, ``tmux`` can run in the background a keep a bunch of shells
(and their corresponding processes) open without any terminal
windows open at all.

**the conf file**

by default, ``tmux`` looks for its local configuration file at
``~/.tmux.conf``.  alternately, you can pass a path to the
``source-file`` command by typing

.. code:: shell
  [prefix] :source-file <path> [Return]


where ``[prefix]`` is the prefix key, the single key that preceeds
any input recognized by ``tmux``.  by default it's ``C-b`` (``Ctrl``+``b``),
and the config file in this repo rebinds it to an ``emacs``-like
``C-x``.

Python utilities
----

when shell scripting is not enough, sensible people turn to Perl.
i've gravitated toward Python, even though it's heavier,
because i find myself writing in it a lot for applications
like web-development where the ecosystem of libraries is helpful.
so it's more convenient for me to transition between working on
projects and making "quick" updates to these scripts without a
Burroughesian भाषा 换.

to use these, i add

.. code:: shell

  export PYTHONPATH=$PYTHONPATH:/path/to.../.dotfiles/pyutils


to `.bashrc`, `.profile`, or the equivalent
(see the above [§](#shell) on using the shell language).
then, the `-m`-for-module flag to the python interpreter
can be used to run any of the given utilities
from the shell.  for example,

.. code:: shell

  python -m s_wifi --help


i'm currently running these scripts with Python `3.5.3`.
on some systems, that interperter might be named `python3`,
where the unadorned `python` program refers to Python `2.7.x`.
these scripts are _not_ compatible with Python 2 — onwards!

`s_wifi` — [it was supposed to be] **simple wifi**

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
