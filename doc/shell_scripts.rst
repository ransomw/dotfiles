shell scripts
====

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

.. code:: shell

  #! /path/to/interpreter


where `#!` is prounounced "shebang" (optionally in capitals
and with a variable number of exclamation points).
this special line is understood by the shell as a queue to pass
rest of the file (the script proper) to the interpreter.

the shell scripts in the repository are currently located in
`shutils/`

FreeBSD
----

Debian GNU/Linux *circa 2018*
----

**bkup.sh**

this script expects two (possibly empty) files
placed alongside it in the same directory,
where is a newline-delimited list

* ``paths_unison_dirs`` — paths relative to your home directory
  to synchronize with removable media with
  `unison <http://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html>`_
* ``paths_git_repos`` — absolute (beginning with ``/``) paths to
  `git <https://git-scm.com/book/en/v2>`_ repositories

i use it backup files from my laptop to an external harddrive.

**gifmaker.sh**

as used to create the image at the top of this readme,

.. code:: shell

  ./gifmaker.sh -p 50 -s 1:32:55 -e 17 -v ~/video/strange.mp4 ~/tmp/strange.gif


creates a clip of
`Dr. Strangelove <https://en.wikipedia.org/wiki/Dr._Strangelove>`_
scaled to 50% of the original video size,
starting 1 hour 32 min 55 seconds in,
with verbose script output,
`seventeen seconds <https://en.wikipedia.org/wiki/Seventeen_Seconds>`_
in length.

**add_text.sh**

originally written to create the stimuli placeholders for
`emo-mcg <http://github.com/ransomw/emo-mcg>`_,
this script draws a text caption on top of a video clip.

for example,

.. code:: shell

  ./shutils/add_text.sh -s 12 mor-iced-tea.mp4 "mor tea?" another.mp4


adds the caption "mor tea?" with font-size `12` to
`the sea captain commerical <https://raw.githubusercontent.com/ransomw/clj-demos/master/setsail/resources/assets/demo/vid/mor-iced-tea.mp4>`_.

**latex_cts_build.sh**

this script builds and displays a
`LaTeX <https://en.wikipedia.org/wiki/LaTeX>`_
pdf document on whenever the `.tex` source files are saved.
it's a wrapper around
`entr <http://entrproject.org/>`_
and
`lualatex <http://luatex.org/>`_,
the successor to ``pdflatex``,
so both of these programs need to be installed.

for example, while editing ``folland.tex``
and other ``.tex`` files in the ``folland/`` directory of
`the samizdat <http://github.com/ransomw/samizdat>`_,
i run

.. code:: shell

  samizdat$ PDFVIEWER=zathura latex_cts_build.sh folland

