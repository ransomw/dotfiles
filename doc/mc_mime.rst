``mc`` and mime types
====


midnight commander (``mc``)
----

before there were graphical user interfaces
(ok, actually
[well after](https://en.wikipedia.org/wiki/Xerox_Alto)),
there were
[orthodox file managers](https://en.wikipedia.org/wiki/File_manager#Orthodox_file_managers)
such as this, the "GNU Midnight Commander".

**the sell**

i think of ``mc`` like a command prompt with a keyboard-controlled GUI.
it does allow using a mouse as well.  however, i've found that
once i'm familiar with once keyboard macros (aka shortcuts),
that using the keyboard actually requires less rather than more
cognitive overhead than maintaining the hand/eye coordination
necessary to navigate with a mouse cursor, at least for simpler,
more piecemeal tasks.

whether you choose to use the keyboard exclusively or if you'd
prefer to use ``mc`` along with a mouse, it's still worth trying out
``mc`` if you in a happen to be in a terminal window, anyway.
indeed, the "third window" within ``mc``,
a single-line prompt toward the bottom of the interface at startup,
can be toggled between a maximized state and minimized state with
``^o`` (or ``C-o``, that is "hold down the ``control`` key on an Apple
keyboard and press ``o``).
when the third window is maximized, you'll see that, inside ``mc``,
you've got your plain old command prompt back again.

so even if the rest of ``mc`` only turns out to be good for listing
files and changing directories (it does _much_ more than this,
btw), it's still often well-worth a two-letter command after
opening up a new terminal window.

finally, note that ``mc`` has a help system built right into the
program, so there's no need to read a wall-of-text ``man`` page
or run ``info info`` (although the latter is at least worth considering).
instead, everything is presented in hypertext for today's
(or the 90's, whatever) modern attention-span.  press ``F1`` or
``m-1`` (that is, hold down ``alt`` and press ``1``) or click ``Help``
at the bottom left of the ``mc`` screen to get started!

**the dotfiles**

all the ``mc`` dotfiles are in ``mc/``, which correponds to
``~/.local/share/mc/`` on your local filesystem,
where ``~`` is an abbreviation for your home directory,
usually an absolute path like ``/home/yourusername``.
run

.. code:: shell

  cd # ~ is the default parameter passed to the change directory command
  pwd # print working directory

to be certain.

----

aside on links
----

the ``cp``-``cat`` dance described above works well
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

.. code:: shell

  ln -s /path/from/root/directory/to/.dotfiles ~/path/to/new/link

note that the first path, the path to a file in this repository
probably ought to be "absolute", which i mean it begins with a
``/``, to indicate that the path walks all the way up to the
directory containing all other directories on your local POSIX
system.  the second path is where a given program expects its
dotfiles to be.

----

**skins**

> The creator of main users of Acme find syntax highlighting unhelpful and distracting.

as it may, i picked up the wild and far-out idea somewhere in
the wwworld that conveying semantic information via coloring
can be as helpful as it is entertaining.

to specify a skin, use the "Options" menu, or run

.. code:: shell

  mc -S <skin_name>


where ``<skin_name>.ini`` is the name of a file in ``...mc/skins/``

mimetypes
----

[MIME](https://wiki.debian.org/MIME) types are
[used](https://specifications.freedesktop.org/mime-apps-spec/latest/)
by my [system](https://www.debian.org/)
to associate file types (like ``*.mp3`` or ``*.pdf``) with programs,
where i have created a link

.. code:: shell

  ln -s /path/to/.dotfiles/mimeapps.list ~/.local/share/applications/mimeapps.list


_Please create a merge request to this document,
replacing these lines, if there is
[more current](http://www.rubicode.com/Software/RCDefaultApp/)
information on associating files with applications on OS X._

this association is achieved by means of a two-step process
(because it turns out this idea of Multipurpose Internet Mail
Extensions is useful for lots of things in addition to this
newfangled "Internet Mail" thing all the kids are talking about
these days :-)

1. a file (like ``*.mp3``) is associated with a mimetype
   (such as ``audio/mpeg``)
   * this can happen just by checking some text
     files (including ``/etc/mime.types``, perhaps)
     that list a bunch of mimetypes
     and the file extension(s) each mimetype corresponds to.

2. a mimetype is associated with a program.  on my system,
   using the ``xdg-open`` command from freedesktop, what happens
   here is
   * that ``mimeapps.list`` that was created is checked for the
     mimetype, or, if it's not found in ``mimeapps.list``,
     it's definitely going to appear in
     ``/usr/share/applications/mimeinfo.cache``,
     albeit perhaps not as the default program for the mimetype.
     the raison d'etre for ``mimeapps.list`` is to allow each
     individual user to specify their perferred programs
   * the corresponding ``<program>.desktop`` file
     (installed for system programs in ``/usr/share/applications/``
     here) allows for some additional information about how to
     run the program within the desktop environment

... yea, it seems ~~indirect and highly decoupled~~ _i mean_,
complicated to me, too.  perhaps i could just edit
``.../mc/mc.ext`` in order to have ``mc`` open files on a keypress
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
rather than modifying ``mimeapps.list`` in a text editor, i run

.. code:: shell

  xdg-mime default <program>.desktop <mimetype>


to ensure all the dots line up when associate files with programs,
consulting

* ``/etc/mime.types`` for mimetype names

and

* ``/usr/share/applications/mimeinfo.cache`` or
  the files in ``/usr/share/applications/`` for available programs

_... however,_ ``xdg-mime`` will overwrite symlink such that a
``cp``-``cat`` dance is still necessary
