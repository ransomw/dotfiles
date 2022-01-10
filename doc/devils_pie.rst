Devil's Pie
====

`devilspie2 <http://www.gusnan.se/devilspie2/>`_
is specific to Xlib, a layer near the bottom of most
GNU/linux windowing environments, so this § can be
skipped if you're running OS X.

my main use-case for my ``devilspie2`` is repositioning
windows to different desktops:
new windows pretty much always open on the current desktop,
and sometimes i'd like to shuffle certain windows off to
a different desktop.
this is useful with the ``latex_cts_build.sh``
[shell script](#shell-scripts),
for example,
because although i usually want to check the PDF viewer
every once in a while,
i don't want it continually stealing focus from the editor.
``devilspie2`` can automatically move the PDF viewer to a
particular desktop each time it opens.

**the dotfiles**

``devilspie2`` is configured via an embedded
`Lua <http://www.lua.org>`_
interpreter.
(check out |lleaves|_
for some examples of this scripting language.)
specifically, it's given the path of a directory as a parameter,
and it runs every ``.lua`` script in that directory in [lexicographic]
order.

.. |lleaves| replace:: ``lleaves``
.. _lleaves: https://github.com/ransomw/exercises/tree/master/lleaves

``.lua`` scripts in ``devilspie/scripts`` is prefixed by a
"priority", a two-digit number between ``00`` and ``99``,
so that for instance, ``00_debug.lua`` runs before any other
Lua script.

additionally, the ``devilspie/run_devilspie.sh`` wrapper in this
repository may be used to omit Lua scripts above a given priority.
it creates a temporary directory,
copies over the desired Lua scripts,
and starts up ``devilspie2``.  for instance,

.. code:: shell
          run_devilspie.sh -d -p 5

will run every script with a priority of ``05`` or lower
and additionally print debug information.

notes
-----

i view the notion of "priority" as something like
`SysV init <https://en.wikipedia.org/wiki/Init#SysV-style>`_
``rc``-levels.
although i'm not sure if it's an appropriate metaphor
for this situtation,
it's the hack i'm currently running with 💔.

``devilspie2`` depends on ``glib``, which i'm not too happy about.

|kpie|_ is an alternative, but i haven't looked into it in any depth.

.. |kpie| replace:: ``kpie``
.. _kpie: https://github.com/skx/kpie

