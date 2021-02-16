"""
this module is used via the
`invoke` CLI utility.
suggested use:
  pip install invoke
  invoke --list
and so on according to self-documenting
interface.
see www.pyinvoke.org for more.

todos:
* invoke recognizes task.py when
  cwd contains task.py.
  running invoke from other directories
  may make some of these utils more
  useful.
"""
import os
import os.path as pth
from functools import (
    reduce,
)
from operator import (
    add,
)
from warnings import warn

from invoke import task
from invoke import exceptions as exc

bp = breakpoint

@task
def dir_diff(
        c,
        dir_a,
        dir_b,
):
    if not pth.isdir(dir_a):
        raise exc.Exit((
            dir_a +
            " is not a directory"
        ))
    if not pth.isdir(dir_b):
        raise exc.Exit((
            dir_b +
            " is not a directory"
        ))
    raise NotImplementedError()


def _in_place_replace_string(
    search,
    replacement,
    filepath,
):
    with open(filepath) as f:
        try:
            contents = f.read()
        except UnicodeDecodeError as ex:
            warn(("skipping " +
                  filepath +
                  " on unicode decode "
                  "error" +
                  str(ex)))
            return
    update = contents.replace(
        search,
        replacement,
    )
    # bp()
    with open(filepath, 'w') as f:
        f.write(update)


@task(
    help={
        'loc': (
            "location could be "
            "directory or file"
        ),
    },
)
def replace_string(
        c,
        search,
        replacement,
        loc,
        verbose=False,
):
    for filepath in (
        [loc]
        if
        pth.isfile(loc)
        else
        reduce(
            add,
            [[pth.join(dirpath,
                       filename)
              for filename in filenames]
             for (dirpath, _, filenames)
             in os.walk(loc)])):
        if verbose:
            print("replacing in " +
                  filepath)
        _in_place_replace_string(
            search,
            replacement,
            filepath,
        )
