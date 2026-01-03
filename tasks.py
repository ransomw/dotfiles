"""
this module is used via the
`invoke` CLI utility.
suggested use:
  pip install invoke
  . <(inv --print-completion-script=zsh)
  invoke --list
and so on according to self-documenting
interface.
see www.pyinvoke.org for more.

### todo
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
from pathlib import Path
import datetime
from warnings import warn

from invoke import task
from invoke import exceptions as exc
from fabric import Connection


def _ensure_clojure(c: Connection):
    pass

def _stop_extant_todo_list_deploy(c: Connection):
    pass


def _ensure_git_repo(c: Connection):
    pass



def _start_todo_list_deploy(c: Connection):
    pass


@task
def deploy_todo_list(ctx, host='161.35.58.205'):
    con = Connection(host)


class ProgrammingError(Exception):
    pass


def _parse_ifconfig_output(
        ifconfig_str):
    ifconfig_lines = ifconfig_str.split('\n')
    ifname = None
    ifname_to_config_lines = {}
    for line in ifconfig_lines:
        if line and line[0] != ' ':
            ifname = line.split(':')[0]
            ifname_to_config_lines[ifname] = [line]
            continue
        if ifname is None:
            raise ProgrammingError()
        ifname_to_config_lines[ifname].append(line)
    return {}

@task
def find_router(
        c,
):
    ifconfig_res = c.run(
        '/sbin/ifconfig',
        hide=True,
        shell='zsh',
    )
    ifconfig_out_parsed = _parse_ifconfig_output(ifconfig_res.stdout)
    ip_addr = '192.168.1.65'
    ip_addr_range = '.'.join(ip_addr.split('.')[:3]+['0-255'])
    # ping scan
    c.run(
        'nmap -sn '+ip_addr_range,
        shell='zsh',
    )



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


@task
def build_docs(c):
    """
    build the ReStructuredText docs in doc/
    """
    with c.cd('doc'):
        c.run('sphinx-build . _build/',
              shell='zsh',)


CONF_FILES = [
    {"p": "zellij",
     "f": [{"s": "zellij_config.kdl",
            "d": "~/.config/zellij/config.kdl"}]},
    {"p": "fish",
     "f": [{"s": "config.fish",
            "d": "~/.config/fish/config.fish"},
           {"s": "fish",
            "d": "~/.config/fish/conf.d"}]},
    {"p": "emacs",
     "f": [{"s": "emacs/init.el",
            "d": "~/.emacs.d/init.el"},
           {"s": "emacs/lisp",
            "d": "~/.emacs.d/lisp"}]},
]

BKUP_DATE_FORMAT = "%Y%m%d-%H%M%S"

def _install_conf_file(src_str, dest_str):
    src = Path(src_str)
    dest_unexpanded = Path(dest_str)
    dest = dest_unexpanded.expanduser()
    if not src.exists():
        print(f"conf source {src_str} doesn't exist")
        return
    if src.samefile(dest):
        print(f"conf file {src_str} already installed to {dest_str}")
        return
    if dest.exists():
        now = datetime.datetime.now()
        timestamp = now.strftime(BKUP_DATE_FORMAT)
        dest_bk = dest.with_name(dest.name+'-'+timestamp+'.bk')
        dest.rename(dest_bk)
        print(f"created a backup of {dest_str}")
    dest.symlink_to(src.absolute())
    print(f"linked {dest_str} to {src_str}")



@task
def install_conf_files(c):
    """
    """
    for program_data in CONF_FILES:
        program = program_data["p"]
        conf_files = program_data["f"]
        if not c.run(f"which {program}"):
            print(f"{program} not installed")
            continue
        for conf_file in conf_files:
            _install_conf_file(conf_file["s"], conf_file["d"])
