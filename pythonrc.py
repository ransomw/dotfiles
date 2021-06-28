"""-*-python-*-
suggested use:
place a dotted symlink to this
file in your home directory
  ln -s /.../pythonrc ~/.pythonrc
and set the environment variable
PYTHONSTARTUP
to the symlink
export PYTHONSTARTUP="${HOME}/.pythonrc"
see also:
https://docs.python.org/3/using/cmdline.html#envvar-PYTHONSTARTUP

### todo
* refactor (using rope) into pyutil
* lint, gofmt-alike, trailing whitespace
  - pylintrc
  - pylint ASTeroid checker(s), if only for example
  - pre-commit
* auto-generate documentation
  - sphinx (?)
  - generate todos from this markdown format
* music player
* mic access
* camera access
* plivo (or twilio fallback) text msgs
* image view & editing
  - scaled .
* document view & editing
  - 4-in-1 pdf
  - pdf text extraction (non-OCR)
"""

from typing import (
    Dict,
)
from collections import (
    namedtuple,
)
import datetime as dt
from datetime import (
    datetime as dto,
    timedelta,
)
from functools import (
    reduce,
    partial,
    wraps,
)
from inspect import (
    getmembers as gm,
    getsource,
    getsourcefile as gsf,
    getmodule,
    ismodule,
    isclass,
)
import operator as op
from operator import (
    add,
    itemgetter as ig,
    attrgetter,
)
import os
import sys
import os.path as pth
import random
from pprint import (
    # pp, 2.8+
    pprint,
)
import math
from copy import (
    deepcopy,
)

import pydoc
from pydoc import (
    pager,
)

import curses
import curses.textpad


import subprocess
from subprocess import (
    Popen,
    PIPE,
)

import shlex

import configparser

import pathlib

from io import (
    BytesIO,
    StringIO,
)

from warnings import warn

import zipfile

import gzip
from gzip import (
    GzipFile,
)

from tempfile import (
    gettempdir,
    mkdtemp,
)

import json

from importlib import (
    reload,
)

import shutil
from shutil import (
    copyfileobj,
)

import threading
import asyncio
import queue

import signal

import time

import rlcompleter
import readline
readline.parse_and_bind("tab: complete")

###


def ppp(obj):
    sio = StringIO()
    pprint(obj, stream=sio)
    pager(sio.getvalue())


def gmn(*args, **kwargs):
    return [m[0] for m in gm(*args, **kwargs)]


def gs(*args, **kwargs):
    pydoc.pager(getsource(*args, **kwargs))


inc = lambda x: x + 1
dec = lambda x: x - 1


class Leaf:
    pass


class Branch:
    pass


class Tree:
    pass


class NameSpace:
    def __init__(self, obj):
        self._obj = obj

    def names(self):
        if ismodule(self._obj):
            return dir(self._obj)
        elif isclass(self._obj):
            return dir(self._obj)
        assert False, repr(self._obj) + " is not a module or class"

    def attrs(self):
        return map(partial(getattr, self._obj), self.names())

    def namespaces(self):
        [Namespace(attr) for attr in self.attrs() if any(juxt(ismodule, isclass)(attr))]


def pysearch_name(name, maxdepth=3):
    res = []
    permissive_getattr = excepts(
        (ModuleNotFoundError, AttributeError), partial(getattr), lambda _: None
    )

    def name_match(mname):
        return name in mname

    res += [sys.modules[mname] for mname in sys.modules.keys() if name_match(mname)]

    def search_class(cls):
        for mname in dir(cls):
            if name_match(mname):
                res.append(permissive_getattr(cls, mname))

    def search_module(module, depth):
        if depth > maxdepth:
            return
        if name in dir(module):
            res.append(permissive_getattr(module, name))
        for (mname, member) in [
            (mname, permissive_getattr(module, mname)) for mname in dir(module)
        ]:
            if not member:
                continue
            if name_match(mname):
                res.append(member)
            if isinstance(member, type):
                search_class(member)
            if ismodule(member):
                search_module(member, depth + 1)

    for mname in list(sys.modules.keys()):
        search_module(sys.modules[mname], 0)
    return res


ls = os.listdir


def cat(filepath, retstr=False):
    with open(filepath) as f:
        fstr = f.read()
    if retstr:
        return fstr
    pydoc.pager(fstr)


run = partial(
    subprocess.run,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    # shell=True,
    # check=True,
)


def run_1(cmd, timeout=None, stream=sys.stdout, err_stream=sys.stderr):
    def output_reader(proc):
        for line in iter((proc.stdout.readline if proc.stdout else lambda: b""), b""):
            print(line.decode("utf-8"), file=stream, end="")
        return

    def err_reader(proc):
        for line in iter((proc.stderr.readline if proc.stderr else lambda: b""), b""):
            print(line.decode("utf-8"), file=err_stream, end="")
        return
        # todo: copyfileobj with .decode("utf-8") wrapper
        proc_strm = proc.stderr or b""

    proc = Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    tout = threading.Thread(target=output_reader, args=(proc,))
    terr = threading.Thread(target=err_reader, args=(proc,))
    tout.start()
    terr.start()
    proc.wait(timeout=timeout)
    tout.join()
    terr.join()


config = configparser.ConfigParser()
config.read(
    [
        os.path.join(os.path.dirname(os.path.realpath(__file__)), "python.conf"),
    ]
)


class PipInstallException(Exception):
    pass


# todo: --cache-dir option,
#    possibly following pre-commit cache strategy
# todo(???): build wheels
def pip_install(package_name):
    name_to_specifier = {
        name: config["package-specifiers"][name]
        for name in config["package-specifiers"]
    }
    if package_name not in name_to_specifier:
        if os.getenv("PY_DBG_IMPORTS"):
            breakpoint()
        raise PipInstallException("unknown package", (package_name,))
    specifier = name_to_specifier[package_name]
    cmd = ["pip", "install",] + (['-e',] if specifier.startswith('git+https://') else []) + [specifier,]
    res = subprocess.run(cmd)
    if res.returncode == 0:
        return
    raise PipInstallException("install failed", (res,))


###
_VENV_DIR = pth.join(str(pathlib.Path().home()), ".pyvenv")
_DEFAULT_VENV = pth.join(_VENV_DIR, "default")
if sys.prefix == sys.base_prefix:
    if not os.getenv("PY_CREATE_VENV"):
        raise Exception("not in venv.  set PY_CREATE_VENV to create")
    venv.create(_DEFAULT_VENV)
    print(". " + pth.join(_DEFAULT_VENV, "bin", "activate"))
    exit()


class ImportBlocker(object):
    def __init__(self):
        self.module_names = set()
        self.package_names = set()

    def find_module(self, fullname, path=None):
        if fullname.split(".")[0] in self.package_names:
            return self
        if fullname in self.module_names:
            return self
        return None

    def exec_module(self, mdl):
        # return an empty namespace
        return {}

    def create_module(self, spec):
        return None


import_blocker = ImportBlocker()
sys.meta_path.append(import_blocker)


def my_except_hook(exctype, value, traceback):
    if exctype is KeyboardInterrupt:
        print("see you later!")
    sys.__excepthook__(exctype, value, traceback)


def install_package(name):
    pass


sys.excepthook = my_except_hook

while True:
    try:
        import toolz
        import toolz.functoolz as ftlz
        import toolz.itertoolz as itlz
        import toolz.dicttoolz as dtlz
        from toolz.functoolz import (
            compose_left,
            excepts,
            compose,
            curry,
            flip,
            juxt,
            thread_last,
        )
        from toolz.itertoolz import (
            accumulate,
            concat,
            concatv,
            cons,
            diff,
            first,
            isdistinct,
            groupby,
            mapcat,
            nth,
            unique,
        )
        from toolz.dicttoolz import (
            keymap,
            valmap,
            itemmap,
        )
        from toolz.curried import (
            get,
            keyfilter,
            valfilter,
            itemfilter,
        )
        import numpy as np
        import ics
        import pandas as pd
        import openpyxl
        import openpyxl.utils.dataframe
        import requests
        import npyscreen

        # without Qt install (see python.conf)
        # gui `pymol.lauch([])` is inoperative
        # and this package is at most useful
        # to parse file formats (protien database -- .pdb), etc.
        import pymol

        import urwid
        import tkinter

        import astor
        import rope
        import rope.base.project
        from rope.base.project import (
            Project as RopeProject,
        )
        import rope.refactor.move
        import rope.refactor.multiproject
        import rope.contrib.generate
        import astor

        sys.path.append(os.path.dirname(os.path.realpath(__file__)))
        import pyutils
        import pyutils.pyjuke as juke
        from pyutils.pastebin import pastebin_app
        from pyutils.cartography.osm import (
            osm_to_shp,
            plot_osm_shp,
        )
        from pyutils.cartography.test_osm import (
            TEST_MAP as TEST_OSM_MAP
        )
        sys.path.pop()

    except ModuleNotFoundError as err:
        package_name = err.name
        try:
            print("attempting to install " + package_name)
            pip_install(package_name)
        except PipInstallException as ex:
            if os.getenv("PY_DBG_IMPORTS"):
                breakpoint()
            import_blocker.package_names.add(package_name)
        continue
    break


# reset to orig
# sys.excepthook = sys.__excepthook__

uninstalled_packages = import_blocker.package_names.copy()
if uninstalled_packages:
    print("uninstalled packages")
    print(uninstalled_packages)

###


nextfn = lambda: plot_osm_shp(osm_to_shp(TEST_OSM_MAP))


def view_pdb(db_id: str):
    fn = view_pdb
    garlic_exec = fn.garlic_exec
    cache_dir = fn.cache_dir_path
    if not pth.exists(cache_dir):
        os.mkdir(cache_dir)
    download_url_base = fn.download_url_base
    pdb_file_path = pth.join(
        cache_dir,
        "{db_id}.pdb".format(
            db_id=db_id,
        ),
    )
    pdb_archive_path = pth.join(
        cache_dir,
        "{db_id}.pdb.gz".format(
            db_id=db_id,
        ),
    )

    #    breakpoint()

    pdb_file_url = requests.compat.urljoin(
        download_url_base,
        "{db_id}.pdb".format(
            db_id=db_id,
        ),
    )
    pdb_archive_url = requests.compat.urljoin(
        download_url_base,
        "{db_id}.pdb.gz".format(
            db_id=db_id,
        ),
    )

    if not pth.exists(pdb_file_path) and not pth.exists(pdb_archive_path):
        res: request.models.Response = requests.get(pdb_archive_url, stream=True)
        assert res.status_code == 200
        chunks = res.iter_content(
            # read data in "whatever size the chunks are recv'd"
            # todo: extract personal dsl fn, documenting the "whatever"
            chunk_size=None,
        )
        with open(pdb_archive_path, "wb") as f:
            for chunk in chunks:
                f.write(chunk)

    if not pth.exists(pdb_file_path):
        with gzip.open(pdb_archive_path) as f_gz, open(pdb_file_path, "wb") as f:
            shutil.copyfileobj(f_gz, f)

    #    breakpoint()
    proc = Popen([garlic_exec, pdb_file_path])
    # todo: workaround garlic numeric keypad requirement
    #   - forward keys from python terminal
    #   - wrap window (X11 or other layer) to access keys
    #          while view selected.
    try:
        print("interrupt Ctl-c to exit")
        while True:
            time.sleep(1024)
    except KeyboardInterrupt:
        pass
    finally:
        proc.terminate()
        print("waiting on garlic exit")
        proc.wait()

    return

    # attempts at optimization
    with open() as f:
        with GzipFile(
            f,
            # default 'rb'.  no text mode opt here.  gzip.open() only
            mode="rb",
        ) as gf:
            gf.read1()


view_pdb.download_url_base = "https://files.rcsb.org/download/"
view_pdb.garlic_exec = "garlic"
view_pdb.cache_dir_path = pth.join(gettempdir(), "view_pdb_cache_dir")


# todo: scrape wikipedia page to pdb code
#    https://en.wikipedia.org/wiki/Muscarinic_acetylcholine_receptor_M4
# todo: search wikipedia page (NLP+force(?))
#    to receptors (pages w/ pdb codes)


def xlst_to_dataframes(filepath):
    wb = openpyxl.load_workbook(filepath)
    return {sn: pd.DataFrame(wb[sn].values) for sn in wb.sheetnames}


def dataframes_to_xlst(dfs: Dict[str, pd.DataFrame], filepath):
    wb = openpyxl.Workbook()
    wb.active
    for (nm, df) in dfs.items():
        ws = wb.create_sheet(nm)
        ws.title
        for row in openpyxl.utils.dataframe.dataframe_to_rows(
            df, index=True, header=True
        ):
            ws.append(row)
    wb.save(filepath)

##>>>##

###
# simple time-boxing and -estimation setup
#
# todo:
#* persistent dict (to disk)
#* calc time estimate sums
#

# activity to minutes
_TIME_ESTIMATE = {}


def _espeak_text(
        text,
        vol=9,
        disp=True,
):
    if disp:
        rainbow_print(text)
    subprocess.run(
        ['espeak',
         '-v', 'f4',
         '-p', '135',
         '-a', str(vol)
         ],
        input=bytes(text, 'utf-8'),
    )


def _xmessage_text(
        text,
        disp=True,
):
    if disp:
        rainbow_print(text)
    proc = subprocess.Popen(
        ['xmessage',
         text,
         ],
    )
    return proc



def print_time_estimates():
    print(_TIME_ESTIMATE)
    # todo: calculate sums


def add_time_estimate_task(
        key_or_ks,
        minutes=None,
):
    if isinstance(key_or_ks, (str,)):
        key = key_or_ks
        act = key #activity
        # if act in _TIME_ESTIMATE:
        #     raise ValueError(
        #         "activity already exists",
        #         (act,))
        minutes = (input(("how long will '"
                          +act+
                          "' take? >[minutes]> ")) if minutes is None else float(minutes))
        _TIME_ESTIMATE[act] = float(minutes)
    elif isinstance(key_or_ks, (list,)):
        ks = key_or_ks
        raise ValueError(
            "nested activities unimplemented",
            (ks,))
    else:
        raise RuntimeError(
            "unknown activity specification",
            (key_or_ks, type(key_or_ks),))


def do_time_estimated_task(
        key_or_ks=None,
        min_warning=2.,
        mute=True,
):
    if key_or_ks == None:
        print("given options")
        print("-------------")
        print_time_estimates()
        print("----")
        for v in _TIME_ESTIMATE.values():
            if not isinstance(v, (int,)):
                raise NotImplementedError(
                    "nested activities unimpl")
        key = ''
        while not key or key not in _TIME_ESTIMATE:
            # todo: tab-completion
            key = input((
                "what task do you want"
                " to work on? )]} "))
            if key not in _TIME_ESTIMATE:
                print(("unknown activity '"+
                       key
                       +"'. "
                       "activities to choose from"
                       " are:"))
                print(', '.join(["'"+act+"'" for act in
                           _TIME_ESTIMATE.keys()]))
        key_or_ks = key
    #
    if isinstance(key_or_ks, (str,)):
        key = key_or_ks
        act = key
        est_min = _TIME_ESTIMATE[key]
        if est_min is None:
            print("no time estimate for '"+act+"'")
        elif not isinstance(est_min, (int, float,)):
            raise ValueError(
                "nested activities unimplemented",
                (est_min,))
        msg_begin = "fine, take "+str(est_min)+" minutes"
        rainbow_print(msg_begin)
        if not mute:
            _espeak_text(msg_begin)
        sec_wait = float(60.*(float(est_min)-float(min_warning)))
        PROG_BAR_STEPS = 10
        step_sec_wait = sec_wait/float(PROG_BAR_STEPS)
        for _ in range(PROG_BAR_STEPS):
            # print("waiting "+str(step_sec_wait), file=sys.stderr,)
            time.sleep(step_sec_wait)
            print('.', end='')
            sys.stdout.flush()
        print()
        msg_warn = ("you have "+str(min_warning)+" minutes left to finish "+
                    act)
        rainbow_print(msg_begin)
        if not mute:
            _espeak_text(msg_warn)
        warn_xm_proc = _xmessage_text(msg_warn)
        sec_wait = 60.*min_warning
        step_sec_wait = sec_wait/float(PROG_BAR_STEPS)
        for _ in range(PROG_BAR_STEPS):
            time.sleep(step_sec_wait)
            print('.', end='')
            sys.stdout.flush()
        print()
        msg_end = ("time's up!  your estimate of "+str(est_min)+" minutes "
                   "to completely complete "+act+" has elapsed. ")
        rainbow_print(msg_begin)
        if not mute:
            _espeak_text(msg_end)
        end_xm_proc = _xmessage_text(msg_end)
        warn_xm_proc.terminate()
        warn_xm_proc.wait()
        finished = None
        while finished is None:
            finished_txt = input("did you finish??? >[yes/no]>> ").strip()
            if finished_txt not in ['yes', 'no',]:
                print("please answer 'yes' or 'no'")
                continue
            finished = finished_txt == 'yes'
        end_xm_proc.terminate()
        end_xm_proc.wait()
        if finished:
            del _TIME_ESTIMATE[key]
            # todo: pos/neg-reinforcement
        else:
            _TIME_ESTIMATE[key] = None
            print("your time-estimate has been discarded.  "
                  # ^ irl stuff.
                  "to add a new estimate:")
            print("add_time_estimate_task('"+key+"')")
    elif isinstance(key_or_ks, (list,)):
        ks = key_or_ks
        raise ValueError(
            "nested activities unimplemented",
            (ks,))
    else:
        raise RuntimeError(
            "unknown activity specification",
            (key_or_ks, type(key_or_ks),))

##<<<##


def edit_text_terminal_curses(text):
    # startup
    stdscr = curses.initscr()
    curses.noecho()
    curses.cbreak()
    stdscr.keypad(True)
    ###

    height, width = stdscr.getmaxyx()
    editwin = curses.newwin(height - 10, width - 10, 1, 1)
    editwin.scrollok(True)
    curses.textpad.rectangle(stdscr, 0, 0, height - 9, width - 9)
    stdscr.refresh()

    box = curses.textpad.Textbox(editwin, insert_mode=True)
    for char in text:
        box.do_command(char)
    # Let the user edit until Ctrl-G is struck.
    box.edit()

    # Get resulting contents
    message = box.gather()

    # exit
    curses.nocbreak()
    stdscr.keypad(False)
    curses.echo()
    curses.endwin()

    return message



def rainbow_print(text):
    # Codes listed are from ECMA-48, Section 8.3.117, p. 61.
    # extracted from /usr/local/share/zsh/5.8/functions/Misc/colors
    ##
    # Attribute codes:
    _ATTRS = {
        name: '\033['+code+'m'
        for (code, name) in (
      ('00', 'none'),
      ('01', 'bold'),
      ('02', 'faint'),
        ('22', 'normal'),
      ('03', 'standout'),
               ('23', 'no-standout'),
      ('04', 'underline'),
          ('24', 'no-underline'),
      ('05', 'blink'),
        ('25', 'no-blink'),
      ('07', 'reverse'),
        ('27', 'no-reverse'),
      ('08', 'conceal'),
        ('28', 'no-conceal'),
                )}
    # Text color codes:
    _COLORS = {
        name: '\033['+code+'m'
        for (code, name) in (
      ('30', 'black'),
        ('40', 'bg-black'),
      ('31', 'red'),
        ('41', 'bg-red'),
      ('32', 'green'),
        ('42', 'bg-green'),
      ('33', 'yellow'),
        ('43', 'bg-yellow'),
      ('34', 'blue'),
        ('44', 'bg-blue'),
      ('35', 'magenta'),
        ('45', 'bg-magenta'),
      ('36', 'cyan'),
        ('46', 'bg-cyan'),
      ('37', 'white'),
        ('47', 'bg-white'),
    )}
    _COLOR_DEFAULTS = {
        name: '\033['+code+'m'
        for (code, name) in (
      ('39', 'default'),
        ('49', 'bg-default'),
                )}
    color_names = [color_name for color_name in
                   _COLORS.keys()
                   if not color_name.startswith('bg-')]
    for color_name, char in zip(color_names*math.ceil(len(text)/len(color_names)), text):
        print(_COLORS[color_name]+char, end='')
    print(_ATTRS['none'])



# todo:
#    * kill line
#    * maintain AST in parallel with text
#        - arrays of lines, lines of arrays
#    * i18n input methods, mathsym input method
#    * undo
#    * see Edit.highlight re. copy and paste impl
#    * additional cursor motion commands
#      - search text
#      - goto line
#    * use urwid Command Map
#    * allow editing in webbrowser
#      (see calc.py example)
def edit_text_terminal_urwid(edit_text, second_edit_text=None,
    word_re=r'[A-z]'
):
    class EditBox(urwid.Filler):
        region_bdry_marker = '|'

        def __init__(self, *args, **kwargs):
            edit_text = kwargs.pop('edit_text', '')
            self.edit = urwid.Edit(
                edit_text=edit_text,
                multiline=True,
            )
            super(EditBox, self).__init__(self.edit, *args, **kwargs)
            self.region_bdry = None

        def keypress(self, size, key):
            clipboard = edit_text_terminal_urwid.clipboard
            if key in ["ctrl n",]:
                key = 'down'
            elif key in ["ctrl p",]:
                key = 'up'
            elif key in ["ctrl b",]:
                key = 'left'
            elif key in ["ctrl f",]:
                key = 'right'
            elif key in ["ctrl a",]:
                key = 'home'
                if False:
                    edit_text_before = self.edit.get_edit_text()[:self.edit.edit_pos]
                    try:
                        newline_before = edit_text_before.rindex('\n')
                    except ValueError:
                        newline_before = -1
                    self.edit.set_edit_pos(newline_before+1)
                    return
            elif key in ["ctrl e",]:
                key = 'end'
                if False:
                    edit_text_after = self.edit.get_edit_text()[self.edit.edit_pos:]
                    try:
                        newline_after = edit_text_after.index('\n')
                    except ValueError:
                        newline_after = len(edit_text_after)
                    self.edit.set_edit_pos(self.edit.edit_pos+newline_after)
                    return
            ###
            elif key in ["meta <",]:
                self.edit.set_edit_pos(0)
            elif key in ["meta >",]:
                self.edit.set_edit_pos(len(self.edit.get_edit_text()))
            elif key in ["ctrl k",]:
                pass
            elif key in ["ctrl d",]:
                key = "delete"
            elif key in ["ctrl _",]:
                pass
            ###
            elif key in [
                    # "ctrl space"
                       "<0>",]:
                if self.region_bdry is None:
                    self.region_bdry = self.edit.edit_pos
                    self.edit.insert_text(
                        self.region_bdry_marker)
                    return
            elif key in ["meta w",]:
                if self.region_bdry is None:
                    return
                if self.edit.edit_pos <= self.region_bdry:
                    cp_text = self.edit.get_edit_text()[
                        self.edit.edit_pos:self.region_bdry]
                else:
                    cp_text = self.edit.get_edit_text()[
                        self.region_bdry+1:self.edit.edit_pos]
                clipboard.append(cp_text)
                self.edit.set_edit_text(
                    self.edit.get_edit_text()[:self.region_bdry]
                    +
                    self.edit.get_edit_text()[self.region_bdry+1:])
                self.region_bdry = None
                return
            elif key in ["ctrl w",]:
                if self.region_bdry is None:
                    return
                if self.edit.edit_pos <= self.region_bdry:
                    cut_text = self.edit.get_edit_text()[
                        self.edit.edit_pos:self.region_bdry]
                    self.edit.set_edit_text(
                        self.edit.get_edit_text()[:self.edit.edit_pos]
                        +
                        self.edit.get_edit_text()[self.region_bdry+1:])
                else:
                    cut_text = self.edit.get_edit_text()[
                        self.region_bdry+1:self.edit.edit_pos]
                    self.edit.set_edit_text(
                        self.edit.get_edit_text()[:self.region_bdry]
                        +
                        self.edit.get_edit_text()[self.edit.edit_pos:])
                    self.edit.set_edit_pos(self.region_bdry)
                clipboard.append(cut_text)
                self.region_bdry = None
                return
            elif key in ["ctrl y",]:
                if clipboard:
                    self.edit.insert_text(itlz.last(
                        clipboard))
                return
            elif key in ["ctrl u",]:
                try:
                    clipboard.pop()
                except IndexError:
                    pass
                return
            return super(EditBox, self).keypress(size, key)


    edit_box = EditBox(
        "top",
        edit_text=edit_text,
    )
    if second_edit_text is not None:
        second_edit_box = EditBox(
            "top",
            edit_text=second_edit_text,
        )
        main_loop_box = urwid.Columns([
            urwid.LineBox(
                edit_box,
            ),
            urwid.LineBox(
                second_edit_box,
            ),
        ])
    else:
        second_edit_box = None
        main_loop_box = urwid.LineBox(
            edit_box,
        )
    ###
    def unhandled_keypress(k):
        #
        if k in ["f10",]:
            raise urwid.ExitMainLoop()
        elif k in ["f9",]:
            edit_text_terminal_urwid.exit_flags.append("save")
            raise urwid.ExitMainLoop()
        ###
    #
    loop = urwid.MainLoop(
        main_loop_box,
        unhandled_input=unhandled_keypress,
    )
    loop.run()
    exit_flags = edit_text_terminal_urwid.exit_flags
    edit_text_terminal_urwid.clipboard = []
    edit_text_terminal_urwid.exit_flags = []
    return {
        'text': edit_box.edit.get_edit_text(),
        'second_text': (None if second_edit_box is None
                        else second_edit_box.edit.get_edit_text()),
        'exit_flags': exit_flags,
        'save': 'save' in exit_flags,
    }
edit_text_terminal_urwid.clipboard = []
edit_text_terminal_urwid.exit_flags = []


def edit_file_terminal(filename):
    filename = os.path.realpath(filename)
    with open(filename) as f:
        text_before = f.read()
    res = edit_text_terminal_urwid(text_before)
    text_after = res['text']
    with open(filename, 'w') as f:
        f.write(text_after)


# todo:
# * sieve/filter (name, time, etc.)
# * select-multiple/highlight items ['<0>']
# * optionally return filtered or selected list
# * scrollbar & mouse click to select
# retain cat-v-style minimalism, and
# implement separate mc clone otherwise.
def simple_file_browser_urwid(dirname='.'):
    if not pth.isdir(dirname):
        raise ValueError("not a directory",
                         (dirname,))
    ###
    list_walker = urwid.SimpleFocusListWalker([])
    def list_walker_modified_callback(
            list_walker, focus):
        for idx, ent in enumerate(list_walker):
            if isinstance(ent, urwid.AttrWrap):
                list_walker[idx] = ent.get_w()
        sel = list_walker[focus]
        hl_sel = urwid.AttrWrap(sel, 'focus')
        list_walker[focus] = hl_sel
    #
    def set_curr_dir(list_walker, name):
        simple_file_browser_urwid.curr_dir = (
            pth.abspath(name))
        filelist = (
            ['./', '../',] +
            [filename + (
                '/' if pth.isdir(
                    pth.join(name, filename))
                else '') for filename
             in os.listdir(name)]
        )
        list_walker.clear()
        list_walker += [
            urwid.Text(text) for text in filelist]
        list_walker_modified_callback(list_walker, 0)
    #
    set_curr_dir(list_walker, dirname)
    list_walker.set_focus_changed_callback(
        partial(list_walker_modified_callback,
                list_walker))
    def unhandled_keypress(key):
        if key in ["f10", "ctrl q"]:
            raise urwid.ExitMainLoop()
        elif key in ["enter"]:
            name, _ = (list_walker[list_walker.focus]
                       .get_w()
                       .get_text())
            if name[-1] == '/':
                set_curr_dir(
                    list_walker,
                    pth.join(
                        simple_file_browser_urwid.curr_dir,
                        name,
                    )
                )
        elif key in ["ctrl n"]:
            if list_walker.focus < len(list_walker) - 1:
                list_walker.focus += 1
        elif key in ["ctrl p"]:
            if list_walker.focus > 0:
                list_walker.focus -= 1
    ###
    listbox = urwid.ListBox(list_walker)
    view = urwid.Frame(listbox)
    palette = (
        ('focus', 'light gray', 'dark blue', 'standout'),
    )
    loop = urwid.MainLoop(
        view,
        palette,
        unhandled_input=unhandled_keypress,
    )
    loop.run()
    return pth.join(
        simple_file_browser_urwid.curr_dir,
        (list_walker[list_walker.focus]
         .get_w()
         .get_text())[0],
    )
simple_file_browser_urwid.curr_dir = None


def python_viewer_urwid(src):
    """ stepping-stone towoard src editor:
    use AST in parallel with text.
    - syntax highlighting
    - folding
    - goto definition
    - find occurences
    - opt line no.s
    """
    pass


def read_ics(filename):
    try:
        with zipfile.ZipFile(filename) as zf:
            ics_name = thread_last(
                zf.namelist(),
                (filter, compose_left(pth.splitext, ig(-1), partial(op.eq, ".ics"))),
                excepts((StopIteration,), first, handler=lambda _: None),
            )
            with zf.open(ics_name) as f:
                cal = ics.Calendar(f.read().decode("utf-8"))
    except zipfile.BadZipFile:
        with open(filename) as f:
            cal = ics.Calendar(f.read())
    return cal


# todo: timezone-aware
def ics_cal_busy_times_this_week(cal, disp=False):
    today = dt.date.today()
    mon = today - timedelta(days=today.weekday())
    if today.weekday() in [5, 6]:
        mon += timedelta(days=7)
    # mon = t.combine(mon, dt.time())
    fri = mon + timedelta(days=4)
    events_this_week = [
        ev for ev in cal.events if mon <= ev.begin.datetime.date() <= fri
    ]
    busy_times_this_week = [
        (ev.begin.datetime, ev.end.datetime) for ev in events_this_week
    ]
    busy_times_this_week.sort(key=ig(0))
    busy_times_by_day = groupby(
        compose(lambda d: d.date().weekday(), ig(0)), busy_times_this_week
    )
    day_abbrevs = ["M", "Tu", "W", "Th", "F"]
    day_abbrev_to_busy_times = dict(
        zip(
            day_abbrevs,
            get(list(range(len(day_abbrevs))), busy_times_by_day, default=[]),
        )
    )
    if not disp:
        return day_abbrev_to_busy_times
    for (day_abbrev, busy_times) in day_abbrev_to_busy_times.items():
        print(
            (
                day_abbrev
                + " "
                + ", ".join(
                    [
                        (
                            busy_time[0].strftime("%H:%M")
                            + "-"
                            + busy_time[1].strftime("%H:%M")
                        )
                        for busy_time in busy_times
                    ]
                )
            )
        )




def mouse_only_ui(cmd_arg):
    cmd = (shlex.split(cmd_arg)
            if isinstance(cmd_arg, (str,))
            else cmd_arg)
    if not isinstance(cmd, (list,)):
        raise ValueError()
    #
    app_state = {}
    def stdio_reader(file_obj_name):
        if file_obj_name not in ['stdout', 'stderr']:
            raise ValueError()
        proc = app_state['proc']
        file_obj = getattr(proc, file_obj_name)
        for line in iter((file_obj.readline
                          if file_obj else lambda: b""), b""):
            app_state[file_obj_name].append(
                line.decode("utf-8"))

    def on_go():
        if 'proc' in app_state and app_state['proc'].poll() is None:
            return
        proc = Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        app_state['proc'] = proc
        # workaround "pipe buffer deadlock"
        # also consider
        # * .communicate() with a future (?)
        # * passing a file object to stdout and stderr
        app_state['stdout'] = []
        app_state['stderr'] = []
        app_state['reader_threads'] = [
            threading.Thread(
                target=stdio_reader,
                args=('stdout',),
            ),
            threading.Thread(
                target=stdio_reader,
                args=('stderr',),
            ),
        ]
        for t in app_state['reader_threads']:
            t.start()

    def on_stop():
        if 'proc' not in app_state:
            return
        proc = app_state['proc']
        #
        for sig in [None, signal.SIGINT,
                signal.SIGKILL,
                signal.SIGTERM,]:
            if sig is not None:
                proc.send_signal(sig)
            if sig is None:
                if proc.poll() is None:
                    continue
                else:
                    break
            try:
                if sig is not None:
                    proc.wait(timeout=3)
            except subprocess.TimeoutExpired as ex:
                continue
            break
        else:
            raise RuntimeError("process didn't exit")
        #
        for t in app_state['reader_threads']:
            t.join()
        if proc.poll() != 0:
            print("error exit")
            print("stdout")
            print(('\n'.join(app_state['stdout'])
                   if app_state['stdout']
                   else "<None>"))
            print("stderr")
            print(('\n'.join(app_state['stderr'])
                   if app_state['stderr']
                   else "<None>"))
        del app_state['proc']
        del app_state['reader_threads']
        del app_state['stdout']
        del app_state['stderr']
    ###
    # ui
    root = tkinter.Tk()
    left_frame = tkinter.Frame(
            root,
#            relief="raised",
#            color="#200",
            )
    left_frame["bg"] = "purple"
    right_frame = tkinter.Frame(
            root,
            )
    right_frame["bg"] = "pink"
    left_frame.pack(
            side="left",
            fill="both",
            anchor="center",
            expand=True,
            )
    right_frame.pack(
            side="right",
            fill="y",
            )
    ##
    # https://stackoverflow.com/questions/42579927/rounded-button-tkinter-python
    # suggests using canvas.
    # elsewhere suggested to use image/bitmap
    class RoundButton(tkinter.Button, tkinter.Canvas):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            # self.create_line(0, 0, 10, 10)
            self.create_line(*self.bbox("all"))
        #
        def _configure(self, *args, **kwargs):
            rv = super()._configure(*args, **kwargs)
            breakpoint()
            return rv
    ##
    go_btn = tkinter.Button(
            left_frame,
            text="Go!",
            command=on_go,
            relief="raised",
            )
    go_btn["bg"] = "#082"
    stop_btn = tkinter.Button(
            right_frame,
            text="stop",
            command=on_stop,
            )
    stop_btn["bg"] = "#086"
    go_btn.pack(
            side="bottom",
            # expand=True,
            fill="both",
            )
    stop_btn.pack(
            side="top",
            )
    root.mainloop()





def rope_move_fn_from_pythonrc(fn_names, pyutils_pkg_name):
    if '.' in pyutils_pkg_name:
        raise ValueError()
    pyutils_rootdir = pth.dirname(pyutils.__file__)
    pythonrc_rootdir = pth.dirname(pyutils_rootdir)
    pyutils_project = RopeProject(pyutils_rootdir)
    pythonrc_project = RopeProject(pythonrc_rootdir)
    #
    pyutils_pkg_specifier = 'pyutils.'+pyutils_pkg_name
    if pythonrc_project.find_module(pyutils_pkg_specifier) is not None:
        raise Exception("create module unimpl")
    new_pkg = rope.contrib.generate.create_package(pythonrc_project, pyutils_pkg_specifier)
    #

    fn_name = fn_names[0]
    #

    dest_pkg = new_pkg

    pythonrc_module = pythonrc_project.get_module('pythonrc.py')

    move_offset = pythonrc_project.get_resource('pythonrc.py').read().index(fn_name)
    pythonrc_ast = astor.code_to_ast(pythonrc_project.get_resource('pythonrc.py').read())

    breakpoint()

    move_obj = rope.refactor.move.create_move(
        pythonrc_project,
        pythonrc_module,
        move_offset,
    )
    if not isinstance(move_obj, (rope.refactor.move.MoveGlobal,)):
        raise RuntimeError()
    changes = move_obj.get_changes(dest_pkg)


    changes_description = changes.get_description()
    print(changes_description)

    breakpoint()

    # pythonrc_project.do(changes)




currfn = lambda: rope_move_fn_from_pythonrc([
    'ics_cal_busy_times_this_week',
    'read_ics',
], 'calendar')


# todo: use `del` to unclutter locals()


#########################################

def thread_loop_demo():


    mock_op = MagicMock()
    mock_op.side_effect = lambda _: random.random() > 0.5


    def thread_entry(thread_queue):
        pass


    thread_queue = queue.Queue()
    loop_thread = threading.Thread(target=thread_entry, args=(thread_queue,), daemon=True)
    loop_thread.start()
    for idx in range(10):
        thread_queue.put({"idx": idx})