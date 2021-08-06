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
* conn gg Photos.
* droplet / bucket / gcloud
  |_--< low-Trust of thumb drives AND spinning
  ..---++intro to req., here <<<EO_ThHyerOrdTHOT
    prioritizing (FOT)
        connect generic stor (DO droplet)
           **after** _or_ **before**
        domain-specific stor (gg Photos)
EO_ThHyerOrdTHOT
* lint, gofmt-alike, trailing whitespace
  - pylintrc
  - pylint ASTeroid checker(s), if only for example
  - pre-commit
* auto-generate documentation
  - sphinx (?)
  - generate todos from this markdown format
* maintain and/or generate breakpoints list
  |_--> keep a bunch && toggle, don't waste
        time [staring at the computer, typ-
        -ing `breakpoint()` over && over.
  - also, cram a bunch of data into `tabulate`
    and inject a special variable into pdb.
* music player
  - "lock" mode: only responsive on specific keypress
    ( or sequence thereof) - displaying sequnce
    on other key.  thisisnot a security feature.
  - read metadata
  - access & cache album art, credits, etc.
    from wikipedia, allmusic, bandcamp, etc.
  - qtile integration (volume, disp toggle)
--- +eFmt(s)
  - annot playback (eg Tuple[timestamp...])
    (ie "verse|chorus|bridge", "measures
     _ to _") within an indiv piece
  - Dict[float, _]: playback speed to (eg) cpu usage
  - mix *at most* two (Th) << multiproc
    (eg "i can play [dylan|hendrix]'s _
     over _.  same song" 19',20' phenomenon)
    (ie global annotations)
  - equalizer & effects
* mic access
* camera access
* tab-complete python names (w/ rope)
  in at least one editor
* plivo (or twilio fallback) text msgs
* image view & editing
  - scaled .
* document view & editing
  - 4-in-1 pdf
  - pdf text extraction (non-OCR)
* generative music / ambient noise
  - binaural beats-type
  - bytebeat
  - etc.
*
......onecet an py-chemist has
     .... <<will i want more>>
/\//____/\/\//\\/\///\/\/\\/\//\/
../  |   /\  |\  |> get other phone
\/|\ |  /--\ | \ |droid (from storage)
/\| \| /    \|  \|> install android build
西 \ |                  on a laptop
   \_|__            (fuschia, 茹果渴能)

and leave this
  to   bury
"""
import typing
from typing import (
    Dict,
    List,
    Hashable,
    Tuple,
    Union,
    Sequence,
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

import ast

import socket
import ipaddress

import rlcompleter
import readline

### END stdlib imports import stdlib

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


def getsourcefiles(*args, **kwargs):
    getsource_results = [getsource(arg, **kwargs)
                         for arg in args]
    sources_string = (
        '#<<<\n'+'\n#<<<\n'.join(getsource_results)
    )
    pydoc.pager(
        sources_string
    )


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

AUTO_DBG=False


def my_except_hook(exctype, value, traceback):
    if exctype is KeyboardInterrupt:
        print("see you later!")
    sys.__excepthook__(exctype, value, traceback)
    if AUTO_DBG:
        # breakpoint()
        import pdb
        pdb.pm() # post-mortem

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
        from rope.base import libutils as rope_libutils
        from rope.base.project import (
            Project as RopeProject,
        )
        from rope.base.resources import (
            File as RopeFile,
            Folder as RopeFolder,
            Resource as RopeResource,
        )
        from rope.base.pyobjectsdef import (
            PyModule as RopePyModule,
        )
        from rope.base.codeanalyze import (
            SourceLinesAdapter as RopeSourceLinesAdapter,
        )
        from rope.base.change import (
            ChangeSet as RopeChangeSet
        )
        import rope.refactor.move
        from rope.refactor.move import (
            MoveGlobal as RopeMoveGlobal,
            MoveModule as RopeMoveModule,
        )
        import rope.refactor.multiproject
        import rope.contrib.generate
        import astor

        from libqtile import qtile

        sys.path.append(os.path.dirname(os.path.realpath(__file__)))
        import pyutils
        from pyutils.file_browse import (simple_file_browser_urwid,)
        from pyutils.calendar import (ics_cal_busy_times_this_week,)
        import pyutils.pyjuke as juke
        from pyutils.pastebin import pastebin_app
        from pyutils.cartography.osm import (
            osm_to_shp,
            plot_osm_shp,
        )
        from pyutils.cartography.test_osm import (
            TEST_MAP as TEST_OSM_MAP
        )
        from pyutils.py_alarm_call.dashbd import (
            dashbd,
        )
        from pyutils import cookbook as cb
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

plot_osm = plot_open_street_map_xml = compose(plot_osm_shp, osm_to_shp)
plot_osm_demo = lambda: plot_osm(TEST_OSM_MAP)



sfbrow_ur = simple_file_browser_urwid


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
    # pyutils_project = RopeProject(pyutils_rootdir)
    pythonrc_project = RopeProject(pythonrc_rootdir)
    #
    pyutils_pkg_specifier = 'pyutils.'+pyutils_pkg_name

    def _close():
        pythonrc_project.close()

    def _ensure_pyutils_pkg(rope_project) -> RopeFolder:
        found_pkg: RopeFolder = rope_project.find_module(pyutils_pkg_specifier)
        if found_pkg is None:
            new_pkg: RopeFolder = rope.contrib.generate.create_package(
                rope_project, pyutils_pkg_specifier)
            pyutils_pkg = new_pkg
        else:
            pyutils_pkg = found_pkg
        assert isinstance(pyutils_pkg, (RopeResource,))
        assert pyutils_pkg.name == pyutils_pkg_name
        assert pyutils_pkg.is_folder()
        name_to_pkg_file = {f.name: f for f in pyutils_pkg.get_files()}
        assert '__init__.py' in name_to_pkg_file
        init_py: RopeFile = name_to_pkg_file['__init__.py']
        return pyutils_pkg


    def _str_to_ast(fstr, fname):
        """ astor.code_to_ast.parse_file extract """
        fstr = fstr.replace('\r\n', '\n').replace('\r', '\n')
        if not fstr.endswith('\n'):
            fstr += '\n'
        return ast.parse(fstr, filename=fname)


    def _rope_file_to_ast(rope_file: RopeFile) -> ast.Module:
        return _str_to_ast(rope_file.read(), rope_file.name)


    def _rope_module_fn_bounds(rope_module: RopePyModule, func_name: str) -> ast.Module:
        module_ast = rope_module.get_ast()
        name_to_function_def: Dict[str, ast.FunctionDef] = {
            node.name: node for node in module_ast.body
            if isinstance(node, (ast.FunctionDef,))
        }
        if func_name not in name_to_function_def:
            raise ValueError()
        function_def = name_to_function_def[func_name]
        bounds = {attr_name: getattr(function_def, attr_name)
                  for attr_name in
                  ['lineno', 'end_lineno',
                   'col_offset', 'end_col_offset']}
        module_lines_adapter: RopeSourceLinesAdapter = rope_module.lines
        file_lines = [module_lines_adapter.get_line(lineno)
                      for lineno in range(module_lines_adapter.length())]
        char = module_lines_adapter.get_line_start(
            function_def.lineno
        ) + function_def.col_offset
        end_char = module_lines_adapter.get_line_start(
            function_def.end_lineno
        ) + function_def.end_col_offset
        bounds.update(char=char, end_char=end_char)
        return bounds


    dest_pkg = _ensure_pyutils_pkg(pythonrc_project)
    pythonrc_module: RopePyModule = pythonrc_project.get_module('pythonrc')
    pythonrc_resource: RopeFile = pythonrc_module.get_resource()

    def _make_move_obj_for_one_fn(fn_name):
        fn_bounds = _rope_module_fn_bounds(pythonrc_module, fn_name)
        if fn_bounds['col_offset'] != 0:
            Exception("expected a top-level function", (fn_name, fn_bounds,))
        move_offset = fn_bounds["char"] + len('def ')


        if not isinstance(
                rope.refactor.move.create_move(
                    pythonrc_project,
                    pythonrc_module.get_resource(),
                    move_offset,
                ), (
            RopeMoveGlobal,
        )):
            raise RuntimeError()

        from rope.refactor.move import _ChangeMoveOccurrencesHandle
        from rope.refactor import occurrences
        from rope.refactor.move import ModuleSkipRenamer
        from rope.base import libutils
        from rope.refactor import importutils
        from rope.base.change import ChangeContents
        class MoveGlobalKeepSrcImports(RopeMoveGlobal):

            def _source_module_changes(self, dest):
                placeholder = '__rope_moving_%s_' % self.old_name
                handle = _ChangeMoveOccurrencesHandle(placeholder)
                occurrence_finder = occurrences.create_finder(
                    self.project, self.old_name, self.old_pyname)
                start, end = self._get_moving_region()
                renamer = ModuleSkipRenamer(occurrence_finder, self.source,
                                            handle, start, end)
                source = renamer.get_changed_module()
                pymodule = libutils.get_string_module(self.project, source, self.source)
                #~ source = self.import_tools.organize_imports(pymodule, sort=False)
                if handle.occurred:
                    pymodule = libutils.get_string_module(
                        self.project, source, self.source)
                    # Adding new import
                    source, imported = importutils.add_import(
                        self.project, pymodule, self._new_modname(dest), self.old_name)
                    source = source.replace(placeholder, imported)
                return ChangeContents(self.source, source)


        return MoveGlobalKeepSrcImports(
            pythonrc_project,
            pythonrc_module.get_resource(),
            move_offset,
        )


    my_changes = None
    for fn_name in fn_names:
        my_move_obj = _make_move_obj_for_one_fn(fn_name)
        fn_changes: RopeChangeSet = my_move_obj.get_changes(dest_pkg)
        if my_changes is None:
            my_changes = fn_changes
        else:
            my_changes.add_change(fn_changes)

    my_changes_description = my_changes.get_description()


    validate_src_res = pythonrc_project.validate(pythonrc_module.get_resource())
    validate_dest_res = pythonrc_project.validate(dest_pkg)
    validate_project_res = pythonrc_project.validate(pythonrc_project.root)

    if (validate_src_res is not None or
        validate_dest_res is not None or
        validate_project_res is not None):
        raise Exception("validation fail")

    ##
    input('take a moment to view the changes')
    pydoc.pager(my_changes_description)
    proceed = None
    while proceed is None:
        proceed_txt = input("perform the move? >[yes/no]>> ").strip()
        if proceed_txt not in ['yes', 'no',]:
            print("please answer 'yes' or 'no'")
            continue
        proceed = proceed_txt == 'yes'
    if not proceed:
        _close()
        return

    pythonrc_project.do(my_changes)

    _close()
    return
    fn_ast: ast.FunctionDef = astor.code_to_ast(globals()[fn_name])
    pythonrc_ast: ast.Module = astor.code_to_ast.parse_file(
        pythonrc_resource.real_path)

    fns_changes = [_make_changeset_for_one_fn(fn_name)
                  for fn_name in fn_names]
    all_changes = reduce(lambda a, b: (a.add_change(b) or True) and a, fns_changes)



mv_fn = rope_move_fn_from_pythonrc

###

class NetmapError(Exception):
    pass


class NetmapParseError(ValueError, NetmapError):
    pass


def _get_own_ip():
    raise NotImplementedError()


def _is_port_open(
        ip_addr,
        port_no,
) -> bool:
    addr = (str(ip_addr), port_no,)
    try:
        conn = socket.create_connection(addr)
    except ConnectionRefusedError:
        return False
    conn.close()
    return True #...#
    sock = socket.socket(
        socket.AF_INET,
        socket.SOCK_STREAM | socket.SOCK_NONBLOCK)
    sock.connect(addr)


def _is_port_open_direct_lib(
        ip_addr,
        port_no,
) -> bool:
    addr = (str(ip_addr), port_no,)
    try:
        conn = socket.create_connection(addr)
    except ConnectionRefusedError:
        return False
    conn.close()
    return True #...#
    sock = socket.socket(
        socket.AF_INET,
        socket.SOCK_STREAM | socket.SOCK_NONBLOCK)
    sock.connect(addr)



def _port_scan_single_host(host, ports,
                           scan_method='high-lvl',):
    scan_method__create_connection = ['high-lvl', 'low-lvl',
                           'hi', 'lo',
                           'create_connection', 'direct_lib']
    scan_method__direct_lib = ['high-lvl', 'low-lvl',
                           'hi', 'lo',
                           'create_connection', 'direct_lib']
    assert scan_method in ['high-lvl', 'low-lvl',
                           'hi', 'lo',
                           'create_connection', 'direct_lib']
    rv = {}
    for port in ports:
        print("scanning port", port)
        single_port_res: bool = _is_port_open_direct_lib(host, port)
        single_port_res: bool = _is_port_open(host, port)

        rv.update({port: single_port_res})
    return rv


def port_scan(host_or_hosts, ports: List[int]):

    def _parse_host(maybe_host):
        try:
            host = str(ipaddress.ip_address(str(host_or_hosts)))
        except Exception as err:
            breakpoint() # get exception class
            raise NetmapParseError("", (err,))
        return host


    def _parse_hosts(hosts):
        if isinstance(hosts, (list, tuple, iter)):
            return [_parse_hosts(host)
                    for host in hosts]
        if isinstance(hosts, (str,)):
            pass


    def _coerce_ports(port_or_ports):
        ports = None
        if isinstance(port_or_ports, int):
            ports = [port_or_ports]
        if isinstance(port_or_ports, (list, tuple, set,)):
            ports = list(port_or_ports)
            if not all([isinstance(p, int) for p in ports]):
                raise NetmapParseError(
                    "recursive port coercion unimpl"
                )
        if ports is None:
            raise NetmapParseError()
        return ports


    single_host = None
    try:
        single_host = _parse_host(host_or_hosts)
    except NetmapParseError:
        pass
    if single_host is not None:
        hosts = [single_host]
    else:
        hosts = _parse_hosts(host_or_hosts)
    rv = {}
    for host in hosts:
        print("scanning host", host)
        single_host_res = _port_scan_single_host(host,
                                                 # ports,
                                                 _coerce_ports(ports),
                                                 )
        rv.update(**{str(host): single_host_res})

    breakpoint()

    return rv


def is_pastebin_up():
    port_scan(_get_own_ip(), [5003, 5005])


currfn = lambda: _is_port_open('192.168.1.64', 5005)
currfn = lambda: port_scan('192.168.1.64', 5005)
def _mount_unmounted_usb_thumb__freebsd():
    raise NotImplementedError()

###

import subprocess as sp
def _mount_unmounted_usb_thumb():
    if (sp.run('uname', stdout=sp.PIPE, check=True).stdout.decode('utf-8').strip()
        !=
        'FreeBSD'):
        raise NotImplementedError()
    return _mount_unmounted_usb_thumb__freebsd()

mount_usb = lambda: _mount_unmounted_usb_thumb


def _man_viewer__freebsd(name, sec=1):
    raise NotImplementedError()


def man_viewer():
    """
    interactive, follow links, ManManPages, ....
    """
    if (sp.run('uname', stdout=sp.PIPE, check=True).stdout.decode('utf-8').strip()
        !=
        'FreeBSD'):
        raise NotImplementedError()
    __man_viewer__freebsd()


#########################################


from time import (
    sleep as sync_sleep,
)
def make_it_BEEEp():
    def _beep_once():
        warn("beep unimpl")
        # terminal bell
    while True:
        _beep_once()
        sync_sleep(1)



currdefns = {defn.__name__: defn for defn
             in [
                 python_viewer_urwid,
                 make_it_BEEEp,
                 mount_usb,
                 ]}



for varname in [
        'mv_fn',
        ]:
    pass


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
