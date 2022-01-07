"""

### todo:
* _py_watch pass aliased kwargs dbg
  (currently kwargs are hardcoded
   in body of defn)
* `some_res = some_defn()` ->`print(_print_dashbd_runs__out)`/
   |_.--> async stream results: don't wait  <--._|
    on entire results to print before displaying

"""

import time
import sys
import enum
import os
from functools import partial
import subprocess as sp
import shlex
import io
import contextlib

class ProgrammingError(Exception):
    pass

class DashbdRunEnvStyle(enum.Enum):
    NOSET = 1
    SET_RUN_ARGS_ONLY = 2
    SET_OS_AND_RUN_ARGS = 3
    SET_OS_ONLY = 4
    SET_EMPTY = 5


_dashbd_sleep = time.sleep

def _dashbd_warn(msg, slpt=2.0):
    warn(str(msg))
    _dashbd_sleep(slpt)

# dupe[ish]: `s_wifi.subps_util`
_dashbd_run__ = partial(
    sp.run,
    stdout=sp.PIPE,
    check=True,
    # shell=True,
)


# code   ⛼
#⛼ motif: ⛼ clutter namespaces with aliases
# design ⛼


def _dashbd_run(cmd,
                dbg_mode=True,
                # motif: ⛼ aliases
                env={},
                env_add={},
                # ⛼
                env_style=DashbdRunEnvStyle.NOSET,
):
    _aliases_env = [env, env_add]
    if env_style in [
            DashbdRunEnvStyle.SET_OS_ONLY,
    ]:
        y = NotImplemented # what's y? type
        raise NotImplementedError()
    if env_style in [
            DashbdRunEnvStyle.SET_RUN_ARGS_ONLY,
            DashbdRunEnvStyle.SET_OS_AND_RUN_ARGS,
    ] and any(_aliases_env):
        warn("environment variables present "
             "despite env_style preventing "
             "their use")
    os_env = dict(os.environ)
    if env_style == DashbdRunEnvStyle.SET_OS_AND_RUN_ARGS:
        run_env = os_env
    else:
        run_env = dict()
    assert not (set(env.keys())
                .intersection(set(env_add.keys()))), (
        "clashing aliases"
    )
    run_env.update(**env_add)
    run_env.update(**env)
    check=True
    shell=False
    if dbg_mode:
        check = False
        shell = True
    if not shell and isinstance(cmd, (str,)):
        orig_cmd = cmd
        cmd = shlex.split(cmd)
    elif not shell:
        assert (isinstance(cmd, (list,)) and
                all([isinstance(s, (str,)) for s in cmd])), (
                    "instead have command "+repr(cmd)+
                    ("we're in dbg mode, ftr" if dbg_mode else ""))
    _run = partial(
        sp.run,
        stdout=sp.PIPE,
        stderr=sp.PIPE,
        check=check,
        shell=shell,
        **({} if env_style ==  DashbdRunEnvStyle.NOSET
           else ({env: dict()} if
                 env_style ==  DashbdRunEnvStyle.SET_EMPTY
                 else {env: run_env,}))
    )
    res = _run(cmd)
    out = res.stdout.decode('utf-8').strip()
    err = res.stderr.decode('utf-8').strip()
    if err:
        warn("process has stderr data")
        if dbg_mode:
            print(stderr)
            breakpoint()
        else:
            RuntimeError("", (res, stderr,))

    return {
        "_raw": res,
        # ⛼
        # motif: ⛼ clutter namespaces with aliases
        # ⛼
        "out": out,
        "output": out,
        "stdout": out,
        "exit_code": res.returncode,
        "exitcode": res.returncode,
        "return_code": res.returncode,
        "returncode": res.returncode,
        # motif: ⛼ aliases
    }


def _print_one_dashbd_run_str(txt: str):
    w_cnt = sys.stdout.write('\n')
    assert w_cnt == 1
    w_cnt = sys.stdout.write(txt)
    assert w_cnt == len(txt)
    w_cnt = sys.stdout.write('\n')
    assert w_cnt == 1
    sys.stdout.flush()
    return # alternately:
    print(txt)
    return # or more alternately:
    import urwid # see todo in _py_watch before gui


def _print_one_dashbd_run(run_res):
    out: str
    if isinstance(run_res, (dict,)):
        out = run_res["out"]
    elif isinstance(run_res, (str,)):
        out = run_res
    else:
        ValueError()
    # leave padding on left
    # in case of paragraph indent, etc.lint
    txt = out.rstrip()
    _print_one_dashbd_run_str(txt)


_CLS = '\x1bc'
assert '\x1bc' == '\033c', (
    "i've no idea what this "
    "equality means. nor do i "
    "know what 'CLS' stands for."
    "   this ought be said at the"
    " outset.")
def _print_dashbd_runs(
    runs_res,
    subps_print=False, # feeling lucky
):
    if subps_print:
        sp.run('printf "'+_CLS+'"',
               stderr=sp.PIPE,
               shell=True,
               check=True,)
    else:
        sys.stdout.write(_CLS)
    for run_res in runs_res:
        _print_one_dashbd_run(run_res)
    sys.stdout.flush()
    return
    # idea:
    import tabulate
    # or
    import pandas as pd
    # for multi-indexes,
    # such as (time, zone)
    # rather than (battry,) or (memry,) scalars




class _DASHBD_MSG_CONDS(enum.Enum):
    LOW_BATT = 1
    # todo
    # ----
    # HI_MEM = 3
    # HI_MEM_OVER_TIME = 4
    # HI_CPU = 5
    # HI_CPU_OVER_TIME = 5
    # HI_TEMP = 6



class _DASHBD_SUBPROC_GRANULARITY(enum.Enum):
    FINE = 1
    MEDIUM = 3
    CORSE = 2


_GRAINS = _DASHBD_SUBPROC_GRANULARITY


def _freebsd_dashbd__once(
        # grain=_GRAINS.FINE,
        # grain = _GRAINS.MEDIUM,
        grain = _GRAINS.CORSE,
):
    corse_runs_res = []
    medium_runs_res = []
    fine_runs_res = []

    if grain == _GRAINS.MEDIUM:
        pass
    if grain == _GRAINS.FINE:
        pass
    if grain == _GRAINS.CORSE:
        pass

    ##
    # battery

    if grain == _GRAINS.CORSE:
        run_res = run_res__battry = _dashbd_run(
            'apm -l |tr -d "\n" && echo -n "% battery"'
        )
        corse_runs_res.append(run_res)
    else:
        _dashbd_warn(NotImplementedError())
        run_res = run_res__battry = _dashbd_run(
            'apm -l |tr -d "\n" && echo -n "% battery"'
        )
        medium_runs_res.append(run_res)
        fine_runs_res.append(run_res)


    ##
    # time

    _space_around_personal_meaning = 4

    _pm_space = ' '*_space_around_personal_meaning

    if grain == _GRAINS.CORSE:
        # TZ env var to date is relpath to /usr/share/zoneinfo/ -- clocks
        run_res = run_res__all_clocks = _dashbd_run(
        f"""echo 'clocks' &&\
            echo -n "{_pm_space}LA/SF/Bonanza{_pm_space}" && TZ=America\/Los_Angeles date &&\
            echo -n "{_pm_space}Mississippi{_pm_space}" && TZ=America\/Chicago date
        """
        )
        corse_runs_res.append(run_res)


    ## aside ...
    "LA/SF/Bonanza"
    # ??? emacs python-mode "split-string"
    #  a-la paredit-mode `paredit-split-sexp`
    {"TZ=America\/Los_Angeles"}
    {"TZ": "America\/Los_Angeles",}
    # .. aside END.
    # END aside.

    # TZ env var to date is relpath to
    #     /usr/share/zoneinfo/ -- clocks
    personal_meaning_to_tz_envvar = {
        personal_meaning: tz_envvar
        for (personal_meaning, tz_envvar,) in [
                (
                    "LA/SF/Bonanza",
                    'America\/Los_Angeles',
                 ),
                (
                    "Mississippi",
                    'America\/Chicago',
                ),
    ]}
    # todo: ^/\^ to config.yaml|json|conf


    echo_and_date_cmds = []
    personal_meaning_to_run_res = {}
    for (personal_meaning,
         tz_envvar,) in personal_meaning_to_tz_envvar.items():
        if grain == _GRAINS.MEDIUM:
            echo_and_date_cmds.append(
                ('        '
                 f'echo -n "{_pm_space}'
                 f'{personal_meaning}'
                 f'{_pm_space}" && '
                 f'TZ={tz_envvar} date'))
        if grain == _GRAINS.FINE:
            if personal_meaning in personal_meaning_to_run_res:
                raise ProgrammingError()
            env_style=DashbdRunEnvStyle.SET_RUN_ARGS_ONLY
            # env_style=DashbdRunEnvStyle.SET_OS_AND_RUN_ARGS
            personal_meaning_to_run_res[
                personal_meaning
            ] = _dashbd_run('date',
                            dbg_mode=True,
                            env={'TZ': tz_envvar,},
                            env_style=env_style,
                            )


    if grain == _GRAINS.MEDIUM:
        medium_runs_res.append('clocks')
        for echo_and_date_cmd in echo_and_date_cmds:
            run_res = run_res__echo_and_date = _dashbd_run(
                echo_and_date_cmd,
                dbg_mode=True,
            )
            medium_runs_res.append(run_res)
    if grain == _GRAINS.FINE:
        fine_runs_res.append('clocks')
        for pm in personal_meaning_to_run_res:
            rr = personal_meaning_to_run_res[pm]
            fmt='{s}{pm}{s}{date_output}'
            run_res = fmt.format(
                s=_pm_space,
                pm=pm,
                date_output=rr['output'],
            )
            fine_runs_res.append(run_res)


    ##
    # memory usage
    #     pkg install `freecolor`
    # -- memory usage


    # hoisted from ##output for convenience
    _grain_to_runs_res = {
        _GRAINS.MEDIUM: medium_runs_res,
        _GRAINS.FINE: fine_runs_res,
        _GRAINS.CORSE: corse_runs_res,
    }

    if grain in [_GRAINS.CORSE, _GRAINS.MEDIUM]:
        run_res = run_res__echo_n_memry = _dashbd_run(
            """ echo 'memory usage' && freecolor """
        )
        _grain_to_runs_res[grain].append(run_res)
    if grain == _GRAINS.FINE:
        run_res = run_res__memry = _dashbd_run(
            'freecolor')
        fine_runs_res.append(run_res)

# todo: extract executable names,
#     and fail fast OR limit output
# if any given exec isn't installed
# ...
# could also get into the theme of
# "alternates" (like in debian w/
# symlinks) :  don't have one
# program? use another!  this is
# probably related to "namespace
# cluttering"


    ##
    # output

    _print_dashbd_runs(
        _grain_to_runs_res[grain],
        # subps_print=True,
    )

    return


    corse_runs_res = []
    medium_runs_res = []
    fine_runs_res = []

    if grain == _GRAINS.MEDIUM:
        pass
    if grain == _GRAINS.FINE:
        pass
    if grain == _GRAINS.CORSE:
        pass



from toolz.itertoolz import (
    first,
    take,
    drop,
    unique,
    get, # also in toolz.curried
)
import toolz.itertoolz as itlz



def _unique(seq):
    seen = []

    (shead,
     stail) = (
         first(seq), # take(1, seq), # equiv
         drop(1, seq))

    # ??? better way to determine if obj (`shead`)
    #   is hashable?
    guess_hashable = True
    try:
        set([shead])
    except TypeError:
        guess_hashable = False

    if guess_hashable:
        try:
            return list(itlz.unique(seq))
        except TypeError as err:
            if not itlz.get(
                    0, itlz.get(0, err.args, '').split(':'),
                    None) == 'unhashable type':
                raise err

    def _handle_item(item):
        if item not in seen:
            seen.append(item)

    _handle_item(shead)
    for item in stail:
        _handle_item(item)
    return seen


@contextlib.contextmanager
def _patch_stdio(stdin=None,
                 except_stderr=True,):
    """
    could be extended (e.g. to
    return stderr) ... if there's
    not already a lib providing the
    functionality
    """
    if stdin is not None:
        raise NotImplementedError()
    sio_out = io.StringIO()
    stdout_orig = sys.stdout
    sys.stdout = sio_out
    #
    sio_err = io.StringIO()
    stderr_orig = sys.stderr
    sys.stderr = sio_err
    try:
        yield sio_out
    finally:
        sys.stdout = stdout_orig
        sio_out.seek(0)



# i usually use None for a sentinel
# except saw this the other day
# and thought to give it a try
# _snt = sentinel = object()
_snt = None
def _py_watch(some_defn,
              args=_snt, kwargs=_snt,
              some_args=_snt, some_kwargs=_snt,
              n_sec=_snt, n=_snt,
              debug=False, # todo XXX
              # **watch_kwargs # for kwargs to _this_ defn
              ):
    """ `n_sec` and `n` are mutually aliased """
    # more ns crowding
    kass = kwarg_alias_sets = [
        {'aliases': set(['n', 'n_sec',]),
         'default': 2.0,},
        {'aliases': set(['args', 'some_args',]),
         'default': tuple(),},
        {'aliases': set(['kwargs', 'some_kwargs',]),
         'default': dict(),},
    ]
    ##
    # handle kwarg aliases as input.
    # assumptions:
    # *) all kwargs given sentinel value in declaration
    # gaurantees:
    # *) all kwargs get same value -- or ValueError
    for kas in kass:
        ##
        # side-note: locals() is re-bound inside comprehension
        ka_to_v__fail = {ka: locals() for ka in kas['aliases']}
        # so this fails
        # ka_to_v__fail = {ka: locals()[ka] for ka in kas['aliases']}
        # altogether, and
        lk, lic = lic_k, locals_in_comprehension = (
            list(iter(ka_to_v__fail.items()))[0])
        assert set(locals_in_comprehension.keys()) == set(['ka', '.0'])
        assert lic['ka'] in kas['aliases']
        # assert instanceof(lic['.0'], tuple_iterator)
        lic_point_0 = list(lic['.0'])
        assert len(lic_point_0) == 0, (
            "/no/ clear clue what's going on here.. "
            ".. looks like C macro exposed to interpreter, "
            "mebbe?")
        # END side-note
        kas_aliases = list(kas['aliases'])
        ka_to_v = dict()
        for ka in kas_aliases:
            ka_to_v[ka] = locals()[ka]
        vs = _unique(ka_to_v.values())
        if (len(vs) > 2 or
            (len(vs) == 2 and _snt not in vs)):
            raise ValueError("", (ka_to_v, vs,))
        if len(vs) == 1 and _snt in vs:
            v = kas['default']
        else:
            v = [v for v in vs if v != _snt][0]
        assert v != _snt
        for ka in kas:
            locals()[ka] = v

    # sio = io.StringIO()
    # stdout_orig = sys.stdout
    # sys.stdout = sio
    # some_res = some_defn(*args, **kwargs)
    # sys.stdout = stdout_orig

    # arg_lst = list(args)
    arg_lst = args
    if debug:
        print('starting watch of ', some_defn, some_defn.__name__)
        print('without parameters')
        # print('with parameters', arg_lst, 'and', kwargs)
        # if arg_lst:
        #     print('   that\'s args:', list(arg_lst), 'len', len(arg_lst))
        print("papering over arg passing for now")
    n = n_sec= 3 #sec


    try:
        while True:
            if debug:
                print("running function unpatched to test")
            some_res = some_defn()
            if debug:
                if some_res is not None:
                    warn("expected watch functions to not return a value?")
                elif debug:
                    print("got None res", some_res)
            with _patch_stdio() as stdout_buf:
                if debug:
                    print("running watch function")
                # todo: xxx err arg passing
                # some_res = some_defn(*args, **kwargs)
                some_res = some_defn()
                if some_res is not None:
                    warn("expected watch functions to not return a value?")
                elif debug:
                    print("got None res", some_res)
                stdout_buf.seek(0) # necessary, given contextmgr?
                _print_dashbd_runs__out = stdout_buf.read()
                if debug:
                    if _print_dashbd_runs__out:
                        print("read results out")
                    else:
                        print("didn't read results out")
                # todo:
                print(_print_dashbd_runs__out)
            if debug:
                print('sleeping ', n,'seconds')
            _dashbd_sleep(n)
    except KeyboardInterrupt:
        pass

_freebsd_dashbd = (lambda grain=_GRAINS.FINE:
                   _py_watch(_freebsd_dashbd__once,
                             kwargs={'grain': grain,},))

def _linux_dashbd(
        # grain=_GRAINS.FINE,
        # grain = _GRAINS.MEDIUM
        grain = _GRAINS.CORSE,
):
    _dashbd_warn(("linux dashboard untested, unused "
                  "(so far)"))
    if grain != _GRAINS.CORSE:
        _dashbd_warn(NotImplementedError())
    try:
        sp.run(
            'watch -n 60 "acpi ; TZ=America\/Los_Angeles date"',
            shell=True,
        )
    except KeyboardInterrupt:
        pass


def dashbd():
    uname_res = sp.run('uname',stdout=sp.PIPE)
    uname_str = uname_res.stdout.decode('utf-8').strip()
    _id_freebsd = 'FreeBSD'
    if uname_str == _id_freebsd:
        _freebsd_dashbd()
    if 'linux' in uname_str.lower():
        _linux_dashbd()
    else:
        raise RuntimeError("", (uname_str, [_id_freebsd, 'linux',],))


if __name__ == '__main__':
    dashbd()
