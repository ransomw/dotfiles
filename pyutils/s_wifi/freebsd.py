import os
import shlex
from functools import (partial,)
import subprocess as sp

##

_run = partial(sp.run, stdout=sp.PIPE, check=True,)

def _rpo(*args, **kwargs):
    """run process output"""
    rr = _run(*args, **kwargs)
    rv = {}
    rv['out'] = rr.stdout.decode('utf-8')
    rv['stdout'] = rv['out']
    rv['lines'] = rv['out'].strip().split('\n')
    return rv


def _rso(cmd_arg, *args, **kwargs):
    """run standard output"""
    cmd = shlex.split(cmd_arg)
    rr = _run(cmd, *args, **kwargs)
    return rr.stdout.decode('utf-8').strip()


##

def _ensure_os():
    if os.name != 'posix':
        raise ValueError()
    if _rso('uname') != 'FreeBSD':
        raise ValueError()
    release_level = _rso('uname -r')
    # observed in the wild:
    # <version string>-RELEASE-p3
    # uname -v has same info.
    # ??? -r or -v
    vs, _, _ = release_level.split('-')
    major_vs, minor_vs = list(map(int, vs.split('.')))
    if major_vs < 11:
        # no `sysctl`
        raise ValueError()


def wireless_adapter_name():
    sysctl_prop = net.wlan.devices
    o = _rso('sysctl '+sysctl_prop)
    breakpoint()
    if not o:
        return None
    label, name = o.split(': ')
    if label != sysctl_prop or not name:
        raise RuntimeError()
    return name


def ifconfig():
    o = _rso('ifconfig')
    lines = o.split()
    adapter_chunks = {}
    raise NotImplementedError()


