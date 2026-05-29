"""
   simple time-boxing and -estimation setup

Example:
    add_time_estimate_task('clean room', 30) # 30 mintues
    print_time_estimates()
    do_time_estimated_task('clean room')

### todo:
* persistent dict (to disk)
* calc time estimate sums
"""
import sys
import shlex
import math
import time
import subprocess
import threading
from warnings import warn
from subprocess import Popen
from typing import (Union, Sequence,)
##>>>##


# activity to minutes
_TIME_ESTIMATE = {}
_PROG_BAR_STEPS = 10

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



def run_1(cmd: Union[str, Sequence], timeout=None, stream=sys.stdout, err_stream=sys.stderr, input_str=None,):

    _cmd_before_coerce = cmd #dbgref
    if isinstance(cmd, (str,)):
        warn("run_1 coerce untested")
        cmd = shlex.split(cmd)
    if not isinstance(cmd, (list,)):
        cmd = list(cmd)


    def input_writer(proc, input_lines):
        for input_line in input_lines.split('\n'):
            input_line_bytes = bytes(input_line, 'utf-8')
            proc.stdin.writelines(
                [input_line_bytes]
            )
        proc.stdin.close()

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

    proc_kwargs = {}
    if input_str is not None:
        proc_kwargs['stdin'] = subprocess.PIPE
    proc = Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, **proc_kwargs)
    tout = threading.Thread(target=output_reader, args=(proc,))
    terr = threading.Thread(target=err_reader, args=(proc,))
    if input_str is not None:
        tin = threading.Thread(target=input_writer, args=(proc,input_str,))
        tin.start()
    tout.start()
    terr.start()
    proc.wait(timeout=timeout)
    tout.join()
    terr.join()
    if input_str is not None:
        tin.join()



def _espeak_text(
        text,
        vol=85,
        disp=True,
        async_run=False,
):
    if disp:
        rainbow_print(text)
    cmd = ['espeak',
           '-v', 'f4',
           '-p', '135',
           '-a', str(vol),
           ]
    if async_run:
        run_1(
            cmd,
            input_str=text,
        )
    else:
        subprocess.run(
            cmd,
            input=bytes(text, 'utf-8'),
        )


def _xmessage_text(
        text,
        disp=True,
        speak=True,
):
    if disp:
        rainbow_print(text)
    if speak:
        _espeak_text(
            text,
            async_run=True,
        )
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


def _get_finished():
    finished = None
    while finished is None:
        finished_txt = input("did you finish??? >[yes/no]>> ").strip()
        if finished_txt not in ['yes', 'no',]:
            print("please answer 'yes' or 'no'")
            continue
        finished = finished_txt == 'yes'
    return finished


def _handle_finished(finished, key):
    if finished:
        del _TIME_ESTIMATE[key]
        # todo: pos/neg-reinforcement
    else:
        _TIME_ESTIMATE[key] = None
        print("your time-estimate has been discarded.  "
              # ^ irl stuff.
              "to add a new estimate:")
        print("add_time_estimate_task('"+key+"')")


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
        warn_xm_proc = None
        key = key_or_ks
        act = key
        try:
            est_min = _TIME_ESTIMATE[key]
        except KeyError:
            print("unknown activity '"+act+"'")
            return
        if est_min is None:
            print("no time estimate for '"+act+"'")
        elif not isinstance(est_min, (int, float,)):
            raise ValueError(
                "nested activities unimplemented",
                (est_min,))
        msg_begin = "take a soft "+str(est_min)
        rainbow_print(msg_begin)
        if not mute:
           for _ in range(3):
                _espeak_text("Beginner's mind concerning "+act)
                time.sleep(4)
        sec_wait = float(60.*(float(est_min)-float(min_warning)))
        try:
            if sec_wait > 0:
                step_sec_wait = sec_wait/float(_PROG_BAR_STEPS)
                for _ in range(_PROG_BAR_STEPS):
                    # print("waiting "+str(step_sec_wait), file=sys.stderr,)
                    time.sleep(step_sec_wait)
                    print('.', end='')
                    sys.stdout.flush()
                print()
                msg_warn = ("you have "+str(min_warning)+" minutes left to finish "+
                            act)
                rainbow_print(msg_warn)
                if not mute:
                    _espeak_text(msg_warn)
                warn_xm_proc = _xmessage_text(msg_warn)
                sec_wait = 60.*min_warning
            else:
                sec_wait = float(60.*float(est_min))
            step_sec_wait = sec_wait/float(_PROG_BAR_STEPS)
            for _ in range(_PROG_BAR_STEPS):
                time.sleep(step_sec_wait)
                print('.', end='')
                sys.stdout.flush()
            print()
        except KeyboardInterrupt:
            print()
            if warn_xm_proc:
               warn_xm_proc.terminate()
               warn_xm_proc.wait()
            _handle_finished(_get_finished(), key)
            return
        msg_end = ("time's up!  your estimate of "+str(est_min)+" minutes "
                   "to completely complete "+act+" has elapsed. ")
        rainbow_print(msg_end)
        if not mute:
            _espeak_text(msg_end)
        end_xm_proc = _xmessage_text(msg_end)
        if warn_xm_proc:
            warn_xm_proc.terminate()
            warn_xm_proc.wait()
        finished = _get_finished()
        end_xm_proc.terminate()
        end_xm_proc.wait()
        _handle_finished(finished, key)
    elif isinstance(key_or_ks, (list,)):
        ks = key_or_ks
        raise ValueError(
            "nested activities unimplemented",
            (ks,))
    else:
        raise RuntimeError(
            "unknown activity specification",
            (key_or_ks, type(key_or_ks),))

def rm_time_estimated_task(*ks):
    key = None
    if len(ks) == 0:
        print("select task")
        print(_TIME_ESTIMATE.keys())
        while key is None:
            perhaps_key = input(">key>")
            if perhaps_key in _TIME_ESTIMATE:
                key = perhaps_key
            else:
                print("unknown key '"+perhaps_key+"'")
    elif len(ks) == 1:
        key = ks[0]
        if not isinstance(key, (str,)):
            raise RuntimeError(
                "keys must be strings",
                (key, type(key),)
            )
    else:
        raise ValueError(
            "nested activities unimplemented",
            (ks,)
        )
    if key is None:
        raise RuntimeError(
            "couldn't determine key to pop",
            (ks,)
        )
    del _TIME_ESTIMATE[key]


def rm_all_time_estimated_tasks():
    for key in _TIME_ESTIMATE.keys():
        del _TIME_ESTIMATE[key]


##<<<##
#todo
##playlist functionality

