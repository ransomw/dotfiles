"""
This is the short end of the stick:
Welcome to your very own [drumroll]

Behavior Modification Stimulus Toolkit

i.e. a package designed to produce stimuli
designed to produce behavior modification.

For starters, here, you will wrap
* espeak
* xmessage
to issue you commands according
to a timer and/or randomness.
You may also want to learn
to add text to pics or videos
to carry the commands.

Most of what will interest you here
is the data structure(s) containing
the commands and how they are issued.

Try to divide Stimuli into
* work
* hobby
* break
 ***
"""

import time
import multiprocessing as mp
import datetime as dt

#
# example of trimming a video
#
# ffmpeg -i ${in}.mp4 -ss 00:00:45 -to 00:02:50 -c:v copy -c:a copy ${out}.mp4
#

#
# example of extracting audio
#
# ffmpeg -i input.mp4 output.wav
#

###
# all this is super-buggy.
# * message timer a one-call that's-all
# * add espeak to message time
# * multimedia kickoffs
# * desktop-switching...
#   ... set a timer, run a task
#    (where tasks are composable)


def _desktop_message(text):
    p = subps.Popen(['xmessage', text])



# todo: consider stdlib threading.Timer
class Timer:
    """
    This is a countdown.
    Call a countup something else,
      a Watch, maybe.
    """

    def __init__(self,
                 dur_min,
                 text='hi there',
                 subps_args=[]):
        self._dur_min = dur_min
        self._text = text
        self._subps_args = subps_args
        self._start = dt.datetime.now()
        self._has_acted = False

    def min_left(self):
        time_delta = (dt.datetime.now() -
                      self._start)
        return (
            time_delta.total_seconds() /
            60.0)

    def poll(self):
        if self._has_acted:
            return 'already'
        if (self.min_left() <
            self._dur_min):
            return 'running'
        if self._text != '':
            _desktop_message(self._text)
        if len(self._subps_args) > 0:
            self._sub_proc = subps.Popen(
                self._subps_args,
            )
        self._has_acted = True
        return 'now'



__timers = []
__timer_heartbeat_proc = None

def set_message_timer(dur_min, text):
    __timers.append(Timer(
        dur_min,
        text=text,
    ))


def _timer_heartbeat_main():
    for timer in __timers:
        action = timer.poll()
    time.sleep(60)


def start_timer_heartbeat():
    if __timer_heartbeat_proc is not None:
        if __timer_heartbeat_proc.is_alive():
            __timer_heartbeat_proc.terminate()
        __timer_heartbeat_proc.join()
    __timer_heartbeat_proc = mp.Process(
        _timer_heartbeat_main,
    )
    __timer_heartbeat_proc.start()



