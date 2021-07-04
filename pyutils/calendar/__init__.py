import datetime as dt
from datetime import timedelta
import operator as op
import os.path as pth
import zipfile
from functools import partial
from operator import itemgetter as ig
import ics
from toolz.functoolz import (
    compose,
    compose_left,
    excepts,
    thread_last,
)
from toolz.itertoolz import (
    first,
    groupby,
)
from toolz.curried import (
    get,
)

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
def ics_cal_busy_times_this_week(cal_or_file, disp=False):
    if isinstance(cal_or_file, (ics.icalendar.Calendar,)):
        cal = cal_or_file
    else:
        cal = read_ics(cal_or_file)
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

