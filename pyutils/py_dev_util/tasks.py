from datetime import (
    datetime as dto,
    timedelta,
)
from operator import (
    add,
)
from pprint import pp
from random import (
    random as rand
)
#
from invoke import task
from invoke import exceptions as exc
#
from toolz.functoolz import (
    compose,
    curry,
    flip,
    juxt,
)
from toolz.itertoolz import (
    accumulate,
    concat,
    concatv,
    diff,
    isdistinct,
    nth,
    unique,
)

_repeatedly = lambda count, fn: [
    fn() for _ in range(count)
]
_maplist = curry(compose(list, map))

@task
def print_random_timeseries(
        c,
        count=8,
):
    start = dto.utcnow()
    deltas = [
        timedelta(minutes=x)
        for x in
        accumulate(add, _repeatedly(
            count, rand,
        ))]
    timeseries = _maplist(
        curry(add, start),
        deltas)
    timeseries_formatted = [
        x.isoformat() for
        x in timeseries
    ]
    pp(timeseries_formatted)



@task
def other_task(
        c,
):
    pass
