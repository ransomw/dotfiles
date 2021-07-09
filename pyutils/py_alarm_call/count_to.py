"""
err on overtime
"""
import typing as ty
from collections import (
    namedtuple as nt,
)


BeatWave = nt('BeatWave ', ['msg',])

NiE = NotImplementedError

def _coerce__int():
    raise NiE()



# count to 5, stop immediate on 5.
rnglk = 5
rnglk = (1, 5)
rnglk = range(1, 6)

# use these strings as count mnumonics
# (instead of numbers), cycling back
# from 'c' to 'a' if rnglk > 5
cyclical=[
    'a',
    'b',
    'a',
    'a',
    'c',
]

# repeat these indices or cyclical
# mnumonic at (usually ~10-20% faster
# than index/mnumonic Audition hence
# centisecond units) the specified rate
# instead of single-point-in-time count
#
# todo: resolve sensible units/percent
#     for Visual counts
#
beat_freq = [
    ("a",
     70, # centiseconds
     ),
    ('b', 50),
]


def count_too(
        # range-like
        rnglk: ty.Union[
            # 1, 2, ... rnglk
            int,
            # i, j = rnglk
            # i, i+1, ..., j
            ty.Tuple[int, int],
            range,
            ],
        cyclical: List[str],
        beat_freq: ty.List[
            ty.Union[ty.Tuple[int,
                  int, # centiseconds
                  ],
            ty.Tuple[str,
                     int, # centiseconds
                     ],
         ]
        ],
):
    raise NiE()

