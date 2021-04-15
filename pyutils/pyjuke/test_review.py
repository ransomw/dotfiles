from .review import *

def test_one_day_review():

    _MIN_QUALITY = 0
    _MAX_QUALITY = 5

    cards = [
        {'sides': ('card-'+str(x), x),
         'reviews': [],
         }
        for x in range(_MIN_QUALITY, _MAX_QUALITY)
    ]

    supermemo_review_concrete(cards)

    assert 1 not in [len(c['reviews']) for c in cards]
    assert curr_sim_date not in [c['reviews'][-1] for c in cards]

