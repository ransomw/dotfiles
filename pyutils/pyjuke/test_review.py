from random import (
    shuffle,
)
from pytest_mock import MockerFixture
from . import review
from .review import *


def random_choice(lst):
    shuffle(lst)
    return lst[0]


def mock_do_concrete_review(*args, **kwargs):
    correct = random_choice([False, True,])
    quality = (random_choice([3,4,5,]) if correct else random_choice([0,1,2,]))
    return quality


def test_one_day_review(
        mocker: MockerFixture,
):

    _MIN_QUALITY = 0
    _MAX_QUALITY = 5

    do_review_mock = mocker.patch.object(
        review, 'do_concrete_review',
        side_effect=mock_do_concrete_review,
    )

    cards = [
        {'front': 'card-'+str(x),
         'back': str(x),
         'reviews': [],
         }
        for x in range(_MIN_QUALITY, _MAX_QUALITY)
    ]

    curr_sim_date = dt.date.today()

    supermemo_review_concrete(cards, review_date=curr_sim_date)

    assert len(cards) > 0
    assert 0 not in [len(c['reviews']) for c in cards]
    assert curr_sim_date not in [c['reviews'][-1] for c in cards]

