from typing import (
    Any,
    List,
    Dict,
)
import datetime as dt
from toolz.curried import (
    get,
)
from toolz.functoolz import (
    compose,
)


Reviewable = Any


def supermemo_review_abstract(
        reviewables: List[Reviewable],
        last_review=compose(get(-1), get('reviews')),
        day_of_review=dt.date.today(),
):
    pass



def supermemo_review_concrete(
        cards,
        review_date=dt.date.today(),
):
    review_cards = [card for card in cards if card['reviews'][-1].review_date <= review_date]
    next_review_cards = []
    shuffle(review_cards)
    while True:
        for review_card in review_cards:
            correct = random_choice([False, True,])
            quality = (random_choice([3,4,5,]) if correct else random_choice([0,1,2,]))
            review_card['reviews'].append(deepcopy(review_card['reviews'][-1]).review(quality))
            if review_card['reviews'][-1].review_date == review_date:
                next_review_cards.append(review_card)
        if next_review_cards:
            review_cards = next_review_cards
        else:
            break

    pass


supermemo_review = supermemo_review_concrete
