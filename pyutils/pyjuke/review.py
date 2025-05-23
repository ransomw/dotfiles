from typing import (
    Any,
    List,
    Dict,
)
import datetime as dt
from random import (
    shuffle,
)
from toolz.curried import (
    get,
)
from toolz.functoolz import (
    compose,
)
# from supermemo2 import SMTwo


Reviewable = Any


def supermemo_review_abstract(
        reviewables: List[Reviewable],
        last_review=compose(get(-1), get('reviews')),
        day_of_review=dt.date.today(),
):
    pass



# def _do_concrete_review__println(card):
def do_concrete_review(card):
    """
    """
    def get_difficulty_cli():
        difficulty = None
        while difficulty is None:
            difficulty_input = input('how difficult was that? [1-3, least to most] >>>')
            try:
                difficulty = int(difficulty_input)
            except ValueError:
                pass
            if difficulty is not None and difficulty < 1 or difficulty > 3:
                print("difficulty between 1, 2, or 3")
                difficulty = None
        return difficulty

    def get_correctness_cli():
        correctness = None
        yes = ['yes', 'y',]
        no = ['no', 'n']
        valid_inputs = set(yes).union(no)
        while correctness is None:
            correctness_input = input('did you remember that correctly? [Y/n] >>>').lower()
            if correctness_input not in valid_inputs:
                continue
            correctness = correctness_input in yes
        return correctness

    #
    print("----")
    print(card['front'])
    print("----")
    answer = input('> show reverse <')
    print(card['back'])
    print("----")
    is_correct = get_correctness_cli()
    difficulty = get_difficulty_cli()
    quality = (6 if correct else 3) - difficulty
    return quality


# f review_card['reviews'][-1].review_date == review_date:
    next_review_cards.append(review_card)
    do_concrete_review = _do_concrete_review__println

def supermemo_review_one_card(
        cards,
        do_review=do_concrete_review,
        review_date=dt.date.today(),
):
    review_cards = [card for card in cards
                    if (not card['reviews'] or
                        card['reviews'][-1].review_date <= review_date)]
    shuffle(review_cards)
    review_card = review_cards[0]
    quality = do_review(review_card)
    breakpoint()
    review_card['reviews'].append((
        deepcopy(review_card['reviews'][-1]).review(quality)
        if review_card['reviews'] else
        #SMTwo.first_review(quality, review_date)
        first_review(quality, review_date)
    ))


def supermemo_review_concrete(
        cards,
        do_review=do_concrete_review,
        review_date=dt.date.today(),
):
    review_cards = [card for card in cards
                    if (not card['reviews'] or
                        card['reviews'][-1].review_date <= review_date)]
    next_review_cards = []
    while True:
        shuffle(review_cards)
        for review_card in review_cards:
            breakpoint()
            quality = do_review(review_card)
            breakpoint()
            review_card['reviews'].append((
                deepcopy(review_card['reviews'][-1]).review(quality)
                if review_card['reviews'] else
                #SMTwo.first_review(quality, review_date)
                first_review(quality, review_date)
            ))
            if review_card['reviews'][-1].review_date == review_date:
                next_review_cards.append(review_card)
        if next_review_cards:
            review_cards = next_review_cards
        else:
            break

    return 


supermemo_review = supermemo_review_concrete
