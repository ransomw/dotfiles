from typing import (
    Any,
    List,
    Dict,
)
from copy import (
    deepcopy,
)
from random import (
    shuffle,
)
from operator import (
    attrgetter,
)
from tempfile import (
    gettempdir,
)
import os.path as pth
import datetime as dt
import pandas as pd
from .review import *

_FLASHCARD_BASE_PATH = gettempdir()
_FLASHCARD_CSV_DATA_PATH = pth.join(_FLASHCARD_BASE_PATH, "flashcard_data.csv")


def set_flashcard_deck():
    global _FLASHCARD_BASE_PATH
    raise NotImplementedError()





_FLASHCARD_EMPTY_DF = pd.DataFrame({
    'jpn': pd.Series([]),
    'eng': pd.Series([]),
    'phonetic': pd.Series([]),
})

def _create_flashcard_csv(column_names):
    df = pd.DataFrame({
        column_name: pd.Series([], dtype='str') for column_name in column_names
    })
    if pth.exists(_FLASHCARD_CSV_DATA_PATH):
        warn("overwriting " + _FLASHCARD_CSV_DATA_PATH)
    with open(_FLASHCARD_CSV_DATA_PATH, 'w') as f:
        df.to_csv(f, index=False,)


def _read_flashcard_csv():
    with open(_FLASHCARD_CSV_DATA_PATH) as f:
        return pd.read_csv(f)


def _add_to_flashcard_csv(vals: Dict[str, str]):
    df = _read_flashcard_csv()
    assert set(vals.keys()) == set(df.columns), (
        "adding data without extant column names"
        "or"
        "missing columns")
    for col in df.columns:
        if vals[col] in df[[col]]:
            raise ValueError("duplicate value in column",
                             (col, vals[col],))
    df = df.append(
        pd.Series([vals[col] for col in df.columns],
                  dtype=str,
                  index=df.columns),
        ignore_index=True,
    )
    with open(_FLASHCARD_CSV_DATA_PATH, 'w') as f:
        df.to_csv(f, index=False,)


def _update_flashcard_csv(vals: Dict[str, str]):
    df = _read_flashcard_csv()
    assert len(vals) > 1
    assert set(vals.keys()) <= set(df.columns)
    raise NotImplementedError()


def currfn():
    _create_flashcard_csv([
        'jpn',
        'eng',
        'phonetic',
    ])
    _add_to_flashcard_csv({
        'jpn': '風chongfeng',
        'eng': 'wind',
        'phonetic': 'Kaze',
    })
    _add_to_flashcard_csv({
        'jpn': '水shui',
        'eng': 'water',
        'phonetic': 'Mizu',
    })
    flashcard_data = _read_flashcard_csv()
    print("flashcard_data")
    print(flashcard_data)



def add_flashcard_data(
        vals: Dict[str, str],
):
    if not pth.exists(_FLASHCARD_CSV_DATA_PATH):
        with open(_FLASHCARD_CSV_DATA_PATH, 'w') as f:
            _FLASHCARD_EMPTY_DF.to_csv(f)

    with open(_FLASHCARD_CSV_DATA_PATH) as f:
        df = pd.read_csv(f)

    assert set(vals.keys()) == set(df.columns), (
        "adding data without extant column names"
        "or"
        "missing columns")

    if set(vals.keys()) != set(_FLASHCARD_DF.columns):
        warn(("adding data without extant column names"
              "or"
              "missing columns"))
        ValueError()


    df = df.append(
        pd.Series(['X', 'Z',],
                  dtype=str,
                  index=df.columns),
        ignore_index=True,
    )


    _FLASHCARD_DF


    return


    df = pd.DataFrame({
        'A': pd.Series([1, 2,]),
        'B': pd.Series([3, 4,]),
    })
    breakpoint()
    df = df.append(
        pd.Series(['X', 'Z',],
                  index=df.columns),
        ignore_index=True,
    )
    df = df.append(
        pd.Series(['T', 'Y', "P",]),
        ignore_index=True,
    )
    breakpoint()
    df.append(
        pd.Series(['X0', 'Z9',],
                  index=df.columns)
    )
    breakpoint()
    raise NotImplementedError()


def read_flashcard_data():

    raise NotImplementedError()


def edit_flashcard():
    raise NotImplementedError()


def nextfn():
    add_flashcard_data({
        'jpn': '風',
        'eng': 'eng',
        'phonetic': 'Kaze',
    })
    add_flashcard_data({
        'jpn': '水',
        'eng': 'water',
        'phonetic': 'Mizu',
    })
    flashcard_data = read_flashcard_data()

    # 'コウモリ'


def _sim_practice_flashcards():

    _MIN_QUALITY = 0
    _MAX_QUALITY = 5

    cards = [
        {'sides': ('card-'+str(x), x),
         'reviews': [],
         }
        for x in range(_MIN_QUALITY, _MAX_QUALITY)
    ]

    first_review_date = dt.date.today()

    # init reviews
    for (i, card,) in enumerate(cards):
        card['reviews'].append(SMTwo.first_review(i, first_practice_date))

    def random_choice(lst):
        shuffle(lst)
        return lst[0]

    curr_sim_date = first_review_date
    review_cards = [card for card in cards if card['reviews'][-1].review_date <= curr_sim_date]
    next_review_cards = []
    shuffle(review_cards)
    while True:
        for review_card in review_cards:
            correct = random_choice([False, True,])
            quality = (random_choice([3,4,5,]) if correct else random_choice([0,1,2,]))
            review_card['reviews'].append(deepcopy(review_card['reviews'][-1]).review(quality))
            if review_card['reviews'][-1].review_date == curr_sim_date:
                next_review_cards.append(review_card)
        if next_review_cards:
            review_cards = next_review_cards
        else:
            break


    assert 1 not in [len(c['reviews']) for c in cards]
    assert curr_sim_date not in [c['reviews'][-1] for c in cards]

    breakpoint()



    first_practice_date = dt.date.today()
    second_practice_date = first_practice_date + dt.timedelta(days=1)
    third_practice_date = second_practice_date + dt.timedelta(days=3)

    first_reviews = [SMTwo.first_review(x, first_practice_date) for x in range(6)]
    second_reviews = [
        deepcopy(first_review).review(x, second_practice_date)
        for first_review in first_reviews
        for x in range(6)]

    first_review_dates = set([r.review_date for r in first_reviews])
    second_review_dates = set([r.review_date for r in second_reviews])

    print("review dates")
    print(first_review_dates)
    print(second_review_dates)

    print("review intervals")
    print(set(map(attrgetter('interval'), first_reviews)))
    print(set(map(attrgetter('interval'), second_reviews)))

    breakpoint()




def practice_flashcards():
    # todo: reverse-order difficulty
    def get_difficulty_input():
        difficulty = None
        while difficulty is None:
            difficulty_input = input('how difficult was that? [1-3] >>>')
            try:
                difficulty = int(difficulty_input)
            except ValueError:
                pass
            if difficulty is not None and difficulty < 1 or difficulty > 3:
                print("difficulty between 1, 2, or 3")
                difficulty = None
        return difficulty

    def get_resp_input(card, print_correctness=True):
        resp = input(card[0] + ' >>> ')
        if resp == card[1]:
            print("correct")
            return True
        else:
            print("incorrect")
            return False


    cards = (
        ('one', '1', []),
        ('two', '2', []),
        # ('three', '3', []),
    )

    curr_date = first_practice_date
    for card in cards:
        resp = input(card[0] + ' >>> ')
        if resp == card[1]:
            print("correct")
        else:
            print("incorrect")
        difficulty = get_difficulty_input()
        curr_card_review = SMTwo.first_review(difficulty, curr_date)
        card[2].append(curr_card_review)

    curr_date = second_practice_date
    for card in cards:
        resp = input(card[0] + ' >>> ')
        if resp == card[1]:
            print("correct")
        else:
            print("incorrect")
        difficulty = get_difficulty_input()
        prev_review = card[2][-1]
        curr_card_review = prev_review.review(difficulty, curr_date)
        card[2].append(curr_card_review)


