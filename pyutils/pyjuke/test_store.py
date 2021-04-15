from copy import (
    deepcopy,
)
from pytest import (
    fixture,
)
from tempfile import (
    mkstemp,
)
from . import store

@fixture
def flashcard_csv_data():
    tmp_filename = mkstemp()
    store._FLASHCARD_CSV_DATA_PATH = tmp_filename
    def read_contents():
        with open(tmp_filename) as f:
            return f.read()
    yield read_contents
    os.unlink(tmp_filename)


def test_csv_rw(
        flashcard_csv_data,
):
    column_names = [
        'jpn',
        'eng',
        'phonetic',
    ]
    store.create_infodb_csv(column_names)
    datum_a = {
        'jpn': '風chongfeng',
        'eng': 'wind',
        'phonetic': 'Kaze',
    }
    store.add_to_infodb_csv(datum_a)

    datum_a_dupe = deepcopy(datum_a).update({
        'eng': 'kaze',
    })

    datum_b = {
        'jpn': '水shui',
        'eng': 'water',
        'phonetic': 'Mizu',
    }
    store.add_to_infodb_csv(datum_b)

    flashcard_raw_data = flashcard_csv_data()

    assert len(
        flashcard_raw_data.strip().split('\n')
    ) == 3, "found header and records' lines"

    flashcard_data = store.read_infodb_csv()

    print("flashcard_data")
    print(flashcard_data)
    breakpoint()


    assert False
