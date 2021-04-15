from tempfile import (
    gettempdir,
)
import os.path as pth
import pandas as pd


_FLASHCARD_BASE_PATH = gettempdir()
_FLASHCARD_CSV_DATA_PATH = pth.join(_FLASHCARD_BASE_PATH, "flashcard_data.csv")


def create_infodb_csv(column_names):
    df = pd.DataFrame({
        column_name: pd.Series([], dtype='str') for column_name in column_names
    })
    if pth.exists(_FLASHCARD_CSV_DATA_PATH):
        warn("overwriting " + _FLASHCARD_CSV_DATA_PATH)
    with open(_FLASHCARD_CSV_DATA_PATH, 'w') as f:
        df.to_csv(f, index=False,)


def read_infodb_csv():
    with open(_FLASHCARD_CSV_DATA_PATH) as f:
        return pd.read_csv(f)


def add_to_infodb_csv(vals: Dict[str, str]):
    df = read_infodb_csv()
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


def update_infodb_csv(vals: Dict[str, str]):
    df = read_infodb_csv()
    assert len(vals) > 1
    assert set(vals.keys()) <= set(df.columns)
    raise NotImplementedError()
