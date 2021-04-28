from typing import (
    Dict,
)
from tempfile import (
    gettempdir,
)
import os.path as pth
from warnings import (
    warn,
)
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
    all_val_names = set(vals.keys())
    assert all_val_names <= set(df.columns)
    for row_idx in range(len(df)):
        row_dict = dict(df.iloc[row_idx])
        same_val_names = [name for name in all_val_names
                          if vals[name] == row_dict[name]]
        different_val_names = [name for name in all_val_names
                               if vals[name] != row_dict[name]]
        if len(different_val_names) == 1:
            different_val_name = different_val_names[0]
            df.loc[row_idx, different_val_name] = vals[different_val_name]
            break
    else:
        raise ValueError("expected to match a row on all except one value name")
    with open(_FLASHCARD_CSV_DATA_PATH, 'w') as f:
        df.to_csv(f, index=False,)
