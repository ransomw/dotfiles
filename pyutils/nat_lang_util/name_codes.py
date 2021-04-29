"""
ISO 639-3 defines "name codes,"
typically 2-3 letter strings,
to identify natural languages.
"""
import os.path as pth
import requests
from operator import (
    itemgetter as ig,
)
import operator as op
from warnings import warn
from functools import (
    reduce,
    partial,
    wraps,
)
import json
import toolz.itertoolz as itlz
from tempfile import (
    gettempdir,
)
import toolz.functoolz as ftlz
from toolz.functoolz import (
    compose_left,
    compose,
    flip,
    juxt,
    thread_last,
)
from toolz.itertoolz import (
    mapcat,
)
from toolz.curried import (
    get,
)


def ensure_lang_name_codes(fn):
    cache_file_path = ensure_lang_name_codes.cache_file_path
    download_url = ensure_lang_name_codes.download_url

    def get_lang_name_codes_data():
        if pth.exists(cache_file_path):
            with open(cache_file_path) as f:
                return f.read()
        res = requests.get(download_url)
        assert res.status_code == 200
        lang_name_codes_data = res.text.replace("\r", "")
        with open(cache_file_path, "w") as f:
            f.write(lang_name_codes_data)
        return lang_name_codes_data

    def get_lang_name_codes():
        data = get_lang_name_codes_data()
        data = list(map(flip(str.split, "\t"), filter(op.truth, data.split("\n"))))
        assert data[0] == ["Id", "Print_Name", "Inverted_Name"]
        rows = data[1:]
        if len(set(map(ig(0), rows))) != len(rows):
            warn("name codes not uniq")
        return {"rows": rows}

    @wraps(fn)
    def wrapped(*args, **kwargs):
        return fn(get_lang_name_codes(), *args, **kwargs)

    return wrapped


# todo: study pre-commit caching strategy
ensure_lang_name_codes.cache_file_path = pth.join(
    gettempdir(), "lang_name_codes__pycache.tsv"
)
# ref. https://iso639-3.sil.org/code_tables/download_tables
ensure_lang_name_codes.download_url = (
    "https://iso639-3.sil.org/sites/iso639-3/files/downloads/iso-639-3_Name_Index.tab"
)


@ensure_lang_name_codes
def ensure_freedict_db_index(lang_name_codes, fn):
    cache_file_path = ensure_freedict_db_index.cache_file_path
    download_url = ensure_freedict_db_index.download_url

    def get_freedict_db_index_data():
        if pth.exists(cache_file_path):
            with open(cache_file_path) as f:
                return f.read()
        res = requests.get(download_url)
        assert res.status_code == 200
        freedict_db_index_data = res.text.replace("\r", "")
        with open(cache_file_path, "w") as f:
            f.write(freedict_db_index_data)
        return freedict_db_index_data

    def get_freedict_db_index():
        data = thread_last(
            get_freedict_db_index_data(),
            json.loads,
            (itlz.remove, get("software", default=None)),
            list,
        )
        for datum in data:
            assert "name" in datum, "entries have language names"
        assert set(map(compose(len, flip(str.split, "-"), ig("name")), data)) == {
            2
        }, "language names are pairs"
        assert set(mapcat(compose(flip(str.split, "-"), ig("name")), data)) <= set(
            map(ig(0), lang_name_codes["rows"])
        ), "language name pair items are known language codes"
        return {"data": data}

    @wraps(fn)
    def wrapped(*args, **kwargs):
        return fn(get_freedict_db_index(), *args, **kwargs)

    return wrapped


ensure_freedict_db_index.cache_file_path = pth.join(
    gettempdir(), "freedict_db_index__pycache.tsv"
)
# ref. https://github.com/freedict/fd-dictionaries/wiki/FreeDict-API
ensure_freedict_db_index.download_url = "https://freedict.org/freedict-database.json"


@ensure_lang_name_codes
def lang_name_code_to_lang_name(lang_name_codes, lang_name_code):
    rows = lang_name_codes["rows"]
    code_to_name = {row[0]: row[1] for row in rows}
    if lang_name_code not in code_to_name:
        raise ValueError("unknown language name code", (lang_name_code,))
    return code_to_name[lang_name_code]


@ensure_lang_name_codes
def lang_name_to_lang_name_codes(lang_name_codes, lang_name):
    rows = lang_name_codes["rows"]

    def name_pred(name):
        return lang_name.lower() in name.lower()

    codes = thread_last(
        rows,
        (filter, compose_left(ig(1), name_pred)),
        (map, ig(0)),
        list,
    )
    # todo: return name->code {}
    return codes



get_lang_info = compose_left(
    lang_name_to_lang_name_codes,
    partial(map, juxt(ftlz.identity, lang_name_code_to_lang_name)),
    list,
)

