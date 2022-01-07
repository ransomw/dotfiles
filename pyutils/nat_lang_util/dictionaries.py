import os
import os.path as pth
import operator as op
from operator import (
    itemgetter as ig,
    attrgetter,
)
from tempfile import (
    gettempdir,
)
import requests
from bs4 import BeautifulSoup as BS
import toolz.itertoolz as itlz
import toolz.functoolz as ftlz
from toolz.functoolz import (
    compose_left,
    compose,
    flip,
    thread_last,
)
from toolz.itertoolz import (
    mapcat,
)
from toolz.curried import (
    get,
)
from wiktionaryparser import WiktionaryParser
from .name_codes import (
    ensure_lang_name_codes,
)


def _url_path_basename(url_path):
    return thread_last(url_path.split("/"), (filter, op.truth), list, itlz.last)


@ensure_lang_name_codes
def get_bilingual_dictionary_downloads(lang_name_codes):
    cache_dir_path = get_bilingual_dictionary_downloads.cache_dir_path
    if pth.exists(cache_dir_path):
        assert pth.isdir(cache_dir_path)
    else:
        os.mkdir(cache_dir_path)
    urls = {
        "mid_omegawiki_index": "http://dictionarymid.sourceforge.net/dictionaries/dictsBinlingualsOmegaWiki.html",
        "mid_other_index": "http://dictionarymid.sourceforge.net/dictionaries/dictsOtherBilinguals.html",
        "digitaltibetan": "http://digitaltibetan.org/Media/Resources/",
    }

    def get_cache_filepath(url):
        parsed_url = requests.compat.urlparse(url)
        url_path_basename = _url_path_basename(parsed_url.path)
        itlz.last(list(filter(op.truth, (parsed_url.path + "/").split("/"))))
        return pth.join(cache_dir_path, parsed_url.netloc + url_path_basename)

    assert len(urls) == len(
        list(map(get_cache_filepath, urls.values()))
    ), "all index urls have unique cache location"

    def get_soup(url):
        cache_filepath = get_cache_filepath(url)
        if pth.exists(cache_filepath):
            with open(cache_filepath) as f:
                return BS(f.read())
        res = requests.get(url)
        assert res.status_code == 200
        with open(cache_filepath, "w") as f:
            f.write(res.text)
        return BS(res.text)

    # BEGIN mid omegawiki
    soup = get_soup(urls["mid_omegawiki_index"])
    assert len(soup.find_all("table")) == 1
    soup.find_all("table")
    trows = soup.find_all("table")[0].find_all("tr")
    assert set(map(attrgetter("name"), trows[0].find_all(recursive=False))) == {
        "th"
    }, "first row consists of headers"
    headers = ["Language", "Download", "Web App", "Size"]
    assert (
        list(map(attrgetter("text"), trows[0].find_all(recursive=False))) == headers
    ), "found expected columns"
    body_trows = [r.find_all(recursive=False) for r in trows[1:]]
    for row in body_trows:
        assert set(map(attrgetter("name"), row)) == {
            "td"
        }, "table body rows consist of descriptors"
        assert (
            len(row[headers.index("Download")].find_all("a")) == 1
        ), "download rows contain precisely one anchor"
    download_urls = list(map(
        compose_left(
            get(headers.index("Download")),
            lambda el: el.find("a").get("href"),  # :/
        ),
        body_trows,
    ))

    # ftlz.pipe(download_urls, requests.compat.urlparse, )

    download_filenames = list(map(compose_left(requests.compat.urlparse, attrgetter("path"), _url_path_basename,), download_urls))

    # download_filenames = ftlz.pipe(
    #     download_urls, requests.compat.urlparse, attrgetter("path"), _url_path_basename
    # )
    # assert all(
    #     [filename.startswith("DfM_OmegaWiki_") for filename in download_filenames]
    # )

    download_lang_dsl_names = thread_last(
        download_filenames,
        (map, compose_left(flip(str.split, "_"), get(2))),
    )

    # for name in download_lang_dsl_names:
    #     assert len(name) == 6
    #     # .. split string on case change fn? ...  snake<>camel, etc.
    #     assert name[0].isupper() and name[3].isupper()
    #     assert (name[1:3] + name[4:]).islower()

    download_lang_names = [
        # match freedict name encoding
        name[:3] + "-" + name[3:]
        for name in download_lang_dsl_names
    ]

    # todo XXX
    # assert set(mapcat(compose(flip(str.split, "-")), download_lang_names)) <= set(
    #     map(ig(0), lang_name_codes["rows"])
    # ), (
    #     "language names from string typing "
    #     "of download filenames "
    #     "are known language codes"
    # )

    omegawiki_dicts = list(zip(download_lang_names, download_filenames))
    return omegawiki_dicts
    # END mid omegawiki

    soup = get_soup(urls["mid_other_index"])

    breakpoint()
    raise NotImplementedError()


get_bilingual_dictionary_downloads.cache_dir_path = pth.join(
    gettempdir(), "bilingal_dictionary_downloads_cache_dir"
)



@ensure_lang_name_codes
def get_syllabary(lang_name_codes, lang_name_code):
    raise NotImplementedError()
    pass


# get_syllabary.cache_dir = pth.join()



def lookup_word_mono(word, lang="english"):
    """
    monolingual dictionary lookup
    """
    parser = WiktionaryParser()
    data = parser.fetch(word, lang)
    return data
