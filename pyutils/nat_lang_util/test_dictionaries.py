import os
from pprint import pp
import pytest
from .dictionaries import *

@pytest.mark.skipif('PYUTILS_TEST_NOINET' in os.environ,
                    reason=("PYUTILS_TEST_NOINET envvar set "
                            "indicating no internet connectivity, "
                            "and external service not mocked"))
def test_get_bilingual_dictionary_downloads__mid_omegawiki():
    bilingual_dictionary_downloads = get_bilingual_dictionary_downloads()
    bilingual_dictionary_downloads_dict = dict(bilingual_dictionary_downloads)
    assert 'Eng-Hin' in bilingual_dictionary_downloads_dict
    assert bilingual_dictionary_downloads_dict['Eng-Hin'].endswith('.zip')

@pytest.mark.skip(reason="unimplemented")
def test_get_bilingual_dictionary_downloads():
    bilingual_dictionary_downloads = get_bilingual_dictionary_downloads()


@pytest.mark.skip(reason="unimplmented")
def test_get_syllabary():
    syllabary = get_syllabary('fra')


@pytest.mark.skipif('PYUTILS_TEST_NOINET' in os.environ,
                    reason=("PYUTILS_TEST_NOINET envvar set "
                            "indicating no internet connectivity, "
                            "and external service not mocked"))
def test_lookup_word_mono():
    wind_defns_eng = lookup_word_mono('wind')
    assert isinstance(wind_defns_eng, (list,))
    for wind_defn_eng in wind_defns_eng:
        assert 'definitions' in wind_defn_eng
        for alt_wind_defn in wind_defn_eng['definitions']:
            assert 'text' in alt_wind_defn
