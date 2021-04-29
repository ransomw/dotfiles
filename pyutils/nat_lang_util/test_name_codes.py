import os
from operator import (
    itemgetter as ig,
)
import pytest
from .name_codes import *

@pytest.mark.skipif('PYUTILS_TEST_NOINET' in os.environ,
                    reason=("PYUTILS_TEST_NOINET envvar set "
                            "indicating no internet connectivity, "
                            "and external service not mocked"))
def test_get_lang_info():
    lang_info = get_lang_info('french')
    code_to_name = dict(lang_info)
    assert 'fra' in code_to_name
    assert code_to_name['fra'] == 'French'
