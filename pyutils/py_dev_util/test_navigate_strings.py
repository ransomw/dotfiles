import os.path as pth

import pytest

from .navigate_strings import (
    module_path_2_str_const_nodes,
    locate_string_literals,
)

__some_module = """
some_str = "some str"
some_tuple = ("another str",)
def a_fn():
    return "a string"
"""

def test_module_path_2_str_const_nodes(
        tmpdir,
):
    module_path = pth.join(
        tmpdir,
        'some_module.py')
    with open(module_path, 'w') as f:
        f.write(__some_module)
    x = module_path_2_str_const_nodes(
        module_path,
    )
    str_const_nodes = x
    assert len(str_const_nodes) == 3
    assert (
        set([node.value for
             node in str_const_nodes])
        ==
        set(["some str",
             "another str",
             "a string"])
    )
    # a note on the ast module api
    for attr in [
            'value',
            'lineno',
            'col_offset',
            'end_lineno',
            'end_col_offset',
    ]:
        for node in str_const_nodes:
            assert hasattr(node, attr)


def test_locate_string_literals(
        tmpdir,
):
    module_path = pth.join(
        tmpdir,
        'some_module.py')
    with open(module_path, 'w') as f:
        f.write(__some_module)
    locs = locate_string_literals(
        module_path,
    )
    assert(
        locs ==
        [(2, "some str"),
         (3, "another str"),
         (5, "a string")]
    )
