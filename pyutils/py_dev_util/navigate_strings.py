"""

"""

import os
import os.path as pth
import itertools
import ast


def module_path_2_ast(module_path):
    with open(module_path) as f:
        module_tree = ast.parse(
            f.read(),
            filename=pth.basename(
                module_path),
        )
    return module_tree


# todo: implement alternate version
#       with node visitor to demo api
def ast_2_str_const_nodes(
        ast_tree,
):
    string_nodes = [
        node for node in ast.walk(ast_tree)
        if (isinstance(node, ast.Constant)
            and isinstance(node.value, str))
    ]
    string_nodes.sort(
        key=lambda n: n.lineno,
    )
    return string_nodes


### todo: consolidate ideas
#     regarding function composition.
#
#   use lambdas like here, or say
#   functoolz.compose()
#   ?
#
module_path_2_str_const_nodes = (
    lambda module_path:
    ast_2_str_const_nodes(
        module_path_2_ast(
            module_path))
)


def locate_string_literals(
        py_module_path,
):
    return [
        (
            node.lineno,
            node.value,
        )
        for node
        in module_path_2_str_const_nodes(
            py_module_path,
        )
    ]
