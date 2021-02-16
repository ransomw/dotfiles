"""
utilities, conveniences,
and comforts
for developing Python,
written in Python.
"""

##
# there is no public api.
# all of this is a sketch.

from .navigate_strings import (
    locate_string_literals as _locate_string_literals
)

locate_string_literals_in_file = _locate_string_literals

__all__ = [
    locate_string_literals_in_file,
]
