"""
sandbox of functions to flatten lists.
flattening lists is usually an indication
of not processing them correctly in the first
place, of course.
"""

### attempts to flatten arbitrarily
#   nested lists

def _flatten_list__C1(cx):
    out = []
    for item_or_sublist in cx:
        if isinstance(item_or_sublist,
                      list):
            for item in _flatten_list__C1(
                    item_or_sublist):
                out.append(item)
        else:
            out.append(item)



def _flatten_list__C2(cx):
    out = []
    for item_or_sublist in cx:
        try:
            sublist = list(item_or_sublist)
        except TypeError as err:
            if err.args[0].endswith('not iterable'):
                out.append(item)
            else:
                raise err
        else:
            for item in _flatten_list__C2(sublist):
                out.append(item)


# these assume a list of lists,
# a tree of depth precisely 2 everywhere.
_flatten_list__A = (
    lambda ax:
    list(itertools.chain.from_iterable(ax))
)
_flatten_list__B = (
    lambda bx:
    [item
     for sublist in bx
     for item in sublist]
)
