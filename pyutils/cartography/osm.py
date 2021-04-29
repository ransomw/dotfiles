"""
[Open Street Map](openstreetmap.org) data.

https://wiki.openstreetmap.org/wiki/OSM_XML
"""
import lxml
import lxml.etree
from operator import (
    add,
    itemgetter as ig,
    attrgetter,
)
from functools import (
    reduce,
    partial,
    wraps,
)
import operator as op
import toolz.functoolz as ftlz
import shapely
import shapely.geometry
from shapely.geometry import (
    LineString,
)
import pyproj
import matplotlib
from matplotlib import pyplot
from toolz.functoolz import (
    compose_left,
    excepts,
    compose,
    curry,
    flip,
    juxt,
    thread_last,
)
from toolz.itertoolz import (
    accumulate,
    concat,
    concatv,
    cons,
    diff,
    first,
    isdistinct,
    groupby,
    mapcat,
    nth,
    unique,
)
from toolz.dicttoolz import (
    keymap,
    valmap,
    itemmap,
)
from toolz.curried import (
    get,
    keyfilter,
    valfilter,
    itemfilter,
)

def osm_to_shp(filename):
    with open(filename) as f:
        fstr = f.read()
        root = lxml.etree.XML(bytes(fstr, "utf-8"))
    tags = [t.tag for t in root]
    assert root.tag == "osm"
    assert set(tags) <= {"node", "relation", "way", "bounds"}
    assert root[0].tag == "bounds"
    assert set(tags[1:]) <= {"node", "relation", "way"}
    assert "way" not in tags or (set(tags[tags.index("way") :]) <= {"relation", "way"})
    assert "relation" not in tags or (
        set(tags[tags.index("relation") :]) <= {"relation"}
    )
    nodes = [el for el in root if el.tag == "node"]
    ways = [el for el in root if el.tag == "way"]
    relations = [el for el in root if el.tag == "relation"]
    bounds_dict = dict(root[0].items())
    min_bound = (bounds_dict["minlat"], bounds_dict["minlon"])
    max_bound = (bounds_dict["maxlat"], bounds_dict["maxlon"])

    # (beginnings of) some cleverness
    juxt(map(compose(keyfilter, flip(str.startswith)), ["min", "max"]))(bounds_dict)

    ### munging #

    el_attrs = lambda x: dict(x.items())
    attr_eq = lambda name, val: (compose(partial(op.eq, val), attrgetter(name)))
    has_tag = lambda el, tag_name: any(
        [
            True
            for x in el
            if (x.tag == "tag" and "k" in el_attrs(x) and el_attrs(x)["k"] == tag_name)
            # (attr_eq('tag', 'tag')(x) and "k" in el_attrs(x) and get("k")(el_attrs(x)) == tag_name)
            # (attr_eq('tag', 'tag')(x) and compose(flip(op.contains, 'k'), el_attrs)(x) and compose(partial(op.eq, tag_name), get("k"), el_attrs)(x))
            # compose(all, juxt(attr_eq('tag', 'tag')(x), compose(partial(op.eq, tag_name), get("k", default=None), el_attrs)))(x)
        ]
    )
    # errors if tag nexist...
    # ... idea: duplicate get() semantics
    get_tag = lambda el, tag_name: ftlz.pipe(
        list(el),
        partial(
            filter,
            compose(
                all,
                juxt(
                    attr_eq("tag", "tag"),
                    compose_left(
                        el_attrs,
                        get("k", default=None),
                        partial(op.eq, tag_name),
                    ),
                ),
            ),
        ),
        first,
        compose_left(el_attrs, get("v")),
    )

    #    filter(compose_left(el_attrs, get('k'), partial(op.eq, 'highway')),
    #    filter(compose_left(el_attrs, flip(op.contains, 'k')),
    #        filter(compose(partial(op.eq, 'tag'), attrgetter('tag')),
    #            list(ways[2]))))

    waterways = list(filter(flip(has_tag, "waterway"), ways))
    highways = list(filter(flip(has_tag, "highway"), ways))
    named_highways = list(filter(flip(has_tag, "name"), highways))

    assert len(set(map(compose(ig("id"), el_attrs), nodes))) == len(
        nodes
    ), "nodes have unique ids"
    nodes_by_id = {el_attrs(n)["id"]: n for n in nodes}

    way_to_lat_lons = compose_left(
        list,
        partial(filter, attr_eq("tag", "nd")),
        partial(
            map,
            compose_left(
                el_attrs,
                ig("ref"),
                flip(get, nodes_by_id),
                el_attrs,
                get(["lat", "lon"]),
                compose(
                    list,
                    partial(map, float),
                ),
            ),
        ),
        list,
    )

    return {
        "named_highways": dict(
            zip(
                list(map(flip(get_tag, "name"), named_highways)),
                map(LineString, list(map(way_to_lat_lons, named_highways))),
            )
        ),
        "bounds": {
            "dict": bounds_dict,
            # "box": shapely.geometry.box(
            #     bounds_dict["minlat"],
            #     bounds_dict["minlon"],
            #     bounds_dict["maxlat"],
            #     bounds_dict["maxlon"],
            # ),
        },
    }

    # introspection on data format
    ways_keys = thread_last(
        ways,
        (mapcat, list),
        (filter, attr_eq("tag", "tag")),
        (map, el_attrs),
        (map, ig("k")),
        set,
    )


def plot_osm_shp(shpd):
    """
    use `descartes` package.
    it's an adaptor from shapely to matplotlib.
    """
    breakpoint()
    raise NotImpletedError()
    pass
