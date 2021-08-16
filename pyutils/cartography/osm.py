"""
[Open Street Map](openstreetmap.org) data.

https://wiki.openstreetmap.org/wiki/OSM_XML
"""
from os import path as pth
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
from math import sqrt

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

from descartes import PolygonPatch


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
        "waterways": dict(
            zip(
                list(map(flip(get_tag, "name"), waterways)),
                map(LineString, list(map(way_to_lat_lons, waterways))),
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




GM = (sqrt(5)-1.0)/2.0
W = 8.0
H = W*GM
SIZE = (W, H)



BLUE = '#6699cc'
GRAY = '#999999'
DARKGRAY = '#333333'
YELLOW = '#ffcc33'
GREEN = '#339933'
RED = '#ff3333'
BLACK = '#000000'

def plot_line(ax, ob, color=GRAY, zorder=1, linewidth=3, alpha=1):
    x, y = ob.xy
    ax.plot(x, y, color=color, linewidth=linewidth, solid_capstyle='round', zorder=zorder, alpha=alpha)

def plot_coords(ax, ob, color=GRAY, zorder=1, alpha=1):
    x, y = ob.xy
    ax.plot(x, y, 'o', color=color, zorder=zorder, alpha=alpha)

def color_isvalid(ob, valid=BLUE, invalid=RED):
    if ob.is_valid:
        return valid
    else:
        return invalid

def color_issimple(ob, simple=BLUE, complex=YELLOW):
    if ob.is_simple:
        return simple
    else:
        return complex

def plot_line_isvalid(ax, ob, **kwargs):
    kwargs["color"] = color_isvalid(ob)
    plot_line(ax, ob, **kwargs)

def plot_line_issimple(ax, ob, **kwargs):
    kwargs["color"] = color_issimple(ob)
    plot_line(ax, ob, **kwargs)

def plot_bounds(ax, ob, zorder=1, alpha=1):
    x, y = zip(*list((p.x, p.y) for p in ob.boundary))
    ax.plot(x, y, 'o', color=BLACK, zorder=zorder, alpha=alpha)

def add_origin(ax, geom, origin):
    x, y = xy = affinity.interpret_origin(geom, origin, 2)
    ax.plot(x, y, 'o', color=GRAY, zorder=1)
    ax.annotate(str(xy), xy=xy, ha='center',
                textcoords='offset points', xytext=(0, 8))

def set_limits(ax, x0, xN, y0, yN,
               bdry_perc=0.05):
    assert x0 < xN
    assert y0 < yN
    xd = xN - x0
    yd = yN - y0
    ax.set_xlim(x0 - xd*bdry_perc, xN + xd*bdry_perc)
    ## need to handle non-integer limits
    # ax.set_xticks(range(x0, xN+1))
    ax.set_ylim(y0 - yd*bdry_perc, yN + yd*bdry_perc)
    # ax.set_yticks(range(y0, yN+1))
    ax.set_aspect("equal")




def plot_osm_shp(shpd):
    """
    use `descartes` package.
    it's an adaptor from shapely to matplotlib.
    """

    pyplot.close(fig='all')

    fig = pyplot.figure(1, # id
                        figsize=SIZE, # inches
                        dpi=90)
    fig.set_frameon(True)


    waterways = fig.add_subplot()

    waterways.spines['left'].set_smart_bounds(True)
    waterways.spines['bottom'].set_smart_bounds(True)

    for name in shpd["waterways"]:
        line_str = shpd["waterways"][name]
        plot_line(waterways, line_str)


    highways = fig.add_subplot()

    highways.spines['left'].set_smart_bounds(True)
    highways.spines['bottom'].set_smart_bounds(True)

    breakpoint()

    for name in shpd["named_highways"]:
        line_str = shpd["named_highways"][name]
        plot_line(highways, line_str)


    pyplot.show()

    patch = PolygonPatch(invalid_poly, facecolor=BLUE, edgecolor=BLUE, alpha=0.5, zorder=2)
    invalid_ax.add_patch(patch)



plot_osm = plot_open_street_map_xml = compose(plot_osm_shp, osm_to_shp)
TEST_MAP = pth.join(pth.dirname(pth.realpath(__file__)), 'map.osm')
plot_osm_demo = lambda: plot_osm(TEST_MAP)
