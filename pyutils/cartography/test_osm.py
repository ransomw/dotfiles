from os import path as pth
from .osm import *

TEST_MAP = pth.join(pth.dirname(pth.realpath(__file__)), 'map.osm')

def test_osm_to_shp():
    shp_dict = osm_to_shp(TEST_MAP)
    assert 'Restbrook Place' in shp_dict['named_highways'].keys()
