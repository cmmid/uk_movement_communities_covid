import pandas as pd
import geopandas as gpd
from pyquadkey2 import quadkey


def oa_tile_intersection(tiles, geometry):

    try:
        assert 'OA' in geometry.columns

    except Exception:
        raise ValueError('geometry must have an OA column.')

    try:
        assert tiles.crs == geometry.crs

    except Exception:
        geometry = geometry.to_crs(tiles.crs)

    intersect = gpd.overlay(geometry, tiles, how='intersection')

    return(pd.DataFrame(intersect.drop(columns='geometry'))[['OA', 'quadkey']].rename(columns = {'quadkey':'quadkey_13'}))


def get_parent(qk):

    qk = quadkey.QuadKey(str(qk))

    return(qk.parent())
