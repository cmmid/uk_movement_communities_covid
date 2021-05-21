import __main__ as main
import sys
import geopandas as gpd
import pandas as pd
from utils import oa_tile_intersection, get_parent

if not hasattr(main, '__file__'):
    argv = ['a',
            'data/processed/geo/tiles.shp',
            'data/raw/census/engwal_oa_bng.shp',
            'data/raw/census/OutputArea2011_PWC.shp',
            'data/raw/census/NI_SA_Centroids.shp']
else:
    argv = sys.argv

tiles = gpd.read_file(argv[1])
tiles.crs = 4326

england_oa = gpd.read_file(argv[2])
scotland_oa = gpd.read_file(argv[3])
ni_oa = gpd.read_file(argv[4])

england_oa.rename(columns = {'OA11CD':'OA'}, inplace = True)
scotland_oa.rename(columns = {'code':'OA'}, inplace = True)
ni_oa.rename(columns = {'SA2011':'OA'}, inplace = True)

oas = {'england': england_oa, 'scotland': scotland_oa, 'ni': ni_oa}

intersection = {}
for oa in oas:
    print('Overlaying {}'.format(oa))
    intersection[oa] = oa_tile_intersection(tiles, oas[oa])

assert intersection['ni'].groupby(['OA']).agg({'quadkey_13':['count']}).reset_index()['quadkey_13']['count'].unique().tolist() == [1]
assert intersection['scotland'].groupby(['OA']).agg({'quadkey_13':['count']}).reset_index()['quadkey_13']['count'].unique().tolist() == [1]
assert intersection['england'].groupby(['OA']).agg({'quadkey_13':['count']}).reset_index()['quadkey_13']['count'].unique().tolist() == [1]

intersection = pd.concat(intersection.values()).reset_index(drop = True)
intersection['quadkey_12'] = [get_parent(qk) for qk in intersection['quadkey_13']]

intersection.to_csv(argv[-1])
