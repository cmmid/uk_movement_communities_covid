#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug  4 16:45:52 2020

@author: hamishgibbs
"""
import __main__ as main
import sys
from pyquadkey2 import quadkey as qk
import pandas as pd
import geopandas as gpd
import numpy as np

if not hasattr(main, '__file__'):
    argv = ['code', 'data/raw/geo/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain-shp/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp',
            'data/processed/census/tiles_zoom_12.shp']
else:
    argv = sys.argv

las = gpd.read_file(argv[1])
las = las.to_crs("EPSG:27700")

tiles = gpd.read_file(argv[2])
tiles.crs = 4326
#%%
tiles['geometry'] = [pt.centroid for pt in tiles['geometry']]
tiles = tiles.to_crs("EPSG:27700")

#%%
print('Extracting quadkeys to la')
intersect = gpd.overlay(tiles, las, how='intersection')
intersect = intersect[['quadkey', 'lad17cd', 'lad17nm', 'lad17nmw', 'st_areasha', 'st_lengths']]

#%%
intersect.to_csv(argv[-1])
