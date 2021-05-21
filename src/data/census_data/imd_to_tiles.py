import __main__ as main
import sys
import geopandas as gpd
import pandas as pd
import numpy as np

if not hasattr(main, '__file__'):
    argv = ['code', 'data/processed/tile_reference/tiles_zoom_12.shp',
            'data/processed/census/oa_tile_reference.csv',
            'data/raw/census_lookups/engwal_OA_lsoa.csv',
            'data/raw/census_lookups/OA_to_DZ.csv',
            'data/raw/censes/NI_SA_Centroids.shp',
            'data/raw/imd_data/Eng_IMD.csv',
            'data/raw/imd_data/WIMD2019_Ranks.csv',
            'data/raw/imd_data/simd2020_withinds.csv',
            'data/raw/imd_data/NorthernIrelandMDM.csv',
            'data/raw/census/Eng_Wal_OA_Mid_Pop.csv',
            'data/raw/census/simd2020_withinds.csv',
            'data/raw/census/NI_Mid_Pop.csv',
            'data/processed/census/quadkey_imd.csv']
else:
    argv = sys.argv

tiles = gpd.read_file(argv[1])
tiles.crs = 4326

oa_tile_lookup = pd.read_csv(argv[2])

oa_lus = {'england': pd.read_csv(argv[3]),
          'scotland': pd.read_csv(argv[4]),
          'ni': gpd.read_file(argv[5])}

oa_lus['ni'] = oa_lus['ni'].loc[:, ['SA2011', 'SOA2011']]

imd_data = {'england': pd.read_csv(argv[6]),
            'wales': pd.read_csv(argv[7]),
            'scotland': pd.read_csv(argv[8]),
            'ni': pd.read_csv(argv[9])}

pop_data = {'england': pd.read_csv(argv[10]),
            'scotland': pd.read_csv(argv[11]),
            'ni': pd.read_csv(argv[12])}

scotland_n_oas = oa_lus['scotland'].groupby('DataZone2011Code').count().reset_index()[['DataZone2011Code', 'OutputArea2011Code']].rename(columns = {'DataZone2011Code':'DZ', 'OutputArea2011Code':'n_oas'})
scotland_pop = pd.merge(imd_data['scotland'], scotland_n_oas)[['DZ', 'Total_population', 'n_oas']]
scotland_pop = pd.merge(oa_lus['scotland'][['OutputArea2011Code', 'DataZone2011Code']].rename(columns={'OutputArea2011Code':'OA', 'DataZone2011Code':'DZ'}), scotland_pop)

scotland_pop['Total_population'] = scotland_pop['Total_population'] / scotland_pop['n_oas']
scotland_pop = scotland_pop.drop(columns = ['n_oas', 'DZ']).rename(columns = {'Total_population':'pop'})

eng_imd = pd.merge(oa_lus['england'], imd_data['england'], left_on='LSOA11CD', right_on='lsoa11cd', how = 'left').dropna(subset = ['WorkPop'])
wales_imd = pd.merge(oa_lus['england'], imd_data['wales'], left_on='LSOA11CD', right_on='LSOA_Code', how = 'left').dropna(subset = ['WIMD2019_Quartile'])
scotand_imd = pd.merge(oa_lus['scotland'], imd_data['scotland'], left_on='DataZone2011Code', right_on='DZ', how = 'left')
ni_imd = pd.merge(oa_lus['ni'], imd_data['ni'], left_on='SOA2011', right_on='SOA2001', how = 'left')

eng_imd = pd.merge(eng_imd, pop_data['england'], left_on='OA11CD', right_on='OA').dropna(subset = ['WorkPop'])
wales_imd = pd.merge(wales_imd, pop_data['england'], left_on='OA11CD', right_on='OA').dropna(subset = ['WIMD2019_Quartile'])
scotand_imd = pd.merge(scotand_imd, scotland_pop, left_on='OutputArea2011Code', right_on='OA')
ni_imd = pd.merge(ni_imd, pop_data['ni'], left_on='SA2011', right_on='Area_Code')

eng_imd['country'] = 'England'
wales_imd['country'] = 'Wales'
scotand_imd['country'] = 'Scotland'
ni_imd['country'] = 'Northern Ireland'

eng_imd = eng_imd.rename(columns = {'Pop':'pop', 'IMD_Rank':'imd'})[['OA', 'pop', 'imd', 'country']]
wales_imd = wales_imd.rename(columns = {'Pop':'pop', 'WIMD2019_Rank':'imd'})[['OA', 'pop', 'imd', 'country']]
scotand_imd = scotand_imd.rename(columns = {'SIMD2020v2_Rank':'imd'})[['OA', 'pop', 'imd', 'country']]
ni_imd = ni_imd.rename(columns = {'SA2011':'OA', 'MYE':'pop', 'MDM_rank':'imd'})[['OA', 'pop', 'imd', 'country']]

imd = pd.concat([eng_imd, wales_imd, scotand_imd, ni_imd])

imd = pd.merge(oa_tile_lookup, imd, left_on = 'OA', right_on = 'OA', how = 'left')

wm = lambda x: np.average(x, weights=imd.loc[x.index, "pop"])

imd = imd.groupby(['country', 'quadkey_12']).agg(wm_imd_rank=("imd", wm)).reset_index()

imd.to_csv(argv[-1])
