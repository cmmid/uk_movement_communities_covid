import __main__ as main
import sys
import geopandas as gpd
import pandas as pd
import numpy as np

if not hasattr(main, '__file__'):
    argv = ['code', 'data/processed/geo/tiles.shp',
            'data/processed/census/oa_tile_reference.csv',
            'data/raw/census_lookups/engwal_OA_lsoa.csv',
            'data/raw/census_lookups/OA_to_DZ.csv',
            'data/raw/census/NI_SA_Centroids.shp',
            'data/raw/ethnicity_data/bulk.csv',
            'data/raw/ethnicity_data/KS201SC.csv',
            'data/raw/ethnicity_data/DT201NI (s).csv',
            'data/raw/census/Eng_Wal_OA_Mid_Pop.csv',
            'data/raw/census/simd2020_withinds.csv',
            'data/raw/census/NI_Mid_Pop.csv',
            'data/processed/census/quadkey_mean_perc_white.csv']
else:
    argv = sys.argv
#%%
tiles = gpd.read_file(argv[1])
tiles.crs = 4326
#%%
oa_tile_lookup = pd.read_csv(argv[2])
#%%
oa_lus = {'england': pd.read_csv(argv[3]),
          'scotland': pd.read_csv(argv[4]),
          'ni': gpd.read_file(argv[5])}

#%%
oa_lus['ni'] = oa_lus['ni'].loc[:, ['SA2011', 'SOA2011']]
#%%
eth_data = {'england': pd.read_csv(argv[6]),
            'scotland': pd.read_csv(argv[7]),
            'ni': pd.read_csv(argv[8])}
#%%
scotland_imd = pd.read_csv(argv[10])
#%%
#check that the admin code is in the lookups
'''
england: lsoa level
Scotland: data zone level
NI: SOA level
'''
pop_data = {'england': pd.read_csv(argv[9]),
            'scotland': pd.read_csv(argv[10]),
            'ni': pd.read_csv(argv[11])}

# Handle scotland population peculiarities
scotland_n_oas = oa_lus['scotland'].groupby('DataZone2011Code').count().reset_index()[['DataZone2011Code', 'OutputArea2011Code']].rename(columns = {'DataZone2011Code':'DZ', 'OutputArea2011Code':'n_oas'})
scotland_pop = pd.merge(scotland_imd, scotland_n_oas)[['DZ', 'Total_population', 'n_oas']]
scotland_pop = pd.merge(oa_lus['scotland'][['OutputArea2011Code', 'DataZone2011Code']].rename(columns={'OutputArea2011Code':'OA', 'DataZone2011Code':'DZ'}), scotland_pop)
scotland_pop['Total_population'] = scotland_pop['Total_population'] / scotland_pop['n_oas']
scotland_pop = scotland_pop.drop(columns = ['n_oas', 'DZ']).rename(columns = {'Total_population':'pop'})

'''
England
'''
eth_data['england'] = pd.melt(eth_data['england'], id_vars = ['geography code'], value_vars = eth_data['england'].columns[3:])
eth_data['england']['variable'] = [x.split('.')[0] for x in eth_data['england']['variable']]
eth_data['england']['white'] = [x == 'White' for x in eth_data['england']['variable']]
eth_data['england']['value'] = [str(x).replace(',', '') for x in eth_data['england']['value']]
eth_data['england']['value'] = pd.to_numeric(eth_data['england']['value'], errors = 'coerce')

eth_data['england'] = eth_data['england'][['geography code', 'white', 'value']].groupby(['geography code', 'white']).sum().reset_index()
eth_data['england'] = eth_data['england'].pivot(index = 'geography code', columns = 'white').reset_index()
eth_data['england'].columns = eth_data['england'].columns.droplevel()

eth_data['england']['perc_white'] = eth_data['england'][True] / (eth_data['england'][True] + eth_data['england'][False])

'''Scotland'''
eth_data['scotland'] = pd.melt(eth_data['scotland'], id_vars = ['Area'], value_vars = eth_data['scotland'].columns[2:])
eth_data['scotland']['variable'] = [x.split('.')[0] for x in eth_data['scotland']['variable']]
eth_data['scotland']['white'] = [x == 'White' for x in eth_data['scotland']['variable']]
eth_data['scotland']['value'] = [str(x).replace('-', '0') for x in eth_data['scotland']['value']]
eth_data['scotland']['value'] = pd.to_numeric(eth_data['scotland']['value'], errors = 'coerce')

eth_data['scotland'] = eth_data['scotland'][['Area', 'white', 'value']].groupby(['Area', 'white']).sum().reset_index()
eth_data['scotland'] = eth_data['scotland'].pivot(index = 'Area', columns = 'white').reset_index()
eth_data['scotland'].columns = eth_data['scotland'].columns.droplevel()
eth_data['scotland']['perc_white'] = eth_data['scotland'][True] / (eth_data['scotland'][True] + eth_data['scotland'][False])

'''NI'''
eth_data['ni'] = pd.melt(eth_data['ni'], id_vars = ['Code'], value_vars = eth_data['ni'].columns[3:])
eth_data['ni']['variable'] = [x.split('.')[0] for x in eth_data['ni']['variable']]
eth_data['ni']['white'] = [x == 'Ethnic group: White' for x in eth_data['ni']['variable']]
eth_data['ni']['value'] = [str(x).replace(',', '') for x in eth_data['ni']['value']]
eth_data['ni']['value'] = pd.to_numeric(eth_data['ni']['value'], errors = 'coerce')
eth_data['ni'] = eth_data['ni'][['Code', 'white', 'value']].groupby(['Code', 'white']).sum().reset_index()
eth_data['ni'] = eth_data['ni'].pivot(index = 'Code', columns = 'white').reset_index()
eth_data['ni'].columns = eth_data['ni'].columns.droplevel()

eth_data['ni']['perc_white'] = eth_data['ni'][True] / (eth_data['ni'][True] + eth_data['ni'][False])

eth_data['england'].columns = ['Code', False, True, 'perc_white']
eth_data['scotland'].columns = ['Area', False, True, 'perc_white']
eth_data['ni'].columns = ['Code', False, True, 'perc_white']

ew_eth = pd.merge(oa_lus['england'], eth_data['england'], left_on='LSOA11CD', right_on='Code', how = 'left')
scotand_eth = pd.merge(oa_lus['scotland'], eth_data['scotland'], left_on='DataZone2011Code', right_on='Area', how = 'left')
ni_eth = pd.merge(oa_lus['ni'], eth_data['ni'], left_on='SA2011', right_on='Code', how = 'left')

ew_eth = pd.merge(ew_eth, pop_data['england'], left_on='OA11CD', right_on='OA')
scotand_eth = pd.merge(scotand_eth, scotland_pop, left_on='OutputArea2011Code', right_on='OA')
ni_eth = pd.merge(ni_eth, pop_data['ni'], left_on='SA2011', right_on='Area_Code')

ew_eth['country'] = ew_eth['OA11CD'].astype(str).str[0]
ew_eth['country'] = [str(x).replace('E', 'England') for x in ew_eth['country']]
ew_eth['country'] = [str(x).replace('W', 'Wales') for x in ew_eth['country']]
scotand_eth['country'] = 'Scotland'
ni_eth['country'] = 'Northern Ireland'

ew_eth = ew_eth.rename(columns = {'Pop':'pop'})[['OA', 'pop', 'perc_white', 'country']]
scotand_eth = scotand_eth[['OA', 'pop', 'perc_white', 'country']]
ni_eth = ni_eth.rename(columns = {'SA2011':'OA', 'MYE':'pop'})[['OA', 'pop', 'perc_white', 'country']]


eth = pd.concat([ew_eth, scotand_eth, ni_eth])


eth = pd.merge(oa_tile_lookup, eth, left_on = 'OA', right_on = 'OA', how = 'left')

wm = lambda x: np.average(x, weights=eth.loc[x.index, "pop"])

eth = eth.groupby(['country', 'quadkey_12']).agg(wm_perc_white=("perc_white", wm)).reset_index()

eth.to_csv(argv[-1])
