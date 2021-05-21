import __main__ as main
import sys
import geopandas as gpd
import pandas as pd
import numpy as np

if not hasattr(main, '__file__'):
    argv = ['code', 'data/processed/geo/tiles.shp',
            'data/processed/census/oa_tile_reference.csv',
            'data/raw/census/engwal_OA_lsoa.csv',
            'data/raw/census/OA_to_DZ.csv',
            'data/raw/census/NI_SA_Centroids.shp',
            'data/raw/age_data/ew_age.csv',
            'data/raw/age_data/QS103SC.csv',
            'data/raw/age_data/KS102NI (s).csv',
            'data/raw/census/Eng_Wal_OA_Mid_Pop.csv',
            'data/raw/census/simd2020_withinds.csv',
            'data/raw/census/NI_Mid_Pop.csv',
            'data/processed/census/quadkey_mean_age.csv']
else:
    argv = sys.argv

tiles = gpd.read_file(argv[1])
tiles.crs = 4326

oa_tile_lookup = pd.read_csv(argv[2])

oa_lus = {'england': pd.read_csv(argv[3]),
          'scotland': pd.read_csv(argv[4]),
          'ni': gpd.read_file(argv[5])}

oa_lus['ni'] = oa_lus['ni'].loc[:, ['SA2011', 'SOA2011']]

age_data = {'england': pd.read_csv(argv[6]),
            'scotland': pd.read_csv(argv[7]),
            'ni': pd.read_csv(argv[8])}

scotland_imd = pd.read_csv(argv[10])

pop_data = {'england': pd.read_csv(argv[9]),
            'scotland': pd.read_csv(argv[10]),
            'ni': pd.read_csv(argv[11])}

scotland_n_oas = oa_lus['scotland'].groupby('DataZone2011Code').count().reset_index()[['DataZone2011Code', 'OutputArea2011Code']].rename(columns = {'DataZone2011Code':'DZ', 'OutputArea2011Code':'n_oas'})
scotland_pop = pd.merge(scotland_imd, scotland_n_oas)[['DZ', 'Total_population', 'n_oas']]
scotland_pop = pd.merge(oa_lus['scotland'][['OutputArea2011Code', 'DataZone2011Code']].rename(columns={'OutputArea2011Code':'OA', 'DataZone2011Code':'DZ'}), scotland_pop)
scotland_pop['Total_population'] = scotland_pop['Total_population'] / scotland_pop['n_oas']
scotland_pop = scotland_pop.drop(columns = ['n_oas', 'DZ']).rename(columns = {'Total_population':'pop'})

wm_e = lambda x: np.average(x, weights=age_data['england'].loc[x.index, "value"])

age_data['england'] = pd.melt(age_data['england'], id_vars = ['Area Codes'], value_vars = age_data['england'].columns[4:])
age_data['england']['variable'] = [str(x).replace('+', '') for x in age_data['england']['variable']]
age_data['england']['variable'] = pd.to_numeric(age_data['england']['variable'], errors = 'coerce')
age_data['england'] = age_data['england'].groupby(['Area Codes']).agg(mean_age = ('variable', wm_e)).reset_index()

# Scotland
wm_s = lambda x: np.average(x, weights=age_data['scotland'].loc[x.index, "value"])

age_data['scotland'] = pd.melt(age_data['scotland'], id_vars = ['Area'], value_vars = age_data['scotland'].columns[2:])
age_data['scotland']['variable'] = [str(x).replace('Under ', '') for x in age_data['scotland']['variable']]
age_data['scotland']['variable'] = [str(x).replace(' and over', '') for x in age_data['scotland']['variable']]
age_data['scotland']['variable'] = pd.to_numeric(age_data['scotland']['variable'], errors = 'coerce')
age_data['scotland']['value'] = pd.to_numeric(age_data['scotland']['value'], errors = 'coerce')
age_data['scotland'].dropna(subset = ['value'], inplace = True)
age_data['scotland'] = age_data['scotland'].groupby(['Area']).agg(mean_age = ('variable', wm_s)).reset_index()

# This is done for ireland
age_data['ni']['Mean age of population']

'''
Merge mean ages with look up tables
'''
ew_age = pd.merge(oa_lus['england'], age_data['england'], left_on='LSOA11CD', right_on='Area Codes', how = 'left')
scotand_age = pd.merge(oa_lus['scotland'], age_data['scotland'], left_on='DataZone2011Code', right_on='Area', how = 'left')
ni_age = pd.merge(oa_lus['ni'], age_data['ni'], left_on='SOA2011', right_on='SOA Code', how = 'left')[['SA2011', 'SOA2011', 'Mean age of population']]

'''
Merge this with OA population estimates
'''

ew_age = pd.merge(ew_age, pop_data['england'], left_on='OA11CD', right_on='OA')
scotand_age = pd.merge(scotand_age, scotland_pop, left_on='OutputArea2011Code', right_on='OA')
ni_age = pd.merge(ni_age, pop_data['ni'], left_on='SA2011', right_on='Area_Code')

ew_age['country'] = ew_age['OA11CD'].astype(str).str[0]
ew_age['country'] = [str(x).replace('E', 'England') for x in ew_age['country']]
ew_age['country'] = [str(x).replace('W', 'Wales') for x in ew_age['country']]
scotand_age['country'] = 'Scotland'
ni_age['country'] = 'Northern Ireland'

ew_age = ew_age.rename(columns = {'Pop':'pop'})[['OA', 'pop', 'mean_age', 'country']]
scotand_age = scotand_age[['OA', 'pop', 'mean_age', 'country']]
ni_age = ni_age.rename(columns = {'SA2011':'OA', 'MYE':'pop', 'Mean age of population':'mean_age'})[['OA', 'pop', 'mean_age', 'country']]

age = pd.concat([ew_age, scotand_age, ni_age])

age = pd.merge(oa_tile_lookup, age, left_on = 'OA', right_on = 'OA', how = 'left')

wm = lambda x: np.average(x, weights=age.loc[x.index, "pop"])

age = age.groupby(['country', 'quadkey_12']).agg(wm_age=("mean_age", wm)).reset_index()

age.to_csv(argv[-1])
