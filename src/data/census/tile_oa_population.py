import __main__ as main
import sys
import pandas as pd

if not hasattr(main, '__file__'):
    argv = ['a',
            'data/processed/census/oa_tile_reference.csv',
            'data/raw/census/Eng_Wal_OA_Mid_Pop.csv',
            'data/raw/census/OA_to_DZ.csv',
            'data/raw/census/simd2020_withinds.csv',
            'data/raw/census/NI_Mid_Pop.csv',
            "data/processed/census/tile_imd.csv"]
else:
    argv = sys.argv

tiles = pd.read_csv(argv[1])

england_oa = pd.read_csv(argv[2])
scotland_zone_ref = pd.read_csv(argv[3])
scotland_oa = pd.read_csv(argv[4])
ni_oa = pd.read_csv(argv[5])

scotland_n_oas = scotland_zone_ref.groupby('DataZone2011Code').count().reset_index()[['DataZone2011Code', 'OutputArea2011Code']].rename(columns = {'DataZone2011Code':'DZ', 'OutputArea2011Code':'n_oas'})

england_oa = england_oa.rename(columns = {'Pop':'pop'})[['OA', 'pop']]

scotland_oa = scotland_oa.rename(columns = {'DZ':'DZ', 'Total_population':'pop'})[['DZ', 'pop']]

ni_oa = ni_oa.rename(columns = {'Area_Code':'OA', 'MYE':'pop'})[['OA', 'pop']]

oas = {'england': england_oa, 'scotland': scotland_oa, 'ni': ni_oa}
pop_summary = {'england': england_oa['pop'].sum(), 'scotland': scotland_oa['pop'].sum(), 'ni': ni_oa['pop'].sum()}

assert pop_summary['england'] == 59115809
assert pop_summary['scotland'] == 5424800
assert pop_summary['ni'] == 1881623

scotland_pop = pd.merge(scotland_oa, scotland_n_oas)[['DZ', 'pop', 'n_oas']]
scotland_oa = pd.merge(scotland_zone_ref[['OutputArea2011Code', 'DataZone2011Code']].rename(columns={'OutputArea2011Code':'OA', 'DataZone2011Code':'DZ'}), scotland_pop)

scotland_oa['pop'] = scotland_oa['pop'] / scotland_oa['n_oas']
scotland_oa.drop(columns = ['n_oas', 'DZ'], inplace = True)

pops = []
for pop in [england_oa, scotland_oa, ni_oa]:
    pops.append(pd.merge(tiles, pop, how = 'left').dropna(subset = ['pop']))
pops = pd.concat(pops)

pops = pops[['quadkey_12', 'pop']].groupby(['quadkey_12']).sum().reset_index()

assert pops['pop'].sum() < sum(pop_summary.values()) + 5
assert pops['pop'].sum() > sum(pop_summary.values()) - 5

pops.to_csv(argv[-1], index = False)
