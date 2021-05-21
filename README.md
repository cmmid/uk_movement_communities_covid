# uk_movement_communities_covid
Repository of code supporting the publication: "Detecting behavioural changes in human movement to inform the spatial scale of interventions against COVID-19." Gibbs et al. 2021.

*Hamish Gibbs1, Emily Nightingale, Yang Liu, James Cheshire, Leon Danon, Liam Smeeth, Carl AB Pearson, Chris Grundy, LSHTM CMMID COVID-19 working group, Adam J Kucharski and Rosalind M Eggo.*

#### Overview

.
├── .gitignore            
├── LICENSE
├── README.md
├── data                        # Directory of all publication data (untracked)
│   ├── raw                     # Directory of raw, unaltered data
│       ├── geo                 # Directory of raw, unaltered geospatial data
│       ├── age_data            # Directory of raw, unaltered age data
│       ├── census              # Directory of raw, unaltered census population data
│       ├── census_lookups      # Directory of raw, unaltered lookups between statistical areas
│       ├── ethnicity_data      # Directory of raw, unaltered ethnicity data
│       └── imd_data            # Directory of raw, imd data
│   └── processed               # Directory of processed data
│       ├── census              # Directory of processed census population data
│       ├── geo                 # Directory of processed geospatial data
│       ├── geo_lu              # Directory of processed geospatial lookups data
│       ├── infomap             # Directory of processed infomap communities
│       ├── mob                 # Directory of processed mobility data
│       └── pop                 # Directory of processed population data
└── src                         # Directory of all source code
    ├── data                    # Directory of data processing code
    ├── cases                   # Directory of cases analysis code
    ├── representative          # Directory of representativity analysis code
    ├── network                 # Directory of network structure analysis code
    ├── communities             # Directory of communities analysis code
    └── local_interventions     # Directory of local intervention communities analysis code
