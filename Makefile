PROJECT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PYTHON_INTERPRETER = /Users/hamishgibbs/anaconda3/bin/python

proj:
	echo $(PROJECT_DIR)


infomap_daily: $(PROJECT_DIR)/data/processed/infomap/infomap_daily.csv

$(PROJECT_DIR)/data/processed/infomap/infomap_daily.csv: $(PROJECT_DIR)/src/data/run_infomap.py $(PROJECT_DIR)/data/processed/mob/movement_daily.csv
	$(PYTHON_INTERPRETER) $^ $@

infomap_weekly: $(PROJECT_DIR)/data/processed/infomap/infomap_weekly.csv

$(PROJECT_DIR)/data/processed/infomap/infomap_weekly.csv: $(PROJECT_DIR)/src/data/run_infomap.py $(PROJECT_DIR)/data/processed/mob/movement_weekly.csv
	$(PYTHON_INTERPRETER) $^ $@

infomap_monthly: $(PROJECT_DIR)/data/processed/infomap/infomap_monthly.csv

$(PROJECT_DIR)/data/processed/infomap/infomap_monthly.csv: $(PROJECT_DIR)/src/data/run_infomap.py $(PROJECT_DIR)/data/processed/mob/movement_monthly.csv
	$(PYTHON_INTERPRETER) $^ $@

infomap_labelled_daily: $(PROJECT_DIR)/data/processed/infomap/infomap_labelled_daily.csv

$(PROJECT_DIR)/data/processed/infomap/infomap_labelled_daily.csv: $(PROJECT_DIR)/src/data/run_label_map.py $(PROJECT_DIR)/data/processed/infomap/infomap_daily.csv
	$(PYTHON_INTERPRETER) $^ $@

tile_oa: $(PROJECT_DIR)/data/processed/census/oa_tile_reference.csv

$(PROJECT_DIR)/data/processed/census/oa_tile_reference.csv: $(PROJECT_DIR)/src/data/census/OA_to_tiles.py \
																														$(PROJECT_DIR)/data/processed/geo/tiles.shp \
																														$(PROJECT_DIR)/data/raw/census/engwal_oa_bng.shp \
																														$(PROJECT_DIR)/data/raw/census/OutputArea2011_PWC.shp \
																														$(PROJECT_DIR)/data/raw/census/NI_SA_Centroids.shp
	$(PYTHON_INTERPRETER) $^ $@

tile_pop: $(PROJECT_DIR)/data/processed/census/tile_12_oa_pop.csv

$(PROJECT_DIR)/data/processed/census/tile_12_oa_pop.csv: $(PROJECT_DIR)/src/data/census/tile_oa_population.py \
																															 $(PROJECT_DIR)/data/processed/census/oa_tile_reference.csv \
																															 $(PROJECT_DIR)/data/raw/census/Eng_Wal_OA_Mid_Pop.csv \
																															 $(PROJECT_DIR)/data/raw/census/OA_to_DZ.csv \
																															 $(PROJECT_DIR)/data/raw/census/simd2020_withinds.csv \
																															 $(PROJECT_DIR)/data/raw/census/NI_Mid_Pop.csv
	$(PYTHON_INTERPRETER) $^ $@

tile_eth: $(PROJECT_DIR)/data/processed/census/quadkey_mean_perc_white.csv

$(PROJECT_DIR)/data/processed/census/quadkey_mean_perc_white.csv: $(PROJECT_DIR)/src/data/census/tile_ethnicity.py \
																												 $(PROJECT_DIR)/data/processed/geo/tiles.shp \
																												 $(PROJECT_DIR)/data/processed/census/oa_tile_reference.csv \
																												 $(PROJECT_DIR)/data/raw/census_lookups/engwal_OA_lsoa.csv \
																												 $(PROJECT_DIR)/data/raw/census_lookups/OA_to_DZ.csv \
																												 $(PROJECT_DIR)/data/raw/census/NI_SA_Centroids.shp \
																												 $(PROJECT_DIR)/data/raw/ethnicity_data/bulk.csv \
																												 $(PROJECT_DIR)/data/raw/ethnicity_data/KS201SC.csv \
																												 $(PROJECT_DIR)/data/raw/ethnicity_data/DT201NI.csv \
																												 $(PROJECT_DIR)/data/raw/census/Eng_Wal_OA_Mid_Pop.csv \
																												 $(PROJECT_DIR)/data/raw/census/simd2020_withinds.csv \
																												 $(PROJECT_DIR)/data/raw/census/NI_Mid_Pop.csv
	$(PYTHON_INTERPRETER) $^ $@

tile_imd: $(PROJECT_DIR)/data/processed/census/quadkey_imd.csv

$(PROJECT_DIR)/data/processed/census/quadkey_imd.csv: $(PROJECT_DIR)/src/data/census/imd_to_tiles.py \
																											 $(PROJECT_DIR)/data/processed/geo/tiles.shp \
																											 $(PROJECT_DIR)/data/processed/census/oa_tile_reference.csv \
																											 $(PROJECT_DIR)/data/raw/census_lookups/engwal_OA_lsoa.csv \
																											 $(PROJECT_DIR)/data/raw/census_lookups/OA_to_DZ.csv \
																											 $(PROJECT_DIR)/data/raw/census/NI_SA_Centroids.shp \
																											 $(PROJECT_DIR)/data/raw/imd_data/Eng_IMD.csv \
																											 $(PROJECT_DIR)/data/raw/imd_data/WIMD2019_Ranks.csv \
																											 $(PROJECT_DIR)/data/raw/imd_data/simd2020_withinds.csv \
																											 $(PROJECT_DIR)/data/raw/imd_data/NorthernIrelandMDM.csv \
																											 $(PROJECT_DIR)/data/raw/census/Eng_Wal_OA_Mid_Pop.csv \
																											 $(PROJECT_DIR)/data/raw/census/simd2020_withinds.csv \
																											 $(PROJECT_DIR)/data/raw/census/NI_Mid_Pop.csv
	$(PYTHON_INTERPRETER) $^ $@

tile_age: $(PROJECT_DIR)/data/processed/census/quadkey_age.csv

$(PROJECT_DIR)/data/processed/census/quadkey_age.csv: $(PROJECT_DIR)/src/data/census/tile_age.py \
																											 $(PROJECT_DIR)/data/processed/geo/tiles.shp \
																											 $(PROJECT_DIR)/data/processed/census/oa_tile_reference.csv \
																											 $(PROJECT_DIR)/data/raw/census_lookups/engwal_OA_lsoa.csv \
																											 $(PROJECT_DIR)/data/raw/census_lookups/OA_to_DZ.csv \
																											 $(PROJECT_DIR)/data/raw/census/NI_SA_Centroids.shp \
																											 $(PROJECT_DIR)/data/raw/age_data/ew_age.csv \
																											 $(PROJECT_DIR)/data/raw/age_data/QS103SC.csv \
																											 $(PROJECT_DIR)/data/raw/age_data/KS102NI.csv \
																											 $(PROJECT_DIR)/data/raw/census/Eng_Wal_OA_Mid_Pop.csv \
																											 $(PROJECT_DIR)/data/raw/census/simd2020_withinds.csv \
																											 $(PROJECT_DIR)/data/raw/census/NI_Mid_Pop.csv
	$(PYTHON_INTERPRETER) $^ $@
