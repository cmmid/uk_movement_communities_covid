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
