# Absenteeism per sector and the relation with the weekly
# percentage COVID-19-like symptoms from Infectieradar
#
# Authors: Jan van de Kassteele (ODD-SIM)
#          Hester Korthals Altes and Jacco Wallinga (EPI-MOD)
# RIVM

source(file = "scripts/01_initialise.R")
source(file = "scripts/02_import_data_infectionradar.R")
source(file = "scripts/02_import_data_absent.R")
source(file = "scripts/03_clean_data_infectionradar.R")
source(file = "scripts/03_clean_data_absent.R")
source(file = "scripts/04_join_datasources.R")
source(file = "scripts/05_make_data_absent_fit.R")
source(file = "scripts/05_make_model_formulas.R")
source(file = "scripts/06_model_fits_ARXp.R")
source(file = "scripts/07_model_lowest_AIC.R")
source(file = "scripts/08_model_post_processing.R")
source(file = "scripts/09_make_data_absent_plot.R")
source(file = "scripts/10_plot_data_infectionradar.R")
source(file = "scripts/10_plot_absent_fit_overview.R")
source(file = "scripts/11_export_figures.R")
