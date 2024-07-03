# iGAS time series decomposition - attribution to several pathogens
#
# Jan van de Kassteele
# Brechje de Gier

# These scripts are common
source(file = "scripts/01_initialise.R")
source(file = "scripts/02_import_data.R")
source(file = "scripts/03_clean_data_selection.R")
source(file = "scripts/04_clean_data_mutations.R")
source(file = "scripts/05_P-spline_trend_setup.R")
source(file = "scripts/06_stan_data_list.R")

# Fit models
source(file = "scripts/07a_fit_model_adult_bla.R")
source(file = "scripts/07b_fit_model_child_bla.R")
source(file = "scripts/07c_fit_model_child_wps.R")

# Post-processing
source(file = "scripts/08a_post_process_adult_bla.R")
source(file = "scripts/08b_post_process_child_bla.R")
source(file = "scripts/08c_post_process_child_wps.R")
source(file = "scripts/08d_post_process_lag_weights.R")

# Visualisations
source(file = "scripts/09a_plot_adult_bla.R")
source(file = "scripts/09b_plot_child_bla.R")
source(file = "scripts/09c_plot_child_wps.R")
source(file = "scripts/09d_plot_observed.R")

# Tabulations
source(file = "scripts/10_tabulate_attributions.R")

# Export results
source(file = "scripts/11_export_results.R")
