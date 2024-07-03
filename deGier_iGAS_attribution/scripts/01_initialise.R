#
# Init
#

# Load R packages
library(tidyverse)
library(rstan)
library(writexl)

# Source functions
list.files(path = "functions", full.names = TRUE) |>
  walk(.f = source)

# Set number of lags
k_lag <- 4 # Lag: 0, 1, ..., k_lag weeks back

# Vectorise argument n of dplyr::lag()
# This enables us to apply this function for multiple n's instead of one
lag_vec <- dplyr::lag |>
  Vectorize(vectorize.args = "n")

# Set Stan opties
options(mc.cores = parallel::detectCores()) # Number of cores
rstan_options(auto_write = TRUE) # Avoid recompling

# Stan settings
n_chains <- 4 # Number of parallel MCMC chains
n_burn <- 500 # Number of warm-up samples per chain after thinning
n_post <- 1000 # Number of posterior samples per chain after thinning
n_thin <- 1

# Colours plot plotting
# Each pathogen gets its own colour from the Okabe-Ito palette (minus black)
# Include "trend" as the first one
cols <- palette.colors(palette = "Okabe-Ito")[-1] |>
  set_names(
    c("trend", "non-invasive GAS", "influenza A", "influenza B", "RSV", "hMPV", "SARS-CoV-2", "varicella"))
