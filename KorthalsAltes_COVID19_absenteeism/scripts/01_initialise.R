#
# Init
#

# Load packages
library(tidyverse)
library(ISOweek)
library(parallel)
library(splines)

# Get operation system
os <- Sys.info()["sysname"]

# Set options
# - Number of cores for parallel processing (Linux only)
# - Weeks start on Mondays (1)
options(
  mc.cores = if_else(os == "Linux", detectCores(), 1L),
  lubridate.week.start = 1)

# Dates in EN (Linux)
if (os == "Linux") {
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
} else {
  Sys.setlocale(category = "LC_TIME", locale = "English")
}

# Average number of week in a year
# Needed for seasonal effect
p_seas <- 365.25/7

# Functions to extract things from model fit
#
# Input:
# object = model fit object, e.g. lm, glm or gam
# pattern = pattern to look for. The default interpretation is a regular expression
# Ouput:
# coef, vcov of model matrix

# Function to extract coefs from model fit based on pattern
extract_coef <- function(object, pattern = ".*") {
  vec <- object |> coef()
  ix <- vec |> names() |> str_detect(pattern)
  vec[ix]
}

# Function to extract var-covar matrix from model fit based on pattern
extract_vcov <- function(object, pattern = ".*") {
  mat <- object |> vcov()
  ix <- mat |> colnames() |> str_detect(pattern)
  mat[ix, ix]
}

# Function to extract model matrix from model fit based on pattern
extract_model_matrix <- function(object, pattern = ".*") {
  mat <- object |> model.matrix()
  ix <- mat |> colnames() |> str_detect(pattern)
  mat[, ix]
}
