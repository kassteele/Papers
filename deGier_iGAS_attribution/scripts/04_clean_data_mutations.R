#
# Mutaties
#

igas_data <- igas_data |>
  mutate(
    # Add categorical variable infA_seas, coded as 1011, 1112, ..., 2223
    # An infA season runfs from July 1st to June 30th
    infA_seas = if_else(
      condition = month(Week) |> between(7, 12),
      true  = str_c(year(Week) |> str_sub(start = -2), (year(Week) + 1) |> str_sub(start = -2)),
      false = str_c((year(Week) - 1) |> str_sub(start = -2), year(Week) |> str_sub(start = -2))) |>
      factor(),
    # For COVID-19, take logarithm because these are particles in waste water
    # If there are no observations, SARSCoV2 is set to 0
    SARSCoV2 = if_else(
      condition = SARSCoV2 > 0,
      true = log(SARSCoV2),
      false = 0),
    # Scale pathogen values by their maximum value
    # This only affects the beta's, not the attribution
    # across(
    #   .cols = Strep:VZV,
    #   .fns = \(x) x/max(x)),
    # Make lagged pathogen columns. These are so-called matrix columns
    # This allows a direct usage of them (as matrix) in the Stan scripts
    across(
      .cols = Strep:VZV,
      .fns = \(x) lag_vec(x, n = 0:k_lag)),
    # Calcuate iGAS coverage by ISIS as proportion
    # We use this variable as a multiplicaiton factor in the model
    # The lower the coverage, the less iGAS cases we expect, and v.v.
    # We must assume that all reporting labs report a similar amount of iGAS
    Coverage_isis = Coverage_isis/max(Coverage_isis))

# Drop records with any NA's
# These are the first few records, because of the lagged values
# We cannot do anything with these records
igas_data <- igas_data |>
  drop_na()
