#
# Post-process lag weigts
#

# Import fited lag weights from rds
w_adult_bla <- readRDS(file = "output/w_adult_bla.rds")
w_child_bla <- readRDS(file = "output/w_child_bla.rds")
w_child_wps <- readRDS(file = "output/w_child_wps.rds")

# Create table with lag weights
tab_lag_weights <- list(
  # First create a list of these objects (the groups)
  adult_bla = w_adult_bla,
  child_bla = w_child_bla,
  child_wps = w_child_wps) |>
  # Loop over each group
  map(
    .f = \(group) group |>
      # Loop over each column (Pathogen)
      map(
        .f = \(x)
        tibble(
          lag = 0:4,
          mean = x |> colMeans(),
          lwr = x |> apply(MARGIN = 2, FUN = quantile, probs = 0.025),
          upr = x |> apply(MARGIN = 2, FUN = quantile, probs = 0.975))) |>
      bind_rows(
        .id = "Pathogen")) |>
  bind_rows(
    .id = "Group")
