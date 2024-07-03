#
# Fit model child_bla
#

fit <- stan(
  file = "Stan/model_child_bla.stan",
  data = data_list,
  init = init_fun,
  chains = n_chains, warmup = n_burn*n_thin, iter = (n_burn + n_post)*n_thin, thin = n_thin,
  pars = c(
    "beta_Trend", "sigma_b_Trend", "phi",
    "beta_Strep", "beta_infA", "beta_infB", "beta_RSV", "beta_hMPV", "beta_SARSCoV2", "beta_VZV",
    "w_Strep", "w_infA", "w_infB", "w_RSV", "w_hMPV", "w_SARSCoV2", "w_VZV",
    "mu_Trend", "mu_Strep", "mu_infA", "mu_infB", "mu_RSV", "mu_hMPV", "mu_SARSCoV2", "mu_VZV", "mu_iGAS"))

# Extract mu MCMC samples
mu_child_bla <- fit |>
  extract(
    pars = c("mu_Trend", "mu_Strep", "mu_infA", "mu_infB", "mu_RSV", "mu_hMPV", "mu_SARSCoV2", "mu_VZV", "mu_iGAS")) |>
  # Transpose to have weeks in the rows, samples in the columns
  map(
     .f = t) |>
  as_tibble() |>
  # It is easier to work with list columns instead of matrix columns
  # Therefore split the matrix by row (week) into a list with n_post*n_chains samples for each week
  mutate(
    across(
      .cols = everything(),
      .fns = \(x) x |> asplit(MARGIN = 1)))

# Extract w MCMC samples and put them in a tibble with matrix columns
w_child_bla <- fit |>
  extract(
    pars = c("w_Strep", "w_infA", "w_infB", "w_RSV", "w_hMPV", "w_SARSCoV2", "w_VZV")) |>
  as_tibble()

# Export fit for futher use
mu_child_bla |> saveRDS(file = "output/mu_child_bla.rds")
w_child_bla |> saveRDS(file = "output/w_child_bla.rds")

# # Diagnose mixing
# fit |> stan_trace(pars = c("beta_Trend", "sigma_b_Trend", "phi"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("beta_Strep", "beta_infB", "beta_RSV", "beta_hMPV", "beta_SARSCoV2", "beta_VZV"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("beta_infA"), ncol = 2, size = 0.1)
# fit |> stan_trace(pars = c("w_Strep"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("w_infA"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("w_infB"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("w_RSV"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("w_hMPV"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("w_SARSCoV2"), ncol = 1, size = 0.1)
# fit |> stan_trace(pars = c("w_VZV"), ncol = 1, size = 0.1)

# # Diagnose divergent transitions
# fit |> pairs(pars = c("beta_Trend", "sigma_b_Trend"))
# fit |> pairs(pars = c("beta_Strep", "beta_infB", "beta_RSV", "beta_hMPV", "beta_SARSCoV2", "beta_VZV"))
# fit |> pairs(pars = c("beta_infA"))
# fit |> pairs(pars = c("w_Strep"))
# fit |> pairs(pars = c("w_infA"))
# fit |> pairs(pars = c("w_infB"))
# fit |> pairs(pars = c("w_RSV"))
# fit |> pairs(pars = c("w_hMPV"))
# fit |> pairs(pars = c("w_SARSCoV2"))
# fit |> pairs(pars = c("w_VZV"))
