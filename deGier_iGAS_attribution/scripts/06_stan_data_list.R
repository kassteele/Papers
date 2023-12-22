#
# Stan data list
#

# Stan requires a list with all data
# For each model, use the same list
# Selection of relevant items will be done in the Stan scripts
data_list <- list(
  # Total number of weeks
  n = igas_data |> nrow(),
  # Number of columns of the matrices
  k_Trend_u = ps_setup |> pluck("Xu_data") |> ncol(),
  k_Trend_p = ps_setup |> pluck("Xp_data") |> ncol(),
  k_infA = igas_data |> pull(infA_seas) |> nlevels(),
  k_lag = k_lag + 1, # +1 because lag 0 is included as well
  # The outcomes, be we will only select one
  iGAS_adult_bla = igas_data |> pull(iGAS_adult_bla),
  iGAS_child_bla = igas_data |> pull(iGAS_child_bla),
  iGAS_child_wps = igas_data |> pull(iGAS_child_wps),
  # P-spline matrices for the large-scale trend
  X_Trend_u = ps_setup |> pluck("Xu_data"),
  X_Trend_p = ps_setup |> pluck("Xp_data"),
  # Matrix for infA season indicator
  # 1 if week corresponds to given infA season, 0 otherwise
  X_infA = igas_data |> model.matrix(object = ~ infA_seas - 1),
  # Pathogens to make te attribution for
  Strep = igas_data |> pull(Strep),
  infA = igas_data |> pull(infA),
  infB = igas_data |> pull(infB),
  RSV = igas_data |> pull(RSV),
  hMPV = igas_data |> pull(hMPV),
  SARSCoV2 = igas_data |> pull(SARSCoV2),
  VZV = igas_data |> pull(VZV),
  # ISIS coverage
  Coverage_isis = igas_data |> pull(Coverage_isis),
  # For the Dirichet prior of the lag weights, use alpha = 1/k prior
  # https://arxiv.org/abs/1504.02689, p205
  alpha = rep(1/(k_lag + 1), k_lag + 1))

# Init function
# This is not really required
init_fun <- function() {
  list(
    beta_Trend = c(1, rep(0, data_list$k_Trend_u - 1)),
    beta_Strep = 1,
    beta_infA = rep(1, data_list$k_infA),
    beta_infB = 1,
    beta_RSV = 1,
    beta_hMPV = 1,
    beta_SARSCoV2 = 1,
    beta_VZV = 1,
    sigma_b_Trend = 1,
    phi = 10)
}
