#
# Step 4. Model post processing
#

# Here we add the four time series components to data_absent_fit
# This new tibble is called data_absent_comp

data_absent_comp <- map2(
  .x = model_fits,
  .y = data_absent_fit,
  .f = function(fit, data) {

    # Coefficients
    beta_trend <- fit |> pluck("model_fit") |> extract_coef("\\(Intercept\\)|holiday|ns\\(t")
    beta_seas  <- fit |> pluck("model_fit") |> extract_coef("sin|cos")
    beta_cov   <- fit |> pluck("model_fit") |> extract_coef("perc_symptoms")
    beta_ar    <- fit |> pluck("model_fit") |> extract_coef("ar")

    # Regression components
    # seasonal term is not always present -> set to 0
    # Use drop() to create a vector instead of a single column matrix
    trend <- drop((fit |> pluck("X_trend")) %*% beta_trend)
    if (length(beta_seas) > 0) {
      seas <- drop((fit |> pluck("X_seas")) %*% beta_seas)
    } else {
      seas = rep(0, nrow(data))
    }
    cov <- drop((fit |> pluck("X_cov")) %*% beta_cov)

    # Observed absenteeism
    obs <- data |> pull(rate_absent)

    # AR component
    # The residuals are lagged for the modelmatrix of the AR component
    res  <- obs - (trend + seas + cov)
    X_ar <- beta_ar |> seq_along() |> sapply(FUN = lag, x = res)
    ar   <- drop(X_ar %*% beta_ar)

    # Determine the lowest value of each component
    # This is used for the addition
    min_trend <- min(trend)
    min_seas  <- min(seas)
    min_cov   <- min(cov)
    min_ar    <- min(ar, na.rm = TRUE) # Always includes NAs, minimum 1, maximum 4

    # Construct components
    # These four terms added up result in the eventual fit
    data <- data |>
      mutate(
        trend = trend,
        seas = seas,
        cov = cov,
        ar = ar,
        res = rate_absent - trend - seas - cov - ar,
        comp_trend = trend + min_seas + min_cov + min_ar,
        comp_seas  = seas - min_seas,
        comp_cov   = cov - min_cov,
        comp_ar    = ar - min_ar)

    # Return output
    return(data)
  }) |>
  # Make a tibble from the list of tibbles
  bind_rows()

# Print Table 1
# Proportion variance explained by componenets
data_absent_comp |>
  group_by(
    Sector) |>
  summarise(
    var_trend = var(trend, na.rm = TRUE),
    var_seas = var(seas, na.rm = TRUE),
    var_infr = var(cov, na.rm = TRUE),
    var_ar = var(ar, na.rm = TRUE),
    var_res = var(res, na.rm = TRUE)) |>
  mutate(
    var_tot = var_trend + var_seas + var_infr + var_ar + var_res,
    across(
      .cols = starts_with("var_"),
      .fns = \(x) round(100*x/var_tot, digits = 1)))
