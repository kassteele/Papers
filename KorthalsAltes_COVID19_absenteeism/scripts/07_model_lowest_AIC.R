#
# Step 3: refit ARX(p) model per sector with the lowest AIC
#

# Modelformula with lowest AIC per Sector
# Dit is een list with een-rij tibble with model formulas
model_formula_lowest_AIC <- model_formulas_AIC |>
  map(
    .f = \(x) x |>
      # Convert to long format
      pivot_longer(
        cols = starts_with("aic_ar"),
        names_to = "ar",
        values_to = "aic") |>
      # The column with ar must be a single integer, not e.g. "aic_ar1", but 1
      mutate(
        ar = ar |> str_extract("\\d") |> as.integer()) |>
      # the best model heeft de lowest AIC
      slice_min(
        aic))

# Fit the best model per sector
# This is a list with arima fits
model_fits <- map2(
  .x = model_formula_lowest_AIC,
  .y = data_absent_fit,
  .f = function(model_formula, data_absent_fit) {

    # Model matrix trend
    X_trend <- model.matrix(
      object = formula(model_formula$formula_trend),
      data = data_absent_fit)

    # Model matrix harmonic terms
    X_harm <- model.matrix(
      object = formula(model_formula$formula_harm),
      data = data_absent_fit)

    # Model matrix covariate
    X_cov <- model.matrix(
      object = formula(model_formula$formula_cov),
      data = data_absent_fit)

    # Combine model matrices
    X <- cbind(X_trend, X_harm, X_cov)

    # Fit best model
    model_fit <- arima(
      x = data_absent_fit |> pull(rate_absent),
      order = c(model_formula$ar, 0, 0),
      xreg = X,
      include.mean = FALSE,
      method = "ML")

    # Return model fit and model matrices
    # Model matrices are not included in arima fit objects... contrary to e.g. lm or glm objects
    return(
      list(
        model_fit = model_fit,
        X_trend = X_trend,
        X_harm = X_harm,
        X_cov = X_cov))

  })
