#
# Step 2. Fit ARX(p) models
#

# Fit the ARX(p) models for each record in model_formulas
# An ARX(p) model is a AR(p) model with covariates (X)
# As equation:
# y_t - sum(X_t*beta) = phi_1*[y_{t-1} - sum(X_{t-1}*beta)] + ... + phi_p*[y_{t-p} - sum(X_{t-p}*beta)] + epsilon_t

# We only store the AIC of the fitted model, not the model itself
# The output is a list with model_formulas + AICs per sector
# The best model per sector is chosen in step 3, and the corresponding model is refitted

# Parallel loop over sector
# This takes a while, even though it happens in parallel
# Note: mclappy() only works on Linux machines. Replace with lapply on Windows

model_formulas_AIC <- data_absent_fit |>
  mclapply(
    FUN = function(data_absent_fit) {

      # Loop over regression models in model_formulas
      for (i in 1:nrow(model_formulas)) {

        # Model matrix trend
        # Here with intercept
        X_trend <- model.matrix(
          object = formula(model_formulas[i, ]$formula_trend),
          data = data_absent_fit)

        # Model matrix seasonal terms
        # Without intercept
        X_seas <- model.matrix(
          object = formula(model_formulas[i, ]$formula_seas),
          data = data_absent_fit)

        # Model matrix covariate
        # Without intercept
        X_cov <- model.matrix(
          object = formula(model_formulas[i, ]$formula_cov),
          data = data_absent_fit)

        # Combine model matrices
        # These are feeded to xreg in the arima() function
        X <- cbind(X_trend, X_seas, X_cov)

        # Given the regression part, loop over the four AR(p) models
        for (p in 1:4) {

          # Fit AR(p) model
          # Use the xreg argument for the regression part
          # Do not estimate the mean, this is already included in xreg
          fit_rate <- arima(
            x = data_absent_fit |> pull(rate_absent),
            order = c(p, 0, 0),
            xreg = X,
            include.mean = FALSE,
            method = "ML")

          # Determine AIC of the corresponding ARX(p) model
          # and add this to model_formulas
          model_formulas[i, str_glue("aic_ar{p}")] <- AIC(fit_rate)

          # End AR(p) models loop
        }

        # End regression models loop
      }

      # Return model_formulas, now with AICs included
      return(model_formulas)

      # End parallel loop over Sector
    })
