#
# Step 1. Make all combinations of possible regression models
#

# The absenteeism time series per sector is decomposed into four components:
# 1. Large-scale time trend
# 2. Seasonal effect
# 3. Effect a covariate, here perc_symtoms from Infectieradar
# 4. Autoregressive component based on absenteeism of the previous week(s)
#
# Added together, this results a the fit
# Residuals are not allowed to show any auto correlation anymore

model_formulas <- expand_grid(
  trend_df = 0:2,  # Trend: constant, linear or simple spline
  seas_sin1 = 0:1, # seasonal term: none or period 1 year (sin)
  seas_cos1 = 0:1, # seasonal term: none or period 1 year (cos)
  seas_sin2 = 0:1, # seasonal term: none or period 1/2 year (sin)
  seas_cos2 = 0:1, # seasonal term: none or period 1/2 year (cos)
  cov_df = 1:2) |> # Covariate effect: linear or eenvoudige spline
  filter(
    # No 1/2 years seasonal term if there is no 1-year period is
    !((seas_sin2 == 1 | seas_cos2 == 1) & seas_cos1 == 0 & seas_sin1 == 0)) |>
  mutate(
    # For the trend, include always an intercept and holiday
    # The intercept is added automatically when making the modelmatrix
    formula_trend = if_else(trend_df == 0, "~ holiday", str_glue("~ holiday + ns(t, df = {trend_df})")),
    # Exclude the intercept (~ 0) with the seasonal term
    # or else the model is not identifiable
    formula_seas = str_c(
      "~ 0",
      if_else(seas_sin1 == 0, "", " + sin(2*pi*t/p_seas)"),
      if_else(seas_cos1 == 0, "", " + cos(2*pi*t/p_seas)"),
      if_else(seas_sin2 == 0, "", " + sin(4*pi*t/p_seas)"),
      if_else(seas_cos2 == 0, "", " + cos(4*pi*t/p_seas)")),
    # Exclude the intercept (~ 0) with the covariate effect
    # or else the model is not identifiable
    formula_cov = str_glue("~ 0 + ns(perc_symptoms, df = {cov_df})"),
    # Allocate AIC columns
    # More than AR(4) was not needed
    aic_ar1 = NA, aic_ar2 = NA, aic_ar3 = NA, aic_ar4 = NA) |>
  # Not needed anymore
  select(
    -(trend_df:cov_df))
