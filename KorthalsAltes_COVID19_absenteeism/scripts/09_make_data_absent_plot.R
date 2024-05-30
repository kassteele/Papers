#
# Data voor plot
#

# Add n_pers to sector levels -> Sector (n_pers)
# This is the mean number of respondents per sector over the study period
data_absent_plot <-
  left_join(
    x = data_absent_comp,
    y = data_absent_comp |>
      group_by(
        Sector) |>
      summarise(
        n_pers_gem = n_pers |> mean() |> (`/`)(100) |> round() |> (`*`)(100)),
    by = "Sector") |>
  mutate(
    Sector_n = str_glue("{Sector}\n({n_pers_gem})") |>
      fct_inorder())

# # Add a symbol to the names of specific sectors with low (<5% coverage) and high (>20%) coverage
# data_absent_plot <- data_absent_plot |>
#   mutate(
#     Sector_n = Sector_n |>
#       fct_recode(
#         # Low coverage
#         "A - Primary sector\n(8700)*" = "A - Primary sector\n(8700)",
#         "B - Mining\n(500)*" = "B - Mining\n(500)",
#         "D - Energy\n(1000)*" = "D - Energy\n(1000)",
#         "O - Public services\n(30000)*" = "O - Public services\n(30000)",
#         # High coverage
#         "E - (Waste) water mgmt\n(9300)^" = "E - (Waste) water mgmt\n(9300)",
#         "K - Finance\n(72900)^" = "K - Finance\n(72900)"))

data_absent_plot <- data_absent_plot |>
  # Determine ymin and ymax for usage in geom_ribbon()
  # If all comp_* columns would have been positive, we could have used geom_area()
  # Unfortunately, life is not that simple
  mutate(
    ymin_trend = 0,
    ymax_trend = pmax(0, comp_trend),
    ymin_seas = ymax_trend,
    ymax_seas = pmax(0, comp_trend + comp_seas),
    ymin_cov = ymax_seas,
    ymax_cov = pmax(0, comp_trend + comp_seas + comp_cov),
    ymin_ar = ymax_cov,
    ymax_ar = pmax(0, comp_trend + comp_seas + comp_cov + comp_ar)) |>
  # The comp_ columns are not needed anymore
  select(
    -starts_with("comp")) |>
  # Because the components are in wide format,
  # we must put them in long format for plotting
  pivot_longer(
    cols = starts_with("ym"),
    names_to = c(".value", "Component"),
    names_sep = "_") |>
  # Rename the components to some human-readable
  mutate(
    Component = Component |>
      factor(
        levels = c(
          "ar",
          "cov",
          "seas",
          "trend"),
        labels = c(
          "Autoregressive effect",
          "Percentage IR participants with\nCOVID-19 like symptoms",
          "Seasonal effect",
          "Long-term trend")))
