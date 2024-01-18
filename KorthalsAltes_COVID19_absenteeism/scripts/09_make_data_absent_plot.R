#
# Data voor plot
#

# Add n_pers to sector levels -> Sector (n_pers)
# Dit is the gemiddelde aantal werknemers over de periode van de fit
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
    Sector_n = str_glue("{str_wrap(Sector, width = 40)}\n({n_pers_gem})") |>
      fct_inorder())

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
