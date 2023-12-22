#
# Plot observed time series
#

# Create time series data for plotting
timeseries_data <- igas_data |>
  mutate(
    across(
      .cols = where(fn = \(x) any(class(x) == "matrix")),
      .fns = \(x) x[, 1]),
    SARSCoV2 = exp(SARSCoV2)) |>
  select(
    Week,
    `GAS pneumonia or sepsis - adults` = iGAS_adult_bla,
    `GAS pneumonia or sepsis - children` = iGAS_child_bla,
    `GAS skin or soft tissue infections - children` = iGAS_child_wps,
    `non-invasive GAS - children` = Strep,
    `influenza A` = infA,
    `influenza B` = infB,
    RSV,
    hMPV,
    `SARS-CoV-2` = SARSCoV2,
    `varicella - children` = VZV) |>
  pivot_longer(
    cols = -Week,
    names_to = "Pathogen",
    values_to = "value") |>
  mutate(
    Pathogen = Pathogen |>  fct_inorder())

# plot_obs_timeseries_bla
plot_obs_timeseries_bla <- ggplot(
  data = timeseries_data |>
    filter(
      !(Pathogen %in% c(
        "GAS skin or soft tissue infections - children",
        "varicella - children"))),
  mapping = aes(x = Week, y = value)) +
  geom_line(
    linewidth = 0.25) +
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    expand = expansion(add = 0),
    date_breaks = "6 months",
    date_minor_breaks = "1 month",
    date_labels = "%b %-d, '%y") +
  labs(
    y = NULL) +
  facet_wrap(
    facets = vars(Pathogen),
    ncol = 1,
    scales = "free_y") +
  theme_bw() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# plot_obs_timeseries_wps
plot_obs_timeseries_wps <- ggplot(
  data = timeseries_data |>
    filter(
      Pathogen %in% c(
        "GAS skin or soft tissue infections - children",
        "varicella - children",
        "non-invasive GAS - children")),
  mapping = aes(x = Week, y = value))+
  geom_line(
    linewidth = 0.25) +
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    expand = expansion(add = 0),
    date_breaks = "6 months",
    date_minor_breaks = "1 month",
    date_labels = "%b %-d, '%y") +
  labs(
    y = NULL) +
  facet_wrap(
    facets = vars(Pathogen),
    ncol = 1,
    scales = "free_y") +
  theme_bw() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
