#
# Plot adult_bla
#

# plot_timeseries_bla
plot_timeseries_bla <- ggplot(
  data = igas_data |>
    map_dfr(
      .f = \(x) if (is.matrix(x)) x[, 1] else x) |>
    select(
      -c(Coverage_isis, infA_seas, iGAS_child_wps, VZV)) |>
    mutate(
      SARSCoV2 = if_else(
        condition = SARSCoV2 > 0,
        true = exp(SARSCoV2),
        false = 0)) |>
    pivot_longer(
      cols = -Week,
      names_to = "Pathogen",
      values_to = "value") |>
    mutate(
      Pathogen = Pathogen |>
        fct_inorder() |>
        fct_recode(
          "GAS pneumonia or sepsis - adults" = "iGAS_adult_bla",
          "GAS pneumonia or sepsis - children" = "iGAS_child_bla",
          "non-invasive GAS - children" = "Strep",
          "influenza A" = "infA",
          "influenza B" = "infB",
          "SARS-CoV-2" = "SARSCoV2")),
  mapping = aes(x = Week, y = value)) +
  geom_line( )+
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
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "none")

# plot_timeseries_wps
plot_timeseries_wps <- ggplot(
    data = igas_data |>
      map_dfr(
        .f = \(x) if (is.matrix(x)) x[, 1] else x) |>
      select(
        c(Week, iGAS_child_wps, Strep, VZV)) |>
      pivot_longer(
        cols = -Week,
        names_to = "Pathogen",
        values_to = "value") |>
      mutate(
        Pathogen = Pathogen |>
          fct_inorder() |>
          fct_recode(
            "GAS skin or soft tissue infections - children" = "iGAS_child_wps",
            "non-invasive GAS - children" = "Strep",
            "varicella - children" = "VZV")),
    mapping = aes(Week, y = value))+
  geom_line() +
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
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "none")
