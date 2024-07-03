#
# Plot adult_bla
#

# Absolute weekly attribution
plot_attr_abs_week_adult_bla <- ggplot() +
  geom_area(
    data = attr_abs_week_adult_bla |>
      mutate(
        Pathogen = Pathogen |>
          fct_recode(
            "trend" = "Trend",
            "non-invasive GAS" = "Strep",
            "influenza A" = "infA",
            "influenza B" = "infB",
            "SARS-CoV-2" = "SARSCoV2")),
    mapping = aes(x = Week, y = Contribution, fill = Pathogen)) +
  geom_line(
    data = igas_data,
    mapping = aes(x = Week, y = iGAS_adult_bla/Coverage_isis),
    linewidth = 0.1) +
  coord_cartesian(
    ylim = c(0, 30)) +
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    expand = expansion(add = 0),
    date_breaks = "6 months",
    date_minor_breaks = "1 month",
    date_labels = "%b %-d, '%y") +
  scale_y_continuous(
    breaks = seq(from = 0, to = 30, by = 10),
    minor_breaks = seq(from = 0, to = 30, by = 2)) +
  scale_fill_manual(
    values = cols) +
  labs(
    x = "Week",
    y = "Attribution to GAS pneumonia or sepsis - adults",
    fill = "Parameter") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Relative weekly attribution
plot_attr_rel_week_adult_bla <- ggplot() +
  geom_area(
    data = attr_rel_week_adult_bla |>
      mutate(
        Pathogen = Pathogen |>
          fct_recode(
            "trend" = "Trend",
            "non-invasive GAS" = "Strep",
            "influenza A" = "infA",
            "influenza B" = "infB",
            "SARS-CoV-2" = "SARSCoV2")),
    mapping = aes(x = Week, y = Contribution, fill = Pathogen)) +
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    expand = expansion(add = 0),
    date_breaks = "6 months",
    date_minor_breaks = "1 month",
    date_labels = "%b %-d, '%y") +
  scale_y_continuous(
    breaks = seq(from = 0, to = 100, by = 20),
    minor_breaks = seq(from = 0, to = 100, by = 10)) +
  scale_fill_manual(
    values = cols) +
  labs(
    x = "Week",
    y = "Relative attribution to GAS pneumonia or sepsis - adults",
    fill = "Parameter") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
