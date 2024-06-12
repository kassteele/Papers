#
# Plot child_wps
#

# Absolute weekly attribution
plot_attr_abs_week_child_wps <- ggplot() +
  geom_area(
    data = attr_abs_week_child_wps |>
      mutate(
        Pathogen = Pathogen |>
          fct_recode(
            "non-invasive GAS" = "Strep",
            "trend" = "Trend",
            "varicella" = "VZV")),
    mapping = aes(x = Week, y = Contribution, fill = Pathogen)) +
  geom_line(
    data = igas_data,
    mapping = aes(x = Week, y = iGAS_child_wps/Coverage_isis),
    linewidth = 0.1) +
  coord_cartesian(
    ylim = c(0, 10)) +
  scale_x_date(
    limits = c(ymd("2010-01-01"), NA),
    expand = expansion(add = 0),
    date_breaks = "6 months",
    date_minor_breaks = "1 month",
    date_labels = "%b %-d, '%y") +
  scale_y_continuous(
    breaks = seq(from = 0, to = 20, by = 2),
    minor_breaks = seq(from = 0, to = 20, by = 0.5)) +
  scale_fill_manual(
    values = cols) +
  labs(
    x = "Week",
    y = "Attribution to GAS skin and soft tissue infections - children aged 0-5 years",
    fill = "Parameter") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Relative weekly attribution
plot_attr_rel_week_child_wps <- ggplot() +
  geom_area(
    data = attr_rel_week_child_wps |>
      mutate(
        Pathogen = Pathogen |>
          fct_recode(
            "non-invasive GAS" = "Strep",
            "trend" = "Trend",
            "varicella" = "VZV")),
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
    y = "Relative attribution to GAS skin and soft tissue infections - children aged 0-5 years",
    fill = "Parameter") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Combined
plot_attr_week_adult_bla <- plot_attr_abs_week_child_wps + plot_attr_rel_week_child_wps +
  plot_layout(
    ncol = 2,
    guides = "collect") +
  plot_annotation(
    tag_levels = "a")
