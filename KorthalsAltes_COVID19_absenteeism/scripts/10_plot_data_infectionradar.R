#
# Plot data_infectionradar
#

plot_data_infectionradar <- ggplot(
  data = data_absent_fit$Total,
  mapping = aes(x = Week, y = perc_symptoms)) +
  geom_line(
    linewidth = 0.2) +
  geom_point(
    size = 2,
    mapping = aes(shape = interpolated)) +
  scale_x_date(
    breaks = seq(ymd("2020-01-01"), ymd("2024-01-01"), by = "3 months"),
    date_minor_breaks = "1 month",
    expand = expansion(add = c(7, 7)),
    date_labels = "%b '%y") +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(from = 0, to = 10, by = 1),
    minor_breaks = seq(from = 0, to = 10, by = 0.5)) +
  scale_shape_manual(
    values = c(16, 1)) +
  labs(
    title = str_glue("Weekly average percentage of participants reporting COVID-19-like symptoms in Infection radar: {date_start} to {date_end}"),
    x = "Date of first day of the week",
    y = "Percentage COVID-19-like symptoms") +
  guides(
    shape = "none") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 10))

# Plot
print(plot_data_infectionradar)
