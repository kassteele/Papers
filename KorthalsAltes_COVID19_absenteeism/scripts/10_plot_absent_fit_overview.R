#
# Plot fitted time series - overview
#

plot_absent_overview <- ggplot(
  data = data_absent_plot,
  mapping = aes(x = Week, ymin = ymin, ymax = ymax, fill = Component)) +
  geom_ribbon() +
  geom_point(
    data = data_absent_plot |> distinct(Sector_n, Week, rate_absent),
    mapping = aes(x = Week, y = rate_absent),
    shape = "+",
    size = 2,
    inherit.aes = FALSE) +
  scale_fill_manual(
    values = c("#8fcae7", "#007bc7", "#b3d7ee", "#d9ebf7")) +
  scale_x_date(
    breaks = seq(ymd("2020-01-01"), ymd("2024-12-01"), by = "3 months"),
    date_minor_breaks = "1 month",
    expand = expansion(add = c(7, 7)),
    date_labels = "%b '%y") +
  scale_y_continuous(
    limits = c(0, 2.25),
    breaks = seq(from = 0, to = 2.5, by = 0.5),
    minor_breaks = seq(from = 0, to = 2.5, by = 0.25)) +
  facet_wrap(
    facets = vars(Sector_n),
    nrow = 4, ncol = 5) +
  labs(
    title = str_glue("Weekly absenteeism notification frequency (yearly basis) per sector of activity: {date_start} to {date_end}"),
    subtitle = "
    Points: reported absenteeism, between brackets the average number of employees over the study period
    Coloured areas: portion of absenteeism explained by the four components given in the legend",
    x = "Date of first day of the week",
    y = "Weekly absenteeism notification frequency\n(yearly-based, average per employee)",
    fill = NULL) +
  guides(
    fill = guide_legend(
      keyheight = 2)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 10))

# Plot
print(plot_absent_overview)
