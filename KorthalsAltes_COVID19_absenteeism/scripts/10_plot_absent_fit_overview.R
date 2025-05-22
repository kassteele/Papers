#
# Plot fitted time series - overview
#

plot_absent_overview <- map(
  .x = 1:2,
  .f = \(page) {
    ggplot(
      data = data_absent_plot,
      mapping = aes(x = Week, ymin = ymin, ymax = ymax, fill = Component)) +
      geom_ribbon() +
      geom_point(
        data = data_absent_plot |> distinct(Sector, Week, rate_absent),
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
      facet_wrap_paginate(
        facets = vars(Sector),
        nrow = 5, ncol = 2,
        page = page) +
      labs(
        x = "Date",
        y = "Absenteeism (reports per person per year)",
        fill = NULL) +
      guides(
        fill = guide_legend(
          nrow = 1,
          keyheight = 2)) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(linewidth = 0.2),
        panel.grid.minor = element_line(linewidth = 0.1),
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom")
  })

# Plot
print(plot_absent_overview)
