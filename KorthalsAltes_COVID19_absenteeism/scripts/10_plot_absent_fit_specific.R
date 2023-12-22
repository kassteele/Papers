#
# Plot fitted time series - specific sectors
#

# Specific:
# P - Education
# Q - Health care and welfare
# Total

tmp <- data_absent_plot |>
  filter(
    Sector |> str_detect(pattern = "P -|Q -|Total")) |>
  droplevels()

plot_absent_specific <- tmp |>
  split(
    f = tmp$Sector) |>
  map(
    .f = \(x) ggplot(
      data = x,
      mapping = aes(x = Week, ymin = ymin, ymax = ymax, fill = Component)) +
      geom_ribbon() +
      geom_point(
        data = x |> distinct(Week, rate_absent),
        mapping = aes(x = Week, y = rate_absent),
        shape = "+",
        size = 4,
        inherit.aes = FALSE) +
      scale_fill_manual(
        values = c("#8fcae7", "#007bc7", "#b3d7ee", "#d9ebf7")) +
      scale_x_date(
        breaks = seq(ymd("2020-01-01"), ymd("2024-01-01"), by = "3 months"),
        date_minor_breaks = "1 month",
        expand = expansion(add = c(7, 7)),
        date_labels = "%b '%y") +
      scale_y_continuous(
        limits = c(0, 2.25),
        breaks = seq(from = 0, to = 5, by = 0.5),
        minor_breaks = seq(from = 0, to = 5, by = 0.25)) +
      labs(
        title = str_glue("Weekly absenteeism notification frequency (yearly basis) in the sector {x$Sector[1]} from {date_start} to {date_end}"),
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
  )
