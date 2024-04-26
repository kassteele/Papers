#
# Export figures
#

ggsave(
  plot = plot_data_infectionradar,
  filename = "output/data_infectionradar.svg",
  width = 18, height = 11.5, units = "cm", scale = 1.2)

ggsave(
  plot = plot_absent_overview,
  filename = "output/IR_absent_overview.svg",
  width = 18, height = 23, units = "cm", scale = 1.2)
