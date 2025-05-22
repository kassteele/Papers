#
# Export figures
#

ggsave(
  plot = plot_data_infectionradar,
  filename = "output/data_infectionradar.svg",
  width = 18, height = 11.5, units = "cm", scale = 1.2)

ggsave(
  plot = plot_absent_overview[[1]],
  filename = "output/IR_absent_overview1.svg",
  width = 18, height = 23, units = "cm", scale = 1.2)
ggsave(
  plot = plot_absent_overview[[2]],
  filename = "output/IR_absent_overview2.svg",
  width = 18, height = 23, units = "cm", scale = 1.2)
