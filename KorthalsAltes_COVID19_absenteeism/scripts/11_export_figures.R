#
# Export figures
#

ggsave(
  plot = plot_data_infectionradar,
  filename = "output/data_infectionradar.pdf",
  width = 15, height = 10, units = "in")

ggsave(
  plot = plot_absent_overview,
  filename = "output/IR_absent_overview.pdf",
  width = 15, height = 10, units = "in")

iwalk(
  plot_absent_specific,
  .f = \(x, Sector) ggsave(
    plot = x,
    filename = str_glue("output/IR_absent_{str_replace_all(Sector, ' ', '_')}.pdf"),
    width = 15, height = 10, units = "in"))
