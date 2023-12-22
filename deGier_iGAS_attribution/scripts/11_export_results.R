#
# Export results
#

# List plots to export
rm(plot_list)
plot_list <- objects() |>
  str_subset(pattern = "plot_attr_week|plot_timeseries")

# List tables to export
rm(tab_list)
tab_list <- objects() |>
  str_subset(pattern = "tab") |>
  str_subset(pattern = "paper")

# Export plots
plot_list |>
  walk(
    .f = \(object) ggsave(
      plot = get(object),
      file = str_glue("output/{object}.pdf"),
      width = 14, height = 7, units = "in", scale = 1.25))

# Export tables
tab_list |>
  walk(
    .f = \(object) write_xlsx(
      x = get(object),
      path = str_glue("output/{object}.xlsx"),
      format_headers = FALSE))
