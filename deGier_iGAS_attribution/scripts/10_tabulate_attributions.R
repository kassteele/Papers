#
# Tabulate attributions
#

# Absolute attributions
tab_attr_abs <- bind_rows(
  bind_rows(
    attr_abs_period_adult_bla,
    attr_abs_overall_adult_bla) |>
    add_column(
      Group = "adult_bla",
      .before = 0),
  bind_rows(
    attr_abs_period_child_bla,
    attr_abs_overall_child_bla) |>
    add_column(
      Group = "child_bla",
      .before = 0),
  bind_rows(
    attr_abs_period_child_wps,
    attr_abs_overall_child_wps) |>
    add_column(
      Group = "child_wps",
      .before = 0))

# Relative attributions
tab_attr_rel <- bind_rows(
  bind_rows(
    attr_rel_period_adult_bla,
    attr_rel_overall_adult_bla) |>
    add_column(
      Group = "adult_bla",
      .before = 0),
  bind_rows(
    attr_rel_period_child_bla,
    attr_rel_overall_child_bla) |>
    add_column(
      Group = "child_bla",
      .before = 0),
  bind_rows(
    attr_rel_period_child_wps,
    attr_rel_overall_child_wps) |>
    add_column(
      Group = "child_wps",
      .before = 0))
