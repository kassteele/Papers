#
# Tabulate attributions
#

# Absolute attributions
tab_attr_abs <- bind_rows(
  bind_rows(
    attr_abs_year_adult_bla,
    attr_abs_overall_adult_bla) |>
    add_column(
      Group = "adult_bla",
      .before = 0),
  bind_rows(
    attr_abs_year_child_bla,
    attr_abs_overall_child_bla) |>
    add_column(
      Group = "child_bla",
      .before = 0),
  bind_rows(
    attr_abs_year_child_wps,
    attr_abs_overall_child_wps) |>
    add_column(
      Group = "child_wps",
      .before = 0))

# Relative attributions
tab_attr_rel <- bind_rows(
  bind_rows(
    attr_rel_year_adult_bla,
    attr_rel_overall_adult_bla) |>
    add_column(
      Group = "adult_bla",
      .before = 0),
  bind_rows(
    attr_rel_year_child_bla,
    attr_rel_overall_child_bla) |>
    add_column(
      Group = "child_bla",
      .before = 0),
  bind_rows(
    attr_rel_year_child_wps,
    attr_rel_overall_child_wps) |>
    add_column(
      Group = "child_wps",
      .before = 0))

#
# Restructure table for paper
#

# These are the parameters
pars <- tab_attr_abs |>
  select(
    -contains(c("Trend", "Strep", "iGAS"))) |>
  names() |>
  str_subset(pattern = "total") |>
  str_subset(pattern = "attr") |>
  str_remove("total_attr_abs_")

# Combine the absolute and relative attributions
tab_attr <- full_join(
  x = tab_attr_abs,
  y = tab_attr_rel,
  by = join_by(Group, Year))

# Create a table with all values
tab_attr_values <- pars |>
  map_dfc(
    .f = \(par) {
      tab_attr |>
        select(
          contains(par)) |>
        rename(
          abs_value = 1, abs_lwr = 2, abs_upr = 3,
          rel_value = 4, rel_lwr = 5, rel_upr = 6) |>
        map_dfr(
          .f = round) |>
        mutate(
          abs_value := str_glue("{abs_value} ({abs_lwr} - {abs_upr})"),
          rel_value := str_glue("{rel_value} ({rel_lwr} - {rel_upr})")) |>
        select(
          !!str_glue("{par}_abs") := abs_value,
          !!str_glue("{par}_rel") := rel_value) |>
        mutate(
          across(
            .cols = everything(),
            .fns = \(x) x |> str_replace("NA \\(NA - NA\\)|NaN \\(NA - NA\\)", "")))
    }
  )

# Combine them in the final table
tab_attr_paper <- bind_cols(
  tab_attr |> select(Group, Year),
  tab_attr_values)

#
# Table observed time series
#

tab_obs_timeseries_paper <- igas_data |>
  mutate(
    Year = Week |> year() |> pmin(2022)) |>
  group_by(
    Year) |>
  summarize(
    child_wps = sum(iGAS_child_wps),
    child_bla = sum(iGAS_child_bla),
    adult_bla = sum(iGAS_adult_bla)) |>
  ungroup()
