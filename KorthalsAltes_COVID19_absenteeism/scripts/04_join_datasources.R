#
# Join data sources
#

# Join data_infectionradar to data_absent
data_absent <- left_join(
  x = data_absent,
  y = data_infectionradar,
  by = "Week") |>
  # Drop all records without perc_symptoms
  # These are the weeks where Infectieradar was not active yet
  drop_na(
    perc_symptoms)

# Period: start date (Monday of 1st week) and end date (Sunday of last week)
# Used for plotting
date_start <- data_absent |> pull(Week) |> min() |> format("%b %-e, %Y")
date_end <- data_absent |> pull(Week) |> max() |> (`+`)(days(6)) |> format("%b %-e, %Y")
