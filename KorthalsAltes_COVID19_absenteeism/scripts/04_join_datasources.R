#
# Join data sources
#

# Join data_infectieradar to data_absent
data_absent <- left_join(
  x = data_absent,
  y = data_infectieradar,
  by = "Week") |>
  # Drop all records without perc_symptoms
  # These are the weeks where Infectieradar was not active yet
  drop_na(
    perc_symptoms)
