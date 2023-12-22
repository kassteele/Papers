#
# Clean data_infectieradar
#

data_infectieradar <- data_infectieradar_org |>
  mutate(
    # Week corresponding to the given date
    Week = Date_of_statistics |>
      floor_date(unit = "week"),
    # A number of records have no data (NA), e.g. due to a technical reason
    # We interpolate (linearly) Perc_covid_symptoms for these dates
    # We call this new variable perc_symptoms for further use in the analysis
    perc_symptoms = approx(
      x = Date_of_statistics,
      y = Perc_covid_symptoms,
      xout = Date_of_statistics)$y) |>
  # Calculate average percentage symptoms per week
  group_by(
    Week) |>
  summarise(
    n_days = n(),
    perc_symptoms = perc_symptoms |> mean()) |>
  # Only keep complete weeks with 7 days
  filter(
    n_days == 7) |>
  select(
    -n_days)
