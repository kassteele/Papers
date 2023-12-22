#
# Make data_absent_fit
#

# This is a modified versie of data_veruim,
# especially for used for model fitting
data_absent_fit <- data_absent |>
  # Additions needed for fitting
  # First for all sectors
  mutate(
    # holiday = 1 if it is a Chrismas holiday week
    holiday = if_else(
      condition = Week %in% c(
        ymd("2020-12-21") + days(c(0, 7)),
        ymd("2021-12-20") + days(c(0, 7, 14)),
        ymd("2022-12-26") + days(c(0, 7))),
      true = 1L,
      false = 0L)) |>
  # Now per Sector
  group_by(
    Sector) |>
  mutate(
    # t is the number of weeks since the first week
    # Needed for trend and seasonal effect
    t = row_number() - 1) |>
  ungroup() |>
  # Split per Sector
  # This creates a list with tibbles to loop over
  split(
    f = data_absent$Sector)
