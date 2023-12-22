#
# Select and rename columns
#

igas_data <- igas_data_org |>
  # Remove "_lag0" in all column names
  rename_with(
    .cols = ends_with("_lag0"),
    .fn = str_remove, "_lag0") |>
  # Select and rename columns
  select(
    Week = Date,
    Coverage_isis,
    iGAS_adult_bla = Adult_bla,
    iGAS_child_bla = Child_bla,
    iGAS_child_wps = Child_wps,
    Strep,
    infA = INFA,
    infB = INFB,
    RSV,
    hMPV = HMPV,
    SARSCoV2 = SARSCOV2,
    VZV)
