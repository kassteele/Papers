#
# Post-process child_bla
#

# Import fit from rds
fit_child_bla <- readRDS(file = "output/fit_child_bla.rds")

# Post-process fit
fit_child_bla <- fit_child_bla |>
  # fit_child_bla containts the absolute attributions, called mu_<pathogen>
  # Rename mu_<pathogen> to attr_abs_<pathogen>
  # This to distinguish between relative attributions, attr_rel_<pathogen>
  rename_with(
    .cols = starts_with("mu"),
    .fn = \(x) x |> str_replace(pattern = "mu", replacement = "attr_abs")) |>
  mutate(
    # Add information from igas_data
    igas_data |> select(Week, Coverage_isis),
    # Add Period for aggregation
    Year = Week |> year(),
    Period = case_when(
      Year %in% 2010:2019 ~ "2010-2019",
      Year %in% 2020:2021 ~ "2020-2021",
      TRUE ~ "2022-2023*")) |>
  # Simplify the tibble by "unchopping" the list columns with MCMC samples into rows
  unchop(
    cols = starts_with("attr_abs")) |>
  mutate(
    # Add MCMC sample number
    # This is needed for the cumulative sum of attr_abs_<pathogen> later on
    # Because the unchopping expands the list columns (the cell contains the samples),
    # we can just add 1:n_samples here, repeated for the total number of weeks in the data (data_list$n)
    Sample = seq_len(n_post*n_chains) |> rep(times = data_list$n),
    # Calculate weekly relative attributions attr_rel_<pathogen> (unit: percentages)
    # These are attr_abs_<pathogen> divided by attr_abs_iGAS x 100%, except for attr_abs_iGAS of course
    # Note that attr_abs_iGAS should first be divided by Coverage_isis
    # because attr_abs_iGAS is the expected number of the OBSERVED counts
    # and attr_abs_<pathogen> were estimated on "corrected-for-coverage" scale
    across(
      .cols = starts_with("attr_abs") & !contains("iGAS"),
      .fns = \(x) 100*x/(attr_abs_iGAS/Coverage_isis),
      .names = "{.col |> str_replace(pattern = 'attr_abs', replacement = 'attr_rel')}"),
    # Add attributions for all res viruses combined
    attr_abs_res = attr_abs_infA + attr_abs_infB + attr_abs_RSV + attr_abs_hMPV + attr_abs_SARSCoV2 + attr_abs_VZV,
    attr_rel_res = attr_rel_infA + attr_rel_infB + attr_rel_RSV + attr_rel_hMPV + attr_rel_SARSCoV2 + attr_rel_VZV,
    # Set pre-covid attributions to NA so they do not get treated as zeroes
    attr_rel_SARSCoV2 = if_else(attr_abs_SARSCoV2 == 0, NA, attr_rel_SARSCoV2))

#
# Absolute attributions
#

# Calculate weekly summaries of attr_abs_<pathogen> for plotting
attr_abs_week_child_bla <- fit_child_bla |>
  select(
    -attr_abs_res) |>
  group_by(
    Week) |>
  summarise(
    across(
      .cols = starts_with("attr_abs") & !contains("iGAS"),
      .fns = mean,
      .names = "{.col |> str_remove('attr_abs_')}")) |>
  pivot_longer(
    cols = -Week,
    names_to = "Pathogen",
    values_to = "Contribution") |>
  mutate(
    Pathogen = Pathogen |> fct_inorder() |> fct_rev())

# Calculate summaries of Period sums of attr_abs_<pathogen> for tabulation
attr_abs_period_child_bla <- fit_child_bla |>
  # Sum over weeks by Period and Sample
  group_by(
    Period, Sample) |>
  reframe(
    across(
      .cols = starts_with("attr_abs"),
      .fns = sum)) |>
  # Calculate summary statistics by Period
  group_by(
    Period) |>
  summarise(
    across(
      .cols = starts_with("attr_abs"),
      .fns = mean,
      .names = "total_{.col}"),
    across(
      .cols = starts_with("attr"),
      .fns = \(x) x |> quantile(probs = 0.025),
      .names = "lwr_{.col}"),
    across(
      .cols = starts_with("attr"),
      .fns = \(x) x |> quantile(probs = 0.975),
      .names = "upr_{.col}")) |>
  ungroup()

# Calculate summaries of overall sums of attr_abs_<pathogen> for tabulation
attr_abs_overall_child_bla <- fit_child_bla |>
  # Sum over all weeks by Sample
  group_by(
    Sample) |>
  reframe(
    across(
      .cols = starts_with("attr_abs"),
      .fns = sum)) |>
  # Calculate summary statistics
  summarise(
    across(
      .cols = starts_with("attr_abs"),
      .fns = mean,
      .names = "total_{.col}"),
    across(
      .cols = starts_with("attr_abs"),
      .fns = \(x) x |> quantile(probs = 0.025),
      .names = "lwr_{.col}"),
    across(
      .cols = starts_with("attr_abs"),
      .fns = \(x) x |> quantile(probs = 0.975),
      .names = "upr_{.col}"))

#
# Relative attributions
#

# Calculate weekly summaries of attr_rel_<pathogen> for plotting
attr_rel_week_child_bla <- fit_child_bla |>
  select(
    -attr_rel_res) |>
  group_by(
    Week) |>
  summarise(
    across(
      .cols = starts_with("attr_rel") & !contains("iGAS"),
      .fns = mean,
      .names = "{.col |> str_remove('attr_rel_')}")) |>
  pivot_longer(
    cols = -Week,
    names_to = "Pathogen",
    values_to = "Contribution") |>
  mutate(
    Pathogen = Pathogen |> fct_inorder() |> fct_rev())

# Calculate summaries of yearly means of attr_rel_<pathogen> for tabulation
attr_rel_period_child_bla <- fit_child_bla |>
  # Mean over weeks by Period and Sample
  group_by(
    Period, Sample) |>
  reframe(
    across(
      .cols = starts_with("attr_rel"),
      .fns = ~ mean(.x, na.rm = TRUE))) |>
  # Calculate summary statistics by Period
  group_by(
    Period) |>
  summarise(
    across(
      .cols = starts_with("attr_rel"),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"),
    across(
      .cols = starts_with("attr"),
      .fns = \(x) x |> quantile(probs = 0.025, na.rm = TRUE),
      .names = "lwr_{.col}"),
    across(
      .cols = starts_with("attr"),
      .fns = \(x) x |> quantile(probs = 0.975, na.rm = TRUE),
      .names = "upr_{.col}")) |>
  ungroup()

# Calculate summaries of overall means of attr_rel_<pathogen> for tabulation
attr_rel_overall_child_bla <- fit_child_bla |>
  # Sum over all weeks by Sample
  group_by(
    Sample) |>
  reframe(
    across(
      .cols = starts_with("attr_rel"),
      .fns = ~ mean(.x, na.rm = TRUE))) |>
  # Calculate summary statistics
  summarise(
    across(
      .cols = starts_with("attr_rel"),
      .fns = mean,
      .names = "mean_{.col}"),
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> quantile(probs = 0.025, na.rm = TRUE),
      .names = "lwr_{.col}"),
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> quantile(probs = 0.975, na.rm = TRUE),
      .names = "upr_{.col}"))
