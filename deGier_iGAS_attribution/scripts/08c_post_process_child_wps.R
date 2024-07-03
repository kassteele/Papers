#
# Post-process child_wps
#

# Import fit from rds
mu_child_wps <- readRDS(file = "output/mu_child_wps.rds")

# Post-process fit
mu_child_wps <- mu_child_wps |>
  # mu_child_wps containts the absolute attributions, called mu_<pathogen>
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
      .names = "{.col |> str_replace(pattern = 'attr_abs', replacement = 'attr_rel')}"))

#
# Absolute attributions
#

# Calculate weekly summaries of attr_abs_<pathogen> for plotting
attr_abs_week_child_wps <- mu_child_wps |>
  group_by(
    Week) |>
  summarise(
    across(
      .cols = starts_with("attr_abs") & !contains("iGAS"),
      .fns = \(x) x |> mean(na.rm = TRUE),
      .names = "{.col |> str_remove('attr_abs_')}")) |>
  pivot_longer(
    cols = -Week,
    names_to = "Pathogen",
    values_to = "Contribution") |>
  mutate(
    Pathogen = Pathogen |> fct_inorder() |> fct_rev())

# Calculate summaries of Period sums of attr_abs_<pathogen> for tabulation
attr_abs_period_child_wps <- mu_child_wps |>
  # Sum over weeks by Year and Sample
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
      .fns = \(x) x |> mean(na.rm = TRUE),
      .names = "total_{.col}"),
    across(
      .cols = starts_with("attr"),
      .fns = \(x) x |> quantile(probs = 0.025, na.rm = TRUE),
      .names = "lwr_{.col}"),
    across(
      .cols = starts_with("attr"),
      .fns = \(x) x |> quantile(probs = 0.975, na.rm = TRUE),
      .names = "upr_{.col}")) |>
  ungroup()

# Calculate summaries of overall sums of attr_abs_<pathogen> for tabulation
attr_abs_overall_child_wps <- mu_child_wps |>
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
      .fns = \(x) x |> mean(na.rm = TRUE),
      .names = "total_{.col}"),
    across(
      .cols = starts_with("attr_abs"),
      .fns = \(x) x |> quantile(probs = 0.025, na.rm = TRUE),
      .names = "lwr_{.col}"),
    across(
      .cols = starts_with("attr_abs"),
      .fns = \(x) x |> quantile(probs = 0.975, na.rm = TRUE),
      .names = "upr_{.col}"))

#
# Relative attributions
#

# Calculate weekly summaries of attr_rel_<pathogen> for plotting
attr_rel_week_child_wps <- mu_child_wps |>
  group_by(
    Week) |>
  summarise(
    across(
      .cols = starts_with("attr_rel") & !contains("iGAS"),
      .fns = \(x) x |> mean(na.rm = TRUE),
      .names = "{.col |> str_remove('attr_rel_')}")) |>
  pivot_longer(
    cols = -Week,
    names_to = "Pathogen",
    values_to = "Contribution") |>
  mutate(
    Pathogen = Pathogen |> fct_inorder() |> fct_rev())

# Calculate summaries of Period means of attr_rel_<pathogen> for tabulation
attr_rel_period_child_wps <- mu_child_wps |>
  # Mean over weeks by Period and Sample
  group_by(
    Period, Sample) |>
  reframe(
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> mean(na.rm = TRUE))) |>
  # Calculate summary statistics by Year
  group_by(
    Period) |>
  summarise(
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> mean(na.rm = TRUE),
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
attr_rel_overall_child_wps <- mu_child_wps |>
  # Sum over all weeks by Sample
  group_by(
    Sample) |>
  reframe(
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> mean(na.rm = TRUE))) |>
  # Calculate summary statistics
  summarise(
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> mean(na.rm = TRUE),
      .names = "mean_{.col}"),
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> quantile(probs = 0.025, na.rm = TRUE),
      .names = "lwr_{.col}"),
    across(
      .cols = starts_with("attr_rel"),
      .fns = \(x) x |> quantile(probs = 0.975, na.rm = TRUE),
      .names = "upr_{.col}"))
