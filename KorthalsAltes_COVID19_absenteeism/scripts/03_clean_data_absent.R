#
# Opschonen data verzuim
#

data_absent <- data_absent_org |>
  # Select and rename columns
  select(
    Sector = Bedrijfstak,
    Week = Week,
    rate_absent = rate_verzuim,
    n_pers = n_personeel) |>
  # Filter out "empty" Sector
  filter(
    Sector != "") |>
  droplevels() |>
  # Calculate absolute absenteeism numbers
  # We use this to sum over the sectors
  mutate(
    n_rate_absent = n_pers*rate_absent) |>
  # Aggregate over sectors and weeks
  group_by(
    Sector, Week) |>
  summarise(
    n_rate_absent = sum(n_rate_absent),
    n_pers = sum(n_pers),
    .groups = "drop")

# The calculate the total per week over all sectors, make a new tibble first
data_absent_total <- data_absent |>
  group_by(
    Week) |>
  summarise(
    Sector = factor("Total"),
    n_rate_absent = sum(n_rate_absent),
    n_pers = sum(n_pers))

# Bind with data_absent
data_absent <- bind_rows(
  data_absent,
  data_absent_total)

# Further mutations
data_absent <- data_absent |>
  # Calculate weekly rate again, but now with total included
  mutate(
    rate_absent = n_rate_absent/n_pers) |>
  # Filter sectors
  # These two sectors contain too few observations for reliable rates:
  # T - Huishoudens als werkgever; niet-gedifferentieerde productie van goederen en diensten door huishoudens voor eigen gebruik
  # U - Extraterritoriale organisaties en lichamen
  filter(
    Sector |>
      str_detect(
        pattern = "T -|U -",
        negate = TRUE)) |>
  droplevels() |>
  # Rename remaining Dutch sector naming to English
  # The abbreviated version
  mutate(
    Sector = Sector |>
      factor(
        labels = c(
          "A - Primary sector",
          "B - Mining",
          "C - Industry",
          "D - Energy",
          "E - (Waste) water mgmt",
          "F - Construction",
          "G - Retail and repair",
          "H - Logistics",
          "I - Hospitality",
          "J - ICT",
          "K - Finance",
          "L - Real estate",
          "M - Consultancy",
          "N - Rental services",
          "O - Public services",
          "P - Education",
          "Q - Healthcare",
          "R - Leisure",
          "S - Other services",
          "Total")))
# # The original long version
# mutate(
#   Sector = Sector |>
#     factor(
#       labels = c(
#         "A - Agriculture, forestry and fishery",
#         "B - Mineral extraction",
#         "C - Industry",
#         "D - Production and distribution of and trade in electricity, natural gas, steam and cooled air",
#         "E - Water collection and distribution, waste and sewage treatment",
#         "F - Construction",
#         "G - Wholesale and retail; car repair",
#         "H - Transport and storage",
#         "I - Lodging, meal and drink supply",
#         "J - Information and communication",
#         "K - Financial institutions",
#         "L - Rental and trade in real estate",
#         "M - Advice, research and other specialistic",
#         "N - Lease of movable property and other services",
#         "O - Public administration, services, and services mandatory social insurances",
#         "P - Education",
#         "Q - Health care and welfare",
#         "R - Culture, sports and recreation",
#         "S - Other services",
#         "Total")))
