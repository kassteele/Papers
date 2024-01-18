#
# Import data Infectionradar
#

# Source: https://data.rivm.nl/covid-19/
# Metadata: COVID-19_infectionradar_symptomen_per_dag.html
data_infectionradar_org <- read_delim(
  file = "https://data.rivm.nl/covid-19/COVID-19_Infectieradar_symptomen_per_dag.csv",
  delim = ";",
  col_types = "--Dd-",
  locale = locale(decimal_mark = ","))
