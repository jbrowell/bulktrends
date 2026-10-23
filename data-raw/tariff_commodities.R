
## Script to update the tariff_commodities dataset.
##
## Downloads and cleans the latest historical tariff data,
## then saves it to data/tariff_commodities.rda.
##
## Run from the package root:
##   source("data-raw/tariff_commodities.R")

# Load the current package source code.
devtools::load_all(".")

# Download and clean the latest dataset.
tariff_commodities <- get_tariff_commodities()

# Save the dataset for inclusion in the package.
usethis::use_data(
  tariff_commodities,
  overwrite = TRUE
)

message(
  "tariff_commodities saved to data/tariff_commodities.rda"
)
message(
  "Created at: ",
  attr(tariff_commodities, "created_at")
)
