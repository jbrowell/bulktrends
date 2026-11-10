# Download and update look-up tables in package data

## Data for unit tests

### Download
raw_table <- uktrades_request(endpoint = "Commodity")$value

### Filter the data
test_comcode_table <- raw_table[
  raw_table$Hs2Code == "01",
]

### Record the date this script was run as an attribute on the dataset
attr(test_comcode_table, "created_at") <- as.character(Sys.Date())

### Add to package
usethis::use_data(test_comcode_table, overwrite = TRUE)


## Ports look-up table

port <- uktrades_request(endpoint = "Port")$value
attr(port, "created_at") <- as.character(Sys.Date())
usethis::use_data(port, overwrite = TRUE)

## Country look-up table
#
# Note: Country names are not as in ISO 3166-1 making matching hard!
#
country <- uktrades_request(endpoint = "Country")$value
attr(country, "created_at") <- as.character(Sys.Date())
usethis::use_data(country, overwrite = TRUE)
