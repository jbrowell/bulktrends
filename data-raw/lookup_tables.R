# Download and update look-up tables in package data

## Download
<<<<<<< HEAD
raw_table <- uktrades_request(endpoint = "Commodity")$value

## Filter the data
test_comcode_table <- raw_table[
  raw_table$Hs2Code == "01",
]

## Record the date this script was run as an attribute on the dataset
attr(test_comcode_table, "created_at") <- as.character(Sys.Date())

## Add to package
usethis::use_data(test_comcode_table, overwrite = TRUE)
=======

## Record the date this script was run as an attribute on the dataset
attr(..., "created_at") <- as.character(Sys.Date())

## Add to package
usethis::use_data(...)
>>>>>>> origin/feature/59-first-unit-tests
