# Download and update look-up tables in package data

## Download
raw_table <- uktrades_request(endpoint = "Commodity")$value

## Filter the data
lookup_table <- raw_table[
  raw_table$Hs2Code == "01" & raw_table$Hs4Code == "0101" & raw_table$Hs6Code == "010129",
]

## Record the date this script was run as an attribute on the dataset
attr(lookup_table, "created_at") <- as.character(Sys.Date())

## Add to package
usethis::use_data(lookup_table, overwrite = TRUE)
