# Download and update look-up tables in package data

## Download

## Record the date this script was run as an attribute on the dataset
attr(..., "created_at") <- as.character(Sys.Date())

## Add to package
usethis::use_data(...)
