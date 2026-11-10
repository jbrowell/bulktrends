# devtools::load_all()
# testthat::test_file("tests/testthat/test-load_uktradeinfo_data.R")

make_uktradeinfo_record <- function(
  perref = "202401",
  comcode = "01011010",
  stat_value = "000000001234",
  net_mass = "000000000567"
) {
  paste0(
    sprintf("%-6s", perref),
    "1",
    sprintf("%-6s", perref),
    sprintf("%-8s", comcode),
    "00000",
    "001",
    "01",
    "001",
    "001",
    "001",
    "00",
    "01",
    sprintf("%-12s", stat_value),
    sprintf("%-12s", net_mass),
    sprintf("%-12s", "000000000000"),
    "0",
    "IMP",
    "1"
  )
}

test_that("read_uktradeinfo parses a fixed-width text file", {
  path <- tempfile(fileext = ".txt")
  writeLines(make_uktradeinfo_record(), path)

  result <- read_uktradeinfo(path)

  expect_s3_class(result, "data.table")
  expect_equal(
    names(result),
    c(
      "PERREF",
      "TYPE",
      "MONTHAC",
      "COMCODE",
      "SITC",
      "COD_SEQ",
      "COD_ALPHA",
      "PORT_SEQ",
      "PORT_CODE",
      "COO_SEQ",
      "COO_ALPHA",
      "MODE_OF_TRANSPORT",
      "STAT_VALUE",
      "NET_MASS",
      "SUMM_UNIT",
      "SUPRESSION",
      "FLOW",
      "REC_TYPE",
      "DATE_START",
      "DATE_END"
    )
  )
  expect_equal(result$COMCODE, "01011010")
  expect_equal(result$STAT_VALUE, 1234)
  expect_equal(result$NET_MASS, 567)
  expect_equal(result$DATE_START, as.Date("2024-01-01"))
  expect_equal(result$DATE_END, as.Date("2024-01-31"))
})

test_that("read_uktradeinfo reads zip files and directories", {
  source_file <- tempfile(fileext = ".txt")
  writeLines(make_uktradeinfo_record(perref = "202402"), source_file)

  zip_file <- tempfile(fileext = ".zip")
  old_wd <- setwd(dirname(source_file))
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(
    zipfile = zip_file,
    files = basename(source_file),
    flags = "-j"
  )

  zip_result <- read_uktradeinfo(zip_file)
  expect_equal(zip_result$DATE_START, as.Date("2024-02-01"))

  data_dir <- tempfile()
  dir.create(data_dir)
  file.copy(source_file, file.path(data_dir, "nested.txt"))
  dir.create(file.path(data_dir, "subdir"))
  file.copy(
    source_file,
    file.path(data_dir, "subdir", "nested-2.txt")
  )

  directory_result <- read_uktradeinfo(data_dir)
  expect_equal(nrow(directory_result), 2L)
  expect_true(all(directory_result$DATE_START == as.Date("2024-02-01")))
})

test_that("read_uktradeinfo rejects invalid and empty paths", {
  expect_error(
    read_uktradeinfo(tempfile()),
    "path is not a file or directory"
  )

  empty_dir <- tempfile()
  dir.create(empty_dir)
  expect_error(
    read_uktradeinfo(empty_dir),
    "No .txt or .zip files found in directory"
  )

  non_txt <- tempfile(fileext = ".csv")
  writeLines("not a trade file", non_txt)
  empty_zip <- tempfile(fileext = ".zip")
  old_wd <- setwd(dirname(non_txt))
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(empty_zip, basename(non_txt), flags = "-j")
  expect_error(
    read_uktradeinfo(empty_zip),
    "No .txt files found inside zip"
  )
})

test_that("update_uktradeinfo adds only periods missing from existing data", {
  data_dir <- tempfile()
  dir.create(data_dir)
  writeLines(
    make_uktradeinfo_record(perref = "202402"),
    file.path(data_dir, "BDSImp2402.txt")
  )

  existing_file <- tempfile(fileext = ".txt")
  writeLines(make_uktradeinfo_record(perref = "202401"), existing_file)
  existing <- read_uktradeinfo(existing_file)
  result <- update_uktradeinfo(existing, data_dir)

  expect_equal(nrow(result), 2L)
  expect_equal(
    result$DATE_START,
    as.Date(c("2024-01-01", "2024-02-01"))
  )
})

test_that("update_uktradeinfo returns existing data when no periods are new", {
  data_dir <- tempfile()
  dir.create(data_dir)
  writeLines(
    make_uktradeinfo_record(perref = "202401"),
    file.path(data_dir, "BDSImp2401.txt")
  )

  existing_file <- tempfile(fileext = ".txt")
  writeLines(make_uktradeinfo_record(perref = "202401"), existing_file)
  existing <- read_uktradeinfo(existing_file)
  result <- update_uktradeinfo(existing, data_dir)

  expect_identical(result, existing)
})

test_that("update_uktradeinfo validates existing data and date ranges", {
  expect_error(
    update_uktradeinfo(data.frame(), tempdir()),
    "`existing` must be a data.table"
  )

  existing <- data.table::data.table(value = 1)
  expect_error(
    update_uktradeinfo(existing, tempdir()),
    "`existing` must contain a DATE_START column"
  )
  expect_error(
    update_uktradeinfo(
      data.table::data.table(DATE_START = as.Date("2024-01-01")),
      tempdir(),
      date_range = as.Date("2024-01-01")
    ),
    "`date_range` must be a length-2 vector"
  )
})
