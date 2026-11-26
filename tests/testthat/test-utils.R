# devtools::load_all()
# testthat::test_file("tests/testthat/test-utils.R")

test_that("detect_date_frequency identifies supported frequencies", {
  expect_equal(
    detect_date_frequency(as.Date(c("2024-01-01", "2024-01-02"))),
    "day"
  )
  expect_equal(
    detect_date_frequency(as.Date(c("2024-01-01", "2024-01-08"))),
    "week"
  )
  expect_equal(
    detect_date_frequency(as.Date(c("2024-01-01", "2024-02-01"))),
    "month"
  )
})

test_that("detect_date_frequency removes duplicate dates and validates input", {
  expect_equal(
    detect_date_frequency(
      c("2024-01-02", "2024-01-01", "2024-01-01")
    ),
    "day"
  )
  expect_error(
    detect_date_frequency(as.Date("2024-01-01")),
    "At least 2 unique dates"
  )
  expect_error(
    detect_date_frequency(as.Date(c("2024-01-01", "2024-01-15"))),
    "Couldn't detect date frequency"
  )
})

test_that("extract_ts aggregates values and fills missing dates", {
  data <- data.table::data.table(
    COMCODE = c("0101", "0101", "0101"),
    DATE_START = as.Date(c("2024-01-01", "2024-01-01", "2024-01-03")),
    NET_MASS = c(2, 3, 7)
  )

  result <- extract_ts(
    data,
    code = "01",
    fill_missing = 0,
    freq = "day"
  )

  expect_s3_class(result, "data.table")
  expect_equal(result$code, rep("01", 3))
  expect_equal(result$DATE_START, as.Date("2024-01-01") + 0:2)
  expect_equal(result$NET_MASS, c(5, 0, 7))
})

test_that("extract_ts supports volume and grouped series", {
  data <- data.table::data.table(
    COMCODE = c("0101", "0101", "0101", "0202"),
    DATE_START = as.Date(c(
      "2024-01-01",
      "2024-01-01",
      "2024-01-02",
      "2024-01-01"
    )),
    PORT_CODE = c("A", "A", "B", "A"),
    volume = 99
  )

  result <- suppressWarnings(
    extract_ts(
      data,
      code = "01",
      response_col = "volume",
      group_by = "PORT_CODE",
      freq = "day",
      fill_missing = 0
    )
  )

  expect_equal(result$volume, c(2, 0, 0, 1))
  expect_equal(result$PORT_CODE, c("A", "A", "B", "B"))
  result_list <- suppressWarnings(
    extract_ts(
      data,
      code = c("01", "02"),
      response_col = "volume",
      freq = "day",
      return_list = TRUE
    )
  )
  expect_length(result_list, 2)
})

test_that("add_date_features adds daily calendar and holiday features", {
  holidays <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    title = c("New Year's Day", "Test holiday"),
    division = c("scotland", "scotland")
  )
  data <- data.table::data.table(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    value = c(1, 2)
  )

  result <- add_date_features(
    data,
    date_col = "date",
    division = "scotland",
    holidays = holidays,
    freq = "day"
  )

  expect_equal(result$value, c(1, 2))
  expect_equal(result$holiday, c("New Year's Day", "Test holiday"))
  expect_equal(result$is_holiday, c(TRUE, TRUE))
  expect_equal(result$day_of_year, c(1L, 2L))
  expect_true(all(
    c("linear_trend", "annual_sin", "annual_cos") %in%
      names(result)
  ))
})

test_that("add_date_features validates dates and frequency", {
  expect_error(
    add_date_features(
      data.frame(date = as.character(as.Date("2024-01-01"))),
      date_col = "date",
      freq = "day"
    ),
    'Input must be of class "Date"'
  )

  data <- data.table::data.table(
    date = as.Date(c("2024-01-01", "2024-01-02"))
  )
  expect_error(
    add_date_features(data, date_col = "date", freq = "quarter"),
    '"freq" must be "day", "week" or "month"'
  )
})
