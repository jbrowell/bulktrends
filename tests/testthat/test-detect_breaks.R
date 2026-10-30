# devtools::load_all()
# testthat::test_file("tests/testthat/test-detect_breaks.R")

test_that("detect_breaks returns detected breaks and segment indicators", {
  data <- data.frame(
    DATE_START = as.Date("2020-01-01") + 0:39,
    value = c(rep(10, 20), rep(30, 20))
  )

  result <- detect_breaks(data, formula = value ~ 1)

  expect_equal(
    result$break_entry,
    data.table::data.table(
      type = "SB",
      ind = 20,
      time = as.Date("2020-01-20")
    )
  )
  expect_equal(
    result$segments$segments,
    factor(
      c(rep("segment1", 20), rep("segment2", 20)),
      levels = c("segment1", "segment2")
    )
  )
})

test_that("detect_breaks returns NULL when no break is detected", {
  data <- data.frame(
    observation_date = as.Date("2020-01-01") + 0:39,
    value = rep(10, 40)
  )

  expect_null(
    detect_breaks(
      data,
      date_col = "observation_date",
      formula = value ~ 1
    )
  )
})

test_that("detect_breaks returns the documented output formats", {
  data <- data.frame(
    DATE_START = as.Date("2020-01-01") + 0:39,
    value = c(rep(10, 20), rep(30, 20))
  )

  result <- detect_breaks(data, formula = value ~ 1)

  expect_type(result, "list")
  expect_named(result, c("break_entry", "segments"))
  expect_s3_class(result$break_entry, "data.table")
  expect_s3_class(result$segments, "data.table")
  expect_named(result$break_entry, c("type", "ind", "time"))
  expect_type(result$break_entry$type, "character")
  expect_type(result$break_entry$ind, "double")
  expect_s3_class(result$break_entry$time, "Date")
  expect_type(result$segments$segments, "integer")
  expect_true(is.factor(result$segments$segments))
})

test_that("detect_breaks rejects inputs in the wrong format", {
  data <- data.frame(
    DATE_START = as.Date("2020-01-01") + 0:39,
    value = c(rep(10, 20), rep(30, 20))
  )

  expect_error(
    detect_breaks(data$value, formula = value ~ 1),
    "'data' must be a data.frame, environment, or list"
  )
  expect_error(
    detect_breaks(data),
    "argument \"formula\" is missing"
  )
  expect_error(
    detect_breaks(data, formula = missing ~ 1),
    "invalid type \\(special\\) for variable 'missing'"
  )
})
