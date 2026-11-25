# detect_date_frequency() ----------------------------------------------------

test_that("detect_date_frequency identifies day, week and month", {
  d0 <- as.Date("2020-01-01")
  expect_equal(
    detect_date_frequency(seq(d0, by = "day", length.out = 30)),
    "day"
  )
  expect_equal(
    detect_date_frequency(seq(d0, by = "week", length.out = 30)),
    "week"
  )
  expect_equal(
    detect_date_frequency(seq(d0, by = "month", length.out = 30)),
    "month"
  )
})

test_that("detect_date_frequency accepts character dates and unsorted input", {
  dates <- format(rev(seq(
    as.Date("2020-01-01"),
    by = "month",
    length.out = 12
  )))
  expect_equal(detect_date_frequency(dates), "month")
})

test_that("detect_date_frequency errors on too few or irregular dates", {
  expect_error(
    detect_date_frequency(as.Date("2020-01-01")),
    "At least 2 unique dates"
  )
  expect_error(
    detect_date_frequency(rep(as.Date("2020-01-01"), 5)),
    "At least 2 unique dates"
  )
  expect_error(
    detect_date_frequency(seq(
      as.Date("2020-01-01"),
      by = "quarter",
      length.out = 8
    )),
    "Couldn't detect date frequency"
  )
})

# add_date_features() --------------------------------------------------------

test_that("add_date_features adds monthly features", {
  d <- make_monthly_ts(n = 24)
  res <- add_date_features(d, "DATE_START", freq = "month")

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), nrow(d))
  expect_true(all(
    c("linear_trend", "day_of_year", "annual_sin", "annual_cos") %in% names(res)
  ))
  expect_false(any(c("day_of_week", "is_holiday") %in% names(res)))
  expect_equal(res$linear_trend, lubridate::decimal_date(d$DATE_START))
  expect_true(all(abs(res$annual_sin) <= 1 & abs(res$annual_cos) <= 1))
})

test_that("add_date_features auto-detects frequency", {
  d <- make_monthly_ts(n = 24)
  expect_equal(
    add_date_features(data.table::copy(d), "DATE_START"),
    add_date_features(data.table::copy(d), "DATE_START", freq = "month")
  )
})

test_that("add_date_features adds daily and holiday features", {
  d <- data.table::data.table(
    DATE_START = seq(as.Date("2024-12-20"), as.Date("2024-12-31"), by = "day"),
    NET_MASS = 1
  )
  res <- add_date_features(d, "DATE_START", freq = "day")

  expect_true(all(
    c("day_of_week", "holiday", "is_holiday") %in% names(res)
  ))
  expect_type(res$is_holiday, "logical")
  expect_true(res[DATE_START == as.Date("2024-12-25"), is_holiday])
  expect_false(res[DATE_START == as.Date("2024-12-20"), is_holiday])
  expect_equal(res$day_of_week, weekdays(d$DATE_START))
})

test_that("add_date_features accepts data.frame input", {
  d <- as.data.frame(make_monthly_ts(n = 12))
  expect_s3_class(
    add_date_features(d, "DATE_START", freq = "month"),
    "data.table"
  )
})

test_that("add_date_features validates inputs", {
  d <- make_monthly_ts(n = 12)
  expect_error(
    add_date_features(data.table::copy(d), "DATE_START", freq = "year"),
    "must be"
  )
  d_chr <- data.table::copy(d)[, DATE_START := as.character(DATE_START)]
  expect_error(
    add_date_features(d_chr, "DATE_START"),
    "must be of class \"Date\""
  )
  expect_warning(
    add_date_features(
      data.table::copy(d),
      "DATE_START",
      freq = "month",
      division = "scotland"
    ),
    "ignored for non-daily data"
  )
})

test_that("add_date_features warns when dates are outside holiday coverage", {
  d <- data.table::data.table(
    DATE_START = seq(as.Date("2010-01-01"), by = "day", length.out = 10),
    NET_MASS = 1
  )
  expect_warning(
    add_date_features(d, "DATE_START", freq = "day"),
    "outside the coverage"
  )
})
