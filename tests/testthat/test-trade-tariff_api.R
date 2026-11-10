# ---- build_comcode_validity_history: input validation -----------------------

test_that("build_comcode_validity_history errors on non-character comcodes", {
  expect_error(build_comcode_validity_history(123))
})

test_that("build_comcode_validity_history errors on empty character vector", {
  expect_error(build_comcode_validity_history(character(0)))
})

test_that("build_comcode_validity_history errors when comcodes contains NA", {
  expect_error(build_comcode_validity_history(c("0101299010", NA)))
})

test_that("build_comcode_validity_history errors on non-digit characters", {
  expect_error(build_comcode_validity_history("0101AB9010"))
})

test_that("build_comcode_validity_history warns and drops wrong-length codes", {
  expect_warning(
    result <- build_comcode_validity_history("12345"),
    "not 8 or 10 digits"
  )
  expect_equal(nrow(result), 0L)
})

# ---- build_comcode_validity_history: core behaviour -------------------------

test_that("build_comcode_validity_history returns expected columns for 10-digit code", {
  fixture <- make_tariff("0101299010", "2021-01-01", NA, 100, "111")
  result <- build_comcode_validity_history(
    "0101299010",
    tariff_commodities = fixture
  )

  expected_cols <- c(
    "input_comcode",
    "comcode",
    "valid_from",
    "valid_to",
    "sid",
    "suffix",
    "overlap_conflict",
    "api_checked",
    "selected_by_current_api",
    "api_valid_from",
    "api_valid_to"
  )
  expect_true(all(expected_cols %in% names(result)))
  expect_false("trackedmodel_ptr_id" %in% names(result))
})

test_that("build_comcode_validity_history returns empty data.frame for absent 10-digit code", {
  fixture <- make_tariff("0101299010", "2021-01-01", NA, 100, "111")
  result <- suppressWarnings(
    build_comcode_validity_history("9999999999", tariff_commodities = fixture)
  )
  expect_equal(nrow(result), 0L)
})

test_that("build_comcode_validity_history warns and expands 8-digit code", {
  fixture <- make_tariff(
    c("0101299010", "0101299090"),
    c("2021-01-01", "2021-01-01"),
    c(NA, NA),
    c(100, 200),
    c("111", "222")
  )
  expect_warning(
    result <- build_comcode_validity_history(
      "01012990",
      tariff_commodities = fixture
    ),
    "8 digits"
  )
  expect_equal(nrow(result), 2L)
  expect_true(all(result$input_comcode == "01012990"))
})

test_that("build_comcode_validity_history returns empty data.frame when 8-digit has no extensions", {
  fixture <- make_tariff("0101299010", "2021-01-01", NA, 100, "111")
  result <- suppressWarnings(
    build_comcode_validity_history("99999999", tariff_commodities = fixture)
  )
  expect_equal(nrow(result), 0L)
})

test_that("build_comcode_validity_history keeps only highest trackedmodel_ptr_id per comcode/sid", {
  fixture <- make_tariff(
    c("0101299010", "0101299010"),
    c("2020-01-01", "2021-01-01"),
    c("2020-12-31", NA),
    c(500, 1000),
    c("111", "111")
  )
  result <- build_comcode_validity_history(
    "0101299010",
    tariff_commodities = fixture
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$valid_from, as.Date("2021-01-01"))
})

# ---- build_comcode_validity_history: overlap detection ----------------------

test_that("overlap_conflict is TRUE when date intervals overlap across different SIDs", {
  fixture <- make_two_sid_fixture(
    "2021-01-01",
    "2022-12-31",
    "111",
    "2022-01-01",
    NA,
    "222"
  )
  result <- suppressWarnings(
    build_comcode_validity_history("0101299010", tariff_commodities = fixture)
  )
  expect_true(all(result$overlap_conflict))
})

test_that("overlap_conflict is FALSE when date intervals do not overlap", {
  fixture <- make_two_sid_fixture(
    "2020-01-01",
    "2020-12-31",
    "111",
    "2021-01-01",
    NA,
    "222"
  )
  result <- suppressWarnings(
    build_comcode_validity_history("0101299010", tariff_commodities = fixture)
  )
  expect_false(any(result$overlap_conflict))
})

test_that("overlap_conflict is FALSE when there is only one SID", {
  fixture <- make_tariff("0101299010", "2021-01-01", NA, 100, "111")
  result <- build_comcode_validity_history(
    "0101299010",
    tariff_commodities = fixture
  )
  expect_false(result$overlap_conflict)
})

test_that("open-ended interval overlaps with any later interval", {
  fixture <- make_two_sid_fixture(
    "2020-01-01",
    NA,
    "111",
    "2025-01-01",
    "2025-12-31",
    "222"
  )
  result <- suppressWarnings(
    build_comcode_validity_history("0101299010", tariff_commodities = fixture)
  )
  expect_true(all(result$overlap_conflict))
})

# ---- get_tariff_commodities: argument validation ----------------------------

test_that("get_tariff_commodities errors on non-logical update_local_package", {
  expect_error(get_tariff_commodities(update_local_package = "yes"))
})

test_that("get_tariff_commodities errors on NA update_local_package", {
  expect_error(get_tariff_commodities(update_local_package = NA))
})

test_that("get_tariff_commodities errors on length-2 update_local_package", {
  expect_error(get_tariff_commodities(update_local_package = c(TRUE, FALSE)))
})

# ---- get_tariff_commodities: integration ------------------------------------

test_that("get_tariff_commodities returns expected data format", {
  skip_if_offline()
  result <- suppressMessages(get_tariff_commodities())
  expect_s3_class(result, "data.frame")
  expected_cols <- c(
    "comcode",
    "valid_from",
    "valid_to",
    "trackedmodel_ptr_id",
    "sid",
    "suffix"
  )
  expect_true(all(expected_cols %in% names(result)))
  expect_equal(attr(result, "created_at"), as.character(Sys.Date()))
  expect_type(result$comcode, "character")
  expect_true(all(grepl("^[0-9]+$", result$comcode)))
  expect_true(all(nchar(result$comcode) %% 2 == 0))
  expect_s3_class(result$valid_from, "Date")
  expect_s3_class(result$valid_to, "Date")
  expect_false(any(grepl("[eE]", result$comcode)))
})
