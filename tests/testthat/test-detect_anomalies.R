test_that("regenerated tsibble columns replace original values", {
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 6)
  input <- tsibble::tsibble(
    DATE_START = dates,
    code = rep("x", length(dates)),
    y = seq_along(dates),
    linear_trend = rep(-1, length(dates)),
    index = DATE_START,
    key = code
  )

  testthat::local_mocked_bindings(
    select_best_model = function(data, ...) {
      data[, linear_trend := seq_len(.N)]
      list(
        data = data,
        formula = y ~ linear_trend,
        baseline_formula = y ~ linear_trend,
        break_detection_formula = NULL
      )
    },
    detect_outliers = function(data, response_col, ...) {
      list(
        data = data,
        tso = list(yadj = data[[response_col]]),
        outliers = data.table::data.table()
      )
    },
    detect_breaks = function(...) NULL,
    .package = "bulktrends"
  )

  result <- detect_anomalies_single_ts(
    ts_data = input,
    id = "x",
    response_col = "y",
    date_col = "DATE_START",
    scale_ts = FALSE,
    model_selection_metric = stats::AIC,
    verbose = FALSE,
    tso_params = list(types = c("AO", "TC")),
    p = function(...) invisible(NULL),
    break_method = "cpt.reg"
  )
  output <- result$list_of_ts

  expect_equal(output$linear_trend, seq_along(dates))
  expect_equal(output$code, input$code)
  expect_equal(output$DATE_START, input$DATE_START)
  expect_identical(tsibble::key_vars(output), "code")
  expect_identical(tsibble::index_var(output), "DATE_START")
})

# detect_anomalies() ---------------------------------------------------------

test_that("detect_anomalies rejects invalid input", {
  expect_error(
    detect_anomalies(1:10),
    "must be a tsibble or a named list"
  )
})

test_that("detect_anomalies returns expected structure for a named list", {
  input <- list(
    s1 = make_monthly_ts(spike_at = 30, seed = 1),
    s2 = make_monthly_ts(seed = 2)
  )
  res <- suppressWarnings(detect_anomalies(input))

  expect_type(res, "list")
  expect_named(res, c("outliers", "list_of_ts", "formulas"))
  expect_s3_class(res$outliers, "data.table")
  expect_true(all(c("id", "anomaly_type") %in% names(res$outliers)))
  expect_true(all(res$outliers$id %in% names(input)))
  expect_true(all(
    res$outliers$anomaly_type %in% c("Outlier", "Break", "None")
  ))
  expect_named(res$list_of_ts, names(input))
  expect_named(res$formulas, names(input))
  expect_true(all(vapply(res$formulas, inherits, logical(1), "formula")))
})

test_that("detect_anomalies flags an injected spike as an anomaly", {
  input <- list(s1 = make_monthly_ts(spike_at = 30, seed = 1))
  res <- suppressWarnings(detect_anomalies(input))

  flagged <- res$outliers[anomaly_type %in% c("Outlier", "Break")]
  expect_gt(nrow(flagged), 0)
  expect_true(any(abs(flagged$ind - 30) <= 1))
})

test_that("detect_anomalies classifies an injected spike as an outlier", {

  input <- list(s1 = make_monthly_ts(spike_at = 30, seed = 1))
  res <- suppressWarnings(detect_anomalies(input))

  out <- res$outliers[anomaly_type == "Outlier"]
  expect_gt(nrow(out), 0)
  expect_true(30 %in% out$ind)
  expect_true(any(grepl("^(AO|TC)", names(res$list_of_ts$s1))))
})

test_that("detect_anomalies returns 'None' when no anomalies are present", {
  # Pure white noise with a constant mean
  set.seed(42)
  d <- data.table::data.table(
    DATE_START = seq(as.Date("2018-01-01"), by = "month", length.out = 60),
    NET_MASS = 100 + stats::rnorm(60)
  )
  res <- suppressWarnings(detect_anomalies(list(s1 = d)))
  expect_equal(res$outliers$anomaly_type, "None")
  expect_equal(res$outliers$id, "s1")
})

test_that("detect_anomalies works with a keyed tsibble", {
  ts <- make_keyed_tsibble()
  res <- suppressWarnings(detect_anomalies(ts))

  expect_named(res$list_of_ts, c("0101000000", "0201000000"))
  expect_true(all(
    vapply(res$list_of_ts, inherits, logical(1), "tbl_ts")
  ))
  # Index class is preserved
  expect_s3_class(res$list_of_ts[[1]]$DATE_START, "yearmonth")
  expect_true(all(res$outliers$id %in% c("0101000000", "0201000000")))
})

test_that("detect_anomalies works with an unkeyed tsibble", {
  d <- make_monthly_ts(spike_at = 30)
  d[, DATE_START := tsibble::yearmonth(DATE_START)]
  ts <- tsibble::as_tsibble(d, index = "DATE_START")
  res <- suppressWarnings(detect_anomalies(ts))
  expect_named(res$list_of_ts, "series")
})

test_that("detect_anomalies skips sparse series", {
  sparse <- make_monthly_ts()
  sparse[1:40, NET_MASS := 0]
  input <- list(dense = make_monthly_ts(), sparse = sparse)

  expect_message(
    res <- suppressWarnings(detect_anomalies(input)),
    "Skipping time series sparse"
  )
  expect_named(res$list_of_ts, "dense")
  expect_false("sparse" %in% res$outliers$id)
})

test_that("detect_anomalies respects custom column names", {
  d <- make_monthly_ts(spike_at = 30)
  data.table::setnames(d, c("DATE_START", "NET_MASS"), c("date", "value"))
  res <- suppressWarnings(
    detect_anomalies(list(s1 = d), response_col = "value", date_col = "date")
  )
  expect_true("value" %in% names(res$list_of_ts$s1))
  expect_true(all.vars(res$formulas$s1)[1] == "value")
})

test_that("detect_anomalies passes tso_params through", {
  input <- list(s1 = make_monthly_ts(spike_at = 30, seed = 1))
  # Very high critical value should suppress outlier detection
  res <- suppressWarnings(
    detect_anomalies(input, tso_params = list(cval = 100))
  )
  expect_false("Outlier" %in% res$outliers$anomaly_type)
})

# detect_anomalies_single_ts() -----------------------------------------------

test_that("detect_anomalies_single_ts returns expected elements", {
  p <- function(...) NULL
  res <- suppressWarnings(detect_anomalies_single_ts(
    ts_data = make_monthly_ts(spike_at = 30),
    id = "s1",
    response_col = "NET_MASS",
    date_col = "DATE_START",
    scale_ts = FALSE,
    model_selection_metric = AIC,
    verbose = FALSE,
    tso_params = list(),
    p = p
  ))
  expect_named(res, c("outliers", "list_of_ts", "formula", "id"))
  expect_equal(res$id, "s1")
  expect_s3_class(res$formula, "formula")
  expect_s3_class(res$list_of_ts, "data.table")
  # Outlier covariates are included in the updated formula
  tso_vars <- grep("^(AO|TC|IO)", names(res$list_of_ts), value = TRUE)
  expect_true(all(tso_vars %in% all.vars(res$formula)))
})

test_that("detect_anomalies_single_ts returns NULL for sparse series", {
  d <- make_monthly_ts()
  d[1:50, NET_MASS := 0]
  expect_message(
    res <- detect_anomalies_single_ts(
      ts_data = d,
      id = "sparse",
      response_col = "NET_MASS",
      date_col = "DATE_START",
      scale_ts = FALSE,
      model_selection_metric = AIC,
      verbose = FALSE,
      tso_params = list(),
      p = function(...) NULL
    ),
    "zeros"
  )
  expect_null(res)
})

test_that("detect_anomalies_single_ts emits messages when verbose", {
  expect_message(
    suppressWarnings(detect_anomalies_single_ts(
      ts_data = make_monthly_ts(),
      id = "s1",
      response_col = "NET_MASS",
      date_col = "DATE_START",
      scale_ts = FALSE,
      model_selection_metric = AIC,
      verbose = TRUE,
      tso_params = list(),
      p = function(...) NULL
    )),
    "Running selected_model for s1"
  )
})
