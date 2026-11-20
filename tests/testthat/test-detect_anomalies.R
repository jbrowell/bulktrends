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
