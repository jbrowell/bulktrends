test_that("segment outlier effects are mapped to the full series and formula", {
  dates <- as.Date("2020-01-01") + 0:39
  input <- data.table::data.table(
    date = dates, y = rep(10, 40), segment_1_AO1 = rep(42, 40)
  )

  testthat::local_mocked_bindings(
    select_best_model = function(data, ...) {
      list(formula = y ~ 1, baseline_formula = y ~ 1, data = data)
    },
    detect_breaks = function(data, ...) {
      list(
        segments = data.frame(segments = rep(1:2, each = 20)),
        break_entry = data.table::data.table(ind = 20L, time = dates[20])
      )
    },
    detect_outliers = function(data, return_tso, ...) {
      if (return_tso) {
        return(list(
          data = cbind(data, data.table::data.table(AO1 = rep(0, 40))),
          outliers = data.frame(),
          tso = list(yadj = data$y)
        ))
      }

      if (data$date[1] == dates[1]) {
        effects <- data.table::data.table(
          AO1 = as.numeric(seq_len(20) == 2),
          TC1 = ifelse(seq_len(20) >= 3, 0.7^(seq_len(20) - 3), 0),
          AO2 = as.numeric(seq_len(20) == 18)
        )
        events <- data.frame(type = c("AO", "TC", "AO"), ind = c(2, 3, 18))
      } else {
        effects <- data.table::data.table(
          AO1 = as.numeric(seq_len(20) == 9),
          TC1 = ifelse(seq_len(20) >= 2, 0.7^(seq_len(20) - 2), 0)
        )
        events <- data.frame(type = c("AO", "TC"), ind = c(9, 2))
      }
      list(data = cbind(data, effects), outliers = events)
    },
    .package = "bulktrends"
  )

  result <- detect_anomalies_single_ts(
    ts_data = input, id = "test", response_col = "y", date_col = "date",
    scale_ts = FALSE, model_selection_metric = AIC, verbose = FALSE,
    tso_params = list(), p = function(...) NULL, break_method = "cpt.reg"
  )

  local_events <- result$outliers[result$outliers$anomaly_type == "Segment_Outlier"]
  expect_equal(local_events$ind, c(2, 3, 29))
  expect_equal(local_events$time, dates[c(2, 3, 29)])
  expect_equal(local_events$segment, c("1", "1", "2"))

  effects <- setdiff(all.vars(result$formula), c("y", "AO1"))
  expect_length(effects, 3)
  expect_true(all(effects %in% names(result$list_of_ts)))
  expect_equal(result$list_of_ts[["segment_1_AO1"]], rep(42, 40))
  expect_equal(result$list_of_ts[[effects[1]]], c(as.numeric(1:20 == 2), rep(0, 20)))
  expect_equal(
    result$list_of_ts[[effects[2]]],
    c(ifelse(1:20 >= 3, 0.7^(1:20 - 3), 0), rep(0, 20))
  )
  expect_equal(result$list_of_ts[[effects[3]]], c(rep(0, 20), as.numeric(1:20 == 9)))
  expect_true("AO1" %in% all.vars(result$formula))
})
