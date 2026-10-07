
# function to do hierarchical forecasting for all codes or selection of codes

forecast_hierarchies <- function(anomaly_result,
                                 codes = names(anomaly_result$data),
                                 train_fold = 24,
                                 step_ahead = 1) {

  results <- list()

  for (code in codes) {
    ts_data <- anomaly_result$data[[code]]
    formula <- anomaly_result$formulas[[code]]

    if (is.null(ts_data)) {
      warning("No time-series data found for code: ", code)
      next
    }
    if (is.null(formula)) {
      warning("No formula found for code: ", code)
      next
    }

    cv_data <- ts_data %>%
      stretch_tsibble(.init = init, .step = step)

    # model fitting
    fit_try <- tryCatch(
      cv_data %>%
        model(
          ets = ETS(
            NET_MASS ~ error("A") + trend(c("A", "N")) + season(c("A", "N"))
          ),
          arima_all = ARIMA(!!formula)
        ),
      error = function(e) NULL
    )

    if (
      is.null(fit_try) ||
      any(fabletools::is_null_model(fit_try$arima_all))
    ) {
      fit_try <- cv_data %>%
        model(
          ets = ETS(
            NET_MASS ~ error("A") + trend(c("A", "N")) + season(c("A", "N"))
          ),
          arima_all = ARIMA(NET_MASS ~ pdq(d = 0) + PDQ(D = 0))
        )
    }

    fit[[code]] <- fit_try

    # future data with covariates
    cv_future[[code]] <- ts_data %>%
      stretch_tsibble(.init = 24 + 1, .step = 1) %>%
      group_by_key() %>%
      slice_tail(n = 1) %>%
      ungroup()

    actual[[code]] <- ts_data
  }

  # bind all code results
  fit_all <- vec_rbind(!!!fit)
  future_all <- vec_rbind(!!!cv_future)
  actual_all <- vec_rbind(!!!actual)


  # ensure id folds match future data - may not be needed
  ids <- intersect(
    unique(fit_all$.id),
    unique(future_all$.id)
  )


  # perform reconciliation and forecasting for each .id fold
  fc_cv <- vector("list", length(ids))
  names(fc_cv) <- as.character(ids)

  for (i in ids) {
    fit_fold <- fit_all %>%
      dplyr::filter(.id == i) %>%
      reconcile(
        mint_ets = min_trace(ets, method = "mint_shrink"),
        mint_arima = min_trace(arima_all, method = "mint_shrink")
      )

    future_fold <- future_all %>%
      dplyr::filter(.id == i)

    fc_cv[[as.character(i)]] <- fit_fold %>%
      forecast(new_data = future_fold)
  }

  # bind all reconciled forecasts
  fc_all <- vec_rbind(!!!fc_cv)

  # save results
    results[[code]] <- list(fit = fit,
                            forecast = dplyr::bind_rows(forecasts),
                            actual = ts_data)
  }

  return(results)
}
