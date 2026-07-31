#' Large-scale anomaly detection
#'
#' This function loops over a list of commodity codes, selects a regression model,
#' and detects anomalies.
#'
#' @param import_data A `data.table` containing trade data. Must include columns `COMCODE` and specified `quantity`.
#' @param codes A vector of HS2/HS4/HS6/CN8 codes
#' @param quantity Quantity to be analysed, e.g. `NET_MASS`, `STAT_VALUE` or `volume`.
#' @param date_col Name of column containing timestamps.
#' @param model_selection_metric A function used for model selection, e.g. `AIC`
#'   or `BIC`. Passed to `select_best_model()`. Default `AIC`.
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`. Default `FALSE`.
#' @param freq See `?extract_ts()`
#' @param verbose If `TRUE`, progress messages are displayed. Default `FALSE`.
#' @param tso_params A named list of additional arguments passed to [tsoutliers::tso()], e.g.
#'   `list(cval = 5, types = c("AO", "TC"), maxit.iloop = 20, maxit.oloop = 10)`.
#'   Default `list()`.
#'
#' @return A list with two elements: (i) `outliers`, a `data.table` with one row per
#'   detected event (or one row with `anomaly_type = "None"` if nothing was found),
#'   and (ii) `list_of_ts`, a named list of `data.table`s — one
#'   per commodity code — containing the time series with fitted regressors and
#'   outlier effect columns appended. Processing runs in parallel via
#'   [future.apply::future_mapply()]; use [future::plan()] to configure workers.
#'
#' @export
detect_anomalies <- function(
  data,
  response_col = "NET_MASS",
  date_col = "DATE_START",
  model_selection_metric = AIC,
  scale_ts = FALSE,
  freq = NULL,
  verbose = FALSE,
  tso_params = list()
) {
  if (inherits(data, "tbl_ts")) {
    key_tbl <- dplyr::group_keys(tsibble::group_by_key(data))

    if (ncol(key_tbl) == 0) {
      series_list <- list(series = data)
      time_series_ids <- "series"
    } else {
      series_list <- dplyr::group_split(tsibble::group_by_key(data))
      time_series_ids <- do.call(paste, c(key_tbl, sep = "|"))
      names(series_list) <- time_series_ids
    }
  } else if (is.list(data)) {
    series_list <- data
    time_series_ids <- names(data)
  } else {
    stop("`data` must be a tsibble or a named list of time series.")
  }

  # time_series_ids <- names(data)

  p <- progressr::progressor(along = time_series_ids)

  results <- future.apply::future_mapply(
    detect_anomalies_single_ts,
    ts_data = series_list,
    id = time_series_ids,
    MoreArgs = list(
      response_col = response_col,
      date_col = date_col,
      scale_ts = scale_ts,
      model_selection_metric = model_selection_metric,
      verbose = verbose,
      tso_params = tso_params,
      p = p
    ),
    SIMPLIFY = FALSE,
    future.globals = list(),
    future.packages = c("bulktrends")
  )

  results <- Filter(Negate(is.null), results)
  all_outliers <- lapply(results, `[[`, "outliers")
  list_of_ts <- lapply(results, `[[`, "list_of_ts")
  names(list_of_ts) <- lapply(results, `[[`, "id")
  all_formulas <- lapply(results, `[[`, "formula")
  names(all_formulas) <- lapply(results, `[[`, "id")
  return(list(
    outliers = rbindlist(all_outliers, fill = TRUE),
    list_of_ts = list_of_ts,
    formulas = all_formulas
  ))
}

#' Detect anomalies in a single time series (Internal)
#' @note This needs to return a list of time series with original tsibble attributes. See suggested pattern in comments...
#'
detect_anomalies_single_ts <- function(
  ts_data,
  id,
  response_col,
  date_col,
  scale_ts,
  model_selection_metric,
  verbose,
  tso_params,
  p
) {
  is_tsibble_input <- inherits(ts_data, "tbl_ts")
  original_ts <- ts_data
  if (is_tsibble_input) {
    data <- ts_data %>%
      dplyr::select(dplyr::all_of(c(date_col, response_col))) %>%
      data.table::as.data.table()
  } else {
    data <- data.table::as.data.table(ts_data)
    data <- data.table::as.data.table(ts_data)[,
      c(date_col, response_col),
      with = FALSE
    ]
  }

  data[[date_col]] <- as.Date(data[[date_col]])

  sparse_rate <- mean(data[[response_col]] == 0, na.rm = T)

  if (!is.na(sparse_rate) && sparse_rate > 0.4) {
    message(paste(
      "Skipping time series",
      id,
      ":",
      round(sparse_rate * 100, 2),
      "% zeros"
    ))
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  if (verbose) {
    message(sprintf("Running selected_model for %s", id))
  }
  # model selection
  selected_model <- tryCatch(
    select_best_model(
      data = data,
      response_col = response_col,
      date_col = date_col,
      metric = model_selection_metric,
      break_detection = TRUE
    ),
    error = function(e) e
  )

  if (inherits(selected_model, "error")) {
    message("Skipping time series", id, ":", selected_model)
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  if (verbose) {
    message(sprintf("Running detect_anomaly for %s", id))
  }

  # matrix of regressors
  ts_data <- selected_model$data
  break_entry <- selected_model$break_entry
  xreg_all <- model.matrix(selected_model$formula, data = ts_data)

  #for ~ -1 models
  if (ncol(xreg_all) == 0) {
    xreg_all <- NULL
  }

  # tso outlier detection
  outliers <- tryCatch(
    detect_outliers(
      data = ts_data,
      response_col = response_col,
      scale_ts = scale_ts,
      xreg = xreg_all,
      tso_params = tso_params
    ),
    error = function(e) e
  )

  if (inherits(outliers, "error")) {
    message("Skipping time series", id, ":", outliers)
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  if (is.null(outliers)) {
    message(paste("Outlier detection failed:", id))
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  ts_data <- outliers$data

  # update formula
  tso_vars <- grep("^(AO|TC|IO)", names(ts_data), value = TRUE)
  if (length(tso_vars) > 0) {
    updated_formula <- update(
      selected_model$formula,
      paste(". ~ . +", paste(tso_vars, collapse = " + "))
    )
  } else {
    updated_formula <- selected_model$formula
  }

  if (!is.null(outliers$outliers) && nrow(outliers$outliers) > 0) {
    # store tso outliers data produced
    new_outliers <- as.data.table(outliers$outliers)
    new_outliers[, id := id]
    new_outliers[, time := ts_data[[date_col]][ind]]
    new_outliers[, anomaly_type := "Outlier"]

    outliers_entry <- new_outliers
  } else {
    outliers_entry <- data.table()
  }

  #add breaks table
  if (!is.null(break_entry) && nrow(break_entry) > 0) {
    break_entry[, id := id]
    break_entry[, anomaly_type := "Break"]

    outliers_entry <- rbindlist(
      list(outliers_entry, break_entry),
      fill = TRUE
    )

    setorder(outliers_entry, time)
  } else {
    break_entry <- data.table()
  }

  # no break no outlier option
  if (nrow(outliers_entry) == 0) {
    outliers_entry <- data.table(
      id = id,
      anomaly_type = "None"
    )
  }

  # back to tsibble if tsibble
  if (is_tsibble_input) {
    ts_data <- data.table::as.data.table(outliers$data)
    ts_data[[date_col]] <- tsibble::yearmonth(ts_data[[date_col]])
    covariate_names <- setdiff(names(ts_data), names(original_ts))
    covariates <- ts_data[, c(date_col, covariate_names), with = FALSE]
    if (length(covariate_names) > 0) {
      covariates <- ts_data[, c(date_col, covariate_names), with = FALSE]
      all_ts <- dplyr::left_join(original_ts, covariates, by = date_col)

      # function to return `all_ts` with attributes as tsibble format
    } else {
      all_ts <- original_ts
    }
  } else {
    all_ts <- ts_data
  }

  p(sprintf("Completed time series %s", id))
  return(list(
    outliers = outliers_entry,
    list_of_ts = all_ts,
    formula = updated_formula,
    id = id
  ))
}
