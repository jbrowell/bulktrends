#' Large-scale anomaly detection
#'
#' This function detects anomalies across multiple time series. Accepts either a keyed
#' [tsibble::tsibble()] (where each key combination is treated as a separate series) or
#' a named list of time series objects. For each series,
#' `select_best_model()` chooses a regression specification (including structural-break detection),
#' and `detect_outliers()` identifies point outliers via `tsoutliers::tso()`.

#' @param data A named list of individual time series, or a keyed tsibble containing
#' one or more trade time series.
#' @param response_col Quantity to be analysed, e.g. `NET_MASS`, `STAT_VALUE` or `volume`.
#' @param date_col Name of column containing timestamps.
#' @param model_selection_metric A function used for model selection, e.g. `AIC`
#'   or `BIC`. Passed to `select_best_model()`. Default `AIC`.
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance
#' using `scale()`. Default `FALSE`.
#' @param verbose If `TRUE`, progress messages are displayed. Default `FALSE`.
#' @param tso_params A named list of additional arguments passed to [tsoutliers::tso()], e.g.
#'   `list(cval = 5, types = c("AO", "TC"), maxit.iloop = 20, maxit.oloop = 10)`.
#'   Default `list()`.
#' @param break_method Options are `"Bai-Perron"` or `"PELT"`. See
#' `?detect_breaks` for details.
#'
#' @return A list with three elements: (i) `outliers`, a `data.table` with one row per
#'   detected event (or one row with `anomaly_type = "None"` if nothing was found).
#'   The `anomaly_type` column takes values `"Outlier"` (global Chen-Liu detection),
#'   `"Segment_Outlier"` (local detection within a PELT/Bai-Perron segment), or
#'   `"Break"` (structural break). The `list_of_ts` element also contains a
#'   `.adjusted_response` column holding the TSO-adjusted series used for break detection.
#'   (ii) `list_of_ts`, a named list of `data.table`s — one
#'   per series — containing the time series with fitted regressors, outlier effect
#'   columns, and `.adjusted_response` appended, and (iii) the final model formula for
#'   each time series. Processing runs in parallel via
#'   [future.apply::future_mapply()]; use [future::plan()] to configure workers.
#'
#' @export
detect_anomalies <- function(
  data,
  response_col = "NET_MASS",
  date_col = "DATE_START",
  model_selection_metric = AIC,
  break_method = NULL,
  scale_ts = FALSE,
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
      p = p,
      break_method = break_method
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

#' Detect anomalies in a single time series
#'
#' @param ts_data A single time series as a tsibble, data.table, or data frame.
#' @param id Identifier for the time series.
#' @param response_col Quantity to be analysed, e.g. `NET_MASS`, `STAT_VALUE` or `volume`.
#' @param date_col Name of column containing timestamps.
#' @param model_selection_metric A function used for model selection, e.g. `AIC`
#'   or `BIC`. Passed to `select_best_model()`. Default `AIC`.
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`. Default `FALSE`.
#' @param verbose If `TRUE`, progress messages are displayed. Default `FALSE`.
#' @param tso_params A named list of additional arguments passed to [tsoutliers::tso()], e.g.
#'   `list(cval = 5, types = c("AO", "TC"), maxit.iloop = 20, maxit.oloop = 10)`.
#'   Default `list()`.
#' @param p A `progressr` progressor function.
#' @param break_method Options are `"Bai-Perron"` or `"PELT"`. See
#' `?detect_breaks` for details.
#'
#' @return A list containing detected outliers, the augmented time series,
#'   the updated model formula, and the series identifier.
#'
#' @keywords internal
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
  p,
  break_method
) {
  is_tsibble_input <- inherits(ts_data, "tbl_ts")
  original_ts <- ts_data

  # ------------------------------------------------------------
  # 1. Prepare data — preserve all input columns
  # ------------------------------------------------------------

  data <- data.table::as.data.table(ts_data)
  data[[date_col]] <- as.Date(data[[date_col]])

  if (anyNA(data[[response_col]])) {
    warning("Missing response values in series ", id, ".")
  }

  sparse_rate <- mean(data[[response_col]] == 0, na.rm = TRUE)

  if (!is.na(sparse_rate) && sparse_rate > 0.4) {
    message(
      "Skipping time series ",
      id,
      ": ",
      round(sparse_rate * 100, 2),
      "% zeros"
    )
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  tso_params <- utils::modifyList(tso_params, list(types = c("AO", "TC")))

  # ------------------------------------------------------------
  # 2. Select the baseline regression model
  # ------------------------------------------------------------

  if (verbose) {
    message("Selecting baseline model for ", id)
  }

  selected_model <- tryCatch(
    select_best_model(
      data = data,
      response_col = response_col,
      date_col = date_col,
      metric = model_selection_metric,
      break_detection = TRUE,
      break_method = break_method
    ),
    error = function(e) e
  )

  if (inherits(selected_model, "error")) {
    message(
      "Skipping time series ",
      id,
      ": model selection failed: ",
      conditionMessage(selected_model)
    )
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  if (is.null(selected_model$formula)) {
    message("Skipping time series ", id, ": no valid model was selected.")
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  baseline_formula <- selected_model$baseline_formula
  ts_data <- data.table::copy(selected_model$data)

  # Remove provisional segments so they don't bias TSO xreg
  if ("segments" %in% names(ts_data)) {
    ts_data[, segments := NULL]
  }

  # ------------------------------------------------------------
  # 3. Global Chen-Liu outlier detection on the original response
  # ------------------------------------------------------------

  if (verbose) {
    message("Running global outlier detection for ", id)
  }

  xreg_all <- stats::model.matrix(baseline_formula, data = ts_data)
  if (ncol(xreg_all) == 0) {
    xreg_all <- NULL
  }

  global_outliers <- tryCatch(
    detect_outliers(
      data = ts_data,
      response_col = response_col,
      scale_ts = scale_ts,
      xreg = xreg_all,
      return_tso = TRUE,
      tso_params = tso_params
    ),
    error = function(e) e
  )

  if (
    inherits(global_outliers, "error") ||
      is.null(global_outliers) ||
      is.null(global_outliers$tso)
  ) {
    message("Skipping time series ", id, ": global outlier detection failed.")
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  ts_data <- global_outliers$data

  # ------------------------------------------------------------
  # 4. Extract the anomaly-adjusted response
  # ------------------------------------------------------------

  adjusted_response <- as.numeric(global_outliers$tso$yadj)

  if (length(adjusted_response) != nrow(ts_data)) {
    message("Skipping time series ", id, ": adjusted response length mismatch.")
    p(sprintf("Skipped %s", id))
    return(NULL)
  }

  if (scale_ts) {
    original_y <- as.numeric(data[[response_col]])
    scale_center <- mean(original_y, na.rm = TRUE)
    scale_sd <- stats::sd(original_y, na.rm = TRUE)
    if (is.finite(scale_sd) && scale_sd > 0) {
      adjusted_response <- adjusted_response * scale_sd + scale_center
    } else {
      adjusted_response <- original_y
    }
  }

  ts_data[, .adjusted_response := adjusted_response]

  # ------------------------------------------------------------
  # 5. Run break detection on the adjusted response
  # ------------------------------------------------------------

  adjusted_formula <- baseline_formula
  adjusted_formula[[2]] <- as.name(".adjusted_response")

  if (verbose) {
    message("Running break detection on the adjusted response for ", id)
  }

  final_breaks <- tryCatch(
    detect_breaks(
      data = ts_data,
      formula = adjusted_formula,
      date_col = date_col,
      method = break_method
    ),
    error = function(e) e
  )

  if (inherits(final_breaks, "error")) {
    message(
      "Break detection failed for ",
      id,
      ": ",
      conditionMessage(final_breaks)
    )
    final_breaks <- NULL
  }

  if (is.null(final_breaks)) {
    segments <- factor(rep(1L, nrow(ts_data)))
    break_entry <- data.table::data.table()
  } else {
    segments <- as.factor(final_breaks$segments$segments)
    break_entry <- data.table::copy(final_breaks$break_entry)
  }

  ts_data[, segments := segments]
  break_indices <- if (nrow(break_entry) > 0) break_entry$ind else integer(0)

  # ------------------------------------------------------------
  # 6. Build the global anomaly-event table
  # ------------------------------------------------------------

  if (
    !is.null(global_outliers$outliers) &&
      nrow(global_outliers$outliers) > 0
  ) {
    global_entry <- data.table::as.data.table(global_outliers$outliers)
    global_entry[, id := id]
    global_entry[, time := ts_data[[date_col]][ind]]
    global_entry[, anomaly_type := "Outlier"]
  } else {
    global_entry <- data.table::data.table()
  }

  # ------------------------------------------------------------
  # 7. Segment-level outlier detection
  # ------------------------------------------------------------

  if (verbose) {
    message("Running segment-level outlier detection for ", id)
  }

  segment_entries <- list()
  for (seg_id in levels(ts_data$segments)) {
    seg_idx <- which(ts_data$segments == seg_id)
    if (length(seg_idx) < 8) {
      next
    }

    seg_data <- data.table::copy(ts_data[seg_idx])
    seg_data[, (response_col) := .adjusted_response]

    seg_xreg <- stats::model.matrix(adjusted_formula, data = seg_data)
    if (ncol(seg_xreg) == 0) {
      seg_xreg <- NULL
    }

    seg_out <- tryCatch(
      detect_outliers(
        data = seg_data,
        response_col = response_col,
        scale_ts = FALSE,
        xreg = seg_xreg,
        return_tso = FALSE,
        tso_params = tso_params
      ),
      error = function(e) e
    )

    if (
      inherits(seg_out, "error") ||
        is.null(seg_out) ||
        is.null(seg_out$outliers) ||
        nrow(seg_out$outliers) == 0
    ) {
      next
    }

    entry <- data.table::as.data.table(seg_out$outliers)
    entry[, ind := seg_idx[ind]]
    entry[, id := id]
    entry[, time := ts_data[[date_col]][ind]]
    entry[, anomaly_type := "Segment_Outlier"]
    entry[, segment := seg_id]
    segment_entries[[length(segment_entries) + 1L]] <- entry
  }

  segment_entry <- if (length(segment_entries) > 0) {
    data.table::rbindlist(segment_entries, fill = TRUE)
  } else {
    data.table::data.table()
  }

  # ------------------------------------------------------------
  # 8. Proximity filter — suppress events within ±5 of a break
  # ------------------------------------------------------------

  prox_filter <- function(dt, bi) {
    if (nrow(dt) == 0 || length(bi) == 0) {
      return(dt)
    }
    dt[sapply(ind, function(i) !any(abs(i - bi) <= 5))]
  }

  global_entry <- prox_filter(global_entry, break_indices)
  segment_entry <- prox_filter(segment_entry, break_indices)

  # ------------------------------------------------------------
  # 9. Add structural-break events and combine
  # ------------------------------------------------------------

  if (nrow(break_entry) > 0) {
    break_entry[, id := id]
    break_entry[, anomaly_type := "Break"]
  }

  outliers_entry <- data.table::rbindlist(
    list(global_entry, segment_entry, break_entry),
    fill = TRUE
  )

  if (nrow(outliers_entry) > 0 && "time" %in% names(outliers_entry)) {
    data.table::setorder(outliers_entry, time)
  }

  if (nrow(outliers_entry) == 0) {
    outliers_entry <- data.table::data.table(
      id = id,
      anomaly_type = "None"
    )
  }

  # ------------------------------------------------------------
  # 10. Update the final formula to include detected outlier effects
  # ------------------------------------------------------------

  tso_vars <- grep("^(AO|TC|IO)", names(ts_data), value = TRUE)
  final_formula <- baseline_formula
  if (length(tso_vars) > 0) {
    final_formula <- stats::update(
      baseline_formula,
      paste(". ~ . +", paste(tso_vars, collapse = " + "))
    )
  }

  # ------------------------------------------------------------
  # 11. Restore original tsibble structure when applicable
  # ------------------------------------------------------------

  if (is_tsibble_input) {
    if (inherits(original_ts[[date_col]], "yearmonth")) {
      ts_data[[date_col]] <- tsibble::yearmonth(ts_data[[date_col]])
    } else {
      ts_data[[date_col]] <- as.Date(ts_data[[date_col]])
    }
    covariate_names <- setdiff(names(ts_data), names(original_ts))
    if (length(covariate_names) > 0) {
      covariates <- ts_data[, c(date_col, covariate_names), with = FALSE]
      all_ts <- dplyr::left_join(original_ts, covariates, by = date_col)
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
    formula = final_formula,
    id = id
  ))
}
