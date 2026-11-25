#' Large-scale anomaly detection
#'
#' This function detects anomalies across multiple time series. Accepts either a keyed
#' [tsibble::tsibble()] (where each key combination is treated as a separate series) or
#' a named list of time series objects. For each series,
#' `select_best_model()` chooses a regression specification (including structural-break detection),
#' and `detect_outliers()` identifies point outliers via `tsoutliers::tso()`.
#'
#' @param data A named list of individual time series, or a keyed tsibble containing
#' one or more trade time series.
#' @param response_col Quantity to be analysed, e.g. `NET_MASS`, `STAT_VALUE` or `volume`.
#' @param date_col Name of column containing timestamps.
#' @param model_selection_metric A function used for model selection, e.g. `AIC`
#'   or `BIC`. Passed to `select_best_model()`. Default `AIC`.
#' @param break_method Function used for structural break detection.
#'   Options are `"breakpoints"` or `"cpt.reg"`. Default `"cpt.reg"`.
#' @param break_params A named list of additional arguments passed to the selected
#'   structural break detection function. For `"cpt.reg"`, this includes the
#'   algorithm selection (`method = "PELT"` or `"AMOC"`) and other arguments
#'   such as `minseglen`. Default `list()`.
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance
#' using `scale()`. Default `FALSE`.
#' @param verbose If `TRUE`, progress messages are displayed. Default `FALSE`.
#' @param tso_params A named list of additional arguments passed to [tsoutliers::tso()], e.g.
#'   `list(cval = 5, types = c("AO", "TC"), maxit.iloop = 20, maxit.oloop = 10)`.
#'   Default `list()`.
#'
#' @details
#' Break method `"breakpoints"` uses the implementation in `strucchangeRcpp::breakpoints()`.
#' Break method `"cpt.reg"` uses the implementation in `changepoint::cpt.reg()`,
#' which supports the PELT and AMOC algorithms. PELT is used by default.
#' Additional arguments can be supplied through `break_params`.
#'
#' @return A list with three elements: (i) `outliers`, a `data.table` with one row per
#'   detected event (or one row with `anomaly_type = "None"` if nothing was found).
#'   The `anomaly_type` column takes values `"Outlier"` (global Chen-Liu detection),
#'   `"Segment_Outlier"` (local detection within a structural-break segment), or
#'   `"Break"` (structural break).
#'   (ii) `list_of_ts`, a named list of `data.table`s — one
#'   per series — containing the time series with fitted regressors, outlier effect
#'   columns, and `adjusted_y` (the TSO-adjusted series used for break detection) appended,
#'   and (iii) the final model formula for
#'   each time series. Processing runs in parallel via
#'   [future.apply::future_mapply()]; use [future::plan()] to configure workers.
#'
#' @export
detect_anomalies <- function(
  data,
  response_col = "NET_MASS",
  date_col = "DATE_START",
  model_selection_metric = AIC,
  break_method = "cpt.reg",
  break_params = list(),
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
      break_method = break_method,
      break_params = break_params
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
#' @param break_method Function used for structural break detection.
#'   Options are `"breakpoints"` or `"cpt.reg"`.
#' @param break_params A named list of additional arguments passed to the selected
#'   structural break detection function. Default `list()`.
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
  break_method,
  break_params = list()
) {
  is_tsibble_input <- inherits(ts_data, "tbl_ts")
  original_ts <- ts_data

  # ------------------------------------------------------------
  # 1. Prepare data — preserve input columns that data.table can
  #    safely subset and bind
  # ------------------------------------------------------------

  data <- data.table::as.data.table(ts_data)

  # Columns that are not plain atomic vectors (e.g. the `agg_vec` keys
  # created by fabletools::aggregate_key(), or list columns) are stored as
  # lists, which data.table row-subsetting and cbind() mishandle. Set them
  # aside here and restore them in step 11.
  set_aside_cols <- names(data)[
    !vapply(data, is.atomic, logical(1))
  ]

  bad_cols <- intersect(set_aside_cols, c(date_col, response_col))
  if (length(bad_cols) > 0) {
    stop(
      "Column(s) ",
      paste0("`", bad_cols, "`", collapse = ", "),
      " in series ",
      id,
      " must be atomic vectors (e.g. numeric or Date)."
    )
  }

  # Key columns are constant within a series and are restored from
  # `original_ts`, so they are not needed in the working data either
  if (is_tsibble_input) {
    set_aside_cols <- union(set_aside_cols, tsibble::key_vars(original_ts))
  }

  set_aside <- lapply(
    stats::setNames(set_aside_cols, set_aside_cols),
    function(v) data[[v]]
  )

  if (length(set_aside_cols) > 0) {
    data[, (set_aside_cols) := NULL]
  }

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

  if (any(tso_params$types %in% c("IO", "LS"))) {
    stop("`tso_params$types` must not include \"IO\" or \"LS\".")
  }

  tso_params <- utils::modifyList(
    list(types = c("AO", "TC")),
    tso_params
  )

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
      break_method = break_method,
      break_params = break_params
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
    .handle_zeroed_residuals(
      detect_outliers(
        data = ts_data,
        response_col = response_col,
        scale_ts = scale_ts,
        xreg = xreg_all,
        return_tso = TRUE,
        tso_params = tso_params
      ),
      id = id
    ),
    error = function(e) e
  )

  if (
    inherits(global_outliers, "error") ||
      is.null(global_outliers) ||
      is.null(global_outliers$tso)
  ) {
    reason <- if (inherits(global_outliers, "error")) {
      conditionMessage(global_outliers)
    } else if (is.null(global_outliers)) {
      "all tso() fitting attempts failed"
    } else {
      "no tso() result was returned"
    }
    message(
      "Skipping time series ",
      id,
      ": global outlier detection failed: ",
      reason
    )
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

  ts_data[, adjusted_y := adjusted_response]

  # ------------------------------------------------------------
  # 5. Run break detection on the adjusted response
  # ------------------------------------------------------------

  if (verbose) {
    message("Running break detection on the adjusted response for ", id)
  }

  ts_data_adj <- data.table::copy(ts_data)
  ts_data_adj[, (response_col) := adjusted_response]

  break_detect_formula <- if (
    !is.null(selected_model$break_detection_formula)
  ) {
    selected_model$break_detection_formula
  } else {
    baseline_formula
  }

  final_breaks <- tryCatch(
    detect_breaks(
      data = ts_data_adj,
      formula = break_detect_formula,
      date_col = date_col,
      break_method = break_method,
      break_params = break_params
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
  segment_effect_vars <- character()

  for (seg_id in levels(ts_data$segments)) {
    seg_idx <- which(ts_data$segments == seg_id)

    if (length(seg_idx) < 8) {
      next
    }

    seg_data <- data.table::copy(ts_data[seg_idx])
    seg_data[, (response_col) := ts_data[["adjusted_y"]][seg_idx]]

    # ------------------------------------------------------------
    # Prepare data for segment-level detection
    # Remove global AO/TC effects to avoid naming collisions
    # ------------------------------------------------------------

    seg_detect_data <- data.table::copy(seg_data)

    global_effects <- grep(
      "^(AO|TC)",
      names(seg_detect_data),
      value = TRUE
    )

    if (length(global_effects) > 0) {
      seg_detect_data[, (global_effects) := NULL]
    }

    # ------------------------------------------------------------
    # Prepare regressors
    # ------------------------------------------------------------

    seg_xreg <- stats::model.matrix(
      baseline_formula,
      data = seg_detect_data
    )

    if (ncol(seg_xreg) == 0) {
      seg_xreg <- NULL
    }

    # ------------------------------------------------------------
    # Run segment-level outlier detection
    # ------------------------------------------------------------

    # Zeroed start-up residuals are not reported for segments: events
    # near a break are removed by the proximity filter in step 8 anyway
    seg_out <- tryCatch(
      .handle_zeroed_residuals(
        detect_outliers(
          data = seg_detect_data,
          response_col = response_col,
          scale_ts = FALSE,
          xreg = seg_xreg,
          return_tso = FALSE,
          tso_params = tso_params
        ),
        id = NULL
      ),
      error = function(e) e
    )

    if (
      inherits(seg_out, "error") ||
        is.null(seg_out) ||
        is.null(seg_out$data)
    ) {
      next
    }
    # ------------------------------------------------------------
    # Retain local AO/TC effect columns in the full time series
    # ------------------------------------------------------------

    local_effects <- grep(
      "^(AO|TC)",
      names(seg_out$data),
      value = TRUE
    )

    if (length(local_effects) > 0) {
      segment_number <- match(
        seg_id,
        levels(ts_data$segments)
      )

      for (effect_name in local_effects) {
        base_effect_name <- paste0(
          "SEG",
          segment_number,
          "_",
          effect_name
        )

        new_effect_name <- utils::tail(
          make.unique(
            c(
              names(ts_data),
              names(original_ts),
              base_effect_name
            )
          ),
          1L
        )

        # Create a full-length effect vector.
        # Values outside this segment are zero.

        effect_values <- numeric(nrow(ts_data))

        segment_effect_values <- seg_out$data[[effect_name]]

        if (length(segment_effect_values) != length(seg_idx)) {
          next
        }

        effect_values[seg_idx] <- segment_effect_values

        ts_data[, (new_effect_name) := effect_values]

        segment_effect_vars <- c(
          segment_effect_vars,
          new_effect_name
        )
      }
    }

    # ------------------------------------------------------------
    # Retain the local outlier event summary separately
    # ------------------------------------------------------------

    if (
      !is.null(seg_out$outliers) &&
        nrow(seg_out$outliers) > 0
    ) {
      entry <- data.table::as.data.table(
        seg_out$outliers
      )

      entry[, ind := seg_idx[ind]]
      entry[, id := id]
      entry[, time := ts_data[[date_col]][ind]]
      entry[, anomaly_type := "Segment_Outlier"]
      entry[, segment := seg_id]

      segment_entries[[length(segment_entries) + 1L]] <- entry
    }
  }

  segment_entry <- if (length(segment_entries) > 0) {
    data.table::rbindlist(
      segment_entries,
      fill = TRUE
    )
  } else {
    data.table::data.table()
  }

  # ------------------------------------------------------------
  # 8. Proximity filter — suppress events and effects within ±5
  #    observations of a structural break
  # ------------------------------------------------------------

  prox_filter <- function(dt, bi) {
    if (nrow(dt) == 0 || length(bi) == 0) {
      return(dt)
    }

    dt[sapply(ind, function(i) !any(abs(i - bi) <= 5))]
  }

  # Identify events that will be suppressed
  suppressed_global <- if (
    nrow(global_entry) > 0 && length(break_indices) > 0
  ) {
    global_entry[
      sapply(ind, function(i) any(abs(i - break_indices) <= 5))
    ]
  } else {
    data.table::data.table()
  }

  suppressed_segment <- if (
    nrow(segment_entry) > 0 && length(break_indices) > 0
  ) {
    segment_entry[
      sapply(ind, function(i) any(abs(i - break_indices) <= 5))
    ]
  } else {
    data.table::data.table()
  }

  # ------------------------------------------------------------
  # Identify global effect columns associated with suppressed events
  # ------------------------------------------------------------

  global_effects_to_remove <- character()

  if (nrow(suppressed_global) > 0) {
    for (i in seq_len(nrow(suppressed_global))) {
      event_type <- suppressed_global$type[i]
      event_ind <- suppressed_global$ind[i]

      candidates <- grep(
        paste0("^", event_type, "\\d+$"),
        names(ts_data),
        value = TRUE
      )

      candidates <- candidates[
        vapply(
          candidates,
          function(v) {
            onset <- which(ts_data[[v]] != 0)[1L]
            !is.na(onset) && onset == event_ind
          },
          logical(1)
        )
      ]

      global_effects_to_remove <- c(
        global_effects_to_remove,
        candidates
      )
    }
  }

  # ------------------------------------------------------------
  # Identify segment-specific effect columns associated with
  # suppressed segment events
  # ------------------------------------------------------------

  segment_effects_to_remove <- character()

  if (nrow(suppressed_segment) > 0) {
    for (i in seq_len(nrow(suppressed_segment))) {
      event_type <- suppressed_segment$type[i]
      event_ind <- suppressed_segment$ind[i]
      event_seg <- suppressed_segment$segment[i]

      segment_number <- match(
        event_seg,
        levels(ts_data$segments)
      )

      candidates <- grep(
        paste0("^SEG", segment_number, "_", event_type, "[0-9]+(\\.[0-9]+)?$"),
        segment_effect_vars,
        value = TRUE
      )

      candidates <- candidates[
        vapply(
          candidates,
          function(v) {
            onset <- which(ts_data[[v]] != 0)[1L]
            !is.na(onset) && onset == event_ind
          },
          logical(1)
        )
      ]

      segment_effects_to_remove <- c(
        segment_effects_to_remove,
        candidates
      )
    }
  }

  # ------------------------------------------------------------
  # Remove the suppressed effects from the augmented time series
  # ------------------------------------------------------------

  effects_to_remove <- unique(
    c(global_effects_to_remove, segment_effects_to_remove)
  )

  if (length(effects_to_remove) > 0) {
    ts_data[, (effects_to_remove) := NULL]

    segment_effect_vars <- setdiff(
      segment_effect_vars,
      segment_effects_to_remove
    )
  }

  # ------------------------------------------------------------
  # Filter suppressed events from the event tables
  # ------------------------------------------------------------

  global_entry <- prox_filter(
    global_entry,
    break_indices
  )

  segment_entry <- prox_filter(
    segment_entry,
    break_indices
  )

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
  # Global outlier effects
  tso_vars <- grep(
    "^(AO|TC)",
    names(ts_data),
    value = TRUE
  )

  # Start with the break-adjusted specification when breaks exist.
  final_formula <- if (!is.null(final_breaks)) {
    .add_segments(break_detect_formula)
  } else {
    baseline_formula
  }

  # Add global AO/TC effects and retained segment-specific effects.
  all_effect_vars <- unique(
    c(tso_vars, segment_effect_vars)
  )

  if (length(all_effect_vars) > 0) {
    final_formula <- stats::update(
      final_formula,
      paste(
        ". ~ . +",
        paste(all_effect_vars, collapse = " + ")
      )
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

    # A single series has one row per index value, so the index alone
    # identifies rows. Key columns stay as they are in `original_ts`.
    join_cols <- tsibble::index_var(original_ts)
    covariate_names <- setdiff(names(ts_data), join_cols)

    if (length(covariate_names) > 0) {
      overlapping_covariates <- intersect(
        names(original_ts),
        covariate_names
      )

      if (length(overlapping_covariates) > 0) {
        original_ts <- dplyr::select(
          original_ts,
          -dplyr::all_of(overlapping_covariates)
        )
      }

      covariates <- ts_data[, c(join_cols, covariate_names), with = FALSE]
      all_ts <- dplyr::left_join(original_ts, covariates, by = join_cols)
    } else {
      all_ts <- original_ts
    }
  } else {
    all_ts <- ts_data

    # Restore set-aside columns by position: rows are never reordered
    for (v in names(set_aside)) {
      all_ts[[v]] <- set_aside[[v]]
    }
    if (length(set_aside) > 0) {
      data.table::setcolorder(
        all_ts,
        intersect(names(original_ts), names(all_ts))
      )
    }
  }

  p(sprintf("Completed time series %s", id))

  return(list(
    outliers = outliers_entry,
    list_of_ts = all_ts,
    formula = final_formula,
    id = id
  ))
}

#' Handle tsoutliers' warning about zeroed start-up residuals
#'
#' `tsoutliers::tso()` warns "the first n residuals were set to zero" when
#' residuals at the start of the ARIMA fit are extreme. Those observations
#' then cannot be flagged as outliers. The warning does not name the series,
#' so it is replaced here by a single message that does. With `id = NULL`
#' the warning is silenced. Other warnings are passed through unchanged.
#'
#' @param expr Expression that calls `detect_outliers()`.
#' @param id Series identifier used in the message, or `NULL` to silence.
#'
#' @return The value of `expr`.
#'
#' @noRd
.handle_zeroed_residuals <- function(expr, id = NULL) {
  zeroed <- NULL

  result <- withCallingHandlers(
    expr,
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("residuals were set to zero", msg, fixed = TRUE)) {
        zeroed <<- msg
        invokeRestart("muffleWarning")
      }
    }
  )

  if (!is.null(id) && !is.null(zeroed)) {
    message(
      "Series ",
      id,
      ": ",
      zeroed,
      "; outliers at the start of the series cannot be detected."
    )
  }

  result
}
