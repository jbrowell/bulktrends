#' Large-scale anomaly detection
#'
#' This function loops over a list of commodity codes, selects a regression model,
#' and detects anomalies.
#'
#' @param import_data A `data.table` containing trade data. Must include columns `COMCODE` and specified `quantity`.
#' @param codes A vector of HS2/HS4/HS6/CN8 codes
#' @param quantity Quantity to be analysed, e.g. `NET_MASS`, `STAT_VALUE` or `volume`.
#' @param date_col Name of column containing timestamps.
#' @param model_selection_metric Selection criteria passed to `select_best_model()`
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`. Default `FALSE`.
#' @param freq See `?extract_ts()`
#' @param verbose If `TRUE`, progress messages are displayed. Default `FALSE`.
#' @param ... Additional arguments passed to `tso()`
#'
#' @return A list containing a 1. table of detected anomalies and 2. a list
#' of time series data with regressors, including outlier effects.
#'
#' @export
detect_anomalies <- function(
  import_data,
  codes,
  quantity = "NET_MASS",
  date_col = "DATE_START",
  model_selection_metric = AIC,
  scale_ts = FALSE,
  freq = NULL,
  verbose = FALSE,
  ...
) {
  ts_prep <- list()
  for (code in codes) {
    ts_data <- tryCatch(
      extract_ts(
        import_data,
        code = code,
        date_col = date_col,
        quantity = quantity,
        fill_missing = 0,
        freq = freq
      ),
      error = function(e) e
    )

    if (inherits(ts_data, "error")) {
      message(paste("Skipping code", code, ":", ts_data))
      next
    } else {
      ts_prep[[code]] <- ts_data
    }
  }

  rm(ts_data)
  codes <- names(ts_prep)
  p <- progressr::progressor(along = codes)

  process_ts <- function(ts_data, code) {
    sparse_rate <- mean(ts_data[[quantity]] == 0, na.rm = T)

    if (!is.na(sparse_rate) && sparse_rate > 0.4) {
      message(paste(
        "Skipping code",
        code,
        ":",
        round(sparse_rate * 100, 2),
        "% zeros"
      ))
      p(sprintf("Skipped %s", code))
      return(NULL)
    }

    if (verbose) {
      message(sprintf("Running selected_model for %s", code))
    }

    selected_model <- tryCatch(
      select_best_model(
        data = ts_data,
        response_col = quantity,
        date_col = date_col,
        metric = model_selection_metric,
        break_detection = TRUE
      ),
      error = function(e) e
    )

    if (inherits(selected_model, "error")) {
      message(paste("Skipping code", code, ":", selected_model))
      p(sprintf("Skipped %s", code))
      return(NULL)
    }

    if (verbose) {
      message(sprintf("Running detect_anomaly for %s", code))
    }

    ts_data <- selected_model$data
    break_entry <- selected_model$break_entry
    xreg_all <- model.matrix(selected_model$formula, data = ts_data)

    #for ~ -1 models
    if (ncol(xreg_all) == 0) {
      xreg_all <- NULL
    }

    detect_anomaly <- detect_outliers(
      data = ts_data,
      quantity = "NET_MASS",
      types = c("AO", "TC", "IO"),
      scale_ts = scale_ts,
      xreg = xreg_all
    )

    ts_data <- detect_anomaly$data

    if (
      !is.null(detect_anomaly$outliers) && nrow(detect_anomaly$outliers) > 0
    ) {
      #store tso outliers data produced
      new_outliers <- as.data.table(detect_anomaly$outliers)
      new_outliers[, code := code]
      new_outliers[,
        model_formula := paste(deparse(selected_model$formula), collapse = " ")
      ]

      new_outliers[, time := ts_data$DATE_START[ind]]
      new_outliers[, anomaly_type := "Outlier"]

      outliers_entry <- new_outliers
    } else {
      outliers_entry <- data.table()
      # code = code,
      # model_formula = deparse(selected_model$formula)
      #)
    }

    #add breaks table
    if (!is.null(break_entry) && nrow(break_entry) > 0) {
      break_entry[, code := code]
      break_entry[,
        model_formula := paste(deparse(selected_model$formula), collapse = " ")
      ]
      break_entry[, anomaly_type := "Break"]

      outliers_entry <- rbindlist(
        list(outliers_entry, break_entry),
        fill = TRUE
      )

      setorder(outliers_entry, time)
    } else {
      break_entry <- data.table()
    }

    #no break no outlier option
    if (nrow(outliers_entry) == 0) {
      outliers_entry <- data.table(
        code = code,
        model_formula = paste(deparse(selected_model$formula), collapse = " "),
        anomaly_type = "None"
      )
    }

    p(sprintf("Completed code %s", code))
    return(list(outliers = outliers_entry, list_of_ts = ts_data, code = code))
  }

  results <- future.apply::future_mapply(
    process_ts,
    ts_prep,
    codes,
    SIMPLIFY = FALSE,
    future.globals = list(),
    future.packages = c("bulktrends")
  )

  results <- Filter(Negate(is.null), results)
  all_outliers <- lapply(results, `[[`, "outliers")
  list_of_ts <- lapply(results, `[[`, "list_of_ts")
  names(list_of_ts) <- lapply(results, `[[`, "code")
  return(list(
    outliers = rbindlist(all_outliers, fill = TRUE),
    list_of_ts = list_of_ts
  ))
}
