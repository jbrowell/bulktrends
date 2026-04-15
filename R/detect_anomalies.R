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
#' @param ... Additional arguments passed to `tso()`
#'
#' @returns A table of detected outliers.
#'
#' @export
detect_anomalies <- function(
    import_data,
    codes,
    quantity = "NET_MASS",
    date_col = "DATE_START",
    model_selection_metric = "aic",
    scale_ts = FALSE,
    freq = NULL,
    ...
){

  all_outliers <- list()
  tso_output <- list()

  ts_prep <- list()
  for (i in seq_along(codes)){

    ts_prep[[i]] <- tryCatch(extract_ts(import_data,
                          code = codes[i],
                          date_col = date_col,
                          quantity = quantity,
                          fill_missing = 0,
                          freq = freq),
                          error = function(e) e)

    if(inherits(ts_prep, "error")){
      message(paste("Skipping code", code, ":", ts_prep))
      next
    }
  }

  process_ts <- function(ts_data, code) {

    selected_model <- tryCatch(select_best_model(data = ts_data,
                                        response_col = quantity,
                                        date_col = date_col,
                                        metric = model_selection_metric,
                                        scale_ts = scale_ts),
                               error = function(e) e)

    if(inherits(selected_model, "error")){
      message(paste("Skipping code", code, ":", selected_model))
      return(NULL)
    }

    detect_anomaly <- tryCatch(
      tso(
        y = if(scale_ts){
          as.ts(scale(ts_data[[quantity]]))
        } else {
          as.ts(ts_data[[quantity]])
        },
        xreg = if(ncol(selected_model$xreg)>0) {
          selected_model$xreg
        } else {NULL},
        ...),
      error = function(e) e)

    if(inherits(detect_anomaly, "error")){
      message(paste("Skipping code", code, ":", detect_anomaly))
      return(NULL)
    }

    #storing all tso output
    tso_entry <- setNames(
      list(list(tso=detect_anomaly,
                y=if(scale_ts){as.ts(scale(ts_data[[quantity]]))} else{as.ts(ts_data[[quantity]])},
                time_index=ts_data[[date_col]])),
      code
    )

    if (nrow(detect_anomaly$outliers)>0){

      #store outliers data produced
      new_outliers <- as.data.table(detect_anomaly$outliers)
      new_outliers[, code := code]
      new_outliers[, model_formula := paste(deparse(selected_model$formula), collapse = " ")]

      new_outliers[, time := ts_data$DATE_START[ind]]

      outliers_entry <- new_outliers
    } else {
      outliers_entry <- data.table(code = code,
                                   model_formula = deparse(selected_model$formula))
    }

    list(outliers = outliers_entry, tso = tso_entry)
  }

  results <- future.apply::future_mapply(process_ts, ts_prep, codes, SIMPLIFY = FALSE)
  results <- Filter(Negate(is.null), results)

  all_outliers <- lapply(results, `[[`, "outliers")
  tso_output <- do.call(c, lapply(results, `[[`, "tso"))

  return(list(outliers=rbindlist(all_outliers, fill = TRUE),
         tso_output=tso_output))

}
