#' Large scale anomaly detection
#'
#' This function loops over a list of commodity codes, selects a regression model,
#' and detects anomalies.
#'
#' @param import_data A `data.table` containing trade data. Must include columns "COMCODE" and `quantity`.
#' @param codes A vector of HS2/HS4/HS6/CN8 codes
#' @param quantity Quantity to be analysed, e.g. "NET_MASS" or "STAT_VALUE".
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

  for (i in seq_along(codes)){
    #create time series
    ts_data <- extract_ts(import_data,
                          code = codes[i],
                          date_col = date_col,
                          quantity = quantity,
                          fill_missing = 0,
                          freq = freq)

    selected_model <- select_best_model(data = ts_data,
                                        response_col = quantity,
                                        date_col = date_col,
                                        metric = model_selection_metric,
                                        scale_ts = scale_ts)

    detect_anomaly <- try(
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
      silent=T)

    if ("try-error" %in% class(detect_anomaly)){
      warning("Anomaly detection failed for code: ", codes[i])
      next
    }

    if (nrow(detect_anomaly$outliers)>0){

      #store outliers data produced
      new_outliers <- as.data.table(detect_anomaly$outliers)
      new_outliers[, code := codes[i]]
      new_outliers[, model_formula := deparse(selected_model$formula)]

      new_outliers[, time := ts_data$DATE_START[ind]]

      all_outliers[[i]] <- new_outliers
    } else {
      all_outliers[[i]] <- data.table(code = codes[i],
                                      model_formula = deparse(selected_model$formula))
    }

  }

  return(rbindlist(all_outliers, fill = TRUE))

}
