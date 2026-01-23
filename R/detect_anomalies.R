#' Large scale anomaly detection
#'
#' This function loops over a list of commodity codes, selects a regression model,
#' and detects anomalies.
#'
#' @param import_data A `data.table` containing trade data. Must include columns "COMCODE", "month" and `quantity`.
#' @param codes A vector of HS2/HS4/HS6/CN8 codes
#' @param quantity Quantity to be analysed, e.g. "NET_MASS" or "STAT_VALUE".
#' @param model_selection_metric Selection criteria passed to `select_best_model()`
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`
#' @param ... Additional arguments passed to `tso()`
#'
#' @returns A table of detected outliers.
#'
#' @export
detect_anomalies <- function(
    import_data,
    codes,
    quantity = "NET_MASS",
    model_selection_metric = "aic",
    scale_ts = TRUE,
    ...
){

  all_outliers <- list()

  for (i in seq_along(codes)){
    #create time series
    ts_data <- extract_ts(import_data, code = codes[i], quantity = quantity)

    selected_model <- select_best_model(ts_data,
                                        metric = model_selection_metric,
                                        scale_ts = scale_ts)

    detect_anomaly <- try(tso(y = if(scale_ts){scale(ts_data)}else{ts_data},
                              xreg = selected_model$xreg,
                              ...),
                          silent=T)

    if ("try-error" %in% class(detect_anomaly)){
      warning("Anomaly detectino failed for code: ", codes[i])
      next
    }

    if (nrow(detect_anomaly$outliers)>0){

      #store outliers data produced
      outliers_dt <- as.data.table(detect_anomaly$outliers)
      outliers_dt[, code := codes[i]]
      outliers_dt[, model_formula := selected_model$formula]
      all_outliers[[i]] <- outliers_dt
    } else {
      all_outliers[[i]] <- data.table(code = codes[i],
                                      model_formula = selected_model$formula)
    }

  }

  return(rbindlist(all_outliers, fill = TRUE))

}
