#' Utility helper functions
NULL
#' Extract of monthly time series of "NET_MASS" for a given commodity code
#'
#' This function extracts the monthly sum "NET_MASS" from import_data for
#' any level of hierarchy (HS2, HS4, HS6 and CN8) found in the dataset.
#'
#' @param import_data A `data.table` containing trade data. Must include "COMCODE".
#' @param code A character string representing any HS2/HS4/HS6/CN8 code.
#'
#' @return Time series object of NET_MASS
#'
#' @export
extract_netmass_ts <- function (import_data,
                                code) {

  all_months <- unique(import_data$month)
  import_data <- import_data[substr(COMCODE, 1, nchar(code)) == code]
  #import_data[,COMCODE:=substr(COMCODE,1,nchar(code))]
  #import_data <- import_data[COMCODE==code]

  missing_months <- all_months[!all_months %in% import_data$month]
  if(length(missing_months) > 0) {
    message("Missing months detected: ", paste(missing_months))}
  else {
    message("All months present")}

  import_data[, NET_MASS := as.numeric(NET_MASS)]
  import_data <-  import_data[, .(NET_MASS = sum(NET_MASS, na.rm = T)), by=month]

  cat("NA after sum: ", sum(is.na(import_data$NET_MASS)), "\n")

  first_month <- import_data[, min(month)]
  ts_data <- ts(import_data$NET_MASS, start = c(year(first_month), month(first_month)), frequency = 12)

  return(ts_data)
}

#'
#' Extract suitable model matrix for a given commodity code
#'
#' This function evaluates a set of formulas with trend and seasonal terms
#' as exogenous variables and selects the ARIMA model with the lowest AICc.
#'
#' @param data A time series object containing the dependent variable.
#'
#' @return A model matrix of the trend and seasonal regressors of the selected model.
#'
#' @export
identify_best_model <- function (data){

  trend = 1:length(data)
  month = rep(1:12, length.out = length(data))
  formulas <- list(~ trend,
                   ~ sin(2*pi*month/12) + cos(2*pi*month/12),
                   ~ trend + sin(2*pi*month/12) + cos(2*pi*month/12),
                   ~ trend + sin(4*pi*month/12) + cos(4*pi*month/12))
  model<- list()
  aicc_values <- rep(Inf, length(formulas))

  for (i in seq_along(formulas)) {
    X <- model.matrix(formulas[[i]], data = data.frame(trend, month))
    #print(X)
    model <- try(auto.arima(data,
                            xreg = X,
                            max.p = 5,
                            max.q = 5,
                            seasonal = FALSE), silent=T)

    if ("try-error" %in% class(model)){
      cat("Model", i, "failed\n")
      next}

    model[[i]] <- model
    aicc_values[i] <- model$aicc
  }
  print(aicc_values)
  best_aic <- which.min(aicc_values)
  return(model.matrix(formulas[[best_aic]], data = data))
}
