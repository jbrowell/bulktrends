#'
#' Extract suitable model matrix for a given commodity code
#'
#' This function evaluates a set of formulas with trend and seasonal terms
#' as exogenous variables and selects the ARIMA model with the lowest value
#'of the selected metric.
#'
#' @param data A time series object containing the dependent variable.
#' @param metric A character string specifying the criteria for model
#' selection. Examples are "aic","aicc" or "bic".
#'
#' @return A model matrix of the trend and seasonal regressors of the selected
#' model and the related model formula.
#'
#' @export
select_best_model <- function (data, metric){

  # CHECK if data will have gaps. If so, this would be incorrect
  # time(ts_data)
  trend = (time(ts_data) - 2016) * 12 #1:length(data)
  month = ((time(ts_data) - min(time(ts_data)))*12) %% 12 + 1 #rep(1:12, length.out = length(data))
  # <><><><><><>

  formulas <- list(~ trend,
                   ~ sin(2*pi*month/12) + cos(2*pi*month/12),
                   ~ trend + sin(2*pi*month/12) + cos(2*pi*month/12),
                   ~ trend + sin(4*pi*month/12) + cos(4*pi*month/12))
  model<- list()
  metric_values <- rep(Inf, length(formulas))

  for (i in seq_along(formulas)) {
    X <- model.matrix(formulas[[i]], data = data.frame(trend, month))
    #print(X)
    model <- try(auto.arima(data,
                            xreg = X,
                            max.p = 5,
                            max.d = 1,
                            max.q = 5,
                            seasonal = F), silent=T)

    if ("try-error" %in% class(model)){
      cat("Model", i, "failed\n")
      next
    }

    model[[i]] <- model
    metric_values[i] <- model[[metric]]
  }
  # print(metric_values)
  best_metric <- which.min(metric_values)
  return(
    list(xreg = model.matrix(formulas[[best_metric]], data = data),
         formula = paste0(formulas[[best_metric]],collapse = ""))
    )
}
