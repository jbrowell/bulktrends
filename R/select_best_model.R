#'
#' Extract suitable model matrix for a given commodity code
#'
#' This function evaluates a set of formulas with linear_trend and seasonal terms
#' as exogenous variables and selects the ARIMA model with the lowest value
#' of the selected metric.
#'
#' @param data A time series object containing the dependent variable.
#' @param metric A character string specifying the criteria for model
#' selection. Examples are "aic","aicc" or "bic".
#' @param formulas A list of formulas specifying candidate models. Covariates available are `linear_trend` and `month`.
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`
#'
#' @returns A model matrix of the linear_trend and seasonal regressors of the selected
#' model and the related model formula.
#'
#' @export
select_best_model <- function (
    data,
    metric = "aic",
    formulas = list(~ -1,
                    ~ 1,
                    ~ linear_trend,
                    ~ sin(2*pi*month/12) + cos(2*pi*month/12),
                    ~ linear_trend + sin(2*pi*month/12) + cos(2*pi*month/12)),
    scale_ts = TRUE
){

  linear_trend <- time(data)
  month = cycle(data)

  model <- list()
  metric_values <- rep(Inf, length(formulas))

  for (i in seq_along(formulas)) {

    if( !formulas[[i]]==formula(~-1) ) {
      X <- model.matrix(formulas[[i]], data = data.frame(linear_trend, month))
    }

    model_fit <- try(
      forecast::auto.arima(
        if(scale_ts){scale(data)}else{data},
        xreg = if(!formulas[[i]]==formula(~-1)){X}else{NULL},
        max.p = 5,
        max.d = 1,
        max.q = 5,
        seasonal = F,
        allowmean = F),
      silent=T)

    if ("try-error" %in% class(model_fit)){
      warning("Model failed: ", formulas[[i]])
      next
    } else {
      model[[i]] <- model_fit
      metric_values[i] <- model_fit[[metric]]
    }
  }

  best_metric <- which.min(metric_values)
  return( list(
    xreg = if( formulas[[best_metric]]==formula(~-1) ){NULL}else{
      model.matrix(formulas[[best_metric]], data = data)},
    formula = paste0(formulas[[best_metric]],collapse = ""))
  )
}
