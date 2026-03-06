#' Extract suitable model matrix for a given commodity code
#'
#' This function evaluates a set of formulas with linear_trend and seasonal terms
#' as exogenous variables and selects the ARIMA model with the lowest value
#' of the selected metric.
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param date_col Name of column containing timestamps.
#' @param formulas A list of formulas specifying candidate models. Covariates
#' available are `linear_trend` and `month`.
#' @param response_col If `formulas` not provided, default formulas will be used
#' but the column containing the response variable must be specified.
#' @param metric A character string specifying the criteria for model
#' selection. Examples are "aic","aicc" or "bic".
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`. Default `FALSE`.
#' @param freq
#'
#' @returns A model matrix of the linear_trend and seasonal regressors of the selected
#' model and the related model formula.
#'
#' @export
select_best_model <- function (
    data,
    date_col = "DATE_START",
    formulas = NULL,
    response_col = NULL,
    metric = "aic",
    scale_ts = FALSE,
    freq = NULL
){

  if(is.null(formulas) & is.null(response_col)) {
    stop("Must supply either formulas or response_col.")
  }

  if(is.null(response_col)){
    f <- formulas[[1]]
    response_col <- all.vars(f)[attr(terms(f), "response")]
  }

  # Check completeness!


  if(is.null(formulas)) {

    if(is.null(freq)){
      freq <- detect_date_frequency(data[[date_col]])
    }

    if(freq=="month"){

      formulas <- list(~ -1,
                       ~ 1,
                       ~ linear_trend,
                       ~ annual_sin + annual_cos,
                       ~ linear_trend + annual_sin + annual_cos)

      formulas <- lapply(formulas, function(f) {
        rhs <- f[[2]]
        as.formula(paste(response_col, "~", deparse(rhs)))
      })

      data[, linear_trend := .I]
      data[, day_of_year := as.integer(format(data[[date_col]],"%j"))]
      data[, annual_sin := sin(2*pi*day_of_year/365)]
      data[, annual_cos := cos(2*pi*day_of_year/365)]


    } else if(detect_date_frequency(data[[date_col]])=="day"){

      formulas <- list(~ -1,
                       ~ 1,
                       ~ linear_trend,
                       ~ annual_sin + annual_cos,
                       ~ linear_trend + annual_sin + annual_cos,
                       ~ linear_trend + annual_sin + annual_cos + day_of_week,
                       ~ linear_trend + annual_sin + annual_cos + day_of_week + is_uk_holiday)

      formulas <- lapply(formulas, function(f) {
        rhs <- f[[2]]
        as.formula(paste(response_col, "~", deparse(rhs)))
      })

      data[, linear_trend := .I]
      data <- add_date_features(data,date_col)

    } else {
      stop("\"formulas=NULL\" and data isn't daily or monthly.")
    }
  }

  model <- list()
  metric_values <- rep(Inf, length(formulas))

  for (i in seq_along(formulas)) {

    is_empty_model <- deparse(formulas[[i]][[3]])=="-1"

    if( !is_empty_model ) {
      X <- model.matrix(formulas[[i]], data = data)
    }

    model_fit <- try(
      forecast::auto.arima(
        if(scale_ts){scale(data[[response_col]])}else{data[[response_col]]},
        xreg = if(is_empty_model){NULL}else{X},
        max.p = 5,
        max.d = 1,
        max.q = 5,
        seasonal = F,
        allowmean = F),
      silent=T)

    if ("try-error" %in% class(model_fit)){
      warning("Model failed: ", deparse(formulas[[i]]),"\n")
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
    formula = formulas[[best_metric]])
  )
}
