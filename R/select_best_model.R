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
#' @param freq Frequency of the input time series. If NULL it will be auto-detected ('day', 'week', or 'month').
#'
#' @return A model matrix of the linear_trend and seasonal regressors of the selected
#' model and the related model formula.
#'
#' @export
select_best_model <- function(
  data,
  date_col = "DATE_START",
  formulas = NULL,
  response_col = NULL,
  metric = AIC,
  break_detection = TRUE,
  freq = NULL
) {
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }

  if (is.null(formulas) & is.null(response_col)) {
    stop("Must supply either formulas or response_col.")
  }

  if (is.null(response_col)) {
    f <- formulas[[1]]
    response_col <- all.vars(f)[attr(terms(f), "response")]
  }

  if (is.null(formulas)) {
    if (is.null(freq)) {
      freq <- detect_date_frequency(data[[date_col]])
    }

    if (freq == "month") {
      formulas <- list(
        ~ -1,
        ~1,
        ~linear_trend,
        ~ annual_sin + annual_cos,
        ~ linear_trend + annual_sin + annual_cos
      )

      formulas <- lapply(formulas, function(f) {
        rhs <- f[[2]]
        as.formula(paste(response_col, "~", deparse(rhs)))
      })

      data[, linear_trend := .I]
      data[, day_of_year := as.integer(format(data[[date_col]], "%j"))]
      data[, annual_sin := sin(2 * pi * day_of_year / 365)]
      data[, annual_cos := cos(2 * pi * day_of_year / 365)]
    } else if (freq == "day") {
      formulas <- list(
        ~ -1,
        ~1,
        ~linear_trend,
        ~ annual_sin + annual_cos,
        ~ linear_trend + annual_sin + annual_cos,
        ~ linear_trend + annual_sin + annual_cos + day_of_week,
        ~ linear_trend + annual_sin + annual_cos + day_of_week + is_holiday
      )

      formulas <- lapply(formulas, function(f) {
        rhs <- f[[2]]
        as.formula(paste(response_col, "~", deparse(rhs)))
      })

      data[, linear_trend := .I]
      data <- add_date_features(data, date_col)
    } else {
      stop("\"formulas=NULL\" and data isn't daily or monthly.")
    }
  }

  model <- list()
  output <- list()
  metric_values <- rep(Inf, length(formulas))
  current_metric <- Inf

  #base models
  for (i in seq_along(formulas)) {
    model_fit <- try(
      lm(formula = formulas[[i]], data = data),
      silent = T
    )

    if ("try-error" %in% class(model_fit)) {
      warning("Model failed: ", deparse(formulas[[i]]), "\n")
      next
    } else {
      model[[i]] <- model_fit
      metric_values[i] <- metric(model_fit)
      if (metric_values[i] < current_metric) {
        current_metric <- metric_values[i]
        output <- list(data = data, formula = formulas[[i]])
      }
    }
  }

  # detect_breaks loop
  if (break_detection) {
    for (i in seq_along(formulas)) {
      breaks <- try(
        detect_breaks(
          data = data,
          date_col = date_col,
          formula = formulas[[i]]
        ),
        silent = TRUE
      )
      if ("try-error" %in% class(breaks)) {
        warning("Breaks detection failed", "\n")
        next
      }
      if (is.null(breaks)) {
        next
      }

      segments <- breaks$segments
      break_entry <- breaks$break_entry
      fmla <- update(formulas[[i]], . ~ . * segments)

      model_fit <- try(
        lm(formula = fmla, data = cbind(data, segments)),
        silent = T
      )

      if ("try-error" %in% class(model_fit)) {
        warning("Model failed: ", deparse(fmla), "\n")
        next
      } else {
        if (metric(model_fit) < current_metric) {
          # current_metric <- model_fit[[metric]]
          current_metric <- metric(model_fit)
          output = list(
            data = cbind(data, segments),
            formula = fmla,
            break_entry = breaks$break_entry
          )
        }
      }
    }
  }
  return(output)
}
