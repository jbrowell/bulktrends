#' Select the best regression model for a time series
#'
#' This function evaluates a set of candidate regression formulas containing
#' trend and seasonal regressors and selects the model with the lowest value
#' of the specified selection metric (for example AIC or BIC).
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param date_col Name of column containing timestamps. Default `"DATE_START"`.
#' @param formulas A list of formulas specifying candidate models. When `NULL`,
#'  default formulas are generated based on `freq`. Available covariates for
#'  monthly data are `linear_trend`, `annual_sin`, and `annual_cos`; for daily
#'  data `day_of_week` and `is_holiday` are also available.
#' @param response_col If `formulas` not provided, default formulas will be used
#' but the column containing the response variable must be specified.
#' @param metric  A function used to evaluate model fit, e.g. [AIC] or [BIC]. Default `AIC`.
#' @param break_detection If `TRUE`, structural break detection is
#' performed and break-adjusted models are also evaluated.
#' @param freq Frequency of the input time series. If NULL it will be auto-detected ('day', 'week', or 'month').
#'
#' @return A list containing (i) `data`, the time series' data with any generated regressors
#' and structural break segment variables, (ii)  `formula`, the optimal selected model formula, and
#' (iii)  `break_entry`, a `data.table` describing any detected structural breaks
#' or `NULL` if no breaks were found.
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
        as.formula(paste(response_col, "~", deparse(f[[2]])))
      })

      data <- add_date_features(data, date_col, freq = "month")
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
        as.formula(paste(response_col, "~", deparse(f[[2]])))
      })

      data <- add_date_features(data, date_col, freq = "day")
    } else {
      stop("\"formulas=NULL\" and data isn't daily or monthly.")
    }
  }

  output <- list(data = data, formula = NULL)

  current_metric <- Inf

  # base models
  for (i in seq_along(formulas)) {
    model_fit <- try(
      lm(formula = formulas[[i]], data = data),
      silent = T
    )

    if ("try-error" %in% class(model_fit)) {
      warning("Model failed: ", deparse(formulas[[i]]), "\n")
      next
    }
    new_metric <- metric(model_fit)
    if (new_metric < current_metric) {
      current_metric <- new_metric
      output <- list(data = data, formula = formulas[[i]])
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
        warning("Breaks detection failed: ", deparse(formulas[[i]]), "\n")
        next
      }
      if (is.null(breaks)) {
        next
      }

      segments <- breaks$segments
      fmla <- update(formulas[[i]], . ~ . * segments)

      model_fit <- try(
        lm(formula = fmla, data = cbind(data, segments)),
        silent = T
      )

      if ("try-error" %in% class(model_fit)) {
        warning("Model failed: ", deparse(fmla), "\n")
        next
      }
      new_metric <- metric(model_fit)
      if (new_metric < current_metric) {
        current_metric <- new_metric
        output <- list(
          data = cbind(data, segments),
          formula = fmla,
          break_entry = breaks$break_entry
        )
      }
    }
  }

  if (is.null(output$formula)) {
    warning("No valid model found. Returning original data with NULL formula.")
  }

  return(output)
}
