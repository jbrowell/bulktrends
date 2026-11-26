#' Select the best regression model for a time series
#'
#' Fits a set of candidate regression models with [lm()] and selects the one
#' with the lowest value of `metric`. When `break_method` is given, each
#' candidate is also refitted with structural breaks, and these models compete
#' with the candidates without breaks.
#'
#' @param data A data frame or `data.table` containing the response and the
#'   date column. The date column must be of class `Date`.
#' @param date_col Name of the column containing timestamps. Default
#'   `"DATE_START"`.
#' @param formulas A list of candidate formulas. If `NULL` (the default),
#'   standard candidates are used and the date features they need are added
#'   with [add_date_features()]; see Details. If you supply your own
#'   formulas, date features are not added, so `data` must already contain
#'   every column the formulas use.
#' @param response_col Name of the response column. Required if `formulas`
#'   is `NULL`; otherwise it is taken from the first formula.
#' @param metric A function used to compare fitted models, e.g. [AIC] or
#'   [BIC]. Default `AIC`.
#' @param break_method Name of the structural break detection method, either
#'   `"breakpoints"` or `"cpt.reg"`, or `NULL` (the default) for no break
#'   detection. See [detect_breaks()].
#' @param break_params A named list of additional arguments passed to the
#'   selected break detection function. Default `list()`.
#' @param freq Frequency of the series: `"day"`, `"week"` or `"month"`. If
#'   `NULL` (the default), it is detected with [detect_date_frequency()].
#'   Only used when `formulas` is `NULL`. Standard candidates exist only for
#'   daily and monthly data, so weekly data with `formulas = NULL` gives an
#'   error.
#'
#' @details
#' The standard candidates have no regressors (`~ -1`), an intercept only
#' (`~ 1`), a linear trend (`~ linear_trend`), annual seasonality
#' (`~ annual_sin + annual_cos`), and trend plus seasonality. For daily data,
#' two further candidates add `day_of_week`, then `day_of_week` and
#' `is_holiday`.
#'
#' With break detection, [detect_breaks()] is run for each candidate. If
#' breaks are found, the candidate is refitted with every term interacted
#' with the `segments` factor (`formula * segments`), or with one mean per
#' segment (`~ segments`) for an intercept-only candidate.
#'
#' A warning is given for each candidate that cannot be fitted, and for each
#' candidate where break detection fails.
#'
#' @return A list with elements:
#'
#' * `data`: `data`, as a `data.table`, with any generated date features and,
#'   if a model with breaks was selected, the `segments` column.
#' * `formula`: the selected formula, or `NULL` (with a warning) if no
#'   candidate could be fitted.
#' * `baseline_formula`: the best candidate without breaks.
#' * `break_detection_formula`: only present if a model with breaks was
#'   selected; the candidate from which it was built.
#' * `break_entry`: only present if a model with breaks was selected; the
#'   `break_entry` table from [detect_breaks()].
#'
#' @export
select_best_model <- function(
  data,
  date_col = "DATE_START",
  formulas = NULL,
  response_col = NULL,
  metric = AIC,
  break_method = NULL,
  break_params = list(),
  freq = NULL
) {
  if (!inherits(data, "data.table")) {
    data <- data.table::as.data.table(data)
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

  output <- list(data = data, formula = NULL, baseline_formula = NULL)

  current_metric <- Inf

  # base models
  for (i in seq_along(formulas)) {
    model_fit <- try(
      lm(formula = formulas[[i]], data = data),
      silent = TRUE
    )

    if ("try-error" %in% class(model_fit)) {
      warning("Model failed: ", deparse(formulas[[i]]), "\n")
      next
    }

    new_metric <- metric(model_fit)

    if (new_metric < current_metric) {
      current_metric <- new_metric
      output <- list(
        data = data,
        formula = formulas[[i]],
        baseline_formula = formulas[[i]]
      )
    }
  }

  # detect_breaks loop
  if (!is.null(break_method)) {
    for (i in seq_along(formulas)) {
      breaks <- try(
        detect_breaks(
          data = data,
          formula = formulas[[i]],
          date_col = date_col,
          break_method = break_method,
          break_params = break_params
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
      fmla <- .add_segments(formulas[[i]])

      model_fit <- try(
        lm(formula = fmla, data = cbind(data, segments)),
        silent = TRUE
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
          baseline_formula = output$baseline_formula,
          break_detection_formula = formulas[[i]],
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
