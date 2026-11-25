#' Detect structural breaks in a time series
#'
#' Identifies structural breaks in the regression relationship given by
#' `formula`, using either the Bai-Perron method or changepoint regression
#' (PELT or AMOC).
#'
#' @param data A data frame or `data.table` containing the response, the
#'   regressors used in `formula`, and the date column.
#' @param formula A formula specifying the regression model whose
#'   coefficients may change at a break, e.g. `NET_MASS ~ 1` for changes in
#'   mean, or `NET_MASS ~ linear_trend` for changes in level and trend.
#' @param break_method Name of the break detection method, either
#'   `"breakpoints"` or `"cpt.reg"`. Required.
#' @param break_params A named list of additional arguments passed to the
#'   selected break detection function. For `"cpt.reg"`, this can include
#'   `method` (`"PELT"` or `"AMOC"`) and `minseglen`. Default `list()`.
#' @param date_col Name of the column containing timestamps. Default
#'   `"DATE_START"`.
#'
#' @return `NULL` if no breaks are detected; otherwise a list with elements:
#'
#' * `break_entry`: a `data.table` with one row per break and columns `type`
#'   (always `"SB"`), `ind` and `time`. Following the convention of
#'   [strucchangeRcpp::breakpoints()] and [changepoint::cpt.reg()], `ind` and
#'   `time` give the last observation of the segment before the break.
#' * `segments`: a `data.table` with a single factor column, `segments`,
#'   giving the segment of each observation. The levels are `"segment1"`,
#'   `"segment2"`, ... for `"breakpoints"` and `"1"`, `"2"`, ... for
#'   `"cpt.reg"`.
#'
#' @details
#' A formula with no terms at all (e.g. `y ~ -1`) has no coefficients that
#' could change, so `NULL` is returned.
#'
#' Method `"breakpoints"` uses [strucchangeRcpp::breakpoints()] with
#' `breaks = "BIC"` and `het.err = FALSE`.
#'
#' Method `"cpt.reg"` uses [changepoint::cpt.reg()], with PELT by default.
#' It always fits a mean term: constant and collinear regressors are removed
#' and a single intercept is included, even if `formula` excludes one. The
#' default `minseglen` is `max(15, 3 * k)`, where `k` is the number of
#' regressors other than the intercept. A break at the last observation is
#' ignored.
#'
#' @export
detect_breaks <- function(
  data,
  formula,
  break_method,
  break_params = list(),
  date_col = "DATE_START"
) {
  # A formula with no regressors and no intercept (e.g. `y ~ -1`) has no
  # parameters that could change, so there is nothing to detect
  if (ncol(stats::model.matrix(formula, data = data)) == 0) {
    return(NULL)
  }

  if (break_method == "breakpoints") {
    bp <- do.call(
      strucchangeRcpp::breakpoints,
      c(
        list(
          formula = formula,
          data = data,
          breaks = "BIC",
          het.err = FALSE
        ),
        break_params
      )
    )

    breaks <- bp$breakpoints

    if (length(breaks) > 0 && !all(is.na(breaks))) {
      time <- data[[date_col]][breaks]

      output <- data.table::data.table(
        type = "SB",
        ind = breaks,
        time = time
      )

      segments <- data.table::data.table(
        segments = strucchangeRcpp::breakfactor(bp)
      )

      segments[, segments := as.factor(segments)]

      return(list(
        break_entry = output,
        segments = segments
      ))
    } else {
      return(NULL)
    }
  } else if (break_method == "cpt.reg") {
    .detect_breaks_cpt_reg(
      data = data,
      formula = formula,
      date_col = date_col,
      break_params = break_params
    )
  } else {
    stop(
      "break_method not recognised. Must be one of ",
      "\"breakpoints\" or \"cpt.reg\"."
    )
  }
}


.detect_breaks_cpt_reg <- function(
  data,
  formula,
  date_col = "DATE_START",
  break_params = list()
) {
  # Prepare data
  data <- data.table::as.data.table(data)

  response <- data[[all.vars(formula)[1]]]
  xreg <- model.matrix(formula, data = data)

  # Precaution: remove the response column if present
  if (all.vars(formula)[1] %in% colnames(xreg)) {
    xreg <- xreg[, colnames(xreg) != all.vars(formula)[1], drop = FALSE]
  }

  # cpt.reg() always fits a mean term and expects it as the first regressor
  # (it only checks the column after the response). Remove any constant
  # columns, resolve collinearity via QR decomposition, then put a single
  # intercept column first. This also supports intercept-only models.
  xreg <- .drop_collinear_cols(.drop_constant_cols(xreg))
  n_regressors <- ncol(xreg)
  xreg <- cbind(`(Intercept)` = 1, xreg)

  cpt_data <- as.matrix(cbind(response, xreg))

  # Default minimum segment length (based on non-intercept regressors)
  min_segment <- max(15, n_regressors * 3)

  # Allow the user to override minseglen
  if (is.null(break_params$minseglen)) {
    break_params$minseglen <- min_segment
  }

  # Default algorithm
  if (is.null(break_params$method)) {
    break_params$method <- "PELT"
  }

  # Detect structural breaks
  bp <- do.call(
    changepoint::cpt.reg,
    c(
      list(data = cpt_data),
      break_params
    )
  )

  # Extract breakpoints
  breaks <- changepoint::cpts(bp)

  # Exclude the final observation
  breaks <- breaks[breaks < nrow(data)]

  if (length(breaks) > 0 && !all(is.na(breaks))) {
    time <- data[[date_col]][breaks]

    output <- data.table::data.table(
      type = "SB",
      ind = breaks,
      time = time
    )

    # Assign observations to segments
    segments <- data.table::data.table(
      segments = findInterval(seq_len(nrow(data)) - 1L, breaks) + 1L
    )

    segments[, segments := as.factor(segments)]

    return(list(
      break_entry = output,
      segments = segments
    ))
  } else {
    return(NULL)
  }
}
