#' Detect structural breaks in a time series
#'
#' This function identifies structural breaks in a time series using either
#' the breakpoints (Bai-Perron) or a changepoint regression (PELT or AMOC) method.
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param formula A formula specifying the regression model for break detection.
#' @param break_method Function used for structural break detection.
#' Options are `"breakpoints"` or `"cpt.reg"`.
#' @param break_params A named list of additional arguments passed to the
#' selected break detection function. For `cpt.reg`, this can include
#' `method` (e.g. `"PELT"` or `"AMOC"`) and `minseglen`.
#' @param date_col Name of column containing timestamps. Default `"DATE_START"`.
#'
#' @return A list containing (i) `break_entry`, a `data.table` with identified
#' breakpoints and their respective indices and timing, and (ii) `segments`,
#' a `data.table` containing a factor identifying the segment to which each
#' observation belongs.
#' Returns `NULL` if no breaks are detected.
#'
#' @details
#' Method `"breakpoints"` uses `strucchangeRcpp::breakpoints()`.
#' Method `"cpt.reg"` uses `changepoint::cpt.reg()`, allowing the user to
#' select PELT or AMOC through `break_params`.
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
