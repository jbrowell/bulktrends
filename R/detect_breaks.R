#' Detect structural breaks in a time series
#'
#' This function identifies structural breaks in a time series using either
#' the Bai-Perron or PELT method.
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param formula A formula specifying the regression model for break detection.
#' @param date_col Name of column containing timestamps. Default `"DATE_START"`.
#' @param method Method used for structural break detection. Options are
#' `"Bai-Perron"` or `"PELT"`.
#'
#' @return A list containing (i) `break_entry`, a `data.table` with identified
#' breakpoints and their respective indices and timing, and (ii) `segments`,
#' a `data.table` containing a factor identifying the segment to which each
#' observation belongs.
#' Returns `NULL` if no breaks are detected.
#'
#' @details
#' Method `"Bai-Perron"` uses `strucchangeRcpp::breakpoints()` with
#' BIC-based model selection.
#' Method `"PELT"` uses `changepoint::cpt.reg()` with the PELT algorithm
#' and a specified minimum segment length.
#'
#' @export
detect_breaks <- function(
  data,
  formula,
  method,
  date_col = "DATE_START"
) {
  if (method == "Bai-Perron") {
    bp <- strucchangeRcpp::breakpoints(
      formula,
      data = data,
      breaks = "BIC",
      het.err = FALSE
    )

    breaks <- bp$breakpoints

    if (length(breaks) > 0 && !all(is.na(breaks))) {
      time <- data[[date_col]][breaks]
      output <- data.table::data.table(type = "SB", ind = breaks, time = time)
      segments <- data.table::data.table(segments = strucchangeRcpp::breakfactor(bp))
      segments[, segments := as.factor(segments)]
      return(list(break_entry = output, segments = segments))
    } else {
      return(NULL)
    }
  } else if (method == "PELT") {
    .detect_breaks_pelt(data, formula, date_col)
  } else {
    stop("method not recognised. Must be one of \"Bai-Perron\" or \"PELT\".")
  }
}


.detect_breaks_pelt <- function(data, formula, date_col = "DATE_START") {
  # Prepare data
  data <- data.table::as.data.table(data)
  response <- data[[all.vars(formula)[1]]]
  xreg <- model.matrix(formula, data = data)

  # Precaution: remove the response column if present
  if (all.vars(formula)[1] %in% colnames(xreg)) {
    xreg <- xreg[, colnames(xreg) != all.vars(formula)[1], drop = FALSE]
  }

  # Remove the automatic intercept if present to prevent conflict with PELT's internal intercept
  if ("(Intercept)" %in% colnames(xreg)) {
    xreg <- xreg[, colnames(xreg) != "(Intercept)", drop = FALSE]
  }

  # Mathematically resolve collinearity/rank-deficiency via QR Decomposition
  if (ncol(xreg) > 0) {
    qr_decomp <- qr(xreg)
    # Keep only the linearly independent columns
    independent_cols <- qr_decomp$pivot[seq_len(qr_decomp$rank)]
    xreg <- xreg[, independent_cols, drop = FALSE]
  }

  # PELT regression requires at least one explanatory column.
  if (ncol(xreg) == 0) {
    return(NULL)
  }

  cpt_data <- as.matrix(cbind(response, xreg))

  # CRITICAL FIX: To prevent local overfitting and spurious 4-day breaks in segments,
  # we enforce a minimum segment length (minseglen) that scale proportionally and allows
  # enough degrees of freedom. Ideally, minseglen should be at least 3 times the number of parameters.
  #The idea is to provide at least three observations per explanatory variable,  thus
  #reducing the risk of fitting regressions with too few observations relative to the number of parameters.
  #15 minimum data points, this can be changed, or the user might specify the number of points as an argument
  min_segment <- max(15, ncol(xreg) * 3) ###!!! here needs some thought; but it works very well.

  # Detect structural breaks using PELT
  bp <- changepoint::cpt.reg(
    data = cpt_data,
    method = "PELT",
    minseglen = min_segment
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
      segments = findInterval(seq_len(nrow(data)), breaks) + 1L
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
