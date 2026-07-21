#' Detect structural breaks in a time series
#'
#' This function uses [strucchangeRcpp::breakpoints()] with BIC-based selection to identify
#' structural breaks in a given time series.
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param date_col Name of column containing timestamps. Default `"DATE_START"`.
#' @param formula A formula specifying the regression model for break detection.
#'
#' @return A list containing (i) `break_entry`, a `data.table` with identified breakpoints and
#' their respective timing and, (ii) `segments`, a matrix of segment indicator
#' variable for use as a regressor.
#' Returns `NULL` if no breaks are detected.
#'
#' @export
detect_breaks <- function(data, date_col = "DATE_START", formula) {
  bp <- strucchangeRcpp::breakpoints(
    formula,
    data = data,
    breaks = "BIC",
    het.err = FALSE
  )

  breaks <- bp$breakpoints

  if (length(breaks) > 0 && !all(is.na(breaks))) {
    time <- data[[date_col]][breaks]
    output <- data.table(type = "SB", ind = breaks, time = time)
    segments <- as.data.table(as.matrix(strucchangeRcpp::breakfactor(bp)))
    colnames(segments) <- paste0("segments")
    segments[, segments := as.factor(segments)]
    return(list(break_entry = output, segments = segments))
  } else {
    return(NULL)
  }
}
