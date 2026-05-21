# Extract column of breakpoint segments of a time series with structural breaks

detect_breaks <- function(data, date_col = "DATE_START", formula) {
  #xreg_breaks <- list()

  bp <- breakpoints(
    formula,
    data = data,
    breaks = "BIC",
    het.err = FALSE
  )

  breaks <- bp$breakpoints

  #time index
  if (!is.null(date_col)) {
    time <- data[[date_col]][breaks]
  }

  #table
  #  output <- data.table(type = "SB", ind = breaks, time = time)

  if (length(breaks) > 0) {
    output <- data.table(type = "SB", ind = breaks, time = time)
    segments <- as.matrix(breakfactor(bp))
    colnames(segments) <- paste0("segments")
    return(list(break_entry = output, segments = segments))
  } else {
    return(NULL)
  }
}
