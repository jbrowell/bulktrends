# Extract column of breakpoint segments of a time series with structural breaks

detect_breaks <- function(data, formula) {
  #xreg_breaks <- list()

  bp <- breakpoints(
    formula,
    data = data,
    breaks = "BIC",
    het.err = FALSE
  )

  if (!is.null(bp)) {
    breaks <- bp$breakpoints

    if (length(breaks) > 0) {
      segments <- as.matrix(breakfactor(bp))
      colnames(segments) <- paste0("segments")
    }
  }
  return(segments)
}
