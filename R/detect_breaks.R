# Extract model matrix of breakpoint covariates of a time series with structural breaks

detect_breaks <- function(data, selected_model) {
  xreg_breaks <- list()

  bp <- breakpoints(
    selected_model$formula,
    data = data,
    breaks = "BIC",
    het.err = FALSE
  )

  if (!is.null(bp)) {
    breaks <- bp$breakpoints

    if (length(breaks) > 0) {
      segments <- breakfactor(bp)
      xreg_breaks <- model.matrix(~segments)[, -1, drop = FALSE]
      colnames(xreg_breaks) <- paste0("BP_SEG_", seq_len(ncol(xreg_breaks)))
    }
  }
  return(xreg_breaks)
}
