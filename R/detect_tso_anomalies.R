# detection of tso outliers

#name to detect_outliers
detect_outliers <- function(
  data,
  date_col = "DATE_START",
  code,
  quantity,
  model_formula,
  types = c("AO", "LS", "TC", "IO"),
  scale_ts = FALSE,
  xreg = NULL,
  ...
) {
  run_tso <- tryCatch(
    tso(
      y = if (scale_ts) {
        as.ts(scale(data[[quantity]]))
      } else {
        as.ts(data[[quantity]])
      },

      xreg = if (!is.null(xreg)) {
        xreg
      } else {
        NULL
      },
      ...
    ),
    error = function(e) e
  )

  if (inherits(run_tso, "error")) {
    return(NULL)
  }

  xreg <- run_tso$fit$xreg

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)

    #identify outlier columns
    outlier_cols <- colnames(xreg)
    outlier_cols <- outlier_cols[
      substr(outlier_cols, 1, 2) %in% c("AO", "LS", "TC", "IO")
    ]

    if (length(outlier_cols) > 0) {
      xreg_outliers <- as.data.table(xreg[, outlier_cols, drop = F])
      data <- cbind(data, xreg_outliers)
    }
  }

  return(list(
    data = data,
    outliers = run_tso$outliers
  ))
}
