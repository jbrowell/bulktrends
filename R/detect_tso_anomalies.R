#' Detection of temporary time series outliers
#'
#' This function detects temporary outliers in a given time series
#' using `tsoutliers::tso()`.
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param date_col Name of column containing timestamps.
#' @param code A vector of HS2/HS4/HS6/CN8 codes
#' @param quantity Quantity to be analysed, e.g. `NET_MASS`, `STAT_VALUE` or `volume`.
#' @param model_formula A formula specifying the regression model for outlier detection.
#' @param types Outlier types to detect. Typical inputs are: AO (Additive Outlier), TC (Temporary Change),
#' IO (Innovational Outlier), LS (Level Shift), or SLS(Seasonal Level Shift).
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`. Default `FALSE`.
#' @param xreg matrix of external regressors passed to `tso()`.
#' @param ... Additional arguments passed directly to `tsoutliers::tso()`.
#'
#' @return A list of (i) the original time series dataset with detected outlier covariates
#' columns appended when available, and (ii) the outlier summary table returned by `tso()`.
#'
#' @export
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
