#' Detection of temporary time series outliers
#'
#' This function detects temporary outliers in a given time series
#' using `tsoutliers::tso()`.
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param quantity Name of the column containing the time series values,
#'  e.g. `"NET_MASS"`, `"STAT_VALUE"` or `volume`.
#' @param types Outlier types to detect. Typical inputs are: AO (Additive Outlier), TC (Temporary Change),
#' IO (Innovational Outlier), LS (Level Shift), or SLS(Seasonal Level Shift). Default `c("AO", "TC", "IO")`.
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`. Default `FALSE`.
#' @param xreg matrix of external regressors passed to `tso()`. Default `NULL`.
#' @param return_tso If `TRUE`, the full `tso()` output is included in the
#'   returned list as element `tso`. Default `FALSE`.
#' @param ... Additional arguments passed directly to `tsoutliers::tso()`.
#'
#' @return A list with elements (i) `data`, the original time series dataset with outlier covariates
#' columns appended when detected, and (ii) `outliers`, the outlier summary table returned by `tso()`.
#' If `return_tso = TRUE`, a third element `tso` is also included. Returns
#'   `NULL` if `tso()` errors.
#'
#' @export
detect_outliers <- function(
  data,
  quantity,
  types = c("AO", "TC", "IO"),
  scale_ts = FALSE,
  xreg = NULL,
  return_tso = FALSE,
  ...
) {
  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    if (ncol(xreg) == 0) {
      xreg <- NULL
    }
  }

  tso_outliers <- tryCatch(
    tso(
      y = if (scale_ts) {
        as.ts(scale(data[[quantity]]))
      } else {
        as.ts(data[[quantity]])
      },
      types = types,
      xreg = xreg,
      ...
    ),
    error = function(e) {
      message("Outlier detection failed: ", e)
      return(NULL)
    }
  )

  if (is.null(tso_outliers)) {
    return(NULL)
  }

  xreg <- tso_outliers$fit$xreg

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)

    #identify outlier columns
    outlier_cols <- colnames(xreg)
    outlier_cols <- outlier_cols[
      substr(outlier_cols, 1, 2) %in% c("AO", "TC", "IO")
    ]

    if (length(outlier_cols) > 0) {
      xreg_outliers <- as.data.table(xreg[, outlier_cols, drop = F])
      data <- cbind(data, xreg_outliers)
    }
  }
  result <- list(data = data, outliers = tso_outliers$outliers)

  if (return_tso) {
    result$tso <- tso_outliers
  }

  return(result)
}
