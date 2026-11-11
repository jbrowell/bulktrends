#' Detection of temporary time series outliers
#'
#' This function detects temporary outliers in a given time series
#' using `tsoutliers::tso()`.
#'
#' @param data A `data.frame` containing the dependent variable and dates.
#' @param response_col Name of the column containing the time series values,
#'  e.g. `"NET_MASS"`, `"STAT_VALUE"` or `volume`.
#' @param scale_ts If `TRUE`, time series is scaled to zero mean and unit variance using `scale()`. Default `FALSE`.
#' @param xreg matrix of external regressors passed to `tso()`. Default `NULL`.
#' @param return_tso If `TRUE`, the full `tso()` output is included in the
#'   returned list as element `tso`. Default `FALSE`.
#' @param tso_params A named list of additional arguments passed directly to `tsoutliers::tso()`,
#'   e.g. `list(cval = 5, types = c("AO", "TC", "IO"))`. Default `list()`.
#'
#' @return A list with elements (i) `data`, the original time series dataset with outlier covariates
#' columns appended when detected, and (ii) `outliers`, the outlier summary table returned by `tso()`.
#' If `return_tso = TRUE`, a third element `tso` is also included. Returns
#'   `NULL` if `tso()` errors.
#'
#' @details
#' If `tso()` fails with the default ARIMA model selection, it is retried with
#' an ARIMA(0,0,0) model that keeps `xreg`. If that also fails, it is retried
#' with ARIMA(0,0,0) and no regressors, i.e. a constant mean. That last model
#' ignores trend, seasonality and level shifts, so it detects fewer outliers.
#' A message is printed each time a fallback is used.
#'
#' @export
detect_outliers <- function(
  data,
  response_col,
  scale_ts = FALSE,
  xreg = NULL,
  return_tso = FALSE,
  tso_params = list()
) {
  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)
    # Drop any intercept column: the ARIMA model fitted by tso() estimates its
    # own mean, so an intercept in xreg is collinear with it and fitting fails
    if ("(Intercept)" %in% colnames(xreg)) {
      xreg <- xreg[, colnames(xreg) != "(Intercept)", drop = FALSE]
    }
    if (ncol(xreg) == 0) {
      xreg <- NULL
    }
  }

  tso_params <- modifyList(
    list(
      y = if (scale_ts) {
        as.ts(scale(data[[response_col]]))
      } else {
        as.ts(data[[response_col]])
      },
      xreg = xreg,
      types = c("AO", "TC"),
      cval = 5
    ),
    tso_params
  )

  run_tso <- function(params) {
    tryCatch(do.call(tsoutliers::tso, params), error = function(e) e)
  }
  arima000 <- list(tsmethod = "arima", args.tsmethod = list(order = c(0, 0, 0)))

  tso_outliers <- run_tso(tso_params)

  # Fallback 1: ARIMA(0,0,0), keeping the regressors so that trend and
  # seasonality are still removed before outliers are tested
  if (inherits(tso_outliers, "error") && !is.null(tso_params$xreg)) {
    message(
      "Outlier detection failed, retrying with ARIMA(0,0,0): ",
      conditionMessage(tso_outliers)
    )
    tso_outliers <- run_tso(utils::modifyList(tso_params, arima000))
  }

  # Fallback 2: ARIMA(0,0,0) with no regressors (constant mean only)
  if (inherits(tso_outliers, "error")) {
    message(
      "Outlier detection failed, retrying with ARIMA(0,0,0) and no xreg: ",
      conditionMessage(tso_outliers)
    )
    fallback_params <- utils::modifyList(tso_params, arima000)
    fallback_params["xreg"] <- list(NULL)
    tso_outliers <- run_tso(fallback_params)
  }

  if (inherits(tso_outliers, "error")) {
    message("Outlier detection failed: ", conditionMessage(tso_outliers))
    tso_outliers <- NULL
  }

  if (is.null(tso_outliers)) {
    return(NULL)
  }

  xreg <- tso_outliers$fit$xreg

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)

    #identify outlier columns
    outlier_cols <- colnames(xreg)
    outlier_cols <- outlier_cols[
      substr(outlier_cols, 1, 2) %in% c("AO", "TC")
    ]

    if (length(outlier_cols) > 0) {
      xreg_outliers <- as.data.table(xreg[, outlier_cols, drop = FALSE])
      # Rename columns to sequential AO1, AO2, TC1, TC2 etc.
      types <- substr(outlier_cols, 1, 2)
      type_counts <- ave(seq_along(types), types, FUN = seq_along)
      colnames(xreg_outliers) <- paste0(types, type_counts)
      data <- cbind(data, xreg_outliers)
    }
  }
  result <- list(data = data, outliers = tso_outliers$outliers)

  if (return_tso) {
    result$tso <- tso_outliers
  }

  return(result)
}
