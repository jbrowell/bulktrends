#' Detection of temporary time series outliers
#'
#' Detects temporary outliers in a time series using [tsoutliers::tso()]:
#' additive outliers (AO) and temporary changes (TC) by default.
#'
#' @param data A data frame or `data.table` containing the response column,
#'   with rows in time order.
#' @param response_col Name of the column containing the time series values,
#'   e.g. `"NET_MASS"`, `"STAT_VALUE"` or `"volume"`.
#' @param scale_ts If `TRUE`, the series is scaled to zero mean and unit
#'   variance with [scale()] before detection. The effect columns and the
#'   `coefhat` values are then in scaled units. Default `FALSE`.
#' @param xreg A matrix of external regressors passed to `tso()`, with one row
#'   per row of `data`. Constant columns (such as an intercept) and linearly
#'   dependent columns are removed first, because `tso()` estimates its own
#'   mean. Default `NULL`.
#' @param return_tso If `TRUE`, the full `tso()` output is included in the
#'   returned list as element `tso`. Default `FALSE`.
#' @param tso_params A named list of additional arguments passed to
#'   `tsoutliers::tso()`, e.g. `list(cval = 4, maxit.iloop = 20)`. Unless
#'   overridden, `types = c("AO", "TC")` and `cval = 5` are used, which differ
#'   from the `tso()` defaults. Other types, such as `"IO"`, can be requested
#'   and appear in `outliers`, but only AO and TC effect columns are added to
#'   `data`. Default `list()`.
#'
#' @return `NULL` if every attempt to fit `tso()` fails; otherwise a list with
#'   elements:
#'
#' * `data`: `data` as a `data.table`, with one effect column per detected AO
#'   or TC outlier appended, numbered in order of detection (`AO1`, `AO2`,
#'   `TC1`, ...).
#' * `outliers`: the outlier table returned by `tso()`, with columns `type`,
#'   `ind` (row position), `time`, `coefhat` (estimated effect) and `tstat`.
#' * `model_used`: the fitting strategy that succeeded: `"default"`,
#'   `"ARIMA(0,0,0)_with_xreg"` or `"ARIMA(0,0,0)_without_xreg"`.
#' * `tso`: the full `tso()` output, only if `return_tso = TRUE`.
#'
#' @details
#' If `tso()` fails with the default ARIMA model selection, it is retried with
#' an ARIMA(0,0,0) model that keeps `xreg`. If that also fails, it is retried
#' with ARIMA(0,0,0) and no regressors, i.e. a constant mean. That last model
#' ignores trend, seasonality and level shifts, so it may detect fewer outliers.
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

    # Drop constant columns (an intercept, or a dummy that never varies) and
    # linearly dependent columns: the ARIMA model fitted by tso() estimates
    # its own mean, so these make xreg collinear and fitting fails
    xreg <- .drop_collinear_cols(.drop_constant_cols(xreg))

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

  # Helper function to run tso() and capture errors
  run_tso <- function(params) {
    tryCatch(
      do.call(tsoutliers::tso, params),
      error = function(e) e
    )
  }

  # ARIMA(0,0,0) fallback specification
  arima000 <- list(
    tsmethod = "arima",
    args.tsmethod = list(order = c(0, 0, 0))
  )

  # First attempt: default ARIMA model selection
  model_used <- "default"
  tso_outliers <- run_tso(tso_params)

  # Fallback 1: ARIMA(0,0,0), retaining external regressors, so that trend and
  # seasonality are still removed before outliers are tested
  if (inherits(tso_outliers, "error") && !is.null(tso_params$xreg)) {
    message(
      "Outlier detection failed, retrying with ARIMA(0,0,0): ",
      conditionMessage(tso_outliers)
    )

    tso_outliers <- run_tso(
      utils::modifyList(tso_params, arima000)
    )

    if (!inherits(tso_outliers, "error")) {
      model_used <- "ARIMA(0,0,0)_with_xreg"
    }
  }

  # Fallback 2: ARIMA(0,0,0), without external regressors (constant mean only)
  if (inherits(tso_outliers, "error")) {
    message(
      "Outlier detection failed, retrying with ARIMA(0,0,0) and no xreg: ",
      conditionMessage(tso_outliers)
    )

    fallback_params <- utils::modifyList(tso_params, arima000)
    fallback_params["xreg"] <- list(NULL)

    tso_outliers <- run_tso(fallback_params)

    if (!inherits(tso_outliers, "error")) {
      model_used <- "ARIMA(0,0,0)_without_xreg"
    }
  }

  # Return NULL if all fitting attempts fail
  if (inherits(tso_outliers, "error")) {
    message(
      "Outlier detection failed: ",
      conditionMessage(tso_outliers)
    )
    return(NULL)
  }

  # Extract fitted regressors
  xreg <- tso_outliers$fit$xreg

  if (!is.null(xreg)) {
    xreg <- as.matrix(xreg)

    # Identify outlier columns
    outlier_cols <- colnames(xreg)
    outlier_cols <- outlier_cols[
      substr(outlier_cols, 1, 2) %in% c("AO", "TC")
    ]

    if (length(outlier_cols) > 0) {
      xreg_outliers <- data.table::as.data.table(
        xreg[, outlier_cols, drop = FALSE]
      )

      # Rename columns to sequential AO1, AO2, TC1, TC2 etc.
      types <- substr(outlier_cols, 1, 2)
      type_counts <- ave(seq_along(types), types, FUN = seq_along)

      colnames(xreg_outliers) <- paste0(types, type_counts)

      data <- cbind(data, xreg_outliers)
    }
  }

  # Compile results
  result <- list(
    data = data,
    outliers = tso_outliers$outliers,
    model_used = model_used
  )

  if (return_tso) {
    result$tso <- tso_outliers
  }

  return(result)
}
