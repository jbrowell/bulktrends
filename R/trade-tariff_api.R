#' Request data from GOV.UK Trade Tariff API
#'
#' @param endpoint Endpoint appended to the base API URL. See details.
#'
#' @details
#' For endpoint documentation visit https://docs.trade-tariff.service.gov.uk/.
#' `endpoint` is appended to "https://www.trade-tariff.service.gov.uk/api/v2/".
#'
#' @return Result of API query.
#'
#' @export
tradetariff_request <- function(endpoint) {

  request <- paste0("https://www.trade-tariff.service.gov.uk/api/v2/", endpoint)

  return(jsonlite::fromJSON(request))

}


#' Get validity dates for one or more commodity codes
#'
#' @param comcodes Character vector of commodity codes. Numeric values are not accepted as leading zeros may be silently dropped.
#'
#' @details
#' Queries the GOV.UK Trade Tariff API for the validity period of each commodity
#' code. An error is raised for any code that is not exactly 10 digits. When
#' multiple codes are supplied, requests are dispatched via
#' \code{future.apply::future_lapply()}; use \code{future::plan()} to control
#' parallelism. Codes that fail are returned with \code{NA} dates and a message
#' is printed for each failure.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{comcode}{The commodity code as a character string.}
#'   \item{valid_from}{The date from which the commodity code is valid.}
#'   \item{valid_to}{The date until which the commodity code is valid.
#'   \code{NA} if the code has no end date (still active).}
#' }
#'
#' @export
comcode_validity_dates <- function(comcodes) {

  if (!is.character(comcodes)) {
    stop("'comcodes' must be a character vector. Numeric values may silently drop leading zeros.")
  }

  if (length(comcodes) > 1) {
    rows <- future.apply::future_lapply(comcodes, comcode_validity_dates)
    return(do.call(rbind, rows))
  }

  if (nchar(comcodes) != 10) {
    stop("Commodity codes should have 10 digits; '", comcodes, "' has ", nchar(comcodes), ".")
  }

  result <- tryCatch(
    tradetariff_request(paste0("commodities/", comcodes)),
    error = function(e) {
      message("Skipping '", comcodes, "': ", conditionMessage(e))
      return(data.frame(comcode = comcodes, valid_from = as.Date(NA), valid_to = as.Date(NA),
                        stringsAsFactors = FALSE))
    }
  )

  if (is.data.frame(result)) return(result)

  attrs <- result$data$attributes

  data.frame(
    COMCODE    = comcodes,
    valid_from = as.Date(substr(attrs$validity_start_date, 1, 10)),
    valid_to   = if (is.null(attrs$validity_end_date) || is.na(attrs$validity_end_date)) {
      as.Date(NA)
    } else {
      as.Date(substr(attrs$validity_end_date, 1, 10))
    },
    stringsAsFactors = FALSE
  )

}
