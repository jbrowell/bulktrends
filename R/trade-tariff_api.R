#' Request data from GOV.UK Trade Tariff API
#'
#' @param endpoint Endpoint appended to the base API URL. See details.
#' @param as_of Optional date (character \code{"YYYY-MM-DD"} or \code{Date})
#'   passed as the \code{as_of} query parameter. When supplied, the API returns
#'   data as it existed on that date, allowing retrieval of commodity codes that
#'   are no longer valid today.
#'
#' @details
#' For endpoint documentation visit https://docs.trade-tariff.service.gov.uk/.
#' `endpoint` is appended to "https://www.trade-tariff.service.gov.uk/api/v2/".
#'
#' @return Result of API query.
#'
#' @export
tradetariff_request <- function(endpoint, as_of = NULL) {

  url <- paste0("https://www.trade-tariff.service.gov.uk/api/v2/", endpoint)

  if (!is.null(as_of)) {
    url <- paste0(url, "?as_of=", format(as.Date(as_of), "%Y-%m-%d"))
  }

  return(jsonlite::fromJSON(url))

}


#' Get validity dates for one or more commodity codes
#'
#' @param comcodes Character vector of commodity codes (8 or 10 digits). Numeric values are not accepted as leading zeros may be silently dropped. Eight-digit codes are expanded to 10 digits by appending \code{"00"}, \code{"10"}, then \code{"20"} in turn, using the first that returns a valid result.
#' @param as_of Optional date (character \code{"YYYY-MM-DD"} or \code{Date}).
#'   When supplied, the API is queried as of that date. Useful when the
#'   validity window of an expired code is already known.
#' @param search_to Integer year. The earliest year tried when searching for
#'   an expired code's validity window. Defaults to 1990.
#'
#' @details
#' Queries the GOV.UK Trade Tariff API for the validity period of each commodity
#' code. A warning is raised for any code that is not exactly 10 digits. When
#' multiple codes are supplied, requests are dispatched via
#' \code{future.apply::future_lapply()}; use \code{future::plan()} to control
#' parallelism. Codes that fail are returned with \code{NA} dates and a message
#' is printed for each failure.
#'
#' When \code{as_of} is \code{NULL} (the default) and a commodity code is not
#' valid on today's date, the function automatically retries the request using
#' January 1st of each year from the previous year back to \code{search_to},
#' stopping at the first successful response. This allows expired codes to be
#' resolved without the caller needing to know the validity window. Supplying
#' \code{as_of} skips this search and queries that single date directly.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{comcode}{The commodity code as supplied (8 or 10 digits).}
#'   \item{comcode_10}{The 10-digit code that returned a successful API result.
#'   Equal to \code{comcode} for 10-digit inputs; the expanded code for 8-digit
#'   inputs. \code{NA} if no valid result was found.}
#'   \item{valid_from}{The date from which the commodity code is valid.}
#'   \item{valid_to}{The date until which the commodity code is valid.
#'   \code{NA} if the code has no end date (still active).}
#' }
#'
#' @export
comcode_validity_dates <- function(comcodes, as_of = NULL, search_to = 1990L) {

  if (!is.character(comcodes)) {
    stop("'comcodes' must be a character vector. Numeric values may silently drop leading zeros.")
  }

  if (length(comcodes) > 1) {
    rows <- future.apply::future_lapply(comcodes, comcode_validity_dates, as_of = as_of, search_to = search_to)
    return(data.table::rbindlist(rows, fill = TRUE))
  }

  if (nchar(comcodes) == 8) {
    primary  <- c("00","10","99","90")
    suffixes <- c(primary, setdiff(sprintf("%02d", 0:99), primary))
    for (suffix in suffixes) {
      result <- comcode_validity_dates(paste0(comcodes, suffix), as_of = as_of, search_to = search_to)
      if (!is.na(result$valid_from) || !is.na(result$valid_to)) {
        result$comcode <- comcodes
        return(result)
      }
    }
    message("Skipping '", comcodes, "': no valid 10-digit expansion found.")
    return(data.frame(comcode = comcodes, comcode_10 = NA_character_, valid_from = as.Date(NA),
                      valid_to = as.Date(NA), stringsAsFactors = FALSE))
  }

  if (nchar(comcodes) != 10) {
    warning("Commodity codes should be 8 or 10 digits; '", comcodes, "' has ", nchar(comcodes), ".")
    return(data.frame(comcode = comcodes, comcode_10 = NA_character_, valid_from = as.Date(NA),
                      valid_to = as.Date(NA), stringsAsFactors = FALSE))
  }

  try_request <- function(date) {
    suppressWarnings(
      tryCatch(
        tradetariff_request(paste0("commodities/", comcodes), as_of = date),
        error = function(e) NULL
      )
    )
  }

  result <- try_request(as_of)

  if (is.null(result) && is.null(as_of)) {
    years <- seq(as.integer(format(Sys.Date(), "%Y")) - 1L, as.integer(search_to), by = -1L)
    for (yr in years) {
      result <- try_request(paste0(yr, "-01-01"))
      if (!is.null(result)) break
    }
  }

  if (is.null(result)) {
    message("Skipping '", comcodes, "': no valid response found for any date.")
    return(data.frame(comcode = comcodes, comcode_10 = NA_character_, valid_from = as.Date(NA),
                      valid_to = as.Date(NA), stringsAsFactors = FALSE))
  }

  attrs <- result$data$attributes

  data.frame(
    comcode    = comcodes,
    comcode_10 = comcodes,
    valid_from = as.Date(substr(attrs$validity_start_date, 1, 10)),
    valid_to   = if (is.null(attrs$validity_end_date) || is.na(attrs$validity_end_date)) {
      as.Date(NA)
    } else {
      as.Date(substr(attrs$validity_end_date, 1, 10))
    },
    stringsAsFactors = FALSE
  )

}
