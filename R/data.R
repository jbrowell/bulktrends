#' COMCODE Groups
#'
#' Useful groups of commodity codes for filtering import data.
#'
#' @format ## `comcode_groups`
#' A named `list` containing a groups of commodity codes
#' \describe{
#'   \item{name}{Name of group}
#'   \item{value}{vector of Cn8 codes (`charater`) in specified group}
#' }
#' @source `bulktrends`
"comcode_groups"


#' UK Trade Tariff commodity code lookup table
#'
#' All goods nomenclature entries from the UK Integrated Online Tariff,
#' downloaded from the Department for Business and Trade Data API. Use
#' \code{update_tariff_commodities()} to refresh this dataset.
#'
#' @format ## `tariff_commodities`
#' A \code{data.frame} with one row per goods nomenclature entry and four columns:
#' \describe{
#'   \item{comcode}{10-digit commodity code as a character string (leading zeros preserved).}
#'   \item{suffix}{Producline suffix. \code{"80"} indicates a declarable commodity;
#'     other values (e.g. \code{"10"}) indicate intermediate nomenclature nodes.}
#'   \item{valid_from}{Date from which the entry is valid.}
#'   \item{valid_to}{Date until which the entry is valid. \code{NA} for codes with
#'     no end date (currently active).}
#' }
#' @source Department for Business and Trade Data API,
#'   \url{https://data.api.trade.gov.uk/v1/datasets/uk-tariff-2021-01-01/versions/latest/tables/commodities/data?format=csv&download}.
#'   Coverage begins 1 January 2021.
"tariff_commodities"
