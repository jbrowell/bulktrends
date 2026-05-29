#' COMCODE Groups
#'
#' Useful groups of commodity codes for filtering import data.
#'
#' @format ## `comcode_groups`
#' A named `list` containing a group of commodity codes
#' \describe{
#'   \item{name}{Name of group}
#'   \item{value}{vector of Cn8 codes (`character`) in specified group}
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

#' UK Bank Holidays
#'
#' A dataset of UK bank holidays sourced from the official gov.uk published
#' data, supplemented by the alphagov/calendars GitHub repository for older
#' date ranges.  The dataset covers all three UK divisions and includes
#' substitute days and one-off special bank holidays (e.g. the Queen's
#' Platinum Jubilee 2022, King's Coronation 2023).
#'
#' @format ## `uk_bank_holidays`
#' A `data.table` with the following columns:
#' \describe{
#'   \item{date}{Date of the bank holiday (`Date`)}
#'   \item{title}{Name of the bank holiday (`character`)}
#'   \item{notes}{Additional notes, e.g. `"Substitute day"` or `"Extra bank holiday"` (`character`)}
#'   \item{division}{UK division: `"england-and-wales"`, `"scotland"`, or `"northern-ireland"` (`character`)}
#' }
#' The dataset also carries a `created_at` attribute (a `character` string in
#' `"YYYY-MM-DD"` format) recording the date on which `data-raw/uk_bank_holidays.R`
#' was last run to generate this file.
#'
#' @details Coverage:
#' \itemize{
#'   \item England and Wales: 2015 to latest available in the gov.uk API
#'   \item Scotland: 2015 to latest available in the gov.uk API
#'   \item Northern Ireland: 2015 to latest available in the gov.uk API
#' }
#'
#' \strong{Sources and priority:} When regenerating via
#' \code{data-raw/uk_bank_holidays.R}, both sources are attempted and combined
#' for maximum date coverage.  Where the same date and division appears in both
#' sources, the higher-priority source wins:
#' \enumerate{
#'   \item \url{https://www.gov.uk/bank-holidays.json} — official API
#'         (highest priority; supersedes the alphagov source)
#'   \item alphagov/calendars GitHub repository — extends coverage into
#'         older date ranges not present in the gov.uk API
#' }
#' Each source is fetched independently; the script continues if one is
#' unavailable.
#'
#' Use [get_uk_bank_holidays()] to fetch up-to-date data directly from
#' \url{https://www.gov.uk/bank-holidays.json}.
#'
#' @source \url{https://www.gov.uk/bank-holidays}
#' @source \url{https://github.com/alphagov/calendars}
"uk_bank_holidays"
