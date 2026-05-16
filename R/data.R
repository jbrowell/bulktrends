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

#' UK Bank Holidays
#'
#' A dataset of UK bank holidays sourced from the official gov.uk published
#' data and the alphagov/calendars GitHub repository.  The dataset covers
#' all three UK divisions and includes substitute days and one-off special
#' bank holidays (e.g. the Queen's Platinum Jubilee 2022, King's Coronation
#' 2023).
#'
#' @format ## `uk_bank_holidays`
#' A `data.table` with the following columns:
#' \describe{
#'   \item{date}{Date of the bank holiday (`Date`)}
#'   \item{title}{Name of the bank holiday (`character`)}
#'   \item{notes}{Additional notes, e.g. `"Substitute day"` or `"Extra bank holiday"` (`character`)}
#'   \item{bunting}{Whether bunting is typically displayed (`logical`)}
#'   \item{division}{UK division: `"england-and-wales"`, `"scotland"`, or `"northern-ireland"` (`character`)}
#' }
#' The dataset also carries a `created_at` attribute (a `character` string in
#' `"YYYY-MM-DD"` format) recording the date on which `data-raw/uk_bank_holidays.R`
#' was last run to generate this file.
#'
#' @details Coverage:
#' \itemize{
#'   \item England and Wales: 2010--2026
#'   \item Scotland: 2015--2026
#'   \item Northern Ireland: 2015--2026
#' }
#'
#' Use [get_uk_bank_holidays()] to fetch up-to-date data directly from
#' \url{https://www.gov.uk/bank-holidays.json}.
#'
#' @source \url{https://www.gov.uk/bank-holidays}
#' @source \url{https://github.com/alphagov/calendars}
"uk_bank_holidays"
