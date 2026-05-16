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
