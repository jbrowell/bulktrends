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
#'   \item{division}{UK division: `"england-and-wales"`, `"scotland"`, or `"northern-ireland"` (`character`)}
#' }
#' The dataset also carries a `created_at` attribute (a `character` string in
#' `"YYYY-MM-DD"` format) recording the date on which `data-raw/uk_bank_holidays.R`
#' was last run to generate this file.
#'
#' @details Coverage of the bundled dataset (as shipped with the package):
#' \itemize{
#'   \item England and Wales: 2010--2026
#'   \item Scotland: 2015--2026
#'   \item Northern Ireland: 2015--2026
#' }
#'
#' \strong{Sources and priority:} When regenerating via
#' \code{data-raw/uk_bank_holidays.R}, all three sources are attempted and
#' combined for maximum date coverage.  Where the same date and division
#' appears in more than one source, the highest-priority source wins:
#' \enumerate{
#'   \item \url{https://www.gov.uk/bank-holidays.json} — official API
#'         (highest priority; supersedes all other sources)
#'   \item alphagov/calendars GitHub repository — extends coverage into
#'         date ranges not present in the gov.uk API
#'   \item Manually compiled data — backstop for dates not covered by either
#'         online source (England & Wales 2010--2014 and 2022--2026;
#'         Scotland and Northern Ireland 2022--2026)
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
