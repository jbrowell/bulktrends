#' Download FSA IPAFFS (Trade Control & Expert System) data
#'
#' Scrapes the Food Standards Agency open data catalogue pages for the Trade
#' Control & Expert System datasets and downloads the monthly CSV files to a
#' local directory.
#'
#' @param dest_dir Path to an existing directory where files will be saved.
#' @param dataset Character vector of datasets to download. One or both of
#'   `"fnao"` (Food of Non-Animal Origin) and `"poao"` (Products of Animal
#'   Origin). Defaults to both.
#' @param from_date Optional lower bound on the file month. A `Date` object or
#'   a character string in `"YYYY-MM"` format. `NULL` (default) means no lower
#'   bound.
#' @param to_date Optional upper bound on the file month. A `Date` object or a
#'   character string in `"YYYY-MM"` format. `NULL` (default) means no upper
#'   bound.
#' @param overwrite Logical. If `FALSE` (default), files already present in
#'   `dest_dir` are skipped.
#'
#' @return Invisibly returns a character vector of paths to downloaded files.
#'
#' @details
#' CSV files are collected from the FSA open data catalogue:
#' \itemize{
#'   \item FNAO: \url{https://data.food.gov.uk/catalog/datasets/71f9bee8-b68c-4ffc-813e-901d1ac20245}
#'   \item POAO: \url{https://data.food.gov.uk/catalog/datasets/1a6ebd38-460e-4734-aa59-40fdd6b8e209}
#' }
#' Both datasets cover January 2017 to the present and are updated monthly.
#' Files whose dates cannot be parsed from the filename are always downloaded.
#'
#' Requires the \pkg{rvest} package. Install it with
#' `install.packages("rvest")` if needed.
#'
#' @examples
#' \dontrun{
#' # Download all available files for both datasets
#' download_fsa_ipaffs(dest_dir = "~/fsa_data")
#'
#' # Download FNAO only for 2024
#' download_fsa_ipaffs(
#'   dest_dir = "~/fsa_data",
#'   dataset = "fnao",
#'   from_date = "2024-01",
#'   to_date = "2024-12"
#' )
#'
#' # Update: download any new POAO files not already on disk
#' download_fsa_ipaffs(dest_dir = "~/fsa_data", dataset = "poao")
#' }
#'
#' @export
download_fsa_ipaffs <- function(
  dest_dir,
  dataset = c("fnao", "poao"),
  from_date = NULL,
  to_date = NULL,
  overwrite = FALSE
) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop(
      "Package 'rvest' is required. Install it with: install.packages('rvest')",
      call. = FALSE
    )
  }

  dataset <- match.arg(dataset, c("fnao", "poao"), several.ok = TRUE)

  from_date <- parse_yearmon(from_date, "'from_date'")
  to_date <- parse_yearmon(to_date, "'to_date'")

  if (!is.null(from_date) && !is.null(to_date) && from_date > to_date) {
    stop("'from_date' must not be later than 'to_date'.", call. = FALSE)
  }

  if (!file_test("-d", dest_dir)) {
    stop("'dest_dir' does not exist: ", dest_dir, call. = FALSE)
  }

  catalog_urls <- c(
    fnao = "https://data.food.gov.uk/catalog/datasets/71f9bee8-b68c-4ffc-813e-901d1ac20245",
    poao = "https://data.food.gov.uk/catalog/datasets/1a6ebd38-460e-4734-aa59-40fdd6b8e209"
  )

  all_links <- unique(unlist(lapply(
    catalog_urls[dataset],
    collect_fsa_csv_hrefs
  ), use.names = FALSE))

  if (length(all_links) == 0L) {
    message("No CSV files found on the FSA catalogue page(s).")
    return(invisible(character(0)))
  }

  if (!is.null(from_date) || !is.null(to_date)) {
    all_links <- filter_fsa_links_by_date(all_links, from_date, to_date)
  }

  if (length(all_links) == 0L) {
    message("No files remain after date filtering.")
    return(invisible(character(0)))
  }

  message("Found ", length(all_links), " file(s) to download.")

  downloaded <- character(0)

  for (url in all_links) {
    fname <- basename(url)
    dest_path <- file.path(dest_dir, fname)

    if (!overwrite && file.exists(dest_path)) {
      message("Skipping (already exists): ", fname)
      next
    }

    message("Downloading: ", fname)
    result <- tryCatch(
      {
        status <- utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
        identical(status, 0L)
      },
      error = function(e) {
        warning(
          "Failed to download ",
          fname,
          ": ",
          conditionMessage(e),
          call. = FALSE
        )
        FALSE
      }
    )

    if (isTRUE(result) && file.exists(dest_path)) {
      downloaded <- c(downloaded, dest_path)
    }
  }

  message("Downloaded ", length(downloaded), " file(s) to ", dest_dir)
  invisible(downloaded)
}

# Scrape an FSA catalogue page and return absolute CSV URLs.
collect_fsa_csv_hrefs <- function(url) {
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) {
      stop(
        "Could not fetch page from ",
        url,
        ".\n",
        "Check your internet connection.\nOriginal error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  hrefs <- rvest::html_attr(rvest::html_elements(page, "a"), "href")
  hrefs[
    !is.na(hrefs) &
      grepl("^https?://", hrefs) &
      grepl("\\.csv$", hrefs, ignore.case = TRUE)
  ]
}

# Parse a first-of-month Date from an FSA CSV filename.
# Returns NA_Date_ when the month/year cannot be identified.
parse_fsa_csv_date <- function(fname) {
  mon_lookup <- c(
    january = 1L, february = 2L, march = 3L, april = 4L,
    may = 5L, june = 6L, july = 7L, august = 8L,
    september = 9L, october = 10L, november = 11L, december = 12L,
    jan = 1L, feb = 2L, mar = 3L, apr = 4L,
    jun = 6L, jul = 7L, aug = 8L, sep = 9L,
    oct = 10L, nov = 11L, dec = 12L
  )

  # Match a month name (full or 3-letter) followed by an optional separator
  # then 2–4 digits for the year.
  month_names <- paste(names(mon_lookup), collapse = "|")
  pattern <- paste0("(?i)(", month_names, ")[^a-zA-Z0-9]*(\\d{2,4})")
  m <- regmatches(fname, regexec(pattern, fname, perl = TRUE))[[1L]]

  if (length(m) < 3L) {
    return(as.Date(NA))
  }

  mm <- mon_lookup[tolower(m[2L])]
  yr_str <- m[3L]
  yyyy <- if (nchar(yr_str) <= 2L) 2000L + as.integer(yr_str) else as.integer(yr_str)

  if (is.na(mm) || yyyy < 2000L || yyyy > 2100L) {
    return(as.Date(NA))
  }

  as.Date(sprintf("%04d-%02d-01", yyyy, mm))
}

filter_fsa_links_by_date <- function(links, from_date, to_date) {
  dates <- vapply(basename(links), parse_fsa_csv_date, as.Date(NA))
  parseable <- !is.na(dates)
  n_skip <- sum(!parseable)

  keep <- rep(TRUE, length(links))
  if (!is.null(from_date)) {
    keep[parseable] <- keep[parseable] & dates[parseable] >= from_date
  }
  if (!is.null(to_date)) {
    keep[parseable] <- keep[parseable] & dates[parseable] <= to_date
  }

  bounds <- paste(
    if (!is.null(from_date)) format(from_date, "%Y-%m") else "start",
    "to",
    if (!is.null(to_date)) format(to_date, "%Y-%m") else "end"
  )
  message(
    "Date filter applied (",
    bounds,
    "): keeping ",
    sum(keep),
    " of ",
    length(links),
    " file(s)."
  )
  if (n_skip > 0L) {
    message(
      "  Note: ",
      n_skip,
      " file(s) kept without date filtering ",
      "(date not recognised in filename)."
    )
  }

  links[keep]
}
