#' Download UK Trade Info bulk data sets
#'
#' Scrapes the UK Trade Info latest bulk data sets page and archive page, then
#' downloads matching ZIP files to a local directory.
#'
#' @param dest_dir Path to an existing directory where files will be saved.
#' @param type Character vector of data types to download. One or more of
#'   `"imports"`, `"exports"`, `"control"`, `"importer_details"`,
#'   `"exporter_details"`, `"preference"`. Defaults to `"imports"`.
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
#' ZIP files are collected from two pages:
#' \itemize{
#'   \item \url{https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/}
#'         — carries the most recent files before they appear in the archive.
#'   \item \url{https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/}
#'         — historical archive.
#' }
#' Duplicate links across the two pages are removed before downloading.
#'
#' File name patterns used to identify data types:
#' \itemize{
#'   \item `"imports"` — monthly import archives (`BDSImpYYMM`)
#'   \item `"exports"` — monthly export archives (`BDSExpYYMM`)
#'   \item `"control"` — commodity code and unit reference files (`SMKA12`)
#'   \item `"importer_details"` — UK/GB importer name and address files (`BDSImpDet`)
#'   \item `"exporter_details"` — UK/GB exporter name and address files (`BDSExpDet`)
#'   \item `"preference"` — import data by trade preference/regime (`BDSPref`)
#' }
#'
#' Date filtering uses the YYMM suffix embedded in HMRC filenames
#' (e.g. `BDSImp2401.zip` → January 2024). Files whose dates cannot be parsed
#' from the filename are always kept.
#'
#' **Semi-annual coverage gap filling.** The most recent 6-month file (e.g.
#' `BDSImp_jan-jun26.zip`) is often published before all 6 months of data are
#' available. After the main download, the function inspects the actual content
#' of the most recent semi-annual file for each requested data type and compares
#' it against the latest single-month files available on the latest bulk-data
#' page. Any months that are present on the latest page but absent from the
#' semi-annual file are downloaded automatically. This ensures that the local
#' copy is always as up-to-date as the source.
#'
#' Requires the \pkg{rvest} package. Install it with
#' `install.packages("rvest")` if needed.
#'
#' @examples
#' \dontrun{
#' # Download all available import archives
#' download_uktradeinfo_bulk(dest_dir = "~/trade_data", type = "imports")
#'
#' # Download imports for 2024 only
#' download_uktradeinfo_bulk(
#'   dest_dir = "~/trade_data",
#'   type = "imports",
#'   from_date = "2024-01",
#'   to_date = "2024-12"
#' )
#'
#' # Download imports and the control file from 2023 onwards
#' download_uktradeinfo_bulk(
#'   dest_dir = "~/trade_data",
#'   type = c("imports", "control"),
#'   from_date = "2023-01"
#' )
#' }
#'
#' @export
download_uktradeinfo_bulk <- function(
  dest_dir,
  type = "imports",
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

  type <- match.arg(
    type,
    c(
      "imports",
      "exports",
      "control",
      "importer_details",
      "exporter_details",
      "preference"
    ),
    several.ok = TRUE
  )

  from_date <- parse_yearmon(from_date, "'from_date'")
  to_date <- parse_yearmon(to_date, "'to_date'")

  if (!is.null(from_date) && !is.null(to_date) && from_date > to_date) {
    stop("'from_date' must not be later than 'to_date'.", call. = FALSE)
  }

  if (!file_test("-d", dest_dir)) {
    stop("'dest_dir' does not exist: ", dest_dir, call. = FALSE)
  }

  base_url <- "https://www.uktradeinfo.com"
  latest_url <- paste0(base_url, "/trade-data/latest-bulk-data-sets/")
  archive_url <- paste0(
    base_url,
    "/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/"
  )

  collect_zip_hrefs <- function(url, required = TRUE) {
    page <- tryCatch(
      rvest::read_html(url),
      error = function(e) {
        msg <- paste0(
          "Could not fetch page from ",
          url,
          ".\n",
          "Check your internet connection.\nOriginal error: ",
          conditionMessage(e)
        )
        if (required) {
          stop(msg, call. = FALSE)
        } else {
          warning(msg, call. = FALSE)
          return(NULL)
        }
      }
    )
    if (is.null(page)) {
      return(character(0))
    }
    hrefs <- rvest::html_attr(rvest::html_elements(page, "a"), "href")
    hrefs[!is.na(hrefs) & grepl("\\.zip$", hrefs, ignore.case = TRUE)]
  }

  latest_hrefs <- collect_zip_hrefs(latest_url, required = FALSE)
  archive_hrefs <- collect_zip_hrefs(archive_url, required = TRUE)
  zip_hrefs <- unique(c(latest_hrefs, archive_hrefs))
  all_links <- paste0(base_url, zip_hrefs)
  latest_links <- paste0(base_url, latest_hrefs)

  patterns <- list(
    imports = "(?i)bdsimp(?!det)",
    exports = "(?i)bdsexp(?!det)",
    control = "(?i)smka12",
    importer_details = "(?i)bdsimpdet",
    exporter_details = "(?i)bdsexpdet",
    preference = "(?i)bdspref"
  )

  combined_pattern <- paste(unname(unlist(patterns[type])), collapse = "|")
  matched_links <- all_links[
    grepl(combined_pattern, basename(all_links), perl = TRUE)
  ]

  if (length(matched_links) == 0) {
    message(
      "No matching files found for type(s): ",
      paste(type, collapse = ", ")
    )
    return(invisible(character(0)))
  }

  if (!is.null(from_date) || !is.null(to_date)) {
    matched_links <- filter_links_by_date(matched_links, from_date, to_date)
  }

  if (length(matched_links) == 0) {
    message("No files remain after date filtering.")
    return(invisible(character(0)))
  }

  message("Found ", length(matched_links), " file(s) to download.")

  downloaded <- character(0)

  for (url in matched_links) {
    fname <- basename(url)
    dest_path <- file.path(dest_dir, fname)

    if (!overwrite && file.exists(dest_path)) {
      message("Skipping (already exists): ", fname)
      next
    }

    message("Downloading: ", fname)
    result <- tryCatch(
      {
        utils::download.file(
          url,
          destfile = dest_path,
          mode = "wb",
          quiet = TRUE
        )
        TRUE
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

  extra <- download_coverage_gaps(dest_dir, type, latest_links, overwrite, to_date)
  downloaded <- c(downloaded, extra)

  dups <- check_bulk_coverage(dest_dir)
  if (nrow(dups) > 0L && interactive()) {
    ans <- readline("Delete redundant monthly files (recommended)? [y/N]: ")
    if (tolower(trimws(ans)) %in% c("y", "yes")) {
      to_delete <- unique(file.path(dest_dir, dups$monthly_file))
      to_delete <- to_delete[file.exists(to_delete)]
      file.remove(to_delete)
      message("Deleted ", length(to_delete), " file(s).")
    }
  }

  invisible(downloaded)
}

# Parse NULL / Date / "YYYY-MM" → first-of-month Date, or stop()
parse_yearmon <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "Date")) {
    return(as.Date(format(x, "%Y-%m-01")))
  }
  if (is.character(x) && length(x) == 1L) {
    d <- tryCatch(as.Date(paste0(x, "-01")), error = function(e) NA)
    if (!is.na(d)) return(d)
  }
  stop(
    arg,
    " must be NULL, a Date, or a 'YYYY-MM' string (e.g. \"2024-01\").",
    call. = FALSE
  )
}

# Return the coverage period of an HMRC filename as list(from, to) Dates.
#
# Recognised formats (trailing portion before ".zip"):
#   _mon-monYY[archive]  e.g. _jan-jun26archive  →  semi-annual
#   YYMM[archive]        e.g. 2602                →  monthly  (from = to = first of month)
#   YYYY[archive]        e.g. 2026                →  annual   (from = Jan 1, to = Dec 31)
#   YY[archive]          e.g. 26                  →  annual   (from = Jan 1, to = Dec 31)
#
# Returns list(from = NA_Date, to = NA_Date) when the pattern is not recognised.
extract_file_coverage <- function(fname) {
  na_result <- list(from = as.Date(NA), to = as.Date(NA))
  mon_lookup <- c(
    jan = 1L,
    feb = 2L,
    mar = 3L,
    apr = 4L,
    may = 5L,
    jun = 6L,
    jul = 7L,
    aug = 8L,
    sep = 9L,
    oct = 10L,
    nov = 11L,
    dec = 12L
  )

  # Semi-annual: _mon-monYY[archive].zip
  parts <- regmatches(
    fname,
    regexec(
      "_([a-zA-Z]{3})-([a-zA-Z]{3})(\\d{2})(?:archive)?\\.zip$",
      fname,
      perl = TRUE
    )
  )[[1L]]
  if (length(parts) == 4L) {
    m1 <- mon_lookup[tolower(parts[2L])]
    m2 <- mon_lookup[tolower(parts[3L])]
    yy <- as.integer(parts[4L])
    if (!is.na(m1) && !is.na(m2)) {
      yyyy <- 2000L + yy
      from <- as.Date(sprintf("%04d-%02d-01", yyyy, m1))
      to <- if (m2 == 12L) {
        as.Date(sprintf("%04d-12-31", yyyy))
      } else {
        as.Date(sprintf("%04d-%02d-01", yyyy, m2 + 1L)) - 1L
      }
      return(list(from = from, to = to))
    }
  }

  # Numeric suffix: YYMM / YYYY / YY
  m <- regmatches(
    fname,
    regexpr("(\\d+)(?:archive)?\\.zip$", fname, perl = TRUE)
  )
  if (length(m) == 0L) {
    return(na_result)
  }

  digits_str <- regmatches(m, regexpr("^\\d+", m))
  n <- nchar(digits_str)

  if (n == 4L) {
    yy <- as.integer(substr(digits_str, 1L, 2L))
    mm <- as.integer(substr(digits_str, 3L, 4L))
    if (mm >= 1L && mm <= 12L) {
      from <- as.Date(sprintf("20%02d-%02d-01", yy, mm))
      to <- if (mm == 12L) {
        as.Date(sprintf("20%02d-12-31", yy))
      } else {
        as.Date(sprintf("20%02d-%02d-01", yy, mm + 1L)) - 1L
      }
      return(list(from = from, to = to))
    } else {
      yyyy <- as.integer(digits_str)
      return(list(
        from = as.Date(sprintf("%04d-01-01", yyyy)),
        to = as.Date(sprintf("%04d-12-31", yyyy))
      ))
    }
  }

  if (n == 2L) {
    yyyy <- 2000L + as.integer(digits_str)
    return(list(
      from = as.Date(sprintf("%04d-01-01", yyyy)),
      to = as.Date(sprintf("%04d-12-31", yyyy))
    ))
  }

  na_result
}

# Open a ZIP and return its actual date coverage from internal filenames.
# Internal files follow YYMM naming (e.g. BDSImp2601.csv → Jan 2026).
# Returns list(from = min_date, to = max_date), or NAs if none are parseable.
extract_zip_coverage <- function(zip_path) {
  na_result <- list(from = as.Date(NA), to = as.Date(NA))
  contents <- tryCatch(
    unzip(zip_path, list = TRUE)$Name,
    error = function(e) NULL
  )
  if (is.null(contents) || length(contents) == 0L) {
    return(na_result)
  }

  parse_yymm <- function(fname) {
    m <- regmatches(basename(fname), regexpr("\\d{4}", basename(fname)))
    if (length(m) == 0L) {
      return(as.Date(NA))
    }
    yy <- as.integer(substr(m, 1L, 2L))
    mm <- as.integer(substr(m, 3L, 4L))
    if (mm < 1L || mm > 12L) {
      return(as.Date(NA))
    }
    as.Date(sprintf("20%02d-%02d-01", yy, mm))
  }

  dates <- vapply(contents, parse_yymm, as.Date(NA))
  dates <- dates[!is.na(dates)]
  if (length(dates) == 0L) {
    return(na_result)
  }
  list(from = min(dates), to = max(dates))
}

# Returns a data.frame(monthly_file, archive_file) of duplicate-coverage pairs.
# zip_paths must be full file paths; basenames are used in the returned data frame.
find_duplicate_coverage <- function(zip_paths) {
  fnames <- basename(zip_paths)
  coverage <- lapply(zip_paths, extract_zip_coverage)
  file_from <- vapply(coverage, `[[`, as.Date(NA), "from")
  file_to <- vapply(coverage, `[[`, as.Date(NA), "to")

  is_monthly <- !is.na(file_from) & (file_from == file_to)
  is_archive <- !is.na(file_from) & (file_from != file_to)

  rows <- list()
  for (i in which(is_monthly)) {
    for (j in which(is_archive)) {
      if (file_from[i] >= file_from[j] && file_to[i] <= file_to[j]) {
        rows[[length(rows) + 1L]] <- data.frame(
          monthly_file = fnames[i],
          archive_file = fnames[j],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      monthly_file = character(),
      archive_file = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

filter_links_by_date <- function(links, from_date, to_date) {
  coverage <- lapply(basename(links), extract_file_coverage)
  file_from <- vapply(coverage, `[[`, as.Date(NA), "from")
  file_to <- vapply(coverage, `[[`, as.Date(NA), "to")
  parseable <- !is.na(file_from)
  n_skip <- sum(!parseable)

  keep <- rep(TRUE, length(links))
  # Overlap: keep if the file's coverage period intersects [from_date, to_date]
  if (!is.null(from_date)) {
    keep[parseable] <- keep[parseable] & file_to[parseable] >= from_date
  }
  if (!is.null(to_date)) {
    keep[parseable] <- keep[parseable] & file_from[parseable] <= to_date
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

# Internal: detect and download monthly files that are missing from the most
# recent semi-annual file for each requested data type.
#
# After the main download pass, the most recent semi-annual ZIP for each type
# is inspected to determine its *actual* data coverage (by reading the names of
# the files inside the ZIP).  Any monthly files available on the latest-bulk-
# data page that cover months after the semi-annual file's actual coverage end
# are then downloaded automatically.
#
# @param dest_dir      Path to directory that already holds the downloaded ZIPs.
# @param type          Character vector of data types (same as main function).
# @param latest_links  Full URLs scraped from the latest-bulk-data-sets page.
# @param overwrite     Passed through — if FALSE, already-present files are
#                      skipped (but still included in the returned paths).
# @param to_date       Optional Date; gap files later than this are ignored.
#
# @return Character vector of paths to additionally downloaded files.
download_coverage_gaps <- function(
  dest_dir,
  type,
  latest_links,
  overwrite = FALSE,
  to_date = NULL
) {
  if (length(latest_links) == 0L) {
    return(character(0))
  }

  type_patterns <- list(
    imports          = "(?i)bdsimp(?!det)",
    exports          = "(?i)bdsexp(?!det)",
    control          = "(?i)smka12",
    importer_details = "(?i)bdsimpdet",
    exporter_details = "(?i)bdsexpdet",
    preference       = "(?i)bdspref"
  )

  # Regex that matches semi-annual filenames (e.g. BDSImp_jan-jun26.zip)
  semi_pattern <- "_[a-zA-Z]{3}-[a-zA-Z]{3}\\d{2}(?:archive)?\\.zip$"

  extra_downloaded <- character(0)

  for (t in type) {
    pat <- type_patterns[[t]]

    # Find semi-annual ZIPs already present in dest_dir for this type
    all_zips <- list.files(
      dest_dir,
      pattern = "\\.zip$",
      ignore.case = TRUE,
      full.names = TRUE
    )
    type_zips <- all_zips[grepl(pat, basename(all_zips), perl = TRUE)]
    semi_zips <- type_zips[grepl(semi_pattern, basename(type_zips), perl = TRUE)]

    if (length(semi_zips) == 0L) next

    # Select the most recent semi-annual file based on filename start date
    semi_file_from <- vapply(
      lapply(basename(semi_zips), extract_file_coverage),
      `[[`, as.Date(NA), "from"
    )
    if (all(is.na(semi_file_from))) next
    most_recent_semi <- semi_zips[which.max(semi_file_from)]

    # Determine what months the semi-annual ZIP actually contains
    actual_cov <- extract_zip_coverage(most_recent_semi)
    if (is.na(actual_cov$to)) next

    # Monthly links on the latest page for this type
    type_latest <- latest_links[grepl(pat, basename(latest_links), perl = TRUE)]
    monthly_latest <- type_latest[
      !grepl(semi_pattern, basename(type_latest), perl = TRUE)
    ]

    if (length(monthly_latest) == 0L) next

    # Start dates of those monthly files (from filenames)
    monthly_from <- vapply(
      lapply(basename(monthly_latest), extract_file_coverage),
      `[[`, as.Date(NA), "from"
    )

    # Gap: monthly files that start after the semi-annual file's actual end
    gap_idx <- which(!is.na(monthly_from) & monthly_from > actual_cov$to)

    # Respect upper date bound supplied by the caller
    if (!is.null(to_date)) {
      gap_idx <- gap_idx[monthly_from[gap_idx] <= to_date]
    }

    if (length(gap_idx) == 0L) next

    gap_links  <- monthly_latest[gap_idx]
    gap_months <- format(monthly_from[gap_idx], "%Y-%m")

    message(
      "Coverage gap detected in '", t, "': ",
      basename(most_recent_semi),
      " contains data up to ", format(actual_cov$to, "%Y-%m"),
      " but monthly file(s) available for: ",
      paste(gap_months, collapse = ", "),
      ". Downloading missing file(s)."
    )

    for (url in gap_links) {
      fname     <- basename(url)
      dest_path <- file.path(dest_dir, fname)

      if (!overwrite && file.exists(dest_path)) {
        message("Skipping (already exists): ", fname)
        extra_downloaded <- c(extra_downloaded, dest_path)
        next
      }

      message("Downloading gap file: ", fname)
      result <- tryCatch(
        {
          utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
          TRUE
        },
        error = function(e) {
          warning(
            "Failed to download ", fname, ": ", conditionMessage(e),
            call. = FALSE
          )
          FALSE
        }
      )
      if (isTRUE(result) && file.exists(dest_path)) {
        extra_downloaded <- c(extra_downloaded, dest_path)
      }
    }
  }

  extra_downloaded
}


#' Check downloaded bulk data files for duplicate coverage
#'
#' Scans a directory of downloaded UK Trade Info bulk data ZIP files and
#' identifies individual monthly files whose data is already contained in a
#' broader archive file (annual or semi-annual). Optionally deletes the
#' redundant monthly files.
#'
#' A monthly file (e.g. `BDSImp2602.zip`) is considered redundant when an
#' archive file present in the same directory is found to contain the same
#' month's data. Coverage is determined by inspecting the filenames inside each
#' ZIP, so partially-built archives (e.g. a `jan-jun` file that currently only
#' contains January and February) are handled correctly.
#'
#' @param dest_dir Path to the directory containing downloaded ZIP files.
#' @param delete Logical. If `TRUE`, redundant monthly files are deleted and
#'   the archive files are retained. Defaults to `FALSE` (report only).
#'
#' @return Invisibly returns a data frame with columns \code{monthly_file} and
#'   \code{archive_file} identifying each duplicate pair. Returns an empty
#'   data frame if no duplicates are found.
#'
#' @seealso [download_uktradeinfo_bulk()]
#'
#' @export
check_bulk_coverage <- function(dest_dir, delete = FALSE) {
  if (!file_test("-d", dest_dir)) {
    stop("'dest_dir' does not exist: ", dest_dir, call. = FALSE)
  }

  zip_paths <- list.files(
    dest_dir,
    pattern = "\\.zip$",
    ignore.case = TRUE,
    full.names = TRUE
  )

  if (length(zip_paths) == 0L) {
    message("No ZIP files found in ", dest_dir)
    return(invisible(
      data.frame(
        monthly_file = character(),
        archive_file = character(),
        stringsAsFactors = FALSE
      )
    ))
  }

  dups <- find_duplicate_coverage(zip_paths)

  if (nrow(dups) == 0L) {
    message("No duplicate coverage found in ", dest_dir)
    return(invisible(dups))
  }

  message("Found ", nrow(dups), " file(s) with duplicate coverage:")
  for (i in seq_len(nrow(dups))) {
    message(
      "  ",
      dups$monthly_file[i],
      "  (covered by ",
      dups$archive_file[i],
      ")"
    )
  }

  if (delete) {
    to_delete <- unique(dups$monthly_file)
    for (f in to_delete) {
      path <- file.path(dest_dir, f)
      if (file.exists(path)) {
        file.remove(path)
        message("Deleted: ", f)
      }
    }
  }

  invisible(dups)
}
