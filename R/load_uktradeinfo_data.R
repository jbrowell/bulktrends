#' Load bulk monthly data from UK Trade Info
#'
#' Load a single bulk data file, a `.zip` archive, or all files in a directory
#' (and its sub-directories) from UK Trade Info.
#'
#' @param path Path to a `.txt` file, a `.zip` archive, or a directory
#'   containing `.txt` and/or `.zip` files. Directories are searched
#'   recursively.
#'
#' @return A `data.table` of trade data with columns containing timestamps.
#'
#' @details
#' `.zip` archives are extracted to a temporary directory, read, and the
#' temporary directory is deleted before the function returns.
#'
#' This function can be slow to read a large number of files but is much faster
#' if a `future::plan()` is set that enables parallel computation.
#'
#'
#' @export
read_uktradeinfo <- function(path) {
  # route 1 - path is a single .txt file:
  if (file_test("-f", path) && !grepl("\\.zip$", path, ignore.case = TRUE)) {
    BDS <- data.table::fread(path, header = F, strip.white = F, sep = NULL)

    BDS <- BDS[, .(
      PERREF = substr(V1, 1, 6),
      TYPE = substr(V1, 7, 7),
      MONTHAC = substr(V1, 8, 13),
      COMCODE = substr(V1, 14, 21),
      SITC = substr(V1, 22, 26),
      COD_SEQ = substr(V1, 27, 29),
      COD_ALPHA = substr(V1, 30, 31),
      PORT_SEQ = substr(V1, 32, 34),
      PORT_CODE = substr(V1, 35, 37),
      COO_SEQ = substr(V1, 38, 40),
      COO_ALPHA = substr(V1, 41, 42),
      MODE_OF_TRANSPORT = substr(V1, 43, 44),
      STAT_VALUE = substr(V1, 45, 56),
      NET_MASS = substr(V1, 57, 68),
      SUMM_UNIT = substr(V1, 69, 80),
      SUPRESSION = substr(V1, 81, 81),
      FLOW = substr(V1, 82, 84),
      REC_TYPE = substr(V1, 85, 85)
    )]

    BDS[, NET_MASS := suppressWarnings(as.numeric(NET_MASS))]
    BDS[, STAT_VALUE := suppressWarnings(as.numeric(STAT_VALUE))]
    BDS[, DATE_START := as.Date(paste0(PERREF, "01"), format = "%Y%m%d")]
    BDS[, DATE_END := DATE_START + base::months(1) - lubridate::days(1)]

    return(BDS)
  } else if (
    file_test("-f", path) && grepl("\\.zip$", path, ignore.case = TRUE)
  ) {
    # route 2 - path is a single .zip file:
    tmp_dir <- tempfile(pattern = "uktradeinfo_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
    utils::unzip(path, exdir = tmp_dir)
    txt_files <- list.files(
      tmp_dir,
      pattern = "\\.txt$",
      full.names = TRUE,
      recursive = TRUE
    )
    if (length(txt_files) == 0L) {
      stop("No .txt files found inside zip: ", path)
    }
    return(data.table::rbindlist(
      lapply(txt_files, read_uktradeinfo),
      use.names = TRUE,
      fill = FALSE
    ))
  } else if (file_test("-d", path)) {
    # route 3 - path is a directory:
    files <- list.files(
      path,
      pattern = "\\.(txt|zip)$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )

    if (length(files) == 0L) {
      stop("No .txt or .zip files found in directory: ", path)
    }

    BDS_all <- data.table::rbindlist(
      future.apply::future_lapply(files, read_uktradeinfo),
      use.names = TRUE,
      fill = FALSE
    )

    data.table::setorder(BDS_all, DATE_START)

    return(BDS_all)
  } else {
    stop("path is not a file or directory.")
  }
}

#' Update a UK Trade Info dataset with new data
#'
#' Add new monthly import data to an existing dataset by reading only files
#' that cover periods not already present, or by refreshing a specified date
#' range.
#'
#' @param existing A `data.table` of existing UK Trade Info data, as returned
#'   by [read_uktradeinfo()], containing at least a `DATE_START` column.
#' @param path Path to a `.txt` file, a `.zip` archive, or a directory
#'   containing `.txt` and/or `.zip` files (searched recursively). Files whose
#'   periods are already represented in `existing` are skipped unless
#'   `date_range` is specified.
#' @param date_range Optional length-2 vector of `Date` objects (or strings
#'   coercible to `Date`) giving the inclusive start and end of a date range to
#'   refresh. When supplied, rows in `existing` whose `DATE_START` falls within
#'   this range are removed and replaced with data read from `path`.
#'
#' @return A `data.table` combining `existing` with any newly loaded data,
#'   ordered by `DATE_START`.
#'
#' @details
#' The period covered by each file is determined from its name. Two formats are
#' recognised:
#' \itemize{
#'   \item `BDSImpYYMM` (e.g. `BDSImp2404.txt`) — a single month.
#'   \item `bdsimp_mon-monYYarchive` (e.g. `bdsimp_jan-jun22archive.zip`) — a
#'     multi-month archive; the file is included if any month in the range is
#'     new or falls within `date_range`.
#' }
#' Files whose names match neither pattern are skipped with a warning.
#'
#' Two-digit years are interpreted as 1990–1999 for `YY >= 90` and
#' 2000–2089 otherwise.
#'
#' This function can be slow to read a large number of files but is much faster
#' if a `future::plan()` is set that enables parallel computation.
#'
#' @export
update_uktradeinfo <- function(existing, path, date_range = NULL) {
  if (!inherits(existing, "data.table")) {
    stop("`existing` must be a data.table.")
  }
  if (!"DATE_START" %in% names(existing)) {
    stop("`existing` must contain a DATE_START column.")
  }

  if (!is.null(date_range)) {
    if (length(date_range) != 2L) {
      stop("`date_range` must be a length-2 vector.")
    }
    date_range <- as.Date(date_range)
  }

  # Collect candidate files
  if (file_test("-f", path)) {
    candidate_files <- path
  } else if (file_test("-d", path)) {
    candidate_files <- list.files(
      path,
      pattern = "\\.(txt|zip)$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  } else {
    stop("`path` is not a file or directory.")
  }

  if (length(candidate_files) == 0L) {
    message("No .txt or .zip files found in path.")
    return(existing)
  }

  existing_periods <- unique(existing$DATE_START)

  periods <- lapply(candidate_files, .uktradeinfo_period_from_filename)
  unrecognised <- candidate_files[vapply(
    periods,
    function(p) is.na(p$from),
    logical(1L)
  )]
  if (length(unrecognised) > 0L) {
    warning(
      "Skipping ",
      length(unrecognised),
      " file(s) with unrecognised filename format:\n",
      paste0("  ", basename(unrecognised), collapse = "\n"),
      call. = FALSE
    )
  }
  candidate_files <- candidate_files[vapply(
    periods,
    function(p) !is.na(p$from),
    logical(1L)
  )]
  periods <- periods[vapply(periods, function(p) !is.na(p$from), logical(1L))]

  if (!is.null(date_range)) {
    keep <- vapply(
      periods,
      function(period) {
        period$from <= date_range[2] & period$to >= date_range[1]
      },
      logical(1L)
    )
    existing <- existing[
      !(DATE_START >= date_range[1] & DATE_START <= date_range[2])
    ]
  } else {
    keep <- vapply(
      periods,
      function(period) {
        months_covered <- seq(period$from, period$to, by = "month")
        any(!(months_covered %in% existing_periods))
      },
      logical(1L)
    )
  }

  new_files <- candidate_files[keep]

  if (length(new_files) == 0L) {
    message("No new data to add.")
    return(existing)
  }

  message(sprintf("Reading %d new file(s):", length(new_files)))
  message(paste0("  ", basename(new_files), collapse = "\n"))

  new_data <- data.table::rbindlist(
    future.apply::future_lapply(new_files, read_uktradeinfo),
    use.names = TRUE,
    fill = FALSE
  )

  result <- data.table::rbindlist(
    list(existing, new_data),
    use.names = TRUE,
    fill = FALSE
  )
  data.table::setorder(result, DATE_START)

  return(result)
}

# Parse the period covered by an HMRC bulk-data filename.
# Returns list(from = Date, to = Date).  Both are NA when the name is not
# recognised.  Three formats are handled:
#   BDSImpYYMM              → single month  (from == to)
#   bdsimp_mon-monYYarchive → semi-annual range
#   bdsimp_YYarchive        → full-year range
.uktradeinfo_period_from_filename <- function(filename) {
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
  base <- tools::file_path_sans_ext(basename(filename))

  # Semi-annual / annual archive: _mon-monYYarchive
  parts <- regmatches(
    base,
    regexec("_([a-zA-Z]{3})-([a-zA-Z]{3})(\\d{2})archive$", base, perl = TRUE)
  )[[1L]]
  if (length(parts) == 4L) {
    m1 <- mon_lookup[tolower(parts[2L])]
    m2 <- mon_lookup[tolower(parts[3L])]
    yy <- as.integer(parts[4L])
    if (!is.na(m1) && !is.na(m2)) {
      year <- ifelse(yy >= 90L, 1900L + yy, 2000L + yy)
      from <- as.Date(sprintf("%04d-%02d-01", year, m1))
      to <- if (m2 == 12L) {
        as.Date(sprintf("%04d-12-31", year))
      } else {
        as.Date(sprintf("%04d-%02d-01", year, m2 + 1L)) - 1L
      }
      return(list(from = from, to = to))
    }
  }

  # Year-only archive: _YYarchive (e.g. bdsimp_18archive)
  parts <- regmatches(
    base,
    regexec("_(\\d{2})archive$", base, perl = TRUE)
  )[[1L]]
  if (length(parts) == 2L) {
    yy <- as.integer(parts[2L])
    year <- ifelse(yy >= 90L, 1900L + yy, 2000L + yy)
    return(list(
      from = as.Date(sprintf("%04d-01-01", year)),
      to = as.Date(sprintf("%04d-12-31", year))
    ))
  }

  # Standard YYMM: four digits at end of stem
  m <- regmatches(base, regexpr("[0-9]{4}$", base))
  if (length(m) == 0L) {
    return(na_result)
  }
  yy <- as.integer(substr(m, 1L, 2L))
  mm <- as.integer(substr(m, 3L, 4L))
  if (mm < 1L || mm > 12L) {
    return(na_result)
  }
  year <- ifelse(yy >= 90L, 1900L + yy, 2000L + yy)
  from <- as.Date(paste0(year, sprintf("%02d", mm), "01"), format = "%Y%m%d")
  to <- if (mm == 12L) {
    as.Date(sprintf("%04d-12-31", year))
  } else {
    as.Date(sprintf("%04d-%02d-01", year, mm + 1L)) - 1L
  }
  list(from = from, to = to)
}
