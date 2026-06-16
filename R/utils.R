#' Detect date frequency
#'
#' This function detects the frequency of a time series.
#'
#' @param dates A vector of dates
#'
#' @return A character string indicating the detected date frequency: "day", "week", or "month".
#'
#' @export
detect_date_frequency <- function(dates) {
  if (!inherits(dates, "Date")) {
    dates <- as.Date(dates)
  }

  # Remove duplicates and sort
  dates <- sort(unique(dates))

  if (length(dates) < 2) {
    stop("At least 2 unique dates are required to detect frequency.")
  }

  median_gap <- median(as.numeric(diff(dates)))

  if (median_gap <= 3) {
    return("day")
  } else if (median_gap == 7) {
    return("week")
  } else if (median_gap >= 28 & median_gap <= 31) {
    return("month")
  } else {
    stop("Couldn't detect date frequency.")
  }
}

#' Extract of monthly or daily time series for a given commodity code
#'
#' This function extracts the sum of a given quantity from import_data for
#' any level of hierarchy (HS2, HS4, HS6 and CN8) found in the dataset.
#'
#' @param import_data A `data.table` containing trade data. Must include columns
#' `COMCODE` and specified `quantity`.
#' @param code A character string representing any HS2/HS4/HS6/CN8 code.
#' @param date_col Name of column containing timestamps.
#' @param quantity Quantity to be extracted and aggregated as time series, e.g.
#' `NET_MASS` or `STAT_VALUE` or `volume`.
#' @param fill_missing This function returns a continuous time series. Values
#' for missing dates are filled with this value.
#' @param freq Frequency of time series data. See details.
#'
#' @return A `data.table` with date and quantity columns.
#'
#' @details Daily or monthly data is expected and detected automatically. Missing
#' values are filled.
#'
#' By default, `freq=NULL` and will be detected automatically. However, this may
#' fail for sparse data, in which case `freq` should be set manually. Options are
#' "day", "week", or "month".
#'
#'
#' @export
extract_ts <- function(
  import_data,
  code,
  date_col = "DATE_START",
  quantity = "NET_MASS",
  fill_missing = NA,
  freq = NULL
) {
  if (!inherits(import_data, "data.table")) {
    import_data <- as.data.table(import_data)
  }

  import_data <- copy(import_data[substr(COMCODE, 1, nchar(code)) == code])

  if (import_data[, any(is.na(get(date_col)))]) {
    import_data <- import_data[!is.na(get(date_col))]
    warning("Some timestamps are NA and have been omitted.")
  }

  if (quantity == "volume") {
    ts_data <- import_data[, .(volume = .N), by = date_col]
  } else {
    ts_data <- import_data[,
      .(agg = sum(get(quantity), na.rm = T)),
      by = date_col
    ]
    setnames(ts_data, "agg", quantity)
  }

  if (is.null(freq)) {
    freq <- detect_date_frequency(ts_data[[date_col]])
  } else {
    if (!freq %in% c("day", "week", "month")) {
      stop("\"freq\" must be \"day\",\"week\" or \"month\"")
    }
  }

  complete_seq <- ts_data[,
    seq(
      min(get(date_col)),
      max(get(date_col)),
      by = freq
    )
  ]

  missing_data <- data.table()
  missing_data[,
    (date_col) := complete_seq[!complete_seq %in% ts_data[, get(date_col)]]
  ]
  missing_data[, (quantity) := fill_missing]

  ts_data <- rbind(ts_data, missing_data)

  return(ts_data[order(get(date_col))])
}


#' Add date features
#'
#' A function that identifies the features (bank holidays) of dates provided, and
#' creates the linear_trend covariates (formerly in select_best_model() ) based on date stamp not row number.
#'
#' @param data A `data.table` containing trade data.
#' @param date_col Name of column containing timestamps.
#' @param division UK division for bank holiday lookup. One of
#'   `"england-and-wales"` (default), `"scotland"`, or `"northern-ireland"`.
#' @param holidays A `data.frame` or `data.table` of bank holidays with at least
#'   `date` and `title` columns, as returned by [get_uk_bank_holidays()].
#'   Defaults to the bundled [uk_bank_holidays] dataset.
#' @param freq Frequency of the input time series. If `NULL` it will be
#'   auto-detected (`'day'`, `'week'`, or `'month'`).
#'
#' @return A `data.table` with the original data and additional calendar features, including
#' day of week, day of year, `holiday` (holiday title or `NA`), and `is_holiday` (`logical`).
#'
#' @details
#' Bank holiday data is sourced from the bundled [uk_bank_holidays] dataset,
#' which was compiled from the official gov.uk published data
#' (\url{https://www.gov.uk/bank-holidays.json}).  The stored dataset includes
#' substitute days and one-off special bank holidays.
#'
#' To use the latest published data call [get_uk_bank_holidays()] and pass the
#' result via the `holidays` argument.
#'
#' @export
add_date_features <- function(
  data,
  date_col,
  division = "england-and-wales",
  holidays = NULL,
  freq = NULL
) {
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }

  # Ensure the date column is Date type
  if (!inherits(data[[date_col]], "Date")) {
    stop("Input must be of class \"Date\".")
  }

  # Auto-detect freq if not provided
  if (is.null(freq)) {
    freq <- detect_date_frequency(data[[date_col]])
  }

  # Warn about ignored arguments for non-daily data
  if (freq != "day") {
    if (!is.null(holidays) || division != "england-and-wales") {
      warning(
        "`division` and `holidays` are ignored for non-daily data.",
        call. = FALSE
      )
    }
  }

  dates <- data[[date_col]]

  # Linear trend: elapsed days since the first observation
  data[, linear_trend := as.numeric(dates - min(dates))]

  data[, day_of_year := as.integer(format(dates, "%j"))]
  data[, annual_sin := sin(2 * pi * day_of_year / 365)]
  data[, annual_cos := cos(2 * pi * day_of_year / 365)]

  # Daily-only features
  if (freq == "day") {
    if (is.null(holidays)) {
      holidays <- uk_bank_holidays
    }

    division <- match.arg(
      division,
      c("england-and-wales", "scotland", "northern-ireland")
    )

    div_holidays <- holidays[holidays$division == division, ]

    # Warn if any dates fall outside the coverage of the holidays dataset
    # dates <- data[[date_col]]
    coverage_min <- min(div_holidays$date)
    coverage_max <- max(div_holidays$date)
    out_of_range <- dates[
      !is.na(dates) & (dates < coverage_min | dates > coverage_max)
    ]
    if (length(out_of_range) > 0) {
      warning(
        length(out_of_range),
        " date(s) in `data` fall outside the coverage of the holidays dataset for '",
        division,
        "' (",
        coverage_min,
        " to ",
        coverage_max,
        "). ",
        "UK holidays will be marked NA for those dates. ",
        "Use get_uk_bank_holidays() to refresh via the `holidays` argument.",
        call. = FALSE
      )
    }

    # Build a named vector: date (as character) -> holiday title
    holiday_lookup <- setNames(
      div_holidays$title,
      as.character(div_holidays$date)
    )

    data[, day_of_week := weekdays(dates)]
    # data$day_of_year <- as.integer(format(dates, "%j"))
    data[, holiday := unname(holiday_lookup[as.character(dates)])]
    data[, is_holiday := !is.na(holiday)]
  }

  return(data)
}


#' Get UK Bank Holidays from gov.uk
#'
#' Fetches the latest UK bank holidays from the official gov.uk API
#' (\url{https://www.gov.uk/bank-holidays.json}) and returns them as a
#' `data.table` in the same format as the bundled [uk_bank_holidays] dataset.
#'
#' @return A `data.table` with columns `date`, `title`, `notes`, and `division`.
#'   Rows are ordered by `division` then `date`.
#'
#' @details
#' The function requires an internet connection.  If the request fails (e.g.
#' when offline), an informative error is raised and you can fall back to the
#' bundled [uk_bank_holidays] dataset instead.
#'
#' The returned data can be passed to [add_date_features()] via its `holidays`
#' argument, or used to refresh the package dataset with
#' `usethis::use_data(get_uk_bank_holidays(), overwrite = TRUE)`.
#'
#' @examples
#' \dontrun{
#' holidays <- get_uk_bank_holidays()
#' head(holidays)
#' }
#'
#' @export
get_uk_bank_holidays <- function() {
  url <- "https://www.gov.uk/bank-holidays.json"

  raw <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) {
      stop(
        "Could not fetch bank holidays from ",
        url,
        ".\n",
        "Check your internet connection or use the bundled `uk_bank_holidays` ",
        "dataset instead.\nOriginal error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  records <- list()
  for (div in names(raw)) {
    div_data <- raw[[div]]
    if (!is.null(div_data$events)) {
      for (ev in div_data$events) {
        records[[length(records) + 1]] <- data.frame(
          date = as.Date(ev$date),
          title = ev$title,
          notes = if (is.null(ev$notes)) "" else ev$notes,
          division = div,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  result <- rbindlist(records)
  setorder(result, division, date)
  result
}

#' View User Guide
#'
#' A function that opens the `bulktrends` user guide in system browser.
#'
#' @param path Optional. Path to specific instance of `UserGuide.html`. If `NULL`, it will be retrieved from the current installation of `bulktrends`.
#'
#' @export
open_userguide <- function(path = NULL) {
  if (is.null(path)) {
    path <- try(
      system.file("docs", "index.html", package = "bulktrends")
    )
  }

  if (file.exists(path)) {
    browseURL(path)
  } else {
    stop("Couldn't find UserGuide.html")
  }
}
