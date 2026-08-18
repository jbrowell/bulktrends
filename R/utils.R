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
#' @param data A `data.table` containing trade data. Must include columns
#' `COMCODE` and specified `quantity`.
#' @param code Any single or groups of HS2/HS4/HS6/CN8 code.
#' @param date_col Name of column containing timestamps.
#' @param response_col Quantity to be extracted and aggregated as time series, e.g.
#' `NET_MASS` or `STAT_VALUE` or `volume`. NA are omitted when summing values.
#' @param fill_missing This function returns a continuous time series. Values
#' for missing dates are filled with this value.
#' @param freq Frequency of time series data. See details.
#' @param group_by Optional vector of column names used to create
#' separate time series. For example, use `"PORT_CODE"` to return one series
#' per port, or `c("PORT_CODE", "COO_ALPHA")` for one series per port-country of origin
#' combination. Default set to `NULL`, which returns time series for the selected
#' commodity code.
#' @param return_list If `TRUE`, returns a list of completed time
#' series per code and group. If `FALSE`, returns one combined `data.table`.
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
  data,
  code,
  date_col = "DATE_START",
  response_col = "NET_MASS",
  fill_missing = NA,
  freq = NULL,
  group_by = NULL,
  return_list = FALSE
) {
  if (length(code) > 1) {
    ts_list <- vector("list", length(code))
    names(ts_list) <- code

    for (i in seq_along(code)) {
      #ts_list[[i]]
      result <- try(
        extract_ts(
          data = data,
          code = code[i],
          date_col = date_col,
          response_col = response_col,
          fill_missing = fill_missing,
          freq = freq,
          group_by = group_by,
          return_list = return_list
        ),
        silent = T
      )

      if (inherits(result, "try-error")) {
        message("Skipping code", code[i], ":", result)
        next
      }

      ts_list[[i]] <- result
    }

    if (return_list) {
      return(unlist(ts_list, recursive = FALSE))
    }
    return(rbindlist(ts_list)) #, idcol = "code"))
  }

  #filter by code
  data <- copy(data[substr(COMCODE, 1, nchar(code)) == code])

  if (data[, any(is.na(get(date_col)))]) {
    data <- data[!is.na(get(date_col))]
    warning("Some timestamps are NA and have been omitted.")
  }

  value_col <- response_col

  if (response_col == "volume") {
    ts_data <- data[,
      .(volume = .N),
      by = c(group_by, date_col)
    ]
  } else {
    ts_data <- data[,
      .(agg = sum(get(response_col), na.rm = TRUE)),
      by = c(group_by, date_col)
    ]
    setnames(ts_data, "agg", response_col)
  }

  if (is.null(freq)) {
    freq <- detect_date_frequency(ts_data[[date_col]])
  } else if (!freq %in% c("day", "week", "month")) {
    stop(
      '"freq" must be "day", "week", "month" or NULL. If NULL, "freq" will be detected automatically.'
    )
  }

  #identical dates for all groups
  complete_seq <- seq(
    min(ts_data[[date_col]]),
    max(ts_data[[date_col]]),
    by = freq
  )

  if (is.null(group_by)) {
    series <- list(ts_data)
  } else {
    series <- split(ts_data, by = group_by, keep.by = TRUE)
  }

  # handle missing data and date order for each group using lapply
  series <- lapply(series, function(x) {
    missing_data <- data.table()
    missing_data[,
      (date_col) := complete_seq[
        !complete_seq %in% x[[date_col]]
      ]
    ]

    if (!is.null(group_by)) {
      for (group_col in group_by) {
        missing_data[, (group_col) := x[[group_col]][1]]
      }
    }

    missing_data[, (value_col) := fill_missing]
    x <- rbind(x, missing_data)[order(get(date_col))]
  })

  for (i in seq_along(series)) {
    series[[i]][, code := code]
    setcolorder(series[[i]], "code")
  }

  if (return_list) {
    return(series)
  }

  return(rbindlist(series))
}


#' Add date features
#'
#' A function that identifies the features (bank holidays) of dates provided.
#'
#' @param data A `data.table` containing trade data.
#' @param date_col Name of column containing timestamps.
#' @param division UK division for bank holiday lookup. One of
#'   `"england-and-wales"` (default), `"scotland"`, or `"northern-ireland"`.
#' @param holidays A `data.frame` or `data.table` of bank holidays with at least
#'   `date` and `title` columns, as returned by [get_uk_bank_holidays()].
#'   Defaults to the bundled [uk_bank_holidays] dataset.
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
  holidays = NULL
) {
  if (!inherits(data, "data.table")) {
    data <- as.data.table(data)
  }

  # Ensure the date column is Date type
  if (!inherits(data[[date_col]], "Date")) {
    stop("Input must be of class \"Date\".")
  }

  if (is.null(holidays)) {
    holidays <- uk_bank_holidays
  }

  division <- match.arg(
    division,
    c("england-and-wales", "scotland", "northern-ireland")
  )

  div_holidays <- holidays[holidays$division == division, ]

  # Warn if any dates fall outside the coverage of the holidays dataset
  dates <- data[[date_col]]
  coverage_min <- min(div_holidays$date)
  coverage_max <- max(div_holidays$date)
  out_of_range <- dates[
    !is.na(dates) & (dates < coverage_min | dates > coverage_max)
  ]
  if (length(out_of_range) > 0) {
    warning(
      length(out_of_range),
      " date(s) in `data` fall outside the coverage of the ",
      "holidays dataset for '",
      division,
      "' (",
      coverage_min,
      " to ",
      coverage_max,
      "). ",
      "UK holidays will be marked NA for those dates. ",
      "Use get_uk_bank_holidays() to fetch up-to-date data and pass it via the ",
      "`holidays` argument.",
      call. = FALSE
    )
  }

  # Build a named vector: date (as character) -> holiday title
  holiday_lookup <- setNames(
    div_holidays$title,
    as.character(div_holidays$date)
  )

  data$day_of_week <- weekdays(dates)
  data$day_of_year <- as.integer(format(dates, "%j"))
  data$holiday <- unname(holiday_lookup[as.character(dates)])
  data$is_holiday <- !is.na(data$holiday)

  data[, annual_sin := sin(2 * pi * day_of_year / 365)]
  data[, annual_cos := cos(2 * pi * day_of_year / 365)]

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


#' Convert UK trade data table to a hierarchical tsibble
#'
#' This function creates a tsibble from UK trade data by deriving commodity-code hierarchy
#' columns, aggregating a selected quantity over time, filling missing time
#' periods with zero, and aggregating values per hierarchies.
#'
#'
#' @param data A `data.table` containing trade data. Must include columns
#' `COMCODE` and specified `quantity`.
#' @param response_col Quantity to be extracted and aggregated as time series, e.g.`NET_MASS`, `volume`
#' @param comcode_level Hierarchy columns to use as tsibble keys
#' and in hierarchical aggregation. Defaults to `c("HS2", "HS4")`.
#' Expected commodity hierarchy columns are `"HS2"`, `"HS4"`,`"HS6"`, `"CN8"`, and `"CN10"`.
#' @param group_by Optional classification group in `data`, e.g. `PORT_CODE`. Use `"category"` to
#' group commodities by plants and animals. Default is `NULL`.
#' @param date_col Name of column containing timestamps.
#' @param freq Frequency of time series data. See details.
#' @param fill_missing This function returns a continuous time series. Values
#' for missing dates are filled with this value.
#' @return A hierarchical tsibble with date, quantity and aggregated hierarchy levels columns.
#'
#' @details Daily or monthly data is expected and detected automatically. Missing
#' values are filled.
#'
#' By default, `freq=NULL` and will be detected automatically. However, this may
#' fail for sparse data, in which case `freq` should be set manually. Options are
#' "day", "week", or "month".
#'
#' The `category` column labels commodity codes found in
#' `comcode_groups$animal_SPS` as `"Animal"` and those in
#' `comcode_groups$plant_SPS` as `"Plant"`.
#'
#' @export
uktrade_tsibble <- function(
  data,
  response_col = "NET_MASS",
  comcode_level = c("HS2", "HS4"),
  group_by = NULL,
  date_col = "DATE_START",
  freq = NULL,
  fill_missing = NA
) {
  #create categories
  data <- copy(data)
  if ("category" %in% group_by) {
    data[,
      category := ifelse(
        COMCODE %in% comcode_groups$animal_SPS,
        "Animal",
        ifelse(COMCODE %in% comcode_groups$plant_SPS, "Plant", NA)
      )
    ]
  } else if (!is.null(group_by)) {
    if (!group_by %in% names(data)) {
      stop("`group_by` column not found in `data`: ", group_by)
    }
  }
  #add hierarchies
  data[, HS2 := substr(COMCODE, 1, 2)]
  data[, HS4 := substr(COMCODE, 1, 4)]
  data[, HS6 := substr(COMCODE, 1, 6)]
  data[, CN8 := substr(COMCODE, 1, 8)]
  data[, CN10 := substr(COMCODE, 1, 10)]

  #NA for daily data
  if (data[, any(is.na(get(date_col)))]) {
    warning("Rows with missing or invalid dates were omitted.")
    data <- data[!is.na(get(date_col))]
  }

  #frequency checks
  if (is.null(freq)) {
    freq <- detect_date_frequency(data[[date_col]])
  } else if (!freq %in% c("day", "week", "month")) {
    stop('"freq" must be "day", "week" or "month"')
  }

  if (freq == "month") {
    data[, (date_col) := tsibble::yearmonth(get(date_col))]
  }

  #tsibble prep
  group_cols <- c(date_col, comcode_level, group_by)

  if (response_col == "volume") {
    imports_tsibble <- data[
      HS2 != "  ",
      .(value = .N),
      by = group_cols
    ]
  } else {
    imports_tsibble <- data[
      HS2 != "  ",
      .(value = sum(get(response_col), na.rm = TRUE)),
      by = group_cols
    ]
  }
  imports_tsibble <- as_tsibble(
    imports_tsibble,
    index = date_col,
    key = c(comcode_level, group_by)
  ) |>
    fill_gaps(value = !!fill_missing)

  hierarchy_terms <- if (length(comcode_level) > 0) {
    paste(comcode_level, collapse = "/")
  }

  group_terms <- if (!is.null(group_by) && length(group_by) > 0) {
    paste(group_by, collapse = "*")
  } else {
    NULL
  }

  level <- rlang::parse_expr(paste(
    c(hierarchy_terms, group_terms),
    collapse = " * "
  ))

  ts_table <- imports_tsibble |>
    aggregate_key(!!level, value = sum(value, na.rm = TRUE))
  setnames(ts_table, "value", response_col)
  return(ts_table)
}
