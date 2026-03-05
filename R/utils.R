#' Detect date frequency
detect_date_frequency <- function(dates) {

  if (! inherits(dates, "Date") ) {
    tryCatch(
      dates <- as.Date(dates),
      stop("Input not of class \"Date\" and couldn't be converted."))
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

#' Extract of monthly time series of "NET_MASS" for a given commodity code
#'
#' This function extracts the monthly sum "NET_MASS" from import_data for
#' any level of hierarchy (HS2, HS4, HS6 and CN8) found in the dataset.
#'
#' @param import_data A `data.table` containing trade data. Must include columns
#' "COMCODE", "month" and `quantity`.
#' @param code A character string representing any HS2/HS4/HS6/CN8 code.
#' @param date_col Name of column containing timestamps.
#' @param quantity Quantity to be extracted and aggregated as time series, e.g.
#' "NET_MASS" or "STAT_VALUE".
#' @param fill_missing This function returns a continuous time series. Values
#' for missing dates are filled with this value.
#'
#' @return A `data.table` with date and quantity columns.
#'
#' @details Daily or monthly data is expected and detected automatically. Missing
#' values are filled.
#'
#'
#' @export
extract_ts <- function (import_data,
                        code,
                        date_col = "DATE_START",
                        quantity = "NET_MASS",
                        fill_missing = NA
) {


  import_data <- copy(import_data[substr(COMCODE, 1, nchar(code)) == code])

  if( quantity=="volume") {
    ts_data <-  import_data[, .(volume=.N), by=date_col]
  } else {
    ts_data <-  import_data[, .(agg = sum(get(quantity), na.rm = T)),
                            by=date_col]
    setnames(ts_data,"agg",quantity)
  }

  freq <- detect_date_frequency(ts_data[,get(date_col)])
  complete_seq <- ts_data[,
                          seq(
                            min(get(date_col)),
                            max(get(date_col)),
                            by = freq)]

  missing_data <- data.table()
  missing_data[, (date_col) := complete_seq[!complete_seq %in% ts_data[,get(date_col)]]]
  missing_data[, (quantity) := fill_missing]

  ts_data <- rbind(ts_data,missing_data)

  return(ts_data[order(get(date_col))])
}


#' Add date features
add_date_features <- function(data, date_col) {

  # Ensure the date column is Date type
  if (! inherits(data[[date_col]], "Date") ) {
    stop("Input must be of class \"Date\".")
  }

  dates <- data[[date_col]]
  years <- as.numeric(unique(format(dates, "%Y")))

  # Build a named vector of date -> holiday name
  # Would prefer to replace this with function to
  # get UK Bank Holidays from gov.uk API and github.com/alphagov
  # https://www.gov.uk/bank-holidays.json
  # https://github.com/alphagov/calendars/blob/master/lib/data/bank-holidays.json

  make_holidays <- function(years) {

    fixed <- c(
      setNames(as.Date(as.character(timeDate::GoodFriday(years))),    rep("Good Friday",        length(years))),
      setNames(as.Date(as.character(timeDate::EasterMonday(years))),  rep("Easter Monday",      length(years))),
      setNames(as.Date(as.character(timeDate::ChristmasDay(years))),  rep("Christmas Day",      length(years))),
      setNames(as.Date(as.character(timeDate::BoxingDay(years))),     rep("Boxing Day",         length(years))),
      setNames(as.Date(as.character(timeDate::NewYearsDay(years))),   rep("New Year's Day",     length(years)))
    )

    find_monday <- function(y, month, start_day, label) {
      d <- seq(as.Date(paste0(y, "-", month, "-", start_day)),
               as.Date(paste0(y, "-", month, "-", start_day))+6, by = "day")
      setNames(d[weekdays(d) == "Monday"][1], label)
    }

    variable <- do.call(c, lapply(years, function(y) c(
      find_monday(y, "05", "01", "Early May Bank Holiday"),
      find_monday(y, "05", "25", "Spring Bank Holiday"),
      find_monday(y, "08", "25", "Summer Bank Holiday")
    )))

    c(fixed, variable)
  }
  warning("One-off bank holidays missing. Update required...")
  holiday_lookup <- make_holidays(years)

  data$day_of_week   <- weekdays(dates)
  data$day_of_year   <- as.integer(format(dates, "%j"))
  data$uk_holiday    <- names(holiday_lookup)[match(dates, holiday_lookup)]
  data$is_uk_holiday <- !is.na(data$uk_holiday)

  data[, annual_sin := sin(2*pi*day_of_year/365)]
  data[, annual_cos := cos(2*pi*day_of_year/365)]

  return(data)
}

#' View User Guide
#'
#' A function that opens the `bulktrends` user guide in system browser.
#'
#' @param path Optional. Path to specific instance of `UserGuide.html`. If `NULL`, it will be retrieved from the current installation of `bulktrends`.
#'
open_userguide <- function(path=NULL) {

  if( is.null(path) ) {
    path <- try(
      system.file("docs", "index.html", package = "bulktrends")
    )
  }

  if( file.exists(path) ){
    browseURL(path)
  } else {
    stop("Couldn't find UserGuide.html")
  }

}
