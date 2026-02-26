#' Utility helper functions
#'
#' Extract of monthly time series of "NET_MASS" for a given commodity code
#'
#' This function extracts the monthly sum "NET_MASS" from import_data for
#' any level of hierarchy (HS2, HS4, HS6 and CN8) found in the dataset.
#'
#' @param import_data A `data.table` containing trade data. Must include columns "COMCODE", "month" and `quantity`.
#' @param code A character string representing any HS2/HS4/HS6/CN8 code.
#' @param quantity Quantity to be extracted and aggregated as time series, e.g. "NET_MASS" or "STAT_VALUE".
#'
#' @return Time series object of NET_MASS
#'
#' @export
extract_ts <- function (import_data,
                        date_variable,
                        code,
                        quantity = "NET_MASS") {


  all_dates <- unique(import_data$date_variable)
  import_data <- copy(import_data[substr(COMCODE, 1, nchar(code)) == code])

  missing <- all_dates[!all_dates %in% import_data$date_variable]
  if(length(missing) > 0) {
    warning("Missing months detected for code ",code,": ", paste(missing))
  }

  #import_data <-  import_data[, .(temp = sum(get(quantity), na.rm = T)), by=date_variable]
  #setnames(import_data,"temp",quantity)

  ts_data <-  import_data[, .(temp = sum(get(quantity), na.rm = T)), by=date_variable]

  #first_date <- import_data[, min(date_variable)]
  #ts_data <- ts(import_data[[quantity]], start = c(year(first_date), month(first_date)), frequency = n)

  return(ts_data)
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
