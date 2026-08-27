# Directory where commodity tariffs from 2021 will be stored
lookup_dir <- system.file("data", package = "bulktrends")

# Create the directory if it doesn't exist
if (!dir.exists(lookup_dir)) {
  dir.create(lookup_dir, recursive = TRUE)
}

update_tariff_commodities <- function() {
  
  url <- paste0(
    "https://data.api.trade.gov.uk/v1/datasets/uk-tariff-2021-01-01/versions/",
    "latest/tables/commodities/data?format=csv&download"
  )
  
  # Read everything as character
  raw <- utils::read.csv(
    url,
    colClasses = "character",
    stringsAsFactors = FALSE
  )
  
  # Clean commodity codes, remove white spaces
  comcode <- trimws(raw$item_id)
  
  # Convert scientific notation to ordinary numbers
  is_scientific <- grepl(
    "^[0-9.]+[eE][+-]?[0-9]+$",
    comcode
  )
  
  comcode[is_scientific] <- vapply(
    comcode[is_scientific],
    function(x) {
      format(
        as.numeric(x),
        scientific = FALSE,
        trim = TRUE
      )
    },
    character(1)
  )
  
  # Make sure all codes have 10 digits, not needed, every single comcode has 10 digits
  #comcode <- sprintf("%010s", comcode)
  
  # Create lookup table
  tariff_commodities_2021 <- data.frame(
    comcode = comcode,
    
    valid_from = as.Date(ifelse(
      raw$validity_start %in% c("", "NULL", "#NA", NA),
      NA,
      raw$validity_start
    )),
    
    valid_to = as.Date(ifelse(
      raw$validity_end %in% c("", "NULL", "#NA", NA),
      NA,
      raw$validity_end
    )),
    
    stringsAsFactors = FALSE
  )
  
  # Save to the lookup directory
  save(
    tariff_commodities_2021,
    file = file.path(
      lookup_dir,
      "tariff_commodities_2021.rda"   
    )
  )
  
  invisible(tariff_commodities_2021)
}


#' Request data from GOV.UK Trade Tariff API
#'
#' @param endpoint Endpoint appended to the base API URL.
#' @param as_of Optional date passed as the `as_of` query parameter.
#'
#' @return Result of API query.
#'
#' @export
tradetariff_request <- function(endpoint, as_of = NULL) {
  
  url <- paste0(
    "https://www.trade-tariff.service.gov.uk/api/v2/",
    endpoint
  )
  
  if (!is.null(as_of)) {
    url <- paste0(
      url,
      "?as_of=",
      format(as.Date(as_of), "%Y-%m-%d")
    )
  }
  
  jsonlite::fromJSON(url)
}


#' Get validity dates for one or more commodity codes
#'
#' @param comcodes Character vector of commodity codes (8 or 10 digits).
#'   Numeric values are not accepted as leading zeros may be silently dropped.
#' @param as_of Optional date passed to the API.
#' @param search_to Integer year. The earliest year tried when searching for
#'   an expired code. Defaults to 1990.
#'
#' @return A data.frame containing the supplied commodity code, the
#'   corresponding 10-digit code, API validity dates, and historical
#'   validity dates from the 2021 tariff dataset.
#'
#' @export
comcode_validity_dates <- function(
    comcodes,
    as_of = NULL,
    search_to = 1990L
) {
  
  # ------------------------------------------------------------
  # Check input
  # ------------------------------------------------------------
  
  if (!is.character(comcodes)) {
    stop(
      "'comcodes' must be a character vector. Numeric values may ",
      "silently drop leading zeros."
    )
  }
  
  # ------------------------------------------------------------
  # Process multiple codes
  # ------------------------------------------------------------
  
  if (length(comcodes) > 1) {
    
    rows <- future.apply::future_lapply(
      comcodes,
      comcode_validity_dates,
      as_of = as_of,
      search_to = search_to
    )
    
    return(
      data.table::rbindlist(
        rows,
        fill = TRUE
      )
    )
  }
  
  # Keep the code exactly as supplied
  supplied_code <- comcodes
  
  # ------------------------------------------------------------
  # If an 8-digit code is supplied, find its 10-digit code
  # using the 2021 tariff dataset
  # ------------------------------------------------------------
  
  if (nchar(supplied_code) == 8) {
    
    matches <- tariff_commodities_2021[
      substr(
        tariff_commodities_2021$comcode,
        1,
        8
      ) == supplied_code,
    ]
    
    primary <- c("00", "10", "99", "90")
    
    suffixes <- if (nrow(matches) > 0) {
      
      candidates <- substr(
        matches$comcode,
        9,
        10
      )
      
      c(
        intersect(primary, candidates),
        setdiff(candidates, primary),
        setdiff(primary, candidates),
        setdiff(
          sprintf("%02d", 0:99),
          union(candidates, primary)
        )
      )
      
    } else {
      
      c(
        primary,
        setdiff(
          sprintf("%02d", 0:99),
          primary
        )
      )
    }
    
    # Try each possible 10-digit code
    for (suffix in suffixes) {
      
      result <- comcode_validity_dates(
        paste0(supplied_code, suffix),
        as_of = as_of,
        search_to = search_to
      )
      
      # A successful API result has a non-NA validity date
      if (
        nrow(result) > 0 &&
        (
          any(!is.na(result$API_valid_from)) ||
          any(!is.na(result$API_valid_to))
        )
      ) {
        
        # Restore the original supplied 8-digit code
        result$COMCODE_supplied <- supplied_code
        
        # The recursive call has already produced the correct
        # 10-digit code and historical records
        result <- result[
          ,
          c(
            "COMCODE_supplied",
            "COMCODE_10",
            "API_valid_from",
            "API_valid_to",
            "historical_valid_from",
            "historical_valid_to"
          )
        ]
        
        return(result)
      }
    }
    
    # No 10-digit code found
    warning(
      "No valid 10-digit expansion found for '",
      supplied_code,
      "'."
    )
    
    return(
      data.frame(
        COMCODE_supplied = supplied_code,
        COMCODE_10 = NA_character_,
        API_valid_from = as.Date(NA),
        API_valid_to = as.Date(NA),
        historical_valid_from = as.Date(NA),
        historical_valid_to = as.Date(NA),
        stringsAsFactors = FALSE
      )
    )
  }
  
  # ------------------------------------------------------------
  # Check that the code is 10 digits
  # ------------------------------------------------------------
  
  if (nchar(supplied_code) != 10) {
    
    warning(
      "Commodity codes should be 8 or 10 digits; '",
      supplied_code,
      "' has ",
      nchar(supplied_code),
      "."
    )
    
    return(
      data.frame(
        COMCODE_supplied = supplied_code,
        COMCODE_10 = NA_character_,
        API_valid_from = as.Date(NA),
        API_valid_to = as.Date(NA),
        historical_valid_from = as.Date(NA),
        historical_valid_to = as.Date(NA),
        stringsAsFactors = FALSE
      )
    )
  }
  
  # ------------------------------------------------------------
  # Query API
  # ------------------------------------------------------------
  
  try_request <- function(date) {
    
    suppressWarnings(
      tryCatch(
        tradetariff_request(
          paste0(
            "commodities/",
            supplied_code
          ),
          as_of = date
        ),
        error = function(e) NULL
      )
    )
  }
  
  # First try the supplied as_of date, or the current API
  result_api <- try_request(as_of)
  
  # If the code is not currently valid, search backwards
  # through previous years
  if (
    is.null(result_api) &&
    is.null(as_of)
  ) {
    
    years <- seq(
      as.integer(format(Sys.Date(), "%Y")) - 1L,
      as.integer(search_to),
      by = -1L
    )
    
    for (yr in years) {
      
      result_api <- try_request(
        paste0(
          yr,
          "-01-01"
        )
      )
      
      if (!is.null(result_api)) {
        break
      }
    }
  }
  
  # ------------------------------------------------------------
  # Prepare API information
  # ------------------------------------------------------------
  
  if (is.null(result_api)) {
    
    api <- data.frame(
      COMCODE_supplied = supplied_code,
      COMCODE_10 = NA_character_,
      API_valid_from = as.Date(NA),
      API_valid_to = as.Date(NA),
      stringsAsFactors = FALSE
    )
    
  } else {
    
    attrs <- result_api$data$attributes
    
    api <- data.frame(
      COMCODE_supplied = supplied_code,
      COMCODE_10 = supplied_code,
      
      API_valid_from = as.Date(
        substr(
          attrs$validity_start_date,
          1,
          10
        )
      ),
      
      API_valid_to = if (
        is.null(attrs$validity_end_date) ||
        is.na(attrs$validity_end_date)
      ) {
        as.Date(NA)
      } else {
        as.Date(
          substr(
            attrs$validity_end_date,
            1,
            10
          )
        )
      },
      
      stringsAsFactors = FALSE
    )
  }
  
  # ------------------------------------------------------------
  # Get ALL historical records for this 10-digit code
  # ------------------------------------------------------------
  
  historical <- tariff_commodities_2021[
    tariff_commodities_2021$comcode == supplied_code,
    c(
      "comcode",
      "valid_from",
      "valid_to"
    )
  ]
  
  names(historical) <- c(
    "COMCODE_10",
    "historical_valid_from",
    "historical_valid_to"
  )
  
  # Remove only exact duplicate rows.
  # Different historical validity periods are retained.
  historical <- unique(historical)
  
  # ------------------------------------------------------------
  # Combine API and historical information
  # ------------------------------------------------------------
  
  if (nrow(historical) == 0) {
    
    result <- api
    
    result$historical_valid_from <- as.Date(NA)
    result$historical_valid_to <- as.Date(NA)
    
  } else {
    
    result <- merge(
      api,
      historical,
      by = "COMCODE_10",
      all.x = TRUE
    )
  }
  
  # ------------------------------------------------------------
  # Put supplied code first
  # ------------------------------------------------------------
  
  result <- result[
    ,
    c(
      "COMCODE_supplied",
      "COMCODE_10",
      "API_valid_from",
      "API_valid_to",
      "historical_valid_from",
      "historical_valid_to"
    )
  ]
  
  result
}

###comcode_validity_dates(c("0101210000", "1601001010"))#cn10
###comcode_validity_dates("01039219")#cn8
###comcode_validity_dates(c("7019650019", "7019650021"))
