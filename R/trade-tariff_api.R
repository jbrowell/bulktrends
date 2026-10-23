
#' Download and clean the historical tariff dataset
#'
#' Downloads the latest available historical tariff dataset,
#' cleans its columns, and returns the resulting data frame.
#'
#' Optionally, the dataset bundled with the locally installed
#' bulktrends package can be updated.
#'
#' @param update_local_package Logical. If TRUE, attempt to overwrite
#'   the tariff dataset in the locally installed package. Defaults
#'   to FALSE.
#'
#' @return A cleaned data frame of historical tariff records, with
#'   a created_at attribute recording the download date.
#'
#' @details
#' To update the dataset bundled with the package, run
#' `source("data-raw/tariff_commodities.R")` from the package root.
#' This downloads and cleans the latest data and saves it to
#' `data/tariff_commodities.rda`.
#' @export
get_tariff_commodities <- function(
    update_local_package = FALSE
) {
  
  # ------------------------------------------------------------------
  # 1. Validate arguments
  # ------------------------------------------------------------------
  
  if (
    !is.logical(update_local_package) ||
    length(update_local_package) != 1L ||
    is.na(update_local_package)
  ) {
    stop(
      "`update_local_package` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # ------------------------------------------------------------------
  # 2. Download the historical tariff dataset
  # ------------------------------------------------------------------
  
  url <- paste0(
    "https://data.api.trade.gov.uk/v1/datasets/",
    "uk-tariff-2021-01-01/versions/latest/",
    "tables/commodities/data?format=csv&download"
  )
  
  message("Fetching historical tariff data ...")
  
  raw <- tryCatch(
    {
      utils::read.csv(
        url,
        colClasses = "character",
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      stop(
        "Unable to download the historical tariff dataset: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  # ------------------------------------------------------------------
  # 3. Validate the downloaded dataset
  # ------------------------------------------------------------------
  
  required_columns <- c(
    "item_id",
    "validity_start",
    "validity_end",
    "trackedmodel_ptr_id",
    "sid",
    "suffix"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(raw)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "The downloaded dataset is missing expected columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if (nrow(raw) == 0L) {
    stop(
      "The downloaded tariff dataset contains no records.",
      call. = FALSE
    )
  }
  
  message("  Success: ", nrow(raw), " records fetched.")
  
  # ------------------------------------------------------------------
  # 4. Clean COMCODEs
  # ------------------------------------------------------------------
  
  comcode <- trimws(raw$item_id)
  
  # Identify COMCODEs represented in scientific notation.
  is_scientific <- grepl(
    "^[0-9.]+[eE][+-]?[0-9]+$",
    comcode
  )
  
  
  # Convert scientific notation to ordinary decimal notation
  # and add a leading zero if the length is odd
  comcode[is_scientific] <- vapply(
    comcode[is_scientific],
    function(x) {
      converted <- format(
        as.numeric(x),
        scientific = FALSE,
        trim = TRUE
      )
      
      if (nchar(converted) %% 2 != 0) {
        converted <- paste0("0", converted)
      }
      
      converted
    },
    character(1)
  )
  
  
  # ------------------------------------------------------------------
  # 5. Clean validity dates
  # ------------------------------------------------------------------
  
  clean_date <- function(x) {
    
    x <- trimws(x)
    x[x %in% c("", "NULL", "#NA")] <- NA_character_
    
    as.Date(x)
  }
  

  
  # ------------------------------------------------------------------
  # 6. Construct the cleaned dataset
  # ------------------------------------------------------------------
  
  tariff_commodities <- data.frame(
    comcode = comcode,
    valid_from = clean_date(raw$validity_start),
    valid_to = clean_date(raw$validity_end),
    trackedmodel_ptr_id = suppressWarnings(
      as.numeric(raw$trackedmodel_ptr_id)
    ),
    sid = as.character(raw$sid),
    suffix = as.character(raw$suffix),
    stringsAsFactors = FALSE
  )
  
  # Record the date the dataset was downloaded.
  attr(tariff_commodities, "created_at") <-
    as.character(Sys.Date())
  
  # ------------------------------------------------------------------
  # 7. Optionally update the locally installed package dataset
  # ------------------------------------------------------------------
  
  if (update_local_package) {
    .update_installed_tariff_data(tariff_commodities)
  }
  
  # ------------------------------------------------------------------
  # 8. Return the cleaned data frame
  # ------------------------------------------------------------------
  
  tariff_commodities
}


#' Build COMCODE validity history
#'
#' Uses the saved historical tariff dataset to identify validity periods
#' for specified COMCODEs. Eight-digit inputs are expanded to all matching
#' ten-digit codes found in the saved dataset.
#'
#' @param comcodes Character vector containing 8-digit or 10-digit codes.
#'
#' @return A data frame containing selected historical records,
#'   overlapping validity conflicts, and current API check results.
#' @export
build_comcode_validity_history <- function(comcodes) {
  
  #invalid formats and missing values cause an error, 
  #while unsupported lengths generate a warning and are skipped.
  
  # ---- 1. Validate input ----------------------------------------
  
  if (!is.character(comcodes) || length(comcodes) == 0L) {
    stop(
      "`comcodes` must be a non-empty character vector.",
      call. = FALSE
    )
  }
  
  if (anyNA(comcodes) || any(!grepl("^[0-9]+$", comcodes))) {
    stop(
      "All COMCODEs must be character strings containing digits only.",
      call. = FALSE
    )
  }
  
  invalid_length <- !nchar(comcodes) %in% c(8L, 10L)
  
  if (any(invalid_length)) {
    warning(
      "Ignoring inputs that are not 8 or 10 digits: ",
      paste(unique(comcodes[invalid_length]), collapse = ", "),
      call. = FALSE
    )
    
    comcodes <- comcodes[!invalid_length]
  }
  
  if (length(comcodes) == 0L) {
    return(data.frame())
  }
  
  # ---- 2. Load the packaged historical dataset -------------------------
  
  data("tariff_commodities", package = "bulktrends")
  tariff_data <- tariff_commodities
  
  # ---- 3. Filter and clean historical records --------------------------
  
  
  # Filter suffix 80 before any other historical processing.
  tariff_data <- tariff_data[
    !is.na(tariff_data$suffix) &
      tariff_data$suffix == "80",
    ,
    drop = FALSE
  ]
  
  # Convert trackedmodel_ptr_id to numeric for correct ordering.
  tariff_data$trackedmodel_ptr_id <- suppressWarnings(
    as.numeric(tariff_data$trackedmodel_ptr_id)
  )
  
  # ---- 4. Expand 8-digit inputs to all matching 10-digit codes ---------
  
  expanded_inputs <- list()
  
  for (input_code in unique(comcodes)) {
    
    if (nchar(input_code) == 10L) {
      
      candidates <- input_code
      
      if (!input_code %in% tariff_data$comcode) {
        warning(
          "COMCODE ", input_code,
          " was not found in the saved historical dataset.",
          call. = FALSE
        )
      }
      
    } else {
      
      warning(
        "COMCODE ", input_code,
        " has 8 digits. Searching for all matching 10-digit extensions.",
        call. = FALSE
      )
      
      candidates <- unique(
        tariff_data$comcode[
          startsWith(tariff_data$comcode, input_code) &
            nchar(tariff_data$comcode) == 10L
        ]
      )
      
      if (length(candidates) == 0L) {
        warning(
          "No 10-digit extensions found for ", input_code, ".",
          call. = FALSE
        )
      }
    }
    
    if (length(candidates) > 0L) {
      expanded_inputs[[input_code]] <- data.frame(
        input_comcode = input_code,
        comcode = candidates,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(expanded_inputs) == 0L) {
    return(data.frame())
  }
  
  input_map <- unique(do.call(rbind, expanded_inputs))
  rownames(input_map) <- NULL
  
  # ---- 5. Select the latest trackedmodel_ptr_id for each COMCODE/SID --------------
  
  selected_records <- tariff_data[
    tariff_data$comcode %in% input_map$comcode,
    ,
    drop = FALSE
  ]
  
  if (nrow(selected_records) == 0L) {
    warning(
      "No matching historical records were found.",
      call. = FALSE
    )
    return(data.frame())
  }
  
  # Keep the row with the largest numeric trackedmodel_ptr_id within each
  # COMCODE/SID pair. If IDs are missing, retain the first available row.
  # Keep the latest revision for each COMCODE/SID pair.
  selected_records$trackedmodel_ptr_id <- suppressWarnings(
    as.numeric(selected_records$trackedmodel_ptr_id)
  )
  
  #order based on the highest trackedmoded_ptr_id for each comcode/sid pair
  selected_records <- selected_records[
    order(
      selected_records$comcode,
      selected_records$sid,
      -selected_records$trackedmodel_ptr_id,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  #Remove duplicate COMCODE/SID pairs, keeps only the first row for each unique combination of comcode/sid with the highest trackedmodel_ptr_id
  selected_records <- selected_records[
    !duplicated(selected_records[, c("comcode", "sid")]),
    ,
    drop = FALSE
  ]
  
  # Keep the original input alongside each expanded COMCODE.
  selected_records <- merge(
    input_map,
    selected_records,
    by = "comcode",
    all = FALSE,
    sort = FALSE
  )
  
  #resets the rows indexes
  rownames(selected_records) <- NULL
  
  # ---- 6. Detect overlapping validity intervals across different SIDs
  
  selected_records$overlap_conflict <- FALSE
  
  codes_to_check <- unique(selected_records$comcode)
  
  for (code in codes_to_check) {
    
    idx <- which(selected_records$comcode == code)
    
    if (length(idx) < 2L) {
      next
    }
    
    conflict_rows <- rep(FALSE, length(idx))
    
    for (i in seq_along(idx)) {
      
      row_i <- selected_records[idx[i], ]
      
      for (j in seq_along(idx)) {
        
        if (i == j) {
          next
        }
        
        row_j <- selected_records[idx[j], ]
        
        # Only compare different SIDs.
        if (identical(row_i$sid, row_j$sid)) {
          next
        }
        
        start_i <- row_i$valid_from
        end_i <- row_i$valid_to
        start_j <- row_j$valid_from
        end_j <- row_j$valid_to
        
        # Missing start dates cannot establish an overlap reliably.
        if (is.na(start_i) || is.na(start_j)) {
          next
        }
        
        # Missing end dates represent open-ended intervals.
        end_i_open <- is.na(end_i)
        end_j_open <- is.na(end_j)
        
        overlaps <- (
          (end_i_open || start_j <= end_i) &&
            (end_j_open || start_i <= end_j)
        )
        
        if (overlaps) {
          conflict_rows[i] <- TRUE
          break
        }
      }
    }
    
    selected_records$overlap_conflict[idx] <- conflict_rows
  }
  
  # ---- 7. Check flagged conflicts against the current tariff API ----
  
  selected_records$api_checked <- FALSE
  selected_records$selected_by_current_api <- NA
  selected_records$api_valid_from <- as.Date(NA)
  selected_records$api_valid_to <- as.Date(NA)
  
  # Query the current API only for COMCODEs with overlapping intervals.
  conflict_codes <- unique(
    selected_records$comcode[selected_records$overlap_conflict]
  )
  
  
  for (code in conflict_codes) {
    
    #Warning when an API request fails and skips to the next COMCODE.
    api_result <- tryCatch(
      jsonlite::fromJSON(
        paste0(
          "https://www.trade-tariff.service.gov.uk/api/v2/commodities/",
          code
        ),
        simplifyVector = FALSE
      ),
      error = function(e) {
        warning(
          "API request failed for COMCODE ", code, ": ",
          conditionMessage(e),
          ". Skipping this code.",
          call. = FALSE
        )
        NULL
      }
    )
    
    if (is.null(api_result)) {
      next
    }
    
    if (is.null(api_result$data)) {
      next
    }
    
    rows <- which(selected_records$comcode == code)
    current_sid <- api_result$data$id
    attrs <- api_result$data$attributes
    
    if (is.null(attrs)) {
      next
    }
    
    if (is.null(current_sid) || length(current_sid) == 0L) {
      next
    }
    
    # Extract the validity dates
    api_valid_from <- if (!is.null(attrs$validity_start_date)) {
      as.Date(substr(attrs$validity_start_date, 1, 10))
    } else as.Date(NA)
    
    api_valid_to <- if (
      !is.null(attrs$validity_end_date) &&
      !is.na(attrs$validity_end_date)
    ) {
      as.Date(substr(attrs$validity_end_date, 1, 10))
    } else as.Date(NA)
    
    # Compare the SIDs
    matches_sid <- as.character(selected_records$sid[rows]) ==
      as.character(current_sid)
    
    selected_records$selected_by_current_api[rows] <- matches_sid
    selected_records$api_valid_from[rows] <- api_valid_from
    selected_records$api_valid_to[rows] <- api_valid_to
    
    # Mark the comparison as completed
    selected_records$api_checked[rows] <- TRUE
    
  }
  
  # ---- 8. Return the final table ------------------------------------
  
  selected_records$trackedmodel_ptr_id <- NULL
  
  selected_records <- selected_records[
    order(
      selected_records$input_comcode,
      selected_records$comcode,
      selected_records$sid
    ),
    ,
    drop = FALSE
  ]
  
  rownames(selected_records) <- NULL
  
  selected_records
}
