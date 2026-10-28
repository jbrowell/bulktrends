#' Load bulk daily data from IPAFFS (Import of Products, Animals, Food and Feed System)
#'
#' Load a single or multiple daily data `.csv` files in a directory (and its
#' sub-directories) from IPAFFS
#'
#'
#' @param path Path to a file to read, or directory containing multiple files,
#' files in sub-directories are read recursively.
#'
#' @return A `data.table` of trade data with columns containing timestamps.
#'
#' @details
#' Leading pairs of 00s dropped
#' Missing leading 0 added to COMCODE of odd length
#' Some files contain country names, other country codes. These are harmonised
#' to both country name and alpha-2 code using the \code{bulktrends::country}
#' lookup table.
#'
#' @export
read_ipaffs <- function(path) {
  if (file_test("-f", path)) {
    BDS <- data.table::fread(path, colClasses = "character")

    col_names <- list(
      DATE_START = c(
        "DeclarationDate",
        "DateOfArrivalAtBIP",
        "DateOfArrival",
        "Declaration",
        "ArrivalAtBip"
      ),
      COMCODE = c("CommodityCode", "Commodities", "Commodity"),
      NET_MASS = c(
        "TotalOfNetWeightKG",
        "NetWeightKg",
        "TotalNetWeight_Kg",
        "NetWeight(Kg)",
        "TotalNetWeightkg",
        "TotalNetWeight_kg",
        "TotalNetWeight_KG",
        "TotalNetWeight(kg)"
      ),
      COO_NAME = c(
        "CountryOfOrigin",
        "CountryofOrigin",
        "countryoforigin",
        "CountryOfOriginName"
      ),
      PORT_NAME = c("BorderInspectionPost", "BCPName")
    )

    for (i in names(col_names)) {
      old_names <- col_names[[i]]
      match <- intersect(old_names, names(BDS))

      if (length(match) == 1) {
        setnames(BDS, match, i)
      } else if (length(match) > 1) {
        warning(
          "Column name correction not possible for: ",
          path,
          "\nMultiple columns match ",
          i,
          ": ",
          paste(match, collapse = ", ")
        )
      }
    }

    if ("DATE_START" %in% names(BDS)) {
      BDS[,
        DATE_START_temp := as.Date(DATE_START, format = "%Y-%m-%d", tz = "GB")
      ] #tz="GB" #format="%d/%m/%Y"
      BDS[
        is.na(DATE_START_temp),
        DATE_START_temp := as.Date(
          DATE_START[is.na(DATE_START_temp)],
          format = "%d/%m/%Y",
          tz = "GB"
        )
      ]

      BDS[, DATE_START := DATE_START_temp]
      BDS[, DATE_START_temp := NULL]
    } else if (
      all(
        c("YearOfDeclaration", "MonthOfDeclaration", "DayOfDeclaration") %in%
          names(BDS)
      )
    ) {
      BDS[,
        DATE_START := as.Date(
          paste0(
            YearOfDeclaration,
            "-",
            MonthOfDeclaration,
            "-",
            DayOfDeclaration
          ),
          format = "%Y-%m-%d",
          tz = "GB"
        )
      ]
    } else if (
      all(
        c("DeclarationYear", "DeclarationMonth", "DeclarationDay") %in%
          names(BDS)
      )
    ) {
      BDS[,
        DATE_START := as.Date(
          paste0(DeclarationYear, "-", DeclarationMonth, "-", DeclarationDay),
          format = "%Y-%m-%d",
          tz = "GB"
        )
      ]
    } else {
      BDS[, DATE_START := NA]
    }
    BDS[, DATE_END := DATE_START]

    if ("NET_MASS" %in% colnames(BDS)) {
      BDS[, NET_MASS := suppressWarnings(as.numeric(NET_MASS))]
    }

    BDS[nchar(COMCODE) %% 2 == 1, COMCODE := paste0("0", COMCODE)]

    while (nrow(BDS[substr(COMCODE, 1, 2) == "00"]) > 0) {
      BDS[substr(COMCODE, 1, 2) == "00", COMCODE := gsub("^00", "", COMCODE)]
    }

    BDS$COO_ALPHA <- ifelse(
      nchar(BDS$COO_NAME) == 2,
      BDS$COO_NAME,
      NA_character_
    )
    BDS[COO_NAME == "UK", COO_ALPHA := "GB"]
    BDS$COO_NAME <- ifelse(nchar(BDS$COO_NAME) > 2, BDS$COO_NAME, NA_character_)

    ## Work in progress. Country name matches are poor. Ref: ISO 3166-1
    # BDS[
    #   is.na(COO_NAME),
    #   COO_NAME := bulktrends::country$CountryName[
    #     match(COO_ALPHA, bulktrends::country$CountryCodeAlpha)
    #   ]
    # ]
    #
    # BDS[
    #   is.na(COO_ALPHA),
    #   COO_ALPHA := bulktrends::country$CountryCodeAlpha[
    #     match(COO_NAME, bulktrends::country$CountryName)
    #   ]
    # ]

    return(BDS)
  } else if (file_test("-d", path)) {
    files <- list.files(path, pattern = ".csv", full.names = T, recursive = T)

    BDS_all <- data.table()

    BDS_all <- data.table::rbindlist(
      future.apply::future_lapply(files, read_ipaffs),
      use.names = T,
      fill = T
    )

    data.table::setorder(BDS_all, DATE_START)

    return(BDS_all)
  } else {
    stop("path is not a file or directory.")
  }
}
