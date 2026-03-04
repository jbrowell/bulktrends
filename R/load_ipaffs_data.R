#' Load bulk daily data from IPAFFS
#'
#'
#'
#' @details
#' Leading pairs of 00s dropped
#' Missing leading 0 added to COMCODE of odd length


read_ipaffs <- function(path) {

  if( file_test("-f", path) ) {

    BDS <- data.table::fread(path, colClasses = "character")

    col_names <- list(DATE_START = c("DeclarationDate", "DateOfArrivalAtBIP",
                                     "DateOfArrival", "Declaration",
                                     "ArrivalAtBip"),
                      COMCODE = c("CommodityCode", "Commodities", "Commodity"),
                      NET_MASS  = c("TotalOfNetWeightKG", "NetWeightKg",
                                    "TotalNetWeight_Kg", "NetWeight(Kg)",
                                    "TotalNetWeightkg", "TotalNetWeight_kg",
                                    "TotalNetWeight_KG", "TotalNetWeight(kg)"))

    for (i in names(col_names)) {

      old_names <- col_names[[i]]
      match <- intersect(old_names, names(BDS))

      if (length(match)) {
        setnames(BDS, match, i)
      }
    }

    if ("DATE_START" %in% names(BDS)) {

      BDS[, DATE_START_temp := as.IDate(DATE_START, format = "%Y-%m-%d", tz="GB")] #tz="GB" #format="%d/%m/%Y"
      BDS[is.na(DATE_START_temp), DATE_START_temp := as.IDate(DATE_START[is.na(DATE_START_temp)], format = "%d/%m/%Y", tz="GB")]

      BDS[, DATE_START := DATE_START_temp]
      BDS[, DATE_START_temp := NULL]
    }
    else if (all(
      c("YearOfDeclaration",
        "MonthOfDeclaration",
        "DayOfDeclaration"
      ) %in% names(BDS)) ) {
      BDS[, DATE_START := as.IDate(
        paste0(YearOfDeclaration,"-",MonthOfDeclaration,"-",DayOfDeclaration),
        format="%Y-%m-%d", tz="GB")]
    }
    else if (all(
      c("DeclarationYear",
        "DeclarationMonth",
        "DeclarationDay"
      ) %in% names(BDS)) ) {
      BDS[, DATE_START := as.IDate(
        paste0(DeclarationYear,"-",DeclarationMonth,"-",DeclarationDay),
        format="%Y-%m-%d", tz="GB")]
    } else {
      BDS[, DATE_START := NA]
    }
    BDS[, DATE_END := DATE_START]


    if( "NET_MASS" %in% colnames(BDS) ) {
      BDS[, NET_MASS := as.numeric(NET_MASS)]
    }

    BDS[nchar(COMCODE)%%2==1, COMCODE := paste0("0",COMCODE)]

    while( nrow(BDS[substr(COMCODE,1,2)=="00"])>0 ) {
      BDS[substr(COMCODE,1,2)=="00",COMCODE := gsub("^00","",COMCODE)]
    }

    return(BDS)

  } else if(file_test("-d", path)) {

    files <- list.files(path, pattern = ".csv",
                        full.names = T,
                        recursive = T)

    BDS_all <- data.table()

    BDS_all <- data.table::rbindlist(
      future.apply::future_lapply(files, read_ipaffs),
      use.names = T,
      fill = T
    )


    return(BDS_all)

  } else {
    stop("path is not a file or directory.")
  }

}
