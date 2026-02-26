#' Load bulk daily data from IPAFFS

read_ipaffs <- function(path) {

  if( file_test("-f", path) ) {

    BDS <- data.table::fread(path, colClasses = "character")

    col_names <- list(date    = c("DeclarationDate", "DateOfArrivalAtBIP", "DateOfArrival", "Declaration", "ArrivalAtBip"),  #"DateofDeclaration"
                      COMCODE = c("CommodityCode", "Commodities", "Commodity"),
                      weight  = c("TotalOfNetWeightKG", "NetWeightKg", "TotalNetWeight_Kg", "NetWeight(Kg)"))

    for (i in names(col_names)) {

      old_names <- col_names[[i]]
      match <- intersect(old_names, names(BDS))
      if (length(match))
        setnames(BDS, match, i)
    }


  if ("date" %in% names(BDS)) {
    BDS[, date := as.IDate(date)]
  }
  else if (all(c("YearOfDeclaration", "MonthOfDeclaration", "DayOfDeclaration") %in% names(BDS))) {
    BDS[, date := as.IDate(sprintf("%s-%s-%s", YearOfDeclaration, MonthOfDeclaration, DayOfDeclaration))]
  }
  else if (all(c("DeclarationYear", "DeclarationMonth", "DeclarationDay") %in% names(BDS))) {
    BDS[, date := as.IDate(sprintf("%s-%s-%s", DeclarationYear, DeclarationMonth, DeclarationDay))]
  } else {
    BDS[, date := NA]
  }

 # BDS[, weight := as.numeric(weight)]
   BDS[, date := as.POSIXct(paste0(date), format= "%Y-%m-%d")]

    return(BDS)

  } else if(file_test("-d", path)) {

    files <- list.files(path, pattern = ".csv",
                        full.names = T,
                        recursive = T)

    BDS_all <- data.table()

    for(f in files) {

      BDS_all <- rbind(BDS_all, read_ipaffs(f), fill=T)
    }


    return(BDS_all)

  } else {
    stop("path is not a file or directory.")
  }

}
