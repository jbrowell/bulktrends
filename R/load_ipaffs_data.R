#' Load bulk daily data from IPAFFS

read_ipaffs <- function(path) {

  if( file_test("-f", path) ) {

    BDS <- data.table::fread(path, colClasses = "character")

    col_names <- list(DATE_START    = c("DeclarationDate", "DateOfArrivalAtBIP", "DateOfArrival", "Declaration", "ArrivalAtBip"),  #"DateofDeclaration"
                      COMCODE = c("CommodityCode", "Commodities", "Commodity"),
                      NET_MASS  = c("TotalOfNetWeightKG", "NetWeightKg", "TotalNetWeight_Kg", "NetWeight(Kg)", "TotalNetWeightkg", "TotalNetWeight_kg", "TotalNetWeight_KG", "TotalNetWeight(kg)"))

    for (i in names(col_names)) {

      old_names <- col_names[[i]]
      match <- intersect(old_names, names(BDS))
      if (length(match))
        setnames(BDS, match, i)}
      #else {stop(paste("Missing column for", i, "in dataset"))}}


  if ("DATE_START" %in% names(BDS)) {
    BDS[, DATE_START := as.IDate(DATE_START)]
  }
  else if (all(c("YearOfDeclaration", "MonthOfDeclaration", "DayOfDeclaration") %in% names(BDS))) {
    #BDS[, DATE_START := as.IDate(sprintf("%s-%s-%s", YearOfDeclaration, MonthOfDeclaration, DayOfDeclaration))]
    BDS[, DATE_START := as.IDate(paste0(YearOfDeclaration,"-",MonthOfDeclaration,"-",DayOfDeclaration),format="%Y-%m-%d")]
  }
  else if (all(c("DeclarationYear", "DeclarationMonth", "DeclarationDay") %in% names(BDS))) {
    #BDS[, DATE_START := as.IDate(sprintf("%s-%s-%s", DeclarationYear, DeclarationMonth, DeclarationDay))]
    BDS[, DATE_START := as.IDate(paste0(DeclarationYear,"-",DeclarationMonth,"-",DeclarationDay),format="%Y-%m-%d")]
  } else {
    BDS[, DATE_START := NA]
  }

#as.IDate(paste0(YearOfDeclaration,"-",MonthOfDeclaration,"-",DayOfDeclaration),format="%Y-%m-%d")

   BDS[, NET_MASS := as.numeric(NET_MASS)]
   #BDS[, date := as.POSIXct(paste0(date), format= "%Y-%m-%d", tz="GB")]

   #BDS[,DATE_START := as.IDate(paste0(date,"01"),format="%Y%m%d")]
   #BDS[,DATE_END := DATE_START + base::months(1) - lubridate::days(1)]
   BDS[,DATE_END := DATE_START]


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
