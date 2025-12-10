#' Load bulk data form UK Trade Info
#'
#' Load a single bulk data file of all files in a directory (and its
#' subdirectories) from UK Trade Info.
#'
#' @param path Path to a file to read, or directory containing multiple files,
#' subdirectories are recursed.
#'
#' @return A `data.table` of trade data.
#'
#' @export
read_uktradeinfo <- function(path) {

  if( file_test("-f", path) ) {

    BDS <- data.table::fread(path, header = F, strip.white = F, sep = NULL)

    BDS <- BDS[,.(PERREF=substr(V1,1,6),
                  TYPE=substr(V1,7,7),
                  MONTHAC=substr(V1,8,13),
                  COMCODE=substr(V1,14,21),
                  SITC=substr(V1,22,26),
                  COD_SEQ=substr(V1,27,29),
                  COD_ALPHA=substr(V1,30,31),
                  PORT_SEQ=substr(V1,32,34),
                  PORT_CODE=substr(V1,35,37),
                  COO_SEQ=substr(V1,38,40),
                  COO_ALPHA=substr(V1,41,42),
                  MODE_OF_TRANSPORT=substr(V1,43,44),
                  STAT_VALUE=substr(V1,45,56),
                  NET_MASS=substr(V1,57,68),
                  SUMM_UNIT=substr(V1,69,80),
                  SUPRESSION=substr(V1,81,81),
                  FLOW=substr(V1,82,84),
                  REC_TYPE=substr(V1,85,85))]

    BDS[,NET_MASS:=as.numeric(NET_MASS)]
    BDS[,STAT_VALUE:=as.numeric(STAT_VALUE)]

    return(BDS)

  } else if(file_test("-d", path)) {

    files <- list.files(path,pattern = ".txt",
                        full.names = T,
                        recursive = T)

    BDS_all <- data.table()

    for(f in files) {

      BDS_all <- rbind(BDS_all, read_uktradeinfo(f))

    }

    return(BDS_all)

  } else {
    stop("path is not a file or directory.")
  }

}
