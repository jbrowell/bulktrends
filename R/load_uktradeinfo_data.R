#' Load bulk data form UK Trade Info
#'
#' Load a single bulk data file of all files in a directory (and its
#' sub-directories) from UK Trade Info.
#'
#' @param path Path to a file to read, or directory containing multiple files,
#' files in sub-directories are read recursively.
#'
#' @return A `data.table` of trade data with a POSIXct timestamp.
#'
#' @export
read_uktradeinfo <- function(path, .top_level = T) {

  # capture the plan the user had in place (if any) before the function starts
  if (.top_level) {   # ensure only the top-level call manages the future plan (i.e. allow the recursive calls in route 2 below to skip the plan logic)
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = T)   # when the function finishes, restore the user’s original plan
  }

  # route 1 - path is a single file:
  if (file_test("-f", path)) {

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

    BDS[,NET_MASS:=suppressWarnings(as.numeric(NET_MASS))]
    BDS[,STAT_VALUE:=suppressWarnings(as.numeric(STAT_VALUE))]
    BDS[, month := as.POSIXct(paste0(PERREF,"01"),format="%Y%m%d")]

    return(BDS)
  }

  # route 2 - path is a directory:
  else if (file_test("-d", path)) {

    files <- list.files(path, pattern = ".txt",
                        full.names = T,
                        recursive = T)

    # parallelize
    # only the top-level call may modify the plan (and only if the user has not already set a plan)
    if (.top_level && inherits(old_plan, "sequential")) {  
      future::plan(multisession, workers = future::availableCores())
    }

    BDS_all <- data.table::rbindlist(
      future.apply::future_lapply(files, read_uktradeinfo,
      .top_level = F
      ),
      use.names = T,
      fill = F
    )

    return(BDS_all)

  } else {
  stop("path is not a file or directory.")
  }

  }
