#' Load bulk data file form UK Trade Info
#'
#' Load a single bulk data file from UK Trade Info
#'
#' @param path Path to the file to load
#'
#' @return A `data.table` of trade data.
#'
#' @export
load_uktradeinfo <- function(path) {

  BDSimp <- data.table::fread(path, header = F, strip.white = F)

  BDSimp <- data.table(
    transform(BDSimp,
              period_reference=substr(V1,1,6),
              type=substr(V1,7,7),
              month_of_account=substr(V1,8,13),
              comcode=substr(V1,14,21),
              SITC=substr(V1,22,26),
              COD_SEQUENCE=substr(V1,27,29),
              COD_ALPHA=substr(V1,30,31),
              port_code=substr(V1,32,34),
              PORT_ALPHA=substr(V1,35,37),
              country_of_origin=substr(V2,1,3),
              COO_ALPHA=substr(V2,4,5),
              mode_of_transport=substr(V2,6,7),
              statistical_value=substr(V2,8,9),
              net_mass=substr(V2,10,21),
              supplementary_unit=substr(V2,22,33),
              suppression=substr(V2,34,34),
              flow=substr(V2,35,37),
              record_type=substr(V2,38,41)))

  BDSimp$V1 <- NULL
  BDSimp$V2 <- NULL

  return(BDSimp)

}
