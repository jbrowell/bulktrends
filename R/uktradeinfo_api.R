#' Request data from UK Trades API
#'
#' Basic example. See https://www.uktradeinfo.com/api-documentation/
#'
#' @export
uktrades_request <- function() {

  require(jsonlite)


  test <- jsonlite::fromJSON("https://api.uktradeinfo.com/Commodity?$filter=Hs6Code eq '010129'&$expand=Exports($filter=MonthId ge 201901 and MonthId le 201912 and startswith(Trader/PostCode, 'CB8'); $expand=Trader)")

  return(test)

}
