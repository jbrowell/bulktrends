#' Request data from UK Trades API
#'
#' @param endpoint  OData queryable endpoint appended to api URL. See details.
#'
#' @details
#' For endpoint documentation visit https://www.uktradeinfo.com/api-documentation/.
#' `endpoint` is appended to "https://api.uktradeinfo.com/".
#'
#' @return Result of API query.
#'
#' @export
uktrades_request <- function(endpoint) {

  require(jsonlite)

  request <- paste0("https://api.uktradeinfo.com/",endpoint)

  return(jsonlite::fromJSON(request))

}
