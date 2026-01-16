#' View User Guide
#'
#' A function that opens the bulktrends user guide in system browser.
#'
#' @param path Optional. Path to specific instance of UserGuide.html. If NULL, it will be retrieved from the current installation of bulktrends.
#'
open_userguide <- function(path=NULL) {

  if( is.null(path) ) {
    path <- try(
      system.file("notebooks", "UserGuide.html", package = "bulktrends")
    )
  }

  if( file.exists(path) ){
    browseURL(path)
  } else {
    stop("Couldn't find UserGuide.html")
  }

}
