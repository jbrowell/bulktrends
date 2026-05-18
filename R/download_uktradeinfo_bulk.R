#' Download UK Trade Info bulk data sets
#'
#' Scrapes the UK Trade Info bulk data sets archive page and downloads
#' matching ZIP files to a local directory.
#'
#' @param dest_dir Path to an existing directory where files will be saved.
#' @param type Character vector of data types to download. One or more of
#'   `"imports"`, `"exports"`, `"control"`, `"importer_details"`,
#'   `"exporter_details"`, `"preference"`. Defaults to `"imports"`.
#' @param overwrite Logical. If `FALSE` (default), files already present in
#'   `dest_dir` are skipped.
#'
#' @return Invisibly returns a character vector of paths to downloaded files.
#'
#' @details
#' Files are sourced from
#' \url{https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/}.
#'
#' File name patterns used to identify data types:
#' \itemize{
#'   \item `"imports"` — monthly import archives (`BDSImpYYMM`)
#'   \item `"exports"` — monthly export archives (`BDSExpYYMM`)
#'   \item `"control"` — commodity code and unit reference files (`SMKA12`)
#'   \item `"importer_details"` — UK/GB importer name and address files (`BDSImpDet`)
#'   \item `"exporter_details"` — UK/GB exporter name and address files (`BDSExpDet`)
#'   \item `"preference"` — import data by trade preference/regime (`BDSPref`)
#' }
#'
#' Requires the \pkg{rvest} package. Install it with
#' `install.packages("rvest")` if needed.
#'
#' @examples
#' \dontrun{
#' # Download import archives to a local folder
#' download_uktradeinfo_bulk(dest_dir = "~/trade_data", type = "imports")
#'
#' # Download both imports and the control file
#' download_uktradeinfo_bulk(
#'   dest_dir = "~/trade_data",
#'   type = c("imports", "control")
#' )
#' }
#'
#' @export
download_uktradeinfo_bulk <- function(
  dest_dir,
  type = "imports",
  overwrite = FALSE
) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop(
      "Package 'rvest' is required. Install it with: install.packages('rvest')",
      call. = FALSE
    )
  }

  type <- match.arg(
    type,
    c(
      "imports",
      "exports",
      "control",
      "importer_details",
      "exporter_details",
      "preference"
    ),
    several.ok = TRUE
  )

  if (!file_test("-d", dest_dir)) {
    stop("'dest_dir' does not exist: ", dest_dir, call. = FALSE)
  }

  base_url <- "https://www.uktradeinfo.com"
  archive_url <- paste0(
    base_url,
    "/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/"
  )

  page <- tryCatch(
    rvest::read_html(archive_url),
    error = function(e) {
      stop(
        "Could not fetch archive page from ",
        archive_url,
        ".\n",
        "Check your internet connection.\nOriginal error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  hrefs <- rvest::html_attr(
    rvest::html_elements(page, "a"),
    "href"
  )
  zip_hrefs <- hrefs[
    !is.na(hrefs) & grepl("\\.zip$", hrefs, ignore.case = TRUE)
  ]
  all_links <- paste0(base_url, zip_hrefs)

  patterns <- list(
    imports = "(?i)bdsimp(?!det)",
    exports = "(?i)bdsexp(?!det)",
    control = "(?i)smka12",
    importer_details = "(?i)bdsimpdet",
    exporter_details = "(?i)bdsexpdet",
    preference = "(?i)bdspref"
  )

  combined_pattern <- paste(unname(unlist(patterns[type])), collapse = "|")
  matched_links <- all_links[
    grepl(combined_pattern, basename(all_links), perl = TRUE)
  ]

  if (length(matched_links) == 0) {
    message(
      "No matching files found for type(s): ",
      paste(type, collapse = ", ")
    )
    return(invisible(character(0)))
  }

  message("Found ", length(matched_links), " file(s) to download.")

  downloaded <- character(0)

  for (url in matched_links) {
    fname <- basename(url)
    dest_path <- file.path(dest_dir, fname)

    if (!overwrite && file.exists(dest_path)) {
      message("Skipping (already exists): ", fname)
      next
    }

    message("Downloading: ", fname)
    result <- tryCatch(
      {
        utils::download.file(
          url,
          destfile = dest_path,
          mode = "wb",
          quiet = TRUE
        )
        TRUE
      },
      error = function(e) {
        warning(
          "Failed to download ",
          fname,
          ": ",
          conditionMessage(e),
          call. = FALSE
        )
        FALSE
      }
    )

    if (isTRUE(result) && file.exists(dest_path)) {
      downloaded <- c(downloaded, dest_path)
    }
  }

  message("Downloaded ", length(downloaded), " file(s) to ", dest_dir)
  invisible(downloaded)
}
