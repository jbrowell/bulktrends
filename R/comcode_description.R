#' Hierarchical classification of a given commodity code
#'
#' This function displays the information related to a given commodity code and
#' any of its subcodes (HS2/HS4/HS6/CN8).
#'
#' @param code A character string representing any HS2/HS4/HS6/CN8 code.
#' @param lookup_table A `data.table` containing details of each code level.
#'
#' @return A display of the hierarchical information of code.
#'
#' @export
comcode_description <- function(code, lookup_table) {
  lookup_table <- as.data.table(lookup_table)
  meta <- lookup_table[Cn8Code == code | Hs2Code == code | Hs4Code == code | Hs6Code == code]

  desc <- list(Hs2 = paste(meta$Hs2Code[1], " — ", meta$Hs2Description[1]),
               Hs4 = paste(meta$Hs4Code[1], " — ", meta$Hs4Description[1]),
               Hs6 = paste(meta$Hs6Code[1], " — ", meta$Hs6Description[1]),
               Cn8 = paste(meta$Cn8Code[1], " — ", meta$Cn8LongDescription[1]))

  # structured format
  cat("\nHierarchical Description for Code:", code, "\n")
  cat("---------------------------------------------\n")
  cat("Chapter (Hs2):          ", desc$Hs2, "\n")
  cat("Heading (Hs4):          ", desc$Hs4, "\n")
  cat("Subheading (Hs6):       ", desc$Hs6, "\n")
  cat("Commodity code (Cn8):   ", desc$Cn8, "\n")
  cat("---------------------------------------------\n\n")
}


