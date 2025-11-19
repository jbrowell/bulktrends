#' Visualisation of a given commodity code.

#' This function displays the total weight per month for all 
#' levels of hierarchy (HS2, HS4, HS6 and CN8) found in the dataset.
#' @param import_data A data.table containing trade data. Must include "COMCODE"
#' @param code A character string representing any HS2/HS4/HS6/CN8 code
#' @param lookup_table A data.table containing details of each code level
#'
#' @return a display of total weight per month per code
#' 
#' @export
comcode_plot <- function(import_data,
                         code,
                         lookup_table,
                         max_unique_comcodes=10) {
  
  #"code" info to meta
  lookup_table <- as.data.table(lookup_table)
  meta <- lookup_table[Cn8Code == code | Hs2Code == code |
                        Hs4Code == code | Hs6Code == code]
  
  #extract related codes once
  allcodes <- na.omit(unique(meta$Cn8Code))
  
  #filter main dataset for all matching codes and merge with meta
  import_data <- import_data[ COMCODE %in% allcodes ]
  
  #add month to import_data
  import_data[, month := as.POSIXct(paste0(PERREF,"01"),format="%Y%m%d")]
  
  #import_data[ , .(NET_MASS = sum(as.numeric(NET_MASS))), by = c("year","COMCODE") ]
  
  if ( import_data[,length(unique(COMCODE))] > max_unique_comcodes) {
    topX <- import_data[,.(NET_MASS=sum(as.numeric(NET_MASS))),by=COMCODE][
      order(NET_MASS,decreasing = T),COMCODE][1:max_unique_comcodes]
    import_data[ ! COMCODE %in% topX, COMCODE := "Other"]
    import_data <- import_data[,.(NET_MASS=sum(as.numeric(NET_MASS))),by=c("COMCODE","month")]
  }
  
  import_data <- merge(import_data, meta,
                       by.x = "COMCODE", by.y = "Cn8Code",
                       all.x=T)
  
  
  ## plot year and net mass
  ggplot(import_data, aes(x = month, y = as.numeric(NET_MASS), fill = factor(COMCODE))) +
    # geom_col() +
    geom_area() +
    scale_fill_manual(values = colorRampPalette(c("#c6dbef", "#6baed6", "#08306b"))(length(unique(import_data$COMCODE)))) +
    labs(title = "Total Weight by Commodity Code",
         subtitle = paste0(
           if (nchar(code) >= 2) {
             import_data[1,paste(Hs2Code, "-", stringr::str_wrap(Hs2Description, width = 85),"\n")]
           } else "",
           if (nchar(code) >= 4) {
             import_data[1,paste(Hs4Code, "-", stringr::str_wrap(Hs4Description, width = 85),"\n")]
           } else "",
           if (nchar(code) >= 6) {
             import_data[1,paste(Hs6Code, "-", stringr::str_wrap(Hs6Description, width = 85),"\n")]
           } else "",
           if (nchar(code) == 8) {
             import_data[1,paste(COMCODE, "-", stringr::str_wrap(Cn8LongDescription, width = 85),"\n")]
           } else ""),
         x = "Year",
         y = "Weight [kg/month]",
         fill = paste0("Commodity Code\n(Top ",max_unique_comcodes,")")) +
    guides(fill = guide_legend(keywidth = 0.8, keyheight = 0.6, ncol = 2)) + 
    theme_minimal(base_family = "serif") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="top",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", size = 12,
                                    color = "#7D2239", lineheight = 1.1),
          plot.subtitle = element_text(face = "bold", size = 11, color = "#003865"),
          legend.text = element_text(size = 9, family= "serif"),
          legend.title = element_text(face = "bold", size = 11, color = "#003865"))+
    scale_y_continuous(labels = scales::comma)
}
