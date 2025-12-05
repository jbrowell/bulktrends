#' Visualisation of a given commodity code.

#' This function displays the total weight per month for all 
#' levels of hierarchy (HS2, HS4, HS6 and CN8) found in the dataset.
#' 
#' @param import_data A `data.table` containing trade data. Must include "COMCODE".
#' @param code A character string representing any HS2/HS4/HS6/CN8 code.
#' @param variable A character string representing the variable to be visualised. If "volume" is supplied, 
#' the function will compute it internally, otherwise it will use an existing column in `import_data`.
#' @param lookup_table A `data.table` containing details of each code level.
#'
#' @return A display of total weight per month per code.
#' 
#' @export
comcode_plot <- function(import_data,
                         code,
                         variable,
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
  
  #generate volume if needed
  if (variable == "volume") {
    import_data[, volume := .N, by="month"]
    variable <- "volume"
  } else {
    variable <- variable
  }
  
  if ( import_data[,length(unique(COMCODE))] > max_unique_comcodes) {
    topX <- import_data[,.(variable=sum(as.numeric(variable))),by=COMCODE][
      order(variable,decreasing = T),COMCODE][1:max_unique_comcodes]
    import_data[ ! COMCODE %in% topX, COMCODE := "Other"]
    
    #import_data <- import_data[,.(variable=sum(as.numeric(variable))),by=c("COMCODE". ,"month")]
    agg <- import_data[, .(variable = sum(as.numeric(variable))), 
                       by=c("COMCODE","month")]
  } 
  else {
    agg <- import_data
  }
  
  #merge data and comcode descriptions
  import_data <- merge(import_data, meta,
                       by.x = "COMCODE", by.y = "Cn8Code",
                       all.x=T)
  
  #check columns
  print(names(import_data))
  
  #plot month and variable
  ggplot(import_data, aes(x = month, y = as.numeric(.data[[variable]]), fill = factor(COMCODE))) +
    geom_area() +
    scale_fill_manual(values = colorRampPalette(c("#c6dbef", "#6baed6", "#08306b"))(length(unique(import_data$COMCODE)))) +
    labs(title = if (variable == "volume") {
      "Total Volume by Commodity Code"
    } else if (variable == "NET_MASS") {
      "Total Weight by Commodity Code"
    } else if (variable == "STAT_VALUE") {
      "Total Value (incl. cost, insurance and freight) by Commodity Code"
    },
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
    y = if (variable == "volume") {
      "Volume [Count/month]"
    } else if (variable == "NET_MASS") {
      "Weight [kg/month]"
    } else if (variable == "STAT_VALUE") {
      "Value [£/month]"
    },
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
