#' Display of outliers from residuals for a given commodity code
#'
#' This function displays outliers in residuals of import volume after estimating 
#' an ARX model for any level of hierarchy (HS2, HS4, HS6 and CN8) found in the dataset.
#'
#' @param import_data A `data.table` containing trade data. Must include "COMCODE".
#' @param code A character string representing any HS2/HS4/HS6/CN8 code.
#' @param lookup_table A `data.table` containing details of each code level.
#'
#' @return A scatter plot of fitted values against residuals and 
#' a display of residuals over time per month per code.
#'
#' @export

monthly_volume_outliers <- function(import_data,
                                    code,
                                    lookup_table) {
  
  #"code" info to meta
  lookup_table <- as.data.table(lookup_table)
  meta <- lookup_table[Cn8Code == code | Hs2Code == code |
                         Hs4Code == code | Hs6Code == code]
  allcodes <- na.omit(unique(meta$Cn8Code))
  
  #filter main dataset and generate volume for code
  import_data <- import_data[ COMCODE %in% allcodes ]
  import_data <- import_data[, .(volume = .N), by="month"]
  
  #declare time series
  ts_imports <- ts(import_data$volume, 
                   start = c(2016, 01), 
                   frequency = 12)
  
  #ARX model using forecast::Arima
  trend <- 1:length(ts_imports)
  seas <- fourier(ts_imports, K=1)   #K=1 creates one pair (sin(2πt/12) and cos(2πt/12))
  #X <- model.matrix(~trend + seas)
  X <- cbind(trend, seas)
  
  model <- Arima(ts_imports, 
                 order = c(1,0,0), 
                 xreg = X)
  
  #convert ts data into data.table for ggplot
  res <- data.table(month = import_data$month,
                    fitted = as.numeric(fitted(model)),
                    resid  = as.numeric(residuals(model)))
  
  #find outliers
  threshold <- median(res$resid) + 3*mad(res$resid)
  outlier <- abs(res$resid) > threshold
  
  #plot with threshold
  a <- ggplot(res, aes(x = month, y = resid)) +
    geom_point(aes(colour = outlier), size = 1.2) +
    scale_colour_manual(values = c("FALSE" = "#003865",
                                   "TRUE" = "red")) +
    labs(title = paste0("Residual Outliers for " , code),
         x = "Year", 
         y = "Residuals")+
    theme_minimal(base_family = "serif")+
    theme(#panel.grid.major.x = element_blank(),
      #panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12,
                                colour = "#7D2239", lineheight = 1.1),
      legend.title = element_text(face = "bold", size = 11, colour = "black"))
  
  #plot fitted vs residuals 1
  b <- ggplot(res, aes(x = fitted, y = resid)) +
    geom_point(size=1, colour="#003865") +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "darkgrey")+
    labs(title = paste0("Fitted values vs Residuals for " , code),
         x = "Fitted Values", 
         y = "Residuals")+
    theme_minimal(base_family = "serif")+
    theme(#panel.grid.major.x = element_blank(),
      #panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12,
                                colour = "#7D2239", lineheight = 1.1),
      legend.title = element_text(face = "bold", size = 11, colour = "black"))
  
  #plot fitted vs residuals 2
  c <- ggplot(res, aes(x = fitted)) +
    geom_line(aes(y = fitted), colour = "#6baed6", linewidth = 0.8) + 
    geom_point(aes(y = resid), colour = "#003865", size = 1) +
    theme_minimal(base_family = "serif") +
    labs(title = paste0("Fitted values vs Residuals for " , code),
         x = "Fitted Values",
         y = "Residuals") +
    theme(#panel.grid.major.x = element_blank(),
      #panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12,
                                colour = "#7D2239", lineheight = 1.1))
  #plot residuals over time
  d <- ggplot(res, aes(x = month, y = resid)) +
    geom_line(color = "darkgrey") +
    geom_point(size = 1.2, color = "#003865") +
    theme_minimal(base_family = "serif") +
    labs(title = paste0("Residuals Over Time for ", code),
         x = "Year", y = "Residuals")+
    theme(#panel.grid.major.x = element_blank(),
      #panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 12,
                                colour = "#7D2239", lineheight = 1.1))
  
  return(list(model=model, plot=a, plot=b, plot=c,plot=d))
}
