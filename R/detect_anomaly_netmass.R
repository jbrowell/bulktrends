# Large scale anomaly detection (Work in progress)

HS2 <- unique(substr(imports$COMCODE, 1,2))
HS4 <- unique(substr(imports$COMCODE, 1,4))
HS6 <- unique(substr(imports$COMCODE, 1,6))
CN8 <- unique(imports$COMCODE)

all_outliers <- list()

#detect_anomaly_netmass <- function(codes){

for (i in HS2){
  #create time series
  ts_data <- extract_netmass_ts(imports, i)

  #detect outliers using function select_best_model()
  selected_model <- select_best_model(ts_data, metric = "aic")
  detect_anomaly <- try(tso(y = ts_data,#log(ts_data),
                        cval=5,
                        types = c("AO", "LS", "TC", "IO"),
                        tsmethod = "auto.arima",
                        xreg = selected_model$xreg), silent=T)

  if ("try-error" %in% class(detect_anomaly)){
    cat("code", i, "failed\n")
    next
  }

  #visualise results
  if (nrow(detect_anomaly$outliers)>0){
    plot.tsoutliers(detect_anomaly)
    title(main = paste("Outliers for", i),col.main = "darkblue", cex.main = 1, line = 2.5)


    #store outliers data produced
    outliers_dt <- as.data.table(detect_anomaly$outliers)
    outliers_dt[, HS2 := i]
    outliers_dt[, model_formula := selected_model$formula]
    all_outliers[[i]] <- outliers_dt
  } else {
    all_outliers[[i]] <- data.table(HS2 = i,
                                    model_formula = selected_model$formula)
  }
  #flag "volatile" chapters?
}
  rbindlist(all_outliers, fill = TRUE)
#}

#combine all results into a single data.table
all_outliers_dt <- rbindlist(all_outliers, fill = TRUE)


all_outliers_dt[chapter=="02",]

#detect_anomaly_netmass(codes = HS2)
