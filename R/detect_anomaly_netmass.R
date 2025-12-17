# Large scale anomaly detection (Work in progress)

chapters <- unique(substr(imports$COMCODE, 1,2))
all_outliers <- list()

for (i in chapters){
  #create time series
  ts_data <- extract_netmass_ts(imports, i)

  #detect outliers using function identify_best_model() in xreg
  selected_model <- select_best_model(ts_data, metric = "aic")
  detect_anomaly <- tso(y = ts_data,#log(ts_data),
                        cval=5,
                        types = c("AO", "LS", "TC", "IO"),
                        tsmethod = "auto.arima",
                        xreg = selected_model$xreg)

  #visualise results
  if (nrow(detect_anomaly$outliers)>0){
    plot.tsoutliers(detect_anomaly)
    title(main = paste("Outliers for", i),col.main = "darkblue", cex.main = 1, line = 2.5)


    #store outliers data produced
    outliers_dt <- as.data.table(detect_anomaly$outliers)
    outliers_dt[, chapter := i]
    outliers_dt[, model_formula := selected_model$formula]
    all_outliers[[i]] <- outliers_dt
  } else {
    all_outliers[[i]] <- data.table(chapter = i,
                                    model_formula = selected_model$formula)
  }
  #flag "volatile" chapters?
}

# Combine all results into a single data.table
all_outliers_dt <- rbindlist(all_outliers, fill = TRUE)


all_outliers_dt[Chapters=="03",]


