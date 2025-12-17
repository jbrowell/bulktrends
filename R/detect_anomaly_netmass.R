# Large scale anomaly detection (Work in progress)

chapters <- unique(substr(imports$COMCODE, 1,2))
all_outliers <- list()

for (i in chapters){
  #create time series
  ts_data <- extract_netmass_ts(imports, i)

  #detect outliers using function identify_best_model() in xreg
  detect_anomaly <- tso(y = log(ts_data),
                        cval=5,
                        types = c("AO", "LS", "TC", "IO"),
                        tsmethod = "auto.arima",
                        xreg = select_best_model(ts_data, metric = "aic"))

  #visualise results
  plot.tsoutliers(detect_anomaly,
                  main = paste("Outliers for", i),
                  col.main = "darkblue", cex.main = 1, line = 2.5)

  #store outliers data produced
  outliers_dt <- as.data.table(detect_anomaly$outliers)
  outliers_dt[, chapters := i]
  all_outliers[[i]] <- outliers_dt

  #flag "volatile" chapters?
}

# Combine all results into a single data.table
all_outliers_dt <- rbindlist(all_outliers, fill = TRUE)


all_outliers_dt[Chapters=="03",]


