
NULL

# Large scale anomaly detection (Work in progress)

chapters <- unique(substr(imports$COMCODE, 1,2))
all_outliers <- list()

for (i in chapters[01:10]){
#create time series
ts_data <- extract_netmass_ts(imports, i)

detect_anomaly <- tso(y = log(ts_data),
                      types = c("AO", "LS", "TC", "IO"),
                      tsmethod = "auto.arima",
                      xreg = identify_best_model(ts_data))

#visualise results
plot.tsoutliers(detect_anomaly)
title(main = paste("Outliers for", i), col.main = "darkblue", cex.main = 1, line = 2.5)

#check and store number of outliers
outliers_dt <- as.data.table(detect_anomaly$outliers)
outliers_dt[, Chapters := i]  # add simcode column
all_outliers[[i]] <- outliers_dt

#flag "volatile" chapters?
}

# Combine all results into a single data.table
all_outliers_dt <- rbindlist(all_outliers, use.names = TRUE, fill = TRUE)




# tso objects ------------------------------------------------
# detect_anomaly$yadj        # adjusted series (without outliers)
# detect_anomaly$effects     # estimated effect of each outlier
# detect_anomaly$fit         # arima() results
# detect_anomaly$outliers    # data table of detected outliers
# -------------------------------------------------------------
