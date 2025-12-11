
# Large scale anomaly detection (Work in progress)

chapters <- unique(substr(imports$COMCODE, 1,2))
result_list <- vector("list", length(chapters))


for (i in chapters){
#create time series
ts_data <- extract_netmass_ts(imports, i)

#detect anomaly using tsouliers::tso
detect_anomaly <- tso(y = ts_data/1000000, 
                      types = c("AO", "LS", "TC", "IO"), 
                      tsmethod = "arima",
                      args.tsmethod = list(order = c(1,0,0)))
                       #                    xreg=cbind(1:length(ts_data), 
                          #                            fourier(ts_data, K=1)))


#visualise results
plot.tsoutliers(detect_anomaly)
title(main = paste("Outliers for", i), col.main = "darkblue", cex.main = 1, line = 2.5)

#check and store number of outliers
count_outlier <- nrow(detect_anomaly$outliers)
result_list[[i]] <- data.table(code = i, outliers = count_outlier)

#store other results?

#flag "volatile" chapters

}




#list of all outliers in all chapters
rbindlist(result_list)



# tso objects ------------------------------------------------
# detect_anomaly$yadj        # adjusted series (without outliers)
# detect_anomaly$effects     # estimated effect of each outlier
# detect_anomaly$fit         # arima() results
# detect_anomaly$outliers    # data table of detected outliers
# -------------------------------------------------------------
