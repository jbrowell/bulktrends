# Synthetic data helpers for anomaly-detection tests

# Monthly series with trend, seasonality, optional spike and level shift
make_monthly_ts <- function(
  n = 72,
  start = as.Date("2018-01-01"),
  spike_at = NULL,
  shift_at = NULL,
  seed = 1
) {
  set.seed(seed)
  dates <- seq(start, by = "month", length.out = n)
  t <- seq_len(n)
  y <- 100 + 0.5 * t + 10 * sin(2 * pi * t / 12) + stats::rnorm(n, sd = 2)
  if (!is.null(spike_at)) {
    y[spike_at] <- y[spike_at] + 60
  }
  if (!is.null(shift_at)) {
    y[shift_at:n] <- y[shift_at:n] + 50
  }
  data.table::data.table(DATE_START = dates, NET_MASS = y)
}

# Keyed tsibble with two monthly series
make_keyed_tsibble <- function() {
  a <- make_monthly_ts(spike_at = 30, seed = 1)
  a[, COMCODE := "0101000000"]
  b <- make_monthly_ts(shift_at = 40, seed = 2)
  b[, COMCODE := "0201000000"]
  d <- rbind(a, b)
  d[, DATE_START := tsibble::yearmonth(DATE_START)]
  tsibble::as_tsibble(d, key = "COMCODE", index = "DATE_START")
}
