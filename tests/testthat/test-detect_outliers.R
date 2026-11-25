test_that("detect_outliers returns expected structure", {
  res <- detect_outliers(make_monthly_ts(spike_at = 30), "NET_MASS")
  expect_type(res, "list")
  expect_named(res, c("data", "outliers"))
  expect_s3_class(res$outliers, "data.frame")
  expect_true(all(c("type", "ind") %in% names(res$outliers)))
})

test_that("detect_outliers finds an injected spike and adds covariates", {
  d <- make_monthly_ts(spike_at = 30)
  xreg <- model.matrix(
    ~ linear_trend + annual_sin + annual_cos,
    data = add_date_features(d, "DATE_START", freq = "month")
  )
  res <- detect_outliers(d, "NET_MASS", xreg = xreg)

  expect_true(30 %in% res$outliers$ind)
  new_cols <- setdiff(names(res$data), names(d))
  expect_gt(length(new_cols), 0)
  expect_true(all(grepl("^(AO|TC)[0-9]+$", new_cols)))
  expect_equal(nrow(res$data), nrow(d))
})

test_that("detect_outliers leaves data unchanged when nothing is found", {
  d <- make_monthly_ts(spike_at = 30)
  res <- detect_outliers(d, "NET_MASS", tso_params = list(cval = 100))
  expect_equal(nrow(res$outliers), 0)
  expect_equal(names(res$data), names(d))
})

test_that("detect_outliers returns tso object when requested", {
  res <- detect_outliers(
    make_monthly_ts(spike_at = 30),
    "NET_MASS",
    return_tso = TRUE
  )
  expect_true("tso" %in% names(res))
  expect_s3_class(res$tso, "tsoutliers")
})

test_that("detect_outliers handles zero-column xreg", {
  d <- make_monthly_ts(spike_at = 30)
  xreg <- matrix(numeric(0), nrow = nrow(d), ncol = 0)
  expect_no_error(detect_outliers(d, "NET_MASS", xreg = xreg))
})

test_that("detect_outliers supports scaling", {
  res <- detect_outliers(
    make_monthly_ts(spike_at = 30),
    "NET_MASS",
    scale_ts = TRUE,
    return_tso = TRUE
  )
  y <- as.numeric(res$tso$y)
  expect_equal(mean(y), 0, tolerance = 1e-8)
  expect_equal(stats::sd(y), 1, tolerance = 1e-8)
})

test_that("detect_outliers returns NULL when tso fails", {
  d <- make_monthly_ts()
  expect_message(
    res <- detect_outliers(
      d,
      "NET_MASS",
      tso_params = list(y = stats::as.ts(rep(NA_real_, nrow(d))))
    ),
    "Outlier detection failed"
  )
  expect_null(res)
})
