test_that("select_best_model requires formulas or response_col", {
  expect_error(
    select_best_model(make_monthly_ts()),
    "Must supply either formulas or response_col"
  )
})

test_that("select_best_model errors for unsupported frequency without formulas", {
  d <- data.table::data.table(
    DATE_START = seq(as.Date("2020-01-06"), by = "week", length.out = 52),
    NET_MASS = stats::rnorm(52)
  )
  expect_error(
    select_best_model(d, response_col = "NET_MASS"),
    "isn't daily or monthly"
  )
})

test_that("select_best_model returns expected structure for monthly data", {
  res <- suppressWarnings(
    select_best_model(
      make_monthly_ts(),
      response_col = "NET_MASS",
      break_method = "cpt.reg"
    )
  )
  expect_type(res, "list")
  expect_true(all(c("data", "formula") %in% names(res)))
  expect_s3_class(res$data, "data.table")
  expect_s3_class(res$formula, "formula")
  expect_equal(all.vars(res$formula)[1], "NET_MASS")
  expect_true(all(
    c("linear_trend", "annual_sin", "annual_cos") %in% names(res$data)
  ))
})

test_that("select_best_model accepts data.frame input", {
  d <- as.data.frame(make_monthly_ts())
  res <- suppressWarnings(
    select_best_model(d, response_col = "NET_MASS", break_method = "cpt.reg")
  )
  expect_s3_class(res$data, "data.table")
})

test_that("select_best_model without break detection returns no break entry", {
  res <- select_best_model(
    make_monthly_ts(shift_at = 40),
    response_col = "NET_MASS",
    break_method = NULL
  )
  expect_null(res$break_entry)
  expect_false("segments" %in% names(res$data))
})

for (bm in c("breakpoints", "cpt.reg")) {
  test_that(
    paste0("select_best_model detects an injected level shift (", bm, ")"),
    {
      res <- suppressWarnings(select_best_model(
        make_monthly_ts(shift_at = 40),
        response_col = "NET_MASS",
        break_method = bm
      ))
      expect_s3_class(res$break_entry, "data.table")
      expect_true("segments" %in% names(res$data))
      expect_true("segments" %in% all.vars(res$formula))
      expect_true(any(abs(res$break_entry$ind - 39) <= 2))
    }
  )
}

test_that("select_best_model uses user-supplied formulas", {
  d <- add_date_features(make_monthly_ts(), "DATE_START", freq = "month")
  f <- list(NET_MASS ~ 1, NET_MASS ~ linear_trend)
  res <- select_best_model(d, formulas = f, break_method = NULL)
  expect_true(
    any(vapply(f, function(x) identical(x, res$formula), logical(1)))
  )
})

test_that("select_best_model respects the metric argument", {
  d <- make_monthly_ts()
  aic <- select_best_model(
    d,
    response_col = "NET_MASS",
    metric = AIC,
    break_method = "cpt.reg"
  )
  bic <- select_best_model(
    d,
    response_col = "NET_MASS",
    metric = BIC,
    break_method = "cpt.reg"
  )
  expect_s3_class(aic$formula, "formula")
  expect_s3_class(bic$formula, "formula")
})

for (bm in c("breakpoints", "cpt.reg")) {
  test_that(
    paste0("select_best_model break detection raises no warnings (", bm, ")"),
    {
      expect_no_warning(select_best_model(
        make_monthly_ts(shift_at = 40),
        response_col = "NET_MASS",
        break_method = bm
      ))
    }
  )
}
