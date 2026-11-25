for (bm in c("breakpoints", "cpt.reg")) {
  test_that(paste0("detect_breaks finds an injected level shift (", bm, ")"), {
    d <- make_monthly_ts(shift_at = 40)
    res <- detect_breaks(
      data = d,
      formula = NET_MASS ~ 1,
      break_method = bm,
      date_col = "DATE_START"
    )

    expect_type(res, "list")
    expect_named(res, c("break_entry", "segments"))
    expect_s3_class(res$break_entry, "data.table")
    expect_named(res$break_entry, c("type", "ind", "time"))
    expect_true(all(res$break_entry$type == "SB"))
    expect_s3_class(res$break_entry$time, "Date")
    expect_equal(res$break_entry$time, d$DATE_START[res$break_entry$ind])
    expect_true(any(abs(res$break_entry$ind - 39) <= 2))

    expect_s3_class(res$segments, "data.table")
    expect_equal(nrow(res$segments), nrow(d))
    expect_s3_class(res$segments$segments, "factor")
    expect_equal(nlevels(res$segments$segments), nrow(res$break_entry) + 1)
  })

  test_that(
    paste0("detect_breaks returns NULL when there is no break (", bm, ")"),
    {
      set.seed(42)
      d <- data.table::data.table(
        DATE_START = seq(as.Date("2018-01-01"), by = "month", length.out = 60),
        NET_MASS = 100 + stats::rnorm(60)
      )
      expect_null(detect_breaks(
        data = d,
        formula = NET_MASS ~ 1,
        break_method = bm,
        date_col = "DATE_START"
      ))
    }
  )

  test_that(
    paste0("detect_breaks errors on missing response column (", bm, ")"),
    {
      d <- make_monthly_ts()
      expect_error(detect_breaks(
        data = d,
        formula = missing_col ~ 1,
        break_method = bm,
        date_col = "DATE_START"
      ))
    }
  )
}
