# devtools::load_all()
# testthat::test_file("tests/testthat/test-comcode_plot.R")

test_that("comcode_plot returns a plot for net mass", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  lookup <- env[[ls(env)[1]]]

  import_data <- data.table::data.table(
    COMCODE = c("01011010", "01011010", "01011090"),
    DATE_START = as.Date(c("2024-01-01", "2024-02-01", "2024-01-01")),
    NET_MASS = c(10, 20, 5)
  )

  result <- comcode_plot(import_data, "01", lookup)

  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$title, "Total Weight by Commodity Code")
  expect_equal(result$labels$y, "Weight [kg/month]")
  expect_setequal(result$data$variable, c(10, 5, 20))
})

test_that("comcode_plot computes volume and labels it correctly", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  lookup <- env[[ls(env)[1]]]

  import_data <- data.table::data.table(
    COMCODE = c("01011010", "01011010", "01011090"),
    DATE_START = as.Date(c("2024-01-01", "2024-02-01", "2024-01-01"))
  )

  result <- comcode_plot(import_data, "01", lookup, variable = "volume")

  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$title, "Total Volume by Commodity Code")
  expect_equal(result$labels$y, "Volume [Count/month]")
  expect_equal(result$data$variable, c(1, 1, 1))
})

test_that("comcode_plot groups less common codes as Other", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  lookup <- env[[ls(env)[1]]]

  import_data <- data.table::data.table(
    COMCODE = c("01011010", "01011090", "01011100"),
    DATE_START = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    NET_MASS = c(10, 5, 1)
  )

  result <- comcode_plot(
    import_data,
    "01",
    lookup,
    max_unique_comcodes = 1
  )

  expect_s3_class(result, "ggplot")
  expect_setequal(result$data$COMCODE, c("01011010", "Other"))
  expect_equal(
    result$data$variable[result$data$COMCODE == "01011010"],
    10
  )
  expect_equal(
    result$data$variable[result$data$COMCODE == "Other"],
    6
  )
})
