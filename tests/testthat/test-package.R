test_that("DESCRIPTION has LazyData: false", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "bulktrends"))
  expect_equal(unname(desc[, "LazyData"]), "false")
})

test_that("tariff_commodities dataset file exists in installed package", {
  expect_true(nzchar(system.file(
    "data",
    "tariff_commodities.rda",
    package = "bulktrends"
  )))
})
