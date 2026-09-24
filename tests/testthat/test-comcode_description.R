test_that("comcode_description handles an unknown code", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_error(
    comcode_description("99999999", env$test_comcode_table)
  )
})

test_that("comcode_description handles an longer code", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_error(
    comcode_description("0101299010", env$test_comcode_table)
  )
})

test_that("comcode_description handles an shorter code", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_error(
    comcode_description("1", env$test_comcode_table)
  )
})

test_that("comcode_description handles odd-length codes", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_error(
    comcode_description("0101299", env$test_comcode_table)
  )
})

test_that("comcode_description works for Hs2 codes", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_output(
    comcode_description("01", env$test_comcode_table),
    ".*01.*Live animals.*NA  —  NA.*NA  —  NA.*NA  —  NA.*"
  )
})

test_that("comcode_description works for Hs4 codes", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_output(
    comcode_description("0101", env$test_comcode_table),
    ".*0101.*Live horses, asses, mules and hinnies.*NA  —  NA.*NA  —  NA"
  )
})

test_that("comcode_description works for Hs6 codes", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_output(
    comcode_description("010129", env$test_comcode_table),
    ".*010129.*Live horses.*excl. pure-bred for breeding.*NA  —  NA.*"
  )
})

test_that("comcode_description works for Cn8 codes", {
  env <- new.env()
  data("test_comcode_table", envir = env)
  expect_output(
    comcode_description("01012990", env$test_comcode_table),
    ".*01012990.*Live horses.*excl. for slaughter, pure-bred for breeding.*"
  )
})
