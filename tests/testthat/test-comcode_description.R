
test_that("comcode_description handles an unknown code", {
  env <- new.env()
  data("lookup_table", envir = env)
  expect_output(
    comcode_description("99999999", env$lookup_table),
    ".*NA  —  NA.*NA  —  NA.*NA  —  NA.*NA  —  NA"
  )
})

# test_that("comcode_description handles an longer code", {  
#   env <- new.env()
#   data("lookup_table", envir = env)
#   expect_error(
#     comcode_description("010129901", env$lookup_table)
#   )
# })

test_that("comcode_description works for Hs2 codes", {
  env <- new.env()
  data("lookup_table", envir = env)
  expect_output(
    comcode_description("01012990", env$lookup_table),
    ".*01.*Live animals"
  )
})

test_that("comcode_description works for Hs4 codes", {
  env <- new.env()
  data("lookup_table", envir = env)
  expect_output(
    comcode_description("01012990", env$lookup_table),
    ".*0101.*Live horses, asses, mules and hinnies"
  )
})

test_that("comcode_description works for Hs6 codes", {
  env <- new.env()
  data("lookup_table", envir = env)
  expect_output(
    comcode_description("01012990", env$lookup_table),
    ".*010129.*Live horses.*excl. pure-bred for breeding.*"
  )
})

test_that("comcode_description works for Cn8 codes", {
  env <- new.env()
  data("lookup_table", envir = env)
  expect_output(
    comcode_description("01012990", env$lookup_table),
    ".*01012990.*Live horses.*excl. for slaughter, pure-bred for breeding.*"
  )
})