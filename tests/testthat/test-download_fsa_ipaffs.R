# devtools::load_all()
# testthat::test_file("tests/testthat/test-download_fsa_ipaffs.R")

test_that("parse_fsa_csv_date parses full and abbreviated month names", {
  expect_equal(
    parse_fsa_csv_date("FNAO January 2024.csv"),
    as.Date("2024-01-01")
  )
  expect_equal(
    parse_fsa_csv_date("POAO Sep-24.csv"),
    as.Date("2024-09-01")
  )
})

test_that("parse_fsa_csv_date handles two-digit years and invalid names", {
  expect_equal(
    parse_fsa_csv_date("FNAO December 24.csv"),
    as.Date("2024-12-01")
  )
  expect_true(is.na(parse_fsa_csv_date("FNAO latest.csv")))
  expect_true(is.na(parse_fsa_csv_date("FNAO January 1999.csv")))
})

test_that("collect_fsa_csv_hrefs returns absolute CSV links only", {
  html_file <- tempfile(fileext = ".html")
  writeLines(
    c(
      "<html><body>",
      '<a href="https://example.com/january-2024.csv">CSV</a>',
      '<a href="https://example.com/notes.pdf">PDF</a>',
      '<a href="/relative.csv">Relative</a>',
      '<a href="https://example.com/february-2024.CSV">CSV</a>',
      "</body></html>"
    ),
    html_file
  )

  expect_equal(
    collect_fsa_csv_hrefs(html_file),
    c(
      "https://example.com/january-2024.csv",
      "https://example.com/february-2024.CSV"
    )
  )
})

test_that("filter_fsa_links_by_date applies inclusive bounds", {
  links <- c(
    "https://example.com/january-2024.csv",
    "https://example.com/february-2024.csv",
    "https://example.com/march-2024.csv"
  )

  expect_equal(
    filter_fsa_links_by_date(
      links,
      from_date = as.Date("2024-02-01"),
      to_date = as.Date("2024-02-01")
    ),
    links[2]
  )
})

test_that("filter_fsa_links_by_date retains unparseable filenames", {
  links <- c(
    "https://example.com/january-2024.csv",
    "https://example.com/current.csv"
  )

  expect_equal(
    filter_fsa_links_by_date(
      links,
      from_date = as.Date("2024-02-01"),
      to_date = NULL
    ),
    links[2]
  )
})

test_that("download_fsa_ipaffs validates arguments", {
  expect_error(
    download_fsa_ipaffs(tempdir(), dataset = "invalid"),
    "'arg' should be one of"
  )
  expect_error(
    download_fsa_ipaffs(tempdir(), from_date = "2024-03", to_date = "2024-02"),
    "'from_date' must not be later than 'to_date'"
  )
  expect_error(
    download_fsa_ipaffs(file.path(tempdir(), "missing-fsa-directory")),
    "'dest_dir' does not exist"
  )
})

test_that("download_fsa_ipaffs returns no files for an empty catalogue", {
  with_mocked_bindings(
    collect_fsa_csv_hrefs = function(url) character(0),
    {
      result <- download_fsa_ipaffs(tempdir(), dataset = "fnao")
    }
  )

  expect_identical(result, character(0))
})
