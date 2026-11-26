# devtools::load_all()
# testthat::test_file("tests/testthat/test-download_uktradeinfo_bulk.R")

################################## NOTES #################################
#
# extract_file_coverage do not recognise files related to the years
# between 2001 and 2012.
#
# ########################################################################

# parse_yearmon --------------------------------------------------------
test_that("parse_yearmon handles NULL", {
    expect_null(
        parse_yearmon(NULL, "'from_date'")
    )
})

test_that("parse_yearmon rejects invalid values", {
    expect_error(
        parse_yearmon("2024", "'from_date'")
    )

    expect_error(
        parse_yearmon("2024-00", "'from_date'")
    )

    expect_error(
        parse_yearmon("2024-1", "'from_date'")
    )

    expect_error(
        parse_yearmon("not-a-date", "'from_date'")
    )

    expect_error(
        parse_yearmon(c("2024-01", "2024-02"), "'from_date'")
    )

    expect_error(
        parse_yearmon(123, "'from_date'")
    )
})

test_that("parse_yearmon parses YYYY-MM strings", {
    expect_equal(
        parse_yearmon("2024-01", "'from_date'"),
        as.Date("2024-01-01")
    )

    expect_equal(
        parse_yearmon("2024-12", "'to_date'"),
        as.Date("2024-12-01")
    )
})

test_that("parse_yearmon parses Date values", {
    expect_equal(
        parse_yearmon(as.Date("2024-06-15"), "'from_date'"),
        as.Date("2024-06-01")
    )

    expect_equal(
        parse_yearmon(as.Date("2024-06-30"), "'to_date'"),
        as.Date("2024-06-01")
    )
})

# extract_file_coverage -------------------------------------------------
test_that("extract_file_coverage provides NA for invalid date format", {
    result <- extract_file_coverage("BDSImp241.zip")

    expect_true(is.na(result$from))
    expect_true(is.na(result$to))
})

test_that("extract_file_coverage provides NA files without date", {
    result <- extract_file_coverage("BDSImp.zip")

    expect_true(is.na(result$from))
    expect_true(is.na(result$to))
})

test_that("extract_file_coverage parses monthly YYMM files", {
    result <- extract_file_coverage("BDSImp2401.zip")

    expect_equal(result$from, as.Date("2024-01-01"))
    expect_equal(result$to, as.Date("2024-01-31"))
})

test_that("extract_file_coverage parses monthly YYMM archive files", {
    result <- extract_file_coverage("BDSImp2401archive.zip")

    expect_equal(result$from, as.Date("2024-01-01"))
    expect_equal(result$to, as.Date("2024-01-31"))
})

test_that("extract_file_coverage parses annual YYYY files", {
    result <- extract_file_coverage("BDSImp2024.zip")

    expect_equal(result$from, as.Date("2024-01-01"))
    expect_equal(result$to, as.Date("2024-12-31"))
})

test_that("extract_file_coverage parses annual YY files", {
    result <- extract_file_coverage("BDSImp24.zip")

    expect_equal(result$from, as.Date("2024-01-01"))
    expect_equal(result$to, as.Date("2024-12-31"))
})

test_that("extract_file_coverage parses semi-annual MMM-MMMYY files", {
    result <- extract_file_coverage("BDSImp_jan-jun24.zip")

    expect_equal(result$from, as.Date("2024-01-01"))
    expect_equal(result$to, as.Date("2024-06-30"))
})

test_that("extract_file_coverage is case insensitive for month names", {
    result <- extract_file_coverage("BDSImp_JaN-JuN24.zip")

    expect_equal(result$from, as.Date("2024-01-01"))
    expect_equal(result$to, as.Date("2024-06-30"))
})

# extract_coverage_field ------------------------------------------------
test_that("extract_coverage_field extracts - from", {
    result <- extract_coverage_field(
        c(
            "BDSImp2401.zip",
            "BDSImp2401archive.zip",
            "BDSImpImp_jan-jun24.zip"
        ),
        "from"
    )

    expect_equal(
        as.Date(result),
        as.Date(c("2024-01-01", "2024-01-01", "2024-01-01"))
    )
})

test_that("extract_coverage_field extracts - to", {
    result <- extract_coverage_field(
        c(
            "BDSImp2401.zip",
            "BDSImp2401archive.zip",
            "BDSImpImp_jan-jun24.zip"
        ),
        "to"
    )

    expect_equal(
        as.Date(result),
        as.Date(c("2024-01-31", "2024-01-31", "2024-06-30"))
    )
})

# filter_links_by_date --------------------------------------------------
test_that("filter_links_by_date raise error for empty links", {
    expect_error(
        filter_links_by_date(
            c(),
            from_date = NULL,
            to_date = NULL
        )
    )

    expect_error(
        filter_links_by_date(
            NULL,
            from_date = NULL,
            to_date = NULL
        )
    )
})

test_that("filter_links_by_date raise error for character", {
    expect_error(
        filter_links_by_date(
            34,
            from_date = NULL,
            to_date = NULL
        )
    )
})

test_that("filter_links_by_date filters using lower bound", {
    links <- c(
        "https://example.com/BDSImp2312.zip",
        "https://example.com/BDSImp2401.zip",
        "https://example.com/BDSImp2406.zip"
    )

    result <- filter_links_by_date(
        links,
        from_date = as.Date("2024-01-01"),
        to_date = NULL
    )

    expect_equal(
        result,
        links[2:3]
    )
})

test_that("filter_links_by_date filters using upper bound", {
    links <- c(
        "https://example.com/BDSImp2312.zip",
        "https://example.com/BDSImp2401.zip",
        "https://example.com/BDSImp2406.zip"
    )

    result <- filter_links_by_date(
        links,
        from_date = NULL,
        to_date = as.Date("2024-01-31")
    )

    expect_equal(
        result,
        links[1:2]
    )
})

test_that("filter_links_by_date uses inclusive overlap", {
    links <- c(
        "https://example.com/BDSImp_jan-jun24.zip",
        "https://example.com/BDSImp_jun-dec24.zip"
    )

    result <- filter_links_by_date(
        links,
        from_date = as.Date("2024-06-01"),
        to_date = as.Date("2024-06-30")
    )

    expect_equal(result, links)
})

test_that("filter_links_by_date keeps unparseable filenames", {
    links <- c(
        "https://example.com/BDSImp2401.zip",
        "https://example.com/file_unparseable.zip"
    )

    result <- filter_links_by_date(
        links,
        from_date = as.Date("2024-02-01"),
        to_date = NULL
    )

    expect_equal(
        result,
        links[2]
    )
})

testthat::test_that("filter_links_by_date works without bounds", {
    links <- c(
        "https://example.com/BDSImp2401.zip",
        "https://example.com/BDSImp2402.zip"
    )

    result <- filter_links_by_date(
        links,
        from_date = NULL,
        to_date = NULL
    )

    testthat::expect_identical(result, links)
})

# extract_zip_coverage ----------------------------------------------------
test_that("extract_zip_coverage returns NA for invalid ZIP", {
    zip_path <- tempfile(fileext = ".zip")
    writeLines("not a zip", zip_path)

    result <- extract_zip_coverage(zip_path)

    expect_true(is.na(result$from))
    expect_true(is.na(result$to))

    unlink(zip_path)
})

test_that("extract_zip_coverage determines coverage from ZIP contents", {
    zip_path <- tempfile(fileext = ".zip")

    files <- c(
        tempfile(pattern = "BDSImp2401_", fileext = ".csv"),
        tempfile(pattern = "BDSImp2402_", fileext = ".csv"),
        tempfile(pattern = "BDSImp2403_", fileext = ".csv")
    )

    lapply(files, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = zip_path,
        files = files,
        flags = "-q"
    )

    result <- extract_zip_coverage(zip_path)

    expect_equal(as.Date(result$from), as.Date("2024-01-01"))
    expect_equal(as.Date(result$to), as.Date("2024-03-01"))

    unlink(c(files, zip_path))
})

test_that("extract_zip_coverage ignores unparseable internal files", {
    zip_path <- tempfile(fileext = ".zip")

    files <- c(
        file.path(tempdir(), "BDSImp.txt"),
        file.path(tempdir(), "BDSImp2401.csv"),
        file.path(tempdir(), "BDSImpnoformat.csv")
    )

    lapply(files, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = zip_path,
        files = files,
        flags = "-q"
    )

    result <- extract_zip_coverage(zip_path)

    expect_equal(as.Date(result$from), as.Date("2024-01-01"))
    expect_equal(as.Date(result$to), as.Date("2024-01-01"))

    unlink(c(files, zip_path))
})

test_that("extract_zip_coverage returns NA when no internal filenames contain dates", {
    zip_path <- tempfile(fileext = ".zip")

    files <- c(
        file.path(tempdir(), "BDSImp.txt"),
        file.path(tempdir(), "BDSImpnoformat.csv")
    )

    lapply(files, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = zip_path,
        files = files,
        flags = "-q"
    )

    result <- extract_zip_coverage(zip_path)

    expect_true(is.na(result$from))
    expect_true(is.na(result$to))

    unlink(c(files, zip_path))
})

# find_duplicate_coverage ---------------------------------------------------
test_that("find_duplicate_coverage detects monthly file inside archive", {
    monthly_zip <- tempfile(fileext = ".zip")
    archive_zip <- tempfile(fileext = ".zip")

    monthly_file <- file.path(tempdir(), "BDSImp2402.csv")
    archive_files <- c(
        file.path(tempdir(), "BDSImp2401.csv"),
        file.path(tempdir(), "BDSImp2402.csv"),
        file.path(tempdir(), "BDSImp2403.csv")
    )

    writeLines("dummy", monthly_file)
    lapply(archive_files, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = monthly_zip,
        files = monthly_file,
        flags = "-q"
    )

    utils::zip(
        zipfile = archive_zip,
        files = archive_files,
        flags = "-q"
    )

    result <- find_duplicate_coverage(
        c(monthly_zip, archive_zip)
    )

    expect_equal(nrow(result), 1L)
    expect_equal(
        result$monthly_file,
        basename(monthly_zip)
    )
    expect_equal(
        result$archive_file,
        basename(archive_zip)
    )

    unlink(c(
        monthly_zip,
        archive_zip,
        monthly_file,
        archive_files
    ))
})

test_that("find_duplicate_coverage does not flag unrelated months", {
    monthly_zip <- tempfile(fileext = ".zip")
    archive_zip <- tempfile(fileext = ".zip")

    monthly_file <- "BDSImp2407.csv"
    archive_files <- c(
        "BDSImp2401.csv",
        "BDSImp2402.csv",
        "BDSImp2403.csv"
    )

    writeLines("dummy", monthly_file)
    lapply(archive_files, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = monthly_zip,
        files = monthly_file,
        flags = "-q"
    )

    utils::zip(
        zipfile = archive_zip,
        files = archive_files,
        flags = "-q"
    )

    result <- find_duplicate_coverage(
        c(monthly_zip, archive_zip)
    )

    testthat::expect_equal(nrow(result), 0L)

    unlink(c(
        monthly_zip,
        archive_zip,
        monthly_file,
        archive_files
    ))
})

test_that("find_duplicate_coverage handles partial archives correctly", {
    monthly_zip <- tempfile(fileext = ".zip")
    archive_zip <- tempfile(fileext = ".zip")

    monthly_file <- file.path(tempdir(), "BDSImp2405.csv")

    archive_files <- c(
        file.path(tempdir(), "BDSImp2401.csv"),
        file.path(tempdir(), "BDSImp2402.csv"),
        file.path(tempdir(), "BDSImp2403.csv"),
        file.path(tempdir(), "BDSImp2404.csv")
        # file.path(tempdir(), "BDSImp_Jan-Apr24.csv")
    )

    writeLines("dummy", monthly_file)
    lapply(archive_files, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = monthly_zip,
        files = monthly_file,
        flags = "-q"
    )

    utils::zip(
        zipfile = archive_zip,
        files = archive_files,
        flags = "-q"
    )

    result <- find_duplicate_coverage(
        c(monthly_zip, archive_zip)
    )

    testthat::expect_equal(nrow(result), 0L)

    unlink(c(
        monthly_zip,
        archive_zip,
        monthly_file,
        archive_files
    ))
})

# check_bulk_coverage ---------------------------------------------------------
test_that("check_bulk_coverage rejects missing directory", {
    expect_error(
        check_bulk_coverage(file.path(tempdir(), "does-not-exist")),
        "does not exist"
    )
})

test_that("check_bulk_coverage returns empty result for empty directory", {
    dest <- tempfile()
    dir.create(dest)

    result <- check_bulk_coverage(dest)

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0L)
    expect_named(result, c("monthly_file", "archive_file"))
})

test_that("check_bulk_coverage identifies duplicate files", {
    dest <- tempfile()
    dir.create(dest)

    monthly_path <- file.path(dest, "BDSImp2402.zip")
    archive_path <- file.path(dest, "BDSImp_jan-jun24.zip")

    monthly_internal <- file.path(tempdir(), "BDSImp2402.csv")
    archive_internal <- c(
        file.path(tempdir(), "BDSImp2401.csv"),
        file.path(tempdir(), "BDSImp2402.csv"),
        file.path(tempdir(), "BDSImp2403.csv")
    )

    writeLines("dummy", monthly_internal)
    lapply(archive_internal, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = monthly_path,
        files = monthly_internal,
        flags = "-q"
    )

    utils::zip(
        zipfile = archive_path,
        files = archive_internal,
        flags = "-q"
    )

    result <- check_bulk_coverage(dest)

    expect_equal(nrow(result), 1L)
    expect_equal(
        result$monthly_file,
        "BDSImp2402.zip"
    )
    expect_equal(
        result$archive_file,
        "BDSImp_jan-jun24.zip"
    )

    unlink(c(monthly_internal, archive_internal))
    unlink(dest, recursive = TRUE)
})

test_that("check_bulk_coverage delete removes redundant monthly files", {
    dest <- tempfile()
    dir.create(dest)

    monthly_path <- file.path(dest, "BDSImp2402.zip")
    archive_path <- file.path(dest, "BDSImp_jan-jun24.zip")

    monthly_internal <- "BDSImp2402.csv"
    archive_internal <- c(
        file.path(tempdir(), "BDSImp2401.csv"),
        file.path(tempdir(), "BDSImp2402.csv"),
        file.path(tempdir(), "BDSImp2403.csv")
    )

    writeLines("dummy", monthly_internal)
    lapply(archive_internal, \(f) writeLines("dummy", f))

    utils::zip(
        zipfile = monthly_path,
        files = monthly_internal,
        flags = "-q"
    )

    utils::zip(
        zipfile = archive_path,
        files = archive_internal,
        flags = "-q"
    )

    result <- check_bulk_coverage(dest, delete = TRUE)

    testthat::expect_equal(nrow(result), 1L)
    testthat::expect_false(file.exists(monthly_path))
    testthat::expect_true(file.exists(archive_path))

    unlink(c(monthly_internal, archive_internal))
    unlink(dest, recursive = TRUE)
})

# .bulk_type_patterns -----------------------------------------------------------
test_that("bulk type patterns match the intended file types", {
    filenames <- c(
        imports = "BDSImp2401.zip",
        importer_details = "BDSImpDet2401.zip",
        exports = "BDSExp2401.zip",
        exporter_details = "BDSExpDet2401.zip",
        control = "SMKA122024.zip",
        preference = "BDSPref2401.zip"
    )

    for (type in names(filenames)) {
        pattern <- .bulk_type_patterns[[type]]

        expect_true(
            grepl(pattern, filenames[[type]], perl = TRUE),
            info = paste("Failed for type:", type)
        )
    }
})

test_that("imports pattern does not match importer details", {
    expect_false(
        grepl(
            .bulk_type_patterns$imports,
            "BDSImpDet2401.zip",
            perl = TRUE
        )
    )
})

test_that("exports pattern does not match exporter details", {
    expect_false(
        grepl(
            .bulk_type_patterns$exports,
            "BDSExpDet2401.zip",
            perl = TRUE
        )
    )
})

# download_uktradeinfo_bulk -------------------------------------------------------
test_that("download_uktradeinfo_bulk rejects missing destination", {
    expect_error(
        download_uktradeinfo_bulk(
            dest_dir = file.path(tempdir(), "does-not-exist")
        ),
        "does not exist"
    )
})

test_that("download_uktradeinfo_bulk rejects invalid type", {
    dest <- tempfile()
    dir.create(dest)

    expect_error(
        download_uktradeinfo_bulk(
            dest_dir = dest,
            type = "invalid"
        )
    )

    unlink(dest, recursive = TRUE)
})

# test_that("download_uktradeinfo_bulk accepts multiple types", {
#     skip_if_not_installed("rvest")

#     dest <- tempfile()
#     dir.create(dest)

#     expect_warning(
#         download_uktradeinfo_bulk(
#             dest_dir = dest,
#             type = c("imports", "not-a-type")
#         )
#     )

#     unlink(dest, recursive = TRUE)
# })

test_that("download_uktradeinfo_bulk rejects reversed date range", {
    skip_if_not_installed("rvest")

    dest <- tempfile()
    dir.create(dest)

    expect_error(
        download_uktradeinfo_bulk(
            dest_dir = dest,
            from_date = "2024-12",
            to_date = "2024-01"
        ),
        "'from_date' must not be later than 'to_date'."
    )

    unlink(dest, recursive = TRUE)
})
