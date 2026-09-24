test_that("read_ipaffs standardises country and port column variants", {
  cases <- list(
    c(country = "CountryofOrigin", port = "portlocation"),
    c(country = "CountryOfOrigin", port = "PortLocation"),
    c(country = "countryoforigin", port = "Portlocation")
  )

  for (case in cases) {
    input <- tempfile(fileext = ".csv")
    writeLines(
      c(
        paste(
          "DeclarationDate",
          "CommodityCode",
          "TotalOfNetWeightKG",
          case[["country"]],
          case[["port"]],
          sep = ","
        ),
        "2026-01-02,0012345,12.5,FR,DOV"
      ),
      input
    )

    out <- read_ipaffs(input)

    expect_true("COO_ALPHA" %in% names(out))
    expect_true("PORT_CODE" %in% names(out))
    expect_false(case[["country"]] %in% names(out))
    expect_false(case[["port"]] %in% names(out))
    expect_identical(out$COO_ALPHA, "FR")
    expect_identical(out$PORT_CODE, "DOV")
  }
})
