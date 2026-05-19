# Download monthly import data from UK Trade Info
#
# dest_dir is suggested for developers to enable smooth running of other
# scripts, including the User Guide.

download_uktradeinfo_bulk(
  dest_dir = "data/Rbuildignore/imports_hmrc/",
  type = "imports",
  overwrite = FALSE
)
