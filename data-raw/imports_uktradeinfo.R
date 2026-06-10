# Download and prep monthly import data from UK Trade Info
#
# data_dir is suggested for developers to enable smooth running of other
# scripts, including the User Guide.
library(bulktrends)

future::plan("multicore")
data_dir <- "data/Rbuildignore"
dir.create(file.path(data_dir, "imports_hmrc/"), showWarnings = FALSE)

# Download latest data
download_uktradeinfo_bulk(
  dest_dir = file.path(data_dir, "imports_hmrc/"),
  type = "imports",
  overwrite = FALSE
)

# Update existing dataset or create if it doesn't exist.
if (file.exists(file.path(data_dir, "imports.gz"))) {
  imports <- fread(file.path(data_dir, "imports.gz"))
  update_uktradeinfo(
    existing = imports,
    path = file.path(data_dir, "imports_hmrc/")
  )
} else {
  imports <- read_uktradeinfo(path = file.path(data_dir, "imports_hmrc/"))
}

# Check dups and save
imports <- imports[!duplicated(imports)]
fwrite(imports, file.path(data_dir, "imports.gz"))
