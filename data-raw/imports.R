# Download and prep monthly import data from UK Trade Info
#
# data_dir is suggested for developers to enable smooth running of other
# scripts, including the User Guide.
library(bulktrends)

future::plan("multicore")
data_dir <- "data/Rbuildignore/"

# Download latest data
download_uktradeinfo_bulk(
  dest_dir = paste0(data_dir, "imports_hmrc/"),
  type = "imports",
  overwrite = FALSE
)

# Update existing dataset or create if it doesn't exist.
if (file.exists(paste0(data_dir, "imports.rds"))) {
  imports <- readRDS(paste0(data_dir, "imports.rds"))
  update_uktradeinfo(
    existing = imports,
    path = paste0(data_dir, "imports_hmrc/")
  )
} else {
  imports <- read_uktradeinfo(path = paste0(data_dir, "imports_hmrc/"))
}

# Check dups and save
imports <- imports[!duplicated(imports)]
saveRDS(imports, file = paste0(data_dir, "imports.rds"))
