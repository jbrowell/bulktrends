# Download and update FSA IPAFFS (Trade Control & Expert System) data
#
# Saves monthly CSV files to data/Rbuildignore/imports_fsa_ipaffs/ with one
# subdirectory per dataset type (fnao, poao). These directories are excluded
# from the package build via .Rbuildignore.
#
# Run with: source("data-raw/imports_fsa_ipaffs.R")
library(bulktrends)

data_dir <- "data/Rbuildignore"
dir.create(file.path(data_dir, "imports_fsa_ipaffs/fnao/"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(data_dir, "imports_fsa_ipaffs/poao/"), recursive = TRUE, showWarnings = FALSE)

download_fsa_ipaffs(
  dest_dir = file.path(data_dir, "imports_fsa_ipaffs/fnao/"),
  dataset = "fnao",
  overwrite = FALSE
)

download_fsa_ipaffs(
  dest_dir = file.path(data_dir, "imports_fsa_ipaffs/poao/"),
  dataset = "poao",
  overwrite = FALSE
)

fnao <- read_ipaffs(file.path(data_dir, "imports_fsa_ipaffs/fnao/"))
head(fnao)
