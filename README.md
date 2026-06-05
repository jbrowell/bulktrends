# bulktrends

An `R` package for analysing and forecasting UK trade data. For example usage, see [the user guide](https://jbrowell.github.io/bulktrends/).

:warning: This package is under development! Expect features and performance to change, including breaking changes, until the first stable release.

## Aims

The functions and scripts of this package are designed to monitor, forecast and hindcast UK import flows using large-scale trade datasets to support evidence-based monitoring and analysis of UK trade patterns. The central interest is to monitor and evaluate for impacts on Sanitary and Phytosanitary (SPS) import dynamics over time. The key objectives include producing robust forecasts with measures of uncertainty, identifying and analysing outlier events and anomalies in the time series, comparing traditional and more novel time series techniques when applied to large trade datasets used for monitoring and providing robust diagnostic tools to identify the optimal methods to be used in this case. 

## Datasets

### 1. Monthly HMRC Imports


The project uses publicly available HM Revenue & Customs (HMRC) import files that will need to be downloaded. Data are published by [UK Trade Info](https://www.uktradeinfo.com/trade-data/), the UK government platform for trade statistics, and consist of monthly import collections of zipped `.txt` files, each containing detailed information on UK imports by commodity, country of origin, for example. See the following resources:

* [Bulk data sets: archive](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/#imports-(bds-imp-yymm)) to access the historical monthly bulk import files. Each zip archive contains monthly `.txt` files representing UK import transactions for a given period.
* [Guidance and technical specifications](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-guidance-and-technical-specifications/) for further information on the contents and format of data files.

#### Storage and loading

Files from [Bulk data sets: archive](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/#imports-(bds-imp-yymm)) should be stored in a dedicated directory. The function `download_uktradeinfo_bulk()` will do this for you. The function `read_uktradeinfo(path)` will load a single file or all files in a given directory and its subdirectories, or zip archive.

This can take some time if loading several years worth of data. We recommend saving and loading the resulting data object using `data.table::fwrite/fread` as a compressed `.gz` file to save time.

### 2. Daily IPAFFS Imports

The package also supports the use of open sourced daily import data of Products, Animals, Food and Feed System (IPAFFS) published by Food Standards Agency (FSA). The data contains individual `.csv` files of animal and non-animal imports to the UK reported separately. The data files are available in the [Imports Intelligence Hub](https://www.food.gov.uk/our-work/imports-intelligence-hub) on the FSA website. See the following resources to access the historical import files:

* [Trade Control – HRFNAO](https://data.food.gov.uk/catalog/datasets/71f9bee8-b68c-4ffc-813e-901d1ac20245) for non-animal origin imports.
* [Trade Control – POAO](https://data.food.gov.uk/catalog/datasets/1a6ebd38-460e-4734-aa59-40fdd6b8e209) for animal origin imports.

#### Storage and loading

Files should be stored in dedicated directories for each dataset type. Ensure those directories exist (e.g. with `dir.create(..., recursive = TRUE)`), then use `download_fsa_ipaffs()` to download all available monthly CSVs and skip files that are already present:

```r
download_fsa_ipaffs(dest_dir = "<my dir>/fnao/", dataset = "fnao")
download_fsa_ipaffs(dest_dir = "<my dir>/poao/", dataset = "poao")
```

Pass `dataset = c("fnao", "poao")` (the default) to download both in a single call to a shared directory.

The function `read_ipaffs(path)` will load a single CSV file or all `.csv` files in a given directory and its subdirectories into a `data.table`.

### 3. Lookup Tables

In addition to trade data, a series of lookup tables are required to interpret some data fields.

#### A. Commodity

This table provides descriptions and hierarchical classification of traded goods. It contains the product description of every commodity code (CN8) and its sub-codes (HS2/HS4/HS6). The table is used in `bulktrends` to aggregate data across hierarchies, label plots and outputs and support interpretability of results. 

#### B. Port Location

This table provides information about the freight location of products as collected on customs declarations. It contains port location codes and their respective names.

##### Access:

Both lookup tables can be accessed via an API function, which allows the data to be retrieved directly from the source website without manual downloading. In this project, this is handled by the `uktrades_request()` function which returns the latest versions of both lookup tables in a structured format ready for use in `R`. Refer to the UserGuide for an example of how to load these tables.

##### Notes:

1. Both monthly and daily import datasets and lookup tables are used together throughout the project. The import data provides the time series values, while the lookup tables provide metadata that supports hierarchical aggregation, classification and interpretation of the data. The datasets are linked when required using common identifiers: `CN8code` in the commodity lookup table corresponds directly to `COMCODE` in the imports dataset and `PortCodeAlpha` in the port lookup table matches `PORT_CODE` in the imports dataset.

2. There is a change in data collection procedure for HMRC UK imports from EU from January 2022 following the UK’s exit from the EU (see [report](https://www.gov.uk/government/statistics/overseas-trade-statistics-methodologies/overseas-trade-in-goods-statistics-methodology-and-quality-report--3#data-sources) for more information). This is reflected as a break in the time series for HMRC `volume`, reducing comparability for this variable before and after 2022.

## Installation and User Guide

This package can be installed by running
```r
devtools::install_github("jbrowell/bulktrends")
```
and is accompanied by a user guide that demonstrates how each function of the package can be used. The user guide is available online at [jbrowell.github.io/bulktrends/](https://jbrowell.github.io/bulktrends/), or to open the user guide from your local installation (e.g. if you are not connected to the internet), run the command
```r
bulktrends::open_userguide()
```

## Instruction for contributors

### Set-up

The following instructions aim to clone and run the package using the appropriate data files and functions.

1. Clone this git repository using your preferred method
2. Create directory `data/Rbuildignore/` in the cloned repository. The contents of the `data/Rbuildignore/` directory are not tracked by git or included when building/installing the package.
3. Download import data. The script `data-raw/imports_uktradeinfo.R` downloads and updates monthly HMRC data; `data-raw/imports_fsa_ipaffs.R` downloads and updates FSA IPAFFS data for both FNAO and POAO. Both scripts can be run with `source()` and will skip files already present on disk.
4. Run through `UserGuide.qmd` and review usage of the main functions included in `bulktrends`.
5. Develop. Ensure contributions are documented, that the package version is incremented in `DESCRIPTION`, and new features are demonstrated in the user guide (see further instructions below).

### Branching Strategy

This package is managed using the [Gitflow workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow). In summary:

* `main` contains the latest stable release.
* `develop` is the integration branch for ongoing development.
* New features should be developed on a `feature/<name>` branch cut from `develop` and merged back via pull request.
* Releases are prepared on a `release/<version>` branch before being merged into both `main` and `develop`.
* Urgent fixes to `main` are handled via `hotfix/<name>` branches.

Create a pull request when you're ready to share with other contributors. Request a review from CoPilot in the first instance for immediate feedback. When ready, tag another contributor for a review.

Currently, no branch protection is in place, so take care, especially when merging into `main`!

### General Guidelines

All development work should be done on a dedicated branch for each new feature. When ready, submit a pull request and request a review from another developer.

Use the `air` auto-formatter to apply the tidyverse style guide. See [here](https://posit-dev.github.io/air/) for set-up instructions.
```bash
...\bulktrends>air format .
```

### Documentation with Roxygen

All functions **must** be documented using the package [`roxygen2`](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html).
It provides a framework for adjacent code and documentation system for `R`. Documentation is written in special comments above each function and is automatically converted into help files. This ensures that code and documentation stay in sync and makes it easier to maintain and extend the package. See link for further information.

### Updating the Userguide

The user guide (`docs/UserGuide.qmd`) should be updated whenever a new function is added or an existing function is modified. This can be a simple working example demonstrating how the function should be used. Remember to render a new version of the html!

### Useful References for Contributors

The following resources are recommended for anyone contributing to this repository:

* [R Packages](https://r-pkgs.org/) for guidance on package structure.
* [Tidyverse style guide](https://style.tidyverse.org/) coding style for readable and consistent `R` code.
* [Advanced R](https://adv-r.hadley.nz/) for advanced and complex `R` programming concepts.

### Current Contributors

The following people have contributed to the development of this repository:

* Jethro Browell (`@jbrowell`), University of Glasgow, ([email](mailto:jethro.browell@glasgow.ac.uk))
* Janeeta Maunthrooa (`@janeetam`), University of Glasgow
* Damien Hicks (`@Gitmojoworking`), Defra

This work has been supported by:

* The UK Engineering and Physical Sciences Research Council ([EP/Z534985/1](https://gtr.ukri.org/projects?ref=EP%2FZ534985%2F1))
* Defra APHW Evidence & Analysis
