# bulktrends
An `R` package for anlaysing and forecasting UK HM Revenue & Customs trade data.

## Aims

Functions and scripts designed for key datasets to monitor, forecast and hindcast the effects of BTOM on SPS (sanitary and phytosanitary) imports.  To include intervals of uncertainty, and identification of outlier events in the time series. Outputs of central interest include a comparative application of a range of time series methods on the big trade flow datasets that are routinely used in DEFRA Animal and Plant Health Agency (APHA) Enforcement and Appeals (E&A), from the traditional to novel tools.  Robust diagnostics to indicate which method(s) may be optimal in this case.

## Datasets

### 1. Imports

The project uses publicly available UK import files that will need to be downloaded, unzipped and stored in the relevant folders to work.

#### Access:

The files can be accessed using the links below:

Monthly trade data from [UK Trade Info](https://www.uktradeinfo.com/trade-data/)

* [Bulk data sets: archive](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/#imports-(bds-imp-yymm))
* [Guidance and technical specifications](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-guidance-and-technical-specifications/)

#### Key variables included:

* PERREF: Reference period (YYYYMM)
* COMCODE - Commodity code
* NET_MASS - Net mass (kg)
* STAT_VALUE - Statistical Value (£)

### 2. Lookup Table

The lookup table provides metadata to interpret each full commodity code and their sub groups. this includes mappings between different hierarchy levels and their respective description. The table is used to aggregate data across hierarchies, label plots and outputs and improve interpretability of time series. It can be accessed using the uktradeinfo_api function. Refer to the Userguide for usage example.

#### Typical fields

* Commodity code and their sub groups (CN8/HS2/HS4/HS6)
* Code description for each sub code.

 **Note**:
The imports dataset and lookup table are used together throughout the project. 
The imports data provides the time series values, while the lookup table enables aggregation, filtering, and reference across commodity codes and related hierarchies.


## Userguide

The `notebooks\` directory contains 'UserGuide.qmd' which demonstrates how each function of the package can be used. It can be accessed using the following link: 

## User instructions

The following instructions aim to get the package running using the appropriate dataset and functions.

1. Download and unzip the bulk import data files for the required time period.
*For example, downloading and unzipping import data for 2021 will produce 12 .txt files, one for each month.
3. Place all .txt files in `data/imports/` directory.
4. Follow the Userguide to load and save the datasets. 
*The userguide contains a function called read_uktradeinfo which (i) reads  all .txt files from the directory, (ii) combines them into a single dataset and, (iii) automatically save the combined dataset as an .rds file in the same directory for faster loading in future sessions. The .rds file allows faster data loading compared to re-reading multiple raw text files each time.
5. Follow the examples in the userguide to perform any analysis required.


## Contributing and Development Guide

Contributions to this repository are welcome.

### General Guidelines

All work should be done on a separate new branch.

### Organisation of R code

* All reusable functions should be placed in the `R/` directory,
* Utility or helper functions should be placed in `R/utils.R` or a similar file.

### Documentation with Roxygen

All functions **must** be documented using the package **[roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html)**.
It provides a framework for adjacent code and documentation so when you modify your code, it's easy to remember that you need to update the documentation. See link for more.

### Updating the Userguide

The userguide must be updated whenever a new function is added or an existing function is modified. This can be a simple working example demonstrating how the function should be used.


### Useful References for Contributors

The following resources are recommended for anyone contributing to this repository:

* [R Packages](https://r-pkgs.org/) for guidance on package structure.
* [Tidyverse style guide](https://style.tidyverse.org/) coding style for readable and consistent R code.
* [Advanced R](https://adv-r.hadley.nz/) for advanced and complex R programming concepts.


### Current Contributors

The following people have contributed to the development of this repository:

* Jethro Browell (`@jbrowell`)
* Janeeta Maunthrooa (`@janeetam`)





