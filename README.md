# bulktrends
An `R` package for anlaysing and forecasting UK HM Revenue & Customs trade data.

## Aims

Functions and scripts designed for key datasets to monitor, forecast and hindcast the effects of BTOM on SPS (sanitary and phytosanitary) imports.  To include intervals of uncertainty, and identification of outlier events in the time series. Outputs of central interest include a comparative application of a range of time series methods on the big trade flow datasets that are routinely used in DEFRA Animal and Plant Health Agency (APHA) Enforcement and Appeals (E&A), from the traditional to novel tools.  Robust diagnostics to indicate which method(s) may be optimal in this case.

## Datasets

### Open import data

The project uses publicly available import files that will need to be downloaded, unzipped and stored in the relevant folders to work.

#### Access:

The datasets can be accessed using the links below:

Monthly trade data from [UK Trade Info](https://www.uktradeinfo.com/trade-data/)

* [Bulk data sets: archive](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/#imports-(bds-imp-yymm))
* [Guidance and technical specifications](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-guidance-and-technical-specifications/)

#### Key variables included:

*PERREF: Reference period (YYYYMM)
*COMCODE - Commodity code
*NET_MASS - Net mass (kg)
*STAT_VALUE - Statistical Value (£)

### Commodity lookup table

This table contains the description of each full commodity code and their hierarchies. It can be accessed using the uktradeinfo_api function.

## Userguide

The `notebooks\` directory contains 'UserGuide.qmd' which demonstrates how each function of the package can be used. It can be accessed using the following link: 

## User instructions:

The following instructions aim to get the package running using the appropriate dataset and functions.

1. Download and unzip the bulk import data files for the required time period.
*For example, downloading and unzipping import data for 2021 will produce 12 .txt files, one for each month.
3. Place all .txt files in `data/imports/` directory.
4. Follow the Userguide to load and save the datasets. 
*The userguide contains a function called read_uktradeinfo which (i) reads  all .txt files from the directory, (ii) combines them into a single dataset and, (iii) automatically save the combined dataset as an .rds file in the same directory for faster loading in future sessions. The .rds file allows faster data loading compared to re-reading multiple raw text files each time. the saved dataset w
5. Follow the examples in the userguide to perform any analysis required.


%### Storing data

%Store data in the `data/` directory.

## Contributing to this repository

Reference texts:

* [R Packages](https://r-pkgs.org/) for package structure
* Try to follow the [tidyverse style guide](https://style.tidyverse.org/)
* [Advanced R](https://adv-r.hadley.nz/) for complex issues

### roxygen2

We use the package [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html), which provides a framework for adjacent code and documentation so when you modify your code, it's easy to remember that you need to update the documentation. See link for more.

