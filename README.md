# bulktrends
An `R` package for anlaysing and forecasting UK HM Revenue & Customs trade data.

## Aims

Functions and scripts designed for key datasets to monitor, forecast and hindcast the effects of BTOM on SPS (sanitary and phytosanitary) imports.  To include intervals of uncertainty, and identification of outlier events in the time series. Outputs of central interest include a comparative application of a range of time series methods on the big trade flow datasets that are routinely used in DEFRA Animal and Plant Health Agency (APHA) Enforcement and Appeals (E&A), from the traditional to novel tools.  Robust diagnostics to indicate which method(s) may be optimal in this case.

## Datasets

### 1. Imports

The project uses publicly available import files that will need to be downloaded from an external source, unzipped and stored locally prior to any analysis. The data are typically provided as monthly raw .txt files, each containing detailed information on UK import activity that can be used for time series and commodity level analysis. Refer to the user instructions section below for guidelines on downloading, storing and loading the data into R.

#### Access:

The monthly UK import data used in this project are published by [UK Trade Info](https://www.uktradeinfo.com/trade-data/), the official UK government platform for trade statistics. See the following resources:

* [Bulk data sets: archive](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-archive/#imports-(bds-imp-yymm)) to access the historical monthly bulk import files. Each archive contains compressed files that, once unzipped, yield monthly .txt files representing UK import transactions for a given period.
* [Guidance and technical specifications](https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/bulk-data-sets-guidance-and-technical-specifications/) to understand the contents and format of data files.


#### Key content:

* `PERREF` -  Period Reference (YYYYMM)
* `COMCODE` - Commodity code
* `NET_MASS` - Net mass (kg)
* `STAT_VALUE` - Statistical Value (£)

### 2. Lookup Tables

Two lookup tables are used in this project. Both are retrieved in the same step (via an API function) but serve different purposes.

#### A. Commodity

This table provides descriptions and hierarchical classification of traded goods. It contains the product description of every commodity code (CN8) and its sub-codes (HS2/HS4/HS6). The table is used to aggregate data across hierarchies, label plots and outputs and improve interpretability of time series. 

#### B. Port Location

This table provides information about the freight location of products as collected on customs declarations. It contains port location codes and their respective names.

#### Access:

Both lookup tables can be accessed via an API (Application Programming Interface), which allows the data to be retrieved directly from the source website without manual downloading. In this project, this is handled by the `uktradeinfo_api()` function which returns the latest versions of both lookup tables in a structured format ready for use in R. Refer to the UserGuide for an example of how to load and use these tables.

 **Note on datasets**:
The imports dataset and lookup tables are used together throughout the project. 
The imports data provides the time series values, while the lookup tables provide metadata that supports hierarchical aggregation, classification and interpretation of the data. The datasets are linked when required using common identifiers: `CN8code` in the commodity lookup table corresponds directly to  `COMCODE` in the imports dataset and `PortCodeAlpha` in the port lookup table matches `PORT_CODE` in the imports dataset.

## Userguide

The `notebooks/` directory contains 'UserGuide.qmd' which demonstrates how each function of the package can be used. It can be accessed directly in R or using the following [link](/notebooks/UserGuide.html).

## User instructions

The following instructions aim to get the package running using the appropriate data files and functions.

1. Download and unzip the import data files for the required time period. For example, downloading and unzipping import data for 2021 will produce 12 .txt files, one for each month.
3. Place all extracted .txt files in `data/imports/` directory of the cloned repository.
4. Follow the UserGuide to load and save the datasets. The UserGuide contains a function called `read_uktradeinfo()` which (i) reads all .txt files from the directory, (ii) combines them into a single dataset and, (iii) automatically save the combined dataset as an .rds file in the same directory for faster loading in future sessions. The .rds file allows faster data loading compared to re-reading multiple raw text files each time.
5. Follow the examples in the UserGuide to perform any analysis required.

 **Note**: If the data is stored outside of the repository, i.e., in an external local directory, ensure to provide the full path to this directory when calling the `read_uktradeinfo()` in the UserGuide. Regardless of where the data is stored, the same loading and processing steps are used.

## Contributing and Development Guide

Contributions to this repository are welcome.

### General Guidelines

All work should be done on a separate new branch.

### Organisation of R code

* All reusable functions should be placed in the `R/` directory,
* Utility or helper functions should be placed in `R/utils.R` or a similar file.

### Documentation with Roxygen

All functions **must** be documented using the package **[roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html)**.
It provides a framework for adjacent code and documentation system for R. Documentation is written in special comments above each function and is automatically converted into help files. This ensures that code and documentation stay in sync and makes it easier to maintain and extend the package. See link for more.

### Updating the Userguide

The UserGuide must be updated whenever a new function is added or an existing function is modified. This can be a simple working example demonstrating how the function should be used.

### Useful References for Contributors

The following resources are recommended for anyone contributing to this repository:

* [R Packages](https://r-pkgs.org/) for guidance on package structure.
* [Tidyverse style guide](https://style.tidyverse.org/) coding style for readable and consistent R code.
* [Advanced R](https://adv-r.hadley.nz/) for advanced and complex R programming concepts.


### Current Contributors

The following people have contributed to the development of this repository:

* Jethro Browell (`@jbrowell`)
* Janeeta Maunthrooa (`@janeetam`)





