# bulktrends

R package for analysing UK trade data. Provides wrappers around the
uktradeinfo and GOV.UK Trade Tariff APIs, plus time series utilities.

You are situated inside an R package source directory. The subdirectory `R/`
contains source files.

The package under development and does not have many users; however, notify me
of any breaking changes.


## Code conventions

- All public functions documented with roxygen2 (`#'`) and tagged `@export`
- Use `::` for all package calls (e.g. `jsonlite::fromJSON()`, `future.apply::future_lapply()`)
- Do not use `require()` or `library()` inside functions
- Function names are snake_case
- Use `future.apply` and `future` where possible 

## Commodity codes

- Always character, never numeric — leading zeros are significant
- Standard length is 10 digits; shorter codes (2, 4, 6, 8) are valid hierarchy levels

## API patterns

- New API wrappers should follow `uktrades_request()` in `R/uktradeinfo_api.R` as a reference
- Parallelism via `future.apply::future_lapply()`; users control workers with `future::plan()`

## Workflow

- Run `devtools::document()` after adding or changing roxygen2 docs to regenerate `NAMESPACE` and `man/`
- When you're running package tests, use `devtools::load_all()`. If you encounter namespacing issues, ask me what to do.
