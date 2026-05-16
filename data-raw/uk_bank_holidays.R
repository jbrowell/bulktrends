## Script to generate the uk_bank_holidays dataset.
##
## Sources (highest to lowest priority; all are attempted every run):
##   1. https://www.gov.uk/bank-holidays.json  — official API, all divisions
##   2. alphagov/calendars GitHub repository   — extends coverage into date
##        ranges not present in the gov.uk API
##
## Where the same (date, division) appears in both sources, the gov.uk API
## wins.  The alphagov source fills in any gaps in older date ranges.
##
## Run with: source("data-raw/uk_bank_holidays.R")
## Requires: jsonlite, data.table, usethis
##
## The created_at attribute on the resulting dataset records the date this
## script was run and is updated automatically every time the script executes.

library(jsonlite)
library(data.table)

# ---------------------------------------------------------------------------
# Helper: parse the flat-events format returned by the gov.uk API
# ---------------------------------------------------------------------------

parse_govuk_json <- function(raw) {
  records <- list()
  for (div in names(raw)) {
    events <- raw[[div]][["events"]]
    if (is.null(events)) {
      next
    }
    for (ev in events) {
      records[[length(records) + 1]] <- data.frame(
        date = as.Date(ev[["date"]]),
        title = ev[["title"]],
        notes = if (is.null(ev[["notes"]])) "" else ev[["notes"]],
        division = div,
        stringsAsFactors = FALSE
      )
    }
  }
  rbindlist(records)
}

# ---------------------------------------------------------------------------
# 1. Try the gov.uk API first
# ---------------------------------------------------------------------------

govuk_url <- "https://www.gov.uk/bank-holidays.json"

govuk_dt <- tryCatch(
  {
    message("Fetching from ", govuk_url, " ...")
    raw <- fromJSON(govuk_url, simplifyVector = FALSE)
    dt <- parse_govuk_json(raw)
    message("  Success: ", nrow(dt), " records fetched.")
    dt
  },
  error = function(e) {
    message(
      "  gov.uk API unavailable (",
      conditionMessage(e),
      "). Continuing without it."
    )
    NULL
  }
)

# ---------------------------------------------------------------------------
# 2. Fetch alphagov data (always attempted for extended coverage)
# ---------------------------------------------------------------------------

alphagov_url <- paste0(
  "https://raw.githubusercontent.com/alphagov/calendars/",
  "master/lib/data/bank-holidays.json"
)

# Translation keys to human-readable English titles (from alphagov en.yml)
title_map <- c(
  "bank_holidays.new_year" = "New Year's Day",
  "bank_holidays.2nd_january" = "2nd January",
  "bank_holidays.st_patrick" = "St Patrick's Day",
  "bank_holidays.good_friday" = "Good Friday",
  "bank_holidays.easter_monday" = "Easter Monday",
  "bank_holidays.early_may" = "Early May bank holiday",
  "bank_holidays.early_may_ve" = "Early May bank holiday (VE day)",
  "bank_holidays.spring" = "Spring bank holiday",
  "bank_holidays.battle_boyne" = "Battle of the Boyne (Orangemen's Day)",
  "bank_holidays.summer" = "Summer bank holiday",
  "bank_holidays.late_august" = "Summer bank holiday",
  "bank_holidays.st_andrew" = "St Andrew's Day",
  "bank_holidays.christmas" = "Christmas Day",
  "bank_holidays.boxing_day" = "Boxing Day",
  "bank_holidays.queen_diamond" = "Queen's Diamond Jubilee"
)
notes_map <- c(
  "common.substitute_day" = "Substitute day",
  "common.extra_bank_holiday" = "Extra bank holiday"
)

alphagov_dt <- tryCatch(
  {
    message("Fetching from alphagov/calendars ...")
    raw_data <- fromJSON(alphagov_url, simplifyVector = FALSE)
    records <- list()
    for (div in names(raw_data$divisions)) {
      div_data <- raw_data$divisions[[div]]
      for (yr in names(div_data)) {
        if (!yr %in% c("slug", "title")) {
          for (ev in div_data[[yr]]) {
            english_title <- title_map[ev$title]
            if (is.na(english_title)) {
              english_title <- ev$title
            }
            notes_val <- ev$notes
            if (notes_val %in% names(notes_map)) {
              notes_val <- notes_map[notes_val]
            }
            records[[length(records) + 1]] <- data.frame(
              date = as.Date(ev$date, "%d/%m/%Y"),
              title = unname(english_title),
              notes = unname(notes_val),
              division = div,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    dt <- rbindlist(records)
    message("  Success: ", nrow(dt), " records fetched.")
    dt
  },
  error = function(e) {
    message(
      "  alphagov/calendars unavailable (",
      conditionMessage(e),
      "). Continuing without it."
    )
    NULL
  }
)

# ---------------------------------------------------------------------------
# 3. Combine all sources: gov.uk > alphagov
#    For duplicate (date, division) pairs, the highest-priority source wins.
# ---------------------------------------------------------------------------

uk_bank_holidays <- if (!is.null(alphagov_dt)) alphagov_dt else data.table()

if (!is.null(govuk_dt) && nrow(govuk_dt) > 0) {
  # gov.uk supersedes alphagov: take all gov.uk + alphagov rows not in gov.uk
  uk_bank_holidays <- rbind(
    govuk_dt,
    uk_bank_holidays[
      !paste(uk_bank_holidays$date, uk_bank_holidays$division) %in%
        paste(govuk_dt$date, govuk_dt$division)
    ]
  )
}

# ---------------------------------------------------------------------------
# 4. Finalise and save
# ---------------------------------------------------------------------------

setorder(uk_bank_holidays, division, date)

# Record the date this script was run as an attribute on the dataset
attr(uk_bank_holidays, "created_at") <- as.character(Sys.Date())

usethis::use_data(uk_bank_holidays, overwrite = TRUE)

# Report actual coverage
cov <- uk_bank_holidays[, .(min = min(date), max = max(date)), by = division]
message("uk_bank_holidays saved to data/uk_bank_holidays.rda")
message("Created at: ", attr(uk_bank_holidays, "created_at"))
message("Coverage:")
for (i in seq_len(nrow(cov))) {
  message("  ", cov$division[i], ": ", cov$min[i], " to ", cov$max[i])
}
