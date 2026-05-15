## Script to generate the uk_bank_holidays dataset.
##
## This script compiles UK bank holidays from two sources:
##   1. The alphagov/calendars GitHub repository (2015–2021, all three divisions)
##   2. Manually compiled data for England and Wales (2010–2014 and 2022–2026)
##
## Run with: source("data-raw/uk_bank_holidays.R")
## Requires: jsonlite, data.table
##
## To refresh from the live gov.uk API, call get_uk_bank_holidays() and
## use usethis::use_data() to overwrite the stored dataset.

library(jsonlite)
library(data.table)

# ---------------------------------------------------------------------------
# 1. Fetch and process alphagov data (2015-2021, all three divisions)
# ---------------------------------------------------------------------------

alphagov_url <- paste0(
  "https://raw.githubusercontent.com/alphagov/calendars/",
  "master/lib/data/bank-holidays.json"
)

# Translation keys to human-readable English titles (from alphagov en.yml)
title_map <- c(
  "bank_holidays.new_year"      = "New Year's Day",
  "bank_holidays.2nd_january"   = "2nd January",
  "bank_holidays.st_patrick"    = "St Patrick's Day",
  "bank_holidays.good_friday"   = "Good Friday",
  "bank_holidays.easter_monday" = "Easter Monday",
  "bank_holidays.early_may"     = "Early May bank holiday",
  "bank_holidays.early_may_ve"  = "Early May bank holiday (VE day)",
  "bank_holidays.spring"        = "Spring bank holiday",
  "bank_holidays.battle_boyne"  = "Battle of the Boyne (Orangemen's Day)",
  "bank_holidays.summer"        = "Summer bank holiday",
  "bank_holidays.late_august"   = "Summer bank holiday",
  "bank_holidays.st_andrew"     = "St Andrew's Day",
  "bank_holidays.christmas"     = "Christmas Day",
  "bank_holidays.boxing_day"    = "Boxing Day",
  "bank_holidays.queen_diamond" = "Queen's Diamond Jubilee"
)

raw_data <- fromJSON(alphagov_url, simplifyVector = FALSE)

records <- list()
for (div in names(raw_data$divisions)) {
  div_data <- raw_data$divisions[[div]]
  for (yr in names(div_data)) {
    if (!yr %in% c("slug", "title")) {
      for (ev in div_data[[yr]]) {
        english_title <- title_map[ev$title]
        if (is.na(english_title)) english_title <- ev$title
        notes_map <- c(
          "common.substitute_day"    = "Substitute day",
          "common.extra_bank_holiday" = "Extra bank holiday"
        )
        notes_val <- ev$notes
        if (notes_val %in% names(notes_map)) notes_val <- notes_map[notes_val]
        records[[length(records) + 1]] <- data.frame(
          date     = as.Date(ev$date, "%d/%m/%Y"),
          title    = unname(english_title),
          notes    = unname(notes_val),
          bunting  = ev$bunting,
          division = div,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}
alphagov_dt <- rbindlist(records)

# ---------------------------------------------------------------------------
# 2. Manually compiled England and Wales bank holidays (2010–2014, 2022–2026)
# ---------------------------------------------------------------------------
# These are sourced from https://www.gov.uk/bank-holidays and include
# substitute days and one-off special bank holidays.

ew_extra <- data.table(
  date = as.Date(c(
    # 2010 – Dec 25 = Saturday, Dec 26 = Sunday (both substituted)
    "2010-01-01", "2010-04-02", "2010-04-05",
    "2010-05-03", "2010-05-31", "2010-08-30",
    "2010-12-27", "2010-12-28",

    # 2011 – Jan 1 = Saturday, Dec 25 = Sunday
    "2011-01-03", "2011-04-22", "2011-04-25",
    "2011-04-29",                # Royal Wedding (extra)
    "2011-05-02", "2011-05-30", "2011-08-29",
    "2011-12-26", "2011-12-27",

    # 2012 – Jan 1 = Sunday, Spring BH moved for Diamond Jubilee
    "2012-01-02", "2012-04-06", "2012-04-09",
    "2012-05-07",
    "2012-06-04", "2012-06-05",  # Spring BH moved + Diamond Jubilee (extra)
    "2012-08-27",
    "2012-12-25", "2012-12-26",

    # 2013
    "2013-01-01", "2013-03-29", "2013-04-01",
    "2013-05-06", "2013-05-27", "2013-08-26",
    "2013-12-25", "2013-12-26",

    # 2014
    "2014-01-01", "2014-04-18", "2014-04-21",
    "2014-05-05", "2014-05-26", "2014-08-25",
    "2014-12-25", "2014-12-26",

    # 2022 – Jan 1 = Saturday, Spring BH moved for Platinum Jubilee, Sep funeral
    "2022-01-03",                # New Year's Day (substitute)
    "2022-04-15", "2022-04-18",
    "2022-05-02",
    "2022-06-02", "2022-06-03",  # Spring BH moved + Platinum Jubilee (extra)
    "2022-08-29",
    "2022-09-19",                # State Funeral of Queen Elizabeth II (extra)
    "2022-12-26", "2022-12-27",  # Dec 25 = Sunday: Boxing Day Mon, Christmas Tue

    # 2023 – Jan 1 = Sunday, Coronation extra BH
    "2023-01-02",                # New Year's Day (substitute)
    "2023-04-07", "2023-04-10",
    "2023-05-01",
    "2023-05-08",                # King's Coronation bank holiday (extra)
    "2023-05-29", "2023-08-28",
    "2023-12-25", "2023-12-26",

    # 2024
    "2024-01-01", "2024-03-29", "2024-04-01",
    "2024-05-06", "2024-05-27", "2024-08-26",
    "2024-12-25", "2024-12-26",

    # 2025
    "2025-01-01", "2025-04-18", "2025-04-21",
    "2025-05-05", "2025-05-26", "2025-08-25",
    "2025-12-25", "2025-12-26",

    # 2026 – Dec 26 = Saturday (Boxing Day substituted)
    "2026-01-01", "2026-04-03", "2026-04-06",
    "2026-05-04", "2026-05-25", "2026-08-31",
    "2026-12-25", "2026-12-28"   # Boxing Day (substitute – Dec 26 = Saturday)
  )),
  title = c(
    # 2010
    "New Year's Day", "Good Friday", "Easter Monday",
    "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
    "Christmas Day", "Boxing Day",

    # 2011
    "New Year's Day", "Good Friday", "Easter Monday",
    "Royal Wedding bank holiday",
    "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
    "Boxing Day", "Christmas Day",

    # 2012
    "New Year's Day", "Good Friday", "Easter Monday",
    "Early May bank holiday",
    "Spring bank holiday", "Queen's Diamond Jubilee",
    "Summer bank holiday",
    "Christmas Day", "Boxing Day",

    # 2013
    "New Year's Day", "Good Friday", "Easter Monday",
    "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
    "Christmas Day", "Boxing Day",

    # 2014
    "New Year's Day", "Good Friday", "Easter Monday",
    "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
    "Christmas Day", "Boxing Day",

    # 2022
    "New Year's Day",
    "Good Friday", "Easter Monday",
    "Early May bank holiday",
    "Spring bank holiday", "Queen's Platinum Jubilee",
    "Summer bank holiday",
    "Bank Holiday for the State Funeral of Queen Elizabeth II",
    "Boxing Day", "Christmas Day",

    # 2023
    "New Year's Day",
    "Good Friday", "Easter Monday",
    "Early May bank holiday",
    "Bank holiday for the coronation of King Charles III",
    "Spring bank holiday", "Summer bank holiday",
    "Christmas Day", "Boxing Day",

    # 2024
    "New Year's Day", "Good Friday", "Easter Monday",
    "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
    "Christmas Day", "Boxing Day",

    # 2025
    "New Year's Day", "Good Friday", "Easter Monday",
    "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
    "Christmas Day", "Boxing Day",

    # 2026
    "New Year's Day", "Good Friday", "Easter Monday",
    "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
    "Christmas Day", "Boxing Day"
  ),
  notes = c(
    # 2010
    "", "", "",
    "", "", "",
    "Substitute day", "Substitute day",

    # 2011
    "Substitute day", "", "",
    "Extra bank holiday",
    "", "", "",
    "", "Substitute day",

    # 2012
    "Substitute day", "", "",
    "",
    "", "Extra bank holiday",
    "",
    "", "",

    # 2013
    "", "", "",
    "", "", "",
    "", "",

    # 2014
    "", "", "",
    "", "", "",
    "", "",

    # 2022
    "Substitute day",
    "", "",
    "",
    "", "Extra bank holiday",
    "",
    "Extra bank holiday",
    "", "Substitute day",

    # 2023
    "Substitute day",
    "", "",
    "",
    "Extra bank holiday",
    "", "",
    "", "",

    # 2024
    "", "", "",
    "", "", "",
    "", "",

    # 2025
    "", "", "",
    "", "", "",
    "", "",

    # 2026
    "", "", "",
    "", "", "",
    "", "Substitute day"
  ),
  bunting = c(
    # 2010
    TRUE, FALSE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE,

    # 2011
    TRUE, FALSE, TRUE,
    TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE,

    # 2012
    TRUE, FALSE, TRUE,
    TRUE,
    TRUE, TRUE,
    TRUE,
    TRUE, TRUE,

    # 2013
    TRUE, FALSE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE,

    # 2014
    TRUE, FALSE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE,

    # 2022
    TRUE,
    FALSE, TRUE,
    TRUE,
    TRUE, TRUE,
    TRUE,
    FALSE,
    TRUE, TRUE,

    # 2023
    TRUE,
    FALSE, TRUE,
    TRUE,
    TRUE,
    TRUE, TRUE,
    TRUE, TRUE,

    # 2024
    TRUE, FALSE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE,

    # 2025
    TRUE, FALSE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE,

    # 2026
    TRUE, FALSE, TRUE,
    TRUE, TRUE, TRUE,
    TRUE, TRUE
  ),
  division = "england-and-wales"
)

# ---------------------------------------------------------------------------
# 3. Combine and save
# ---------------------------------------------------------------------------

uk_bank_holidays <- rbind(alphagov_dt, ew_extra)
setorder(uk_bank_holidays, division, date)

usethis::use_data(uk_bank_holidays, overwrite = TRUE)

message("uk_bank_holidays saved to data/uk_bank_holidays.rda")
message("Coverage: england-and-wales 2010-2026, scotland/northern-ireland 2015-2021")
