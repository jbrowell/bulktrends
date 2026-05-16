## Script to generate the uk_bank_holidays dataset.
##
## Primary source: https://www.gov.uk/bank-holidays.json
##   (all three UK divisions, all years available in the API)
##
## Fallback (used when gov.uk is not accessible):
##   1. alphagov/calendars GitHub repository (2015–2021, all three divisions)
##   2. Manually compiled data:
##        England & Wales  : 2010–2014 and 2022–2026
##        Scotland         : 2022–2026
##        Northern Ireland : 2022–2026
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
    if (is.null(events)) next
    for (ev in events) {
      records[[length(records) + 1]] <- data.frame(
        date     = as.Date(ev[["date"]]),
        title    = ev[["title"]],
        notes    = if (is.null(ev[["notes"]])) "" else ev[["notes"]],
        bunting  = if (is.null(ev[["bunting"]])) NA else ev[["bunting"]],
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

govuk_dt <- tryCatch({
  message("Fetching from ", govuk_url, " ...")
  raw <- fromJSON(govuk_url, simplifyVector = FALSE)
  dt  <- parse_govuk_json(raw)
  message("  Success: ", nrow(dt), " records fetched.")
  dt
}, error = function(e) {
  message("  gov.uk API unavailable (", conditionMessage(e), ").")
  message("  Falling back to alphagov + manually compiled data.")
  NULL
})

if (!is.null(govuk_dt)) {
  uk_bank_holidays <- govuk_dt

} else {

  # -------------------------------------------------------------------------
  # 2a. Fallback: fetch alphagov data (2015–2021, all three divisions)
  # -------------------------------------------------------------------------

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
  notes_map <- c(
    "common.substitute_day"     = "Substitute day",
    "common.extra_bank_holiday" = "Extra bank holiday"
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

  # -------------------------------------------------------------------------
  # 2b. Fallback: manually compiled data
  #     England & Wales  2010-2014 and 2022-2026
  #     Scotland         2022-2026
  #     Northern Ireland 2022-2026
  #
  # Sources: https://www.gov.uk/bank-holidays
  # -------------------------------------------------------------------------

  manual_dt <- data.table(
    date = as.Date(c(
      # -----------------------------------------------------------------------
      # England and Wales 2010–2014
      # -----------------------------------------------------------------------
      # 2010 – Dec 25 = Saturday, Dec 26 = Sunday (both substituted)
      "2010-01-01", "2010-04-02", "2010-04-05",
      "2010-05-03", "2010-05-31", "2010-08-30",
      "2010-12-27", "2010-12-28",
      # 2011 – Jan 1 = Saturday, Dec 25 = Sunday
      "2011-01-03", "2011-04-22", "2011-04-25",
      "2011-04-29",               # Royal Wedding (extra)
      "2011-05-02", "2011-05-30", "2011-08-29",
      "2011-12-26", "2011-12-27",
      # 2012 – Jan 1 = Sunday, Spring BH moved for Diamond Jubilee
      "2012-01-02", "2012-04-06", "2012-04-09",
      "2012-05-07",
      "2012-06-04", "2012-06-05", # Spring BH moved + Diamond Jubilee (extra)
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
      # -----------------------------------------------------------------------
      # England and Wales 2022–2026
      # -----------------------------------------------------------------------
      # 2022 – Jan 1 = Saturday; Spring BH moved for Platinum Jubilee
      "2022-01-03",               # New Year's Day (substitute)
      "2022-04-15", "2022-04-18",
      "2022-05-02",
      "2022-06-02", "2022-06-03", # Spring BH moved + Platinum Jubilee (extra)
      "2022-08-29",
      "2022-09-19",               # State Funeral of Queen Elizabeth II (extra)
      "2022-12-26", "2022-12-27", # Dec 25 = Sunday: Boxing Day Mon, Christmas Tue
      # 2023 – Jan 1 = Sunday; Coronation extra BH
      "2023-01-02",               # New Year's Day (substitute)
      "2023-04-07", "2023-04-10",
      "2023-05-01",
      "2023-05-08",               # King's Coronation (extra)
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
      "2026-12-25", "2026-12-28", # Boxing Day (substitute – Dec 26 = Sat)
      # -----------------------------------------------------------------------
      # Scotland 2022–2026
      # -----------------------------------------------------------------------
      # 2022 – Jan 1 = Saturday, Jan 2 = Sunday (both substituted);
      #        Spring BH moved for Platinum Jubilee; Dec 25 = Sunday
      "2022-01-03",               # New Year's Day (substitute)
      "2022-01-04",               # 2nd January (substitute)
      "2022-04-15",
      "2022-05-02",
      "2022-06-02", "2022-06-03", # Spring BH moved + Platinum Jubilee (extra)
      "2022-08-01",               # Summer BH = first Monday of August
      "2022-09-19",               # State Funeral (extra)
      "2022-11-30",               # St Andrew's Day (Wednesday)
      "2022-12-26",               # Boxing Day (Monday)
      "2022-12-27",               # Christmas Day (substitute – Dec 25 = Sun)
      # 2023 – Jan 1 = Sunday (2nd January keeps Mon, NY gets Tue); Coronation extra
      "2023-01-02",               # 2nd January (Monday)
      "2023-01-03",               # New Year's Day (substitute – Tue)
      "2023-04-07",
      "2023-05-01",
      "2023-05-08",               # King's Coronation (extra)
      "2023-05-29",
      "2023-08-07",               # Summer BH = first Monday of August
      "2023-11-30",               # St Andrew's Day (Thursday)
      "2023-12-25",
      "2023-12-26",
      # 2024
      "2024-01-01",
      "2024-01-02",               # 2nd January
      "2024-03-29",
      "2024-05-06",
      "2024-05-27",
      "2024-08-05",               # Summer BH = first Monday of August
      "2024-12-02",               # St Andrew's Day (substitute – Nov 30 = Sat)
      "2024-12-25",
      "2024-12-26",
      # 2025
      "2025-01-01",
      "2025-01-02",               # 2nd January
      "2025-04-18",
      "2025-05-05",
      "2025-05-26",
      "2025-08-04",               # Summer BH = first Monday of August
      "2025-12-01",               # St Andrew's Day (substitute – Nov 30 = Sun)
      "2025-12-25",
      "2025-12-26",
      # 2026 – Dec 26 = Saturday
      "2026-01-01",
      "2026-01-02",               # 2nd January
      "2026-04-03",
      "2026-05-04",
      "2026-05-25",
      "2026-08-03",               # Summer BH = first Monday of August
      "2026-11-30",               # St Andrew's Day (Monday)
      "2026-12-25",
      "2026-12-28",               # Boxing Day (substitute – Dec 26 = Sat)
      # -----------------------------------------------------------------------
      # Northern Ireland 2022–2026
      # -----------------------------------------------------------------------
      # 2022 – Jan 1 = Saturday; Spring BH moved; Dec 25 = Sunday
      "2022-01-03",               # New Year's Day (substitute)
      "2022-03-17",               # St Patrick's Day (Thursday)
      "2022-04-15",
      "2022-04-18",
      "2022-05-02",
      "2022-06-02", "2022-06-03", # Spring BH moved + Platinum Jubilee (extra)
      "2022-07-12",               # Battle of the Boyne (Tuesday)
      "2022-08-29",               # Summer BH = last Monday of August
      "2022-09-19",               # State Funeral (extra)
      "2022-12-26",               # Boxing Day (Monday)
      "2022-12-27",               # Christmas Day (substitute – Dec 25 = Sun)
      # 2023 – Jan 1 = Sunday; Coronation extra
      "2023-01-02",               # New Year's Day (substitute)
      "2023-03-17",               # St Patrick's Day (Friday)
      "2023-04-07",
      "2023-04-10",
      "2023-05-01",
      "2023-05-08",               # King's Coronation (extra)
      "2023-05-29",
      "2023-07-12",               # Battle of the Boyne (Wednesday)
      "2023-08-28",               # Summer BH = last Monday of August
      "2023-12-25",
      "2023-12-26",
      # 2024
      "2024-01-01",
      "2024-03-18",               # St Patrick's Day (substitute – Mar 17 = Sun)
      "2024-03-29",
      "2024-04-01",
      "2024-05-06",
      "2024-05-27",
      "2024-07-12",               # Battle of the Boyne (Friday)
      "2024-08-26",               # Summer BH = last Monday of August
      "2024-12-25",
      "2024-12-26",
      # 2025
      "2025-01-01",
      "2025-03-17",               # St Patrick's Day (Monday)
      "2025-04-18",
      "2025-04-21",
      "2025-05-05",
      "2025-05-26",
      "2025-07-14",               # Battle of the Boyne (substitute – Jul 12 = Sat)
      "2025-08-25",               # Summer BH = last Monday of August
      "2025-12-25",
      "2025-12-26",
      # 2026 – Dec 26 = Saturday
      "2026-01-01",
      "2026-03-17",               # St Patrick's Day (Tuesday)
      "2026-04-03",
      "2026-04-06",
      "2026-05-04",
      "2026-05-25",
      "2026-07-13",               # Battle of the Boyne (substitute – Jul 12 = Sun)
      "2026-08-31",               # Summer BH = last Monday of August
      "2026-12-25",
      "2026-12-28"                # Boxing Day (substitute – Dec 26 = Sat)
    )),
    title = c(
      # England and Wales 2010
      "New Year's Day", "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # England and Wales 2011
      "New Year's Day", "Good Friday", "Easter Monday",
      "Royal Wedding bank holiday",
      "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
      "Boxing Day", "Christmas Day",
      # England and Wales 2012
      "New Year's Day", "Good Friday", "Easter Monday",
      "Early May bank holiday",
      "Spring bank holiday", "Queen's Diamond Jubilee",
      "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # England and Wales 2013
      "New Year's Day", "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # England and Wales 2014
      "New Year's Day", "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # England and Wales 2022
      "New Year's Day",
      "Good Friday", "Easter Monday",
      "Early May bank holiday",
      "Spring bank holiday", "Queen's Platinum Jubilee",
      "Summer bank holiday",
      "Bank Holiday for the State Funeral of Queen Elizabeth II",
      "Boxing Day", "Christmas Day",
      # England and Wales 2023
      "New Year's Day",
      "Good Friday", "Easter Monday",
      "Early May bank holiday",
      "Bank holiday for the coronation of King Charles III",
      "Spring bank holiday", "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # England and Wales 2024
      "New Year's Day", "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # England and Wales 2025
      "New Year's Day", "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # England and Wales 2026
      "New Year's Day", "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday", "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # Scotland 2022
      "New Year's Day",
      "2nd January",
      "Good Friday",
      "Early May bank holiday",
      "Spring bank holiday", "Queen's Platinum Jubilee",
      "Summer bank holiday",
      "Bank Holiday for the State Funeral of Queen Elizabeth II",
      "St Andrew's Day",
      "Boxing Day", "Christmas Day",
      # Scotland 2023
      "2nd January",
      "New Year's Day",
      "Good Friday",
      "Early May bank holiday",
      "Bank holiday for the coronation of King Charles III",
      "Spring bank holiday",
      "Summer bank holiday",
      "St Andrew's Day",
      "Christmas Day", "Boxing Day",
      # Scotland 2024
      "New Year's Day", "2nd January",
      "Good Friday",
      "Early May bank holiday", "Spring bank holiday",
      "Summer bank holiday",
      "St Andrew's Day",
      "Christmas Day", "Boxing Day",
      # Scotland 2025
      "New Year's Day", "2nd January",
      "Good Friday",
      "Early May bank holiday", "Spring bank holiday",
      "Summer bank holiday",
      "St Andrew's Day",
      "Christmas Day", "Boxing Day",
      # Scotland 2026
      "New Year's Day", "2nd January",
      "Good Friday",
      "Early May bank holiday", "Spring bank holiday",
      "Summer bank holiday",
      "St Andrew's Day",
      "Christmas Day", "Boxing Day",
      # Northern Ireland 2022
      "New Year's Day",
      "St Patrick's Day",
      "Good Friday", "Easter Monday",
      "Early May bank holiday",
      "Spring bank holiday", "Queen's Platinum Jubilee",
      "Battle of the Boyne (Orangemen's Day)",
      "Summer bank holiday",
      "Bank Holiday for the State Funeral of Queen Elizabeth II",
      "Boxing Day", "Christmas Day",
      # Northern Ireland 2023
      "New Year's Day",
      "St Patrick's Day",
      "Good Friday", "Easter Monday",
      "Early May bank holiday",
      "Bank holiday for the coronation of King Charles III",
      "Spring bank holiday",
      "Battle of the Boyne (Orangemen's Day)",
      "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # Northern Ireland 2024
      "New Year's Day",
      "St Patrick's Day",
      "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday",
      "Battle of the Boyne (Orangemen's Day)",
      "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # Northern Ireland 2025
      "New Year's Day",
      "St Patrick's Day",
      "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday",
      "Battle of the Boyne (Orangemen's Day)",
      "Summer bank holiday",
      "Christmas Day", "Boxing Day",
      # Northern Ireland 2026
      "New Year's Day",
      "St Patrick's Day",
      "Good Friday", "Easter Monday",
      "Early May bank holiday", "Spring bank holiday",
      "Battle of the Boyne (Orangemen's Day)",
      "Summer bank holiday",
      "Christmas Day", "Boxing Day"
    ),
    notes = c(
      # England and Wales 2010
      "", "", "", "", "", "", "Substitute day", "Substitute day",
      # England and Wales 2011
      "Substitute day", "", "", "Extra bank holiday", "", "", "",
      "", "Substitute day",
      # England and Wales 2012
      "Substitute day", "", "", "", "", "Extra bank holiday", "", "", "",
      # England and Wales 2013
      "", "", "", "", "", "", "", "",
      # England and Wales 2014
      "", "", "", "", "", "", "", "",
      # England and Wales 2022
      "Substitute day", "", "", "",
      "", "Extra bank holiday", "", "Extra bank holiday", "", "Substitute day",
      # England and Wales 2023
      "Substitute day", "", "", "", "Extra bank holiday", "", "", "", "",
      # England and Wales 2024
      "", "", "", "", "", "", "", "",
      # England and Wales 2025
      "", "", "", "", "", "", "", "",
      # England and Wales 2026
      "", "", "", "", "", "", "", "Substitute day",
      # Scotland 2022
      "Substitute day", "Substitute day", "", "",
      "", "Extra bank holiday", "", "Extra bank holiday", "", "", "Substitute day",
      # Scotland 2023
      "", "Substitute day", "", "", "Extra bank holiday", "", "", "", "", "",
      # Scotland 2024
      "", "", "", "", "", "", "Substitute day", "", "",
      # Scotland 2025
      "", "", "", "", "", "", "Substitute day", "", "",
      # Scotland 2026
      "", "", "", "", "", "", "", "", "Substitute day",
      # Northern Ireland 2022
      "Substitute day", "", "", "", "",
      "", "Extra bank holiday", "", "", "Extra bank holiday", "", "Substitute day",
      # Northern Ireland 2023
      "Substitute day", "", "", "", "", "Extra bank holiday", "", "", "", "", "",
      # Northern Ireland 2024
      "", "Substitute day", "", "", "", "", "", "", "", "",
      # Northern Ireland 2025
      "", "", "", "", "", "", "Substitute day", "", "", "",
      # Northern Ireland 2026
      "", "", "", "", "", "", "Substitute day", "", "", "Substitute day"
    ),
    bunting = c(
      # England and Wales 2010
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2011
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2012
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2013
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2014
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2022
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
      # England and Wales 2023
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2024
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2025
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # England and Wales 2026
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Scotland 2022
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
      # Scotland 2023
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Scotland 2024
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Scotland 2025
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Scotland 2026
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Northern Ireland 2022
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE,
      # Northern Ireland 2023
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Northern Ireland 2024
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Northern Ireland 2025
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      # Northern Ireland 2026
      TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ),
    division = c(
      rep("england-and-wales", 8),   # 2010
      rep("england-and-wales", 9),   # 2011
      rep("england-and-wales", 9),   # 2012
      rep("england-and-wales", 8),   # 2013
      rep("england-and-wales", 8),   # 2014
      rep("england-and-wales", 10),  # 2022
      rep("england-and-wales", 9),   # 2023
      rep("england-and-wales", 8),   # 2024
      rep("england-and-wales", 8),   # 2025
      rep("england-and-wales", 8),   # 2026
      rep("scotland", 11),           # 2022
      rep("scotland", 10),           # 2023
      rep("scotland",  9),           # 2024
      rep("scotland",  9),           # 2025
      rep("scotland",  9),           # 2026
      rep("northern-ireland", 12),   # 2022
      rep("northern-ireland", 11),   # 2023
      rep("northern-ireland", 10),   # 2024
      rep("northern-ireland", 10),   # 2025
      rep("northern-ireland", 10)    # 2026
    )
  )

  uk_bank_holidays <- rbind(alphagov_dt, manual_dt)

}

# ---------------------------------------------------------------------------
# 3. Finalise and save
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
