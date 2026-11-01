# Focused regression, from the repository root:
# Rscript --vanilla -e "source('demo/test_calendar.R', encoding = 'UTF-8')"
# Uses the application's existing dplyr dependency; no database or network.
suppressPackageStartupMessages(library(dplyr))
source("production_calendar.R", encoding = "UTF-8")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

# Reproduce the KPI inner_join: the old calendar dropped all 2026 rows.
requests <- data.frame(
  Дата = as.Date(c("2026-11-02", "2026-11-03", "2026-11-04", "2026-11-05")),
  Рейсы = c(2, 4, 1, 2), check.names = FALSE
)
joined <- inner_join(requests, transition_table_workDay, by = "Дата")
assert(nrow(joined) == nrow(requests), "Calendar join dropped current KPI rows")
assert(sum(joined$Рабочих_дней) == 3, "November 4 must be a holiday; November 3 still counts as one workday")
assert(sum(joined$Рейсы) / sum(joined$Рабочих_дней) == 3, "KPI denominator changed")

calendar <- transition_table_workDay
assert(inherits(calendar$Дата, "Date"), "Calendar dates must retain Date class")
assert(!anyDuplicated(calendar$Дата), "Duplicate dates would multiply KPI rows")
assert(all(diff(calendar$Дата) == 1), "Calendar must have no missing dates")
assert(all(calendar$Рабочих_дней %in% c(0, 1)), "Existing KPI contract counts days, not hours")

expect_days <- function(dates, values) {
  actual <- calendar$Рабочих_дней[match(as.Date(dates), calendar$Дата)]
  assert(identical(as.integer(actual), as.integer(values)), paste("Wrong calendar day:", paste(dates, collapse = ", ")))
}

# Historic working Saturday and transferred days; keep the original contract.
expect_days(c("2021-11-01", "2021-11-05", "2021-12-31", "2022-03-05", "2022-03-07", "2023-02-24", "2023-05-08"),
            c(1, 0, 0, 1, 0, 0, 0))
expect_days(c("2022-02-23", "2023-03-04", "2023-03-05"), c(0, 0, 0))
# A weekend holiday moves to Monday; explicit transfers include working Saturdays.
expect_days(c("2023-11-06", "2024-02-29", "2024-04-27", "2024-04-29", "2024-04-30", "2024-11-02", "2024-12-28", "2024-12-30", "2024-12-31"),
            c(0, 1, 1, 0, 0, 1, 1, 0, 0))
expect_days(c("2025-02-24", "2025-03-10", "2025-05-02", "2025-05-08", "2025-06-13", "2025-11-01", "2025-11-03", "2025-12-31"),
            c(1, 1, 0, 0, 0, 1, 0, 0))
expect_days(c("2026-01-09", "2026-03-09", "2026-05-11", "2026-11-03", "2026-12-31"),
            c(0, 0, 0, 1, 0))
expect_days(c("2027-02-20", "2027-02-22", "2027-05-03", "2027-05-10", "2027-06-14", "2027-11-05", "2027-12-31"),
            c(1, 0, 0, 0, 0, 0, 0))

annual_workdays <- tapply(calendar$Рабочих_дней, format(calendar$Дата, "%Y"), sum)
assert(identical(as.integer(annual_workdays[as.character(2022:2027)]), c(247L, 247L, 248L, 247L, 247L, 247L)),
       "Annual working-day totals differ from the federal five-day calendars")

assert(identical(production_calendar_for_dates(requests$Дата), calendar), "Known dates must retain the join contract")
assert(identical(production_calendar_for_dates(as.Date(character())), calendar), "An empty result is valid")
for (invalid_dates in list(as.Date("2028-01-01"), as.Date("2021-10-31"), as.Date(NA), "2026-11-02")) {
  error <- tryCatch({ production_calendar_for_dates(invalid_dates); NULL }, error = identity)
  assert(inherits(error, "error"), "Out-of-range/missing/untyped dates must not be silently dropped")
}
cat("Calendar regression checks passed: KPI join, continuity, transfers, 2022-2027 annual totals, unsupported-date guard.\n")
