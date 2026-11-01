# Regression of the real unformed-bills aggregation without database access.
# Run from the root with the same locale/library setup as demo/test_reports.ps1.
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib"), .libPaths()))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
helpers_source <- file("helpers.R", open = "rt", encoding = "UTF-8")
helpers_code <- parse(helpers_source)
close(helpers_source)
loader_assignment <- Filter(function(expression) {
  is.call(expression) && identical(expression[[1]], as.name("<-")) &&
    identical(expression[[2]], as.name("get_unformed_bills"))
}, as.list(helpers_code))
stopifnot(length(loader_assignment) == 1L)

run_fixture <- function(reference_date, dates, amounts, statuses) {
  test_env <- new.env(parent = globalenv())
  reference_date <- as.Date(reference_date)
  test_env$Sys.Date <- function() reference_date
  test_env$Sys.time <- function() as.POSIXct(reference_date, tz = "Europe/Moscow")
  test_env$fixture <- data.frame(
    Контрагент = rep("Demo client", length(dates)),
    Месяц = as.POSIXct(dates, tz = "Europe/Moscow"),
    Сумма = amounts, Статус = statuses, stringsAsFactors = FALSE
  )
  eval(loader_assignment[[1]], envir = test_env)
  loader <- test_env$get_unformed_bills
  expressions <- as.list(body(loader))
  query <- which(vapply(expressions, function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("unformed_bills"))
  }, logical(1)))
  stopifnot(length(query) == 1L)
  # Replace only the SQL fetch with its result shape; all aggregation is production code.
  expressions[[query]] <- quote(unformed_bills <- fixture)
  body(loader) <- as.call(expressions)
  loader()
}

check_period <- function(reference_date, dates, month_names, expected_buckets) {
  result <- run_fixture(reference_date, dates, c(10, 20, 30, 40),
                        c("Получили ТСД", "Ожидает ТСД", "Ожидает ТСД", "Получили ТСД"))
  columns <- c("Контрагент", "Статус", month_names, "Текущий год", "Ранее", "Всего")
  stopifnot(identical(names(result), columns), nrow(result) == 2L)
  bucket_columns <- c(month_names, "Текущий год", "Ранее")
  stopifnot(identical(as.numeric(colSums(result[bucket_columns])), expected_buckets),
            all(rowSums(result[bucket_columns]) == result$Всего),
            sum(result$Всего) == 100, !anyNA(result))
}

check_period("2026-10-31", c("2026-10-01", "2026-09-01", "2026-08-01", "2025-12-01"),
             c("2026.10", "2026.09"), c(10, 20, 30, 40))
check_period("2026-11-15", c("2026-11-01", "2026-10-01", "2026-09-01", "2025-12-01"),
             c("2026.11", "2026.10"), c(10, 20, 30, 40))
check_period("2027-01-15", c("2027-01-01", "2026-12-01", "2026-11-01", "2025-12-01"),
             c("2027.01", "2026.12"), c(10, 20, 0, 70))

sparse <- run_fixture("2026-11-15", "2026-11-01", 10, "Получили ТСД")
stopifnot(nrow(sparse) == 1L, sparse$`2026.11` == 10, sparse$`2026.10` == 0,
          sparse$`Текущий год` == 0, sparse$Ранее == 0, sparse$Всего == 10)
empty <- run_fixture("2026-11-15", character(), numeric(), character())
stopifnot(nrow(empty) == 0L, identical(names(empty), names(sparse)))
cat("Unformed bills: PASS (October/November, January rollover, sparse/empty input, totals)\n")
