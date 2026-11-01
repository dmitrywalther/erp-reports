# Regression: PostgreSQL bigint counters and empty report slices.
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib"), .libPaths()))
suppressPackageStartupMessages(library(bit64))
source("report_utils.R", encoding = "UTF-8")
run_ratio_checks <- function() {
previous_options <- options(warn = 2)
on.exit(options(previous_options), add = TRUE)

stopifnot(identical(report_ratio(as.integer64(c(12, 0)), as.integer64(c(4, 0))), c(3, NA_real_)))
stopifnot(identical(report_ratio(c(0, 5, -5, NA, Inf), c(0, 0, 0, 1, 1)), rep(NA_real_, 5)))
stopifnot(identical(report_ratio(c(120, -30, 0), 10), c(12, -3, 0)))
stopifnot(identical(report_ratio(numeric(0), numeric(0)), numeric(0)))
stopifnot(identical(report_ratio(as.integer64("4000000000"), as.integer64(2)), 2000000000))
cat("Report ratios: PASS (empty/zero/NA/Inf, bigint, negative margin, vector inputs)\n")
}
run_ratio_checks()
