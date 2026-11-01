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

# Execute the real seven NDZ day-cell/footer callbacks with strict warnings.
local({
  previous_options <- options(warn = 2)
  on.exit(options(previous_options), add = TRUE)
  find_assignment <- function(expr, name) {
    if (missing(expr) || (!is.call(expr) && !is.expression(expr))) return(NULL)
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(paste(deparse(expr[[2]]), collapse = ""), name)) return(expr)
    for (part in as.list(expr)) {
      found <- find_assignment(part, name)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  con <- file("server.R", open = "rt", encoding = "UTF-8")
  code <- parse(con)
  close(con)
  definition <- find_assignment(code, "output$dz_unconfirmed_receivables")
  stopifnot(!is.null(definition))
  report_env <- new.env(parent = environment())
  report_env$renderReactable <- function(expr) expr
  report_env$reactable <- function(data, ..., columns) columns
  report_env$colDef <- function(...) list(...)
  report_env$colFormat <- function(...) list(...)
  report_env$div <- htmltools::div
  report_env$DZ_unconfirmed_receivables_table <- list(data = NULL)
  columns <- eval(definition[[3]], report_env)
  columns <- Filter(function(column) is.function(column$footer) && is.function(column$cell), columns)
  stopifnot(length(columns) == 7L)
  sources <- sub("[.]x$", ".y", names(columns))
  fixtures <- list(c(NA_real_, 0, 4.5), c(4.5, NA_real_, 0),
                   c(NA_integer_, 0L, 3L), c(0, 0), NA_real_, numeric(0), integer(0))
  for (days in fixtures) {
    expected <- days
    expected[is.na(expected)] <- 0L
    data <- setNames(rep(list(days), length(sources)), sources)
    report_env$DZ_unconfirmed_receivables_table$data <- data
    for (column in columns) {
      # Inspect the exact normalization statements as well as rendered output.
      frame <- new.env(parent = environment(column$footer))
      eval(as.expression(as.list(body(column$footer))[2:3]), frame)
      stopifnot(identical(frame$days, expected))
      rendered <- as.character(column$footer(rep(100, length(days))))
      expected_label <- if (length(days)) paste(format(mean(expected), big.mark = " ", digits = 2,
                                                      scientific = FALSE), "дней") else "– дней"
      stopifnot(grepl(expected_label, rendered, fixed = TRUE),
                !grepl("NaN|NA дней", rendered))
      for (index in seq_along(days)) {
        rendered <- as.character(column$cell(100, index))
        stopifnot(grepl(paste(format(expected[index], big.mark = " ", digits = 2,
                                    scientific = FALSE), "дней"), rendered, fixed = TRUE))
      }
    }
  }
  cat("NDZ day callbacks: PASS (all seven columns, mixed NA/zero/fractional days, integer types, empty footer).\n")
})
