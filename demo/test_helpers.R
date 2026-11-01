# Read-only smoke test of the demo data loaders.
# Run: Rscript --vanilla demo/test_helpers.R [--days=60]
# Windows: .\demo\test_helpers.ps1
# This checks query execution/result shapes, not report business correctness or UI.

main <- function() {
  launch_args <- commandArgs(trailingOnly = FALSE)
  if (!"--vanilla" %in% launch_args) stop("Run with Rscript --vanilla.")
  script_arg <- grep("^--file=", launch_args, value = TRUE)
  if (length(script_arg) != 1L) stop("Run this file with Rscript.")
  script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
  setwd(dirname(dirname(script_path)))

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L || (length(args) && !grepl("^--days=[0-9]+$", args))) {
    stop("Usage: Rscript --vanilla demo/test_helpers.R [--days=60]")
  }
  days <- if (length(args)) as.integer(sub("^--days=", "", args)) else 60L
  if (is.na(days) || days < 1L || days > 3650L) stop("days must be 1..3650.")

  if (.Platform$OS.type == "windows") {
    if (Sys.setlocale("LC_CTYPE", "Russian_Russia.1251") == "") {
      stop("A Cyrillic locale is required by the Windows R 4.0 demo.")
    }
  }
  Sys.setenv(ERP_REPORTS_DEMO = "true", TZ = "Europe/Moscow", LANGUAGE = "en",
             R_CONFIG_ACTIVE = "default", REPORTS_ELASTIC_URL = "http://elastic-stub",
             PGCLIENTENCODING = "UTF8", PGCONNECT_TIMEOUT = "5",
             PGOPTIONS = "-c default_transaction_read_only=on -c statement_timeout=60000")
  # Override all inherited production connection settings before reading config.yml.
  for (service in c("ERP", "LOADBOARD", "PRIVATE_AUTH", "PROFILES")) {
    values <- as.list(c("127.0.0.1", "5433", "postgres", "", "erp_demo"))
    names(values) <- sprintf("REPORTS_%s_PG_%s", service,
                             c("HOST", "PORT", "USER", "PASSWORD", "DB"))
    do.call(Sys.setenv, values)
  }
  demo_library <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(demo_library)) {
    demo_library <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib")
  }
  if (dir.exists(demo_library)) .libPaths(c(demo_library, .libPaths()))
  options(encoding = "UTF-8", dplyr.summarise.inform = FALSE)
  suppressPackageStartupMessages({
    library(DBI)
    library(RPostgres)
    library(tidyverse)
    library(lubridate)
    library(jsonlite)
    library(config)
  })

  settings <- config::get(file = "config.yml")
  for (key in c("db_erp", "db_profiles", "db_tenders", "db_auth")) {
    entry <- settings[[key]]
    if (!identical(entry$host, "127.0.0.1") ||
        !identical(as.character(entry$port), "5433") ||
        !identical(entry$dbname, "erp_demo")) {
      stop("Refusing to test a connection outside the local demo database.")
    }
  }
  # Connect explicitly first, so an unavailable database fails before sourcing helpers.
  probe <- DBI::dbConnect(RPostgres::Postgres(), host = "127.0.0.1", port = 5433,
                          user = "postgres", password = "", dbname = "erp_demo")
  on.exit(if (DBI::dbIsValid(probe)) DBI::dbDisconnect(probe), add = TRUE)
  identity <- DBI::dbGetQuery(probe, paste(
    "SELECT current_database() AS db, host(inet_server_addr()) AS host,",
    "inet_server_port() AS port, current_setting('transaction_read_only') AS ro"))
  if (identity$db != "erp_demo" || identity$host != "127.0.0.1" ||
      identity$port != 5433L || identity$ro != "on") {
    stop("The database identity/read-only check failed.")
  }
  freshness <- DBI::dbGetQuery(probe, paste(
    "SELECT count(*)::integer AS requests, min(created_at)::date AS first_date,",
    "max(created_at)::date AS last_date,",
    "count(*) FILTER (WHERE created_at >= CURRENT_DATE - 30",
    "AND created_at < CURRENT_DATE + INTERVAL '1 day')::integer AS recent_requests",
    "FROM erp_requests.requests"))
  cat("Target: local erp_demo at 127.0.0.1:5433 (read-only)\n")
  cat(sprintf("Requests: %d; dates: %s..%s; created in last 30 days: %d\n",
              freshness$requests, freshness$first_date, freshness$last_date,
              freshness$recent_requests))
  if (!freshness$requests) stop("The demo database has no requests.")

  runtime <- new.env(parent = globalenv())
  runtime$DEMO_MODE <- TRUE
  on.exit({
    for (name in c("con", "con1", "con2", "con3")) {
      if (exists(name, envir = runtime, inherits = FALSE)) {
        connection <- base::get(name, envir = runtime)
        if (DBI::dbIsValid(connection)) DBI::dbDisconnect(connection)
      }
    }
  }, add = TRUE)
  source("demo/elastic_stub.R", local = runtime, encoding = "UTF-8")
  source("helpers.R", local = runtime, encoding = "UTF-8")
  source("production_calendar.R", local = runtime, encoding = "UTF-8")

  start_date <- as.POSIXct(paste(Sys.Date() - days, "00:00:00"), tz = "Europe/Moscow")
  end_date <- as.POSIXct(paste(Sys.Date(), "23:59:59"), tz = "Europe/Moscow")
  # Match the app's SQL timestamp formatting without changing the instants.
  attr(start_date, "tzone") <- "UTC"
  attr(end_date, "tzone") <- "UTC"
  epoch_loaders <- c("getSB", "get_SB_drivers_status")
  all_names <- ls(envir = runtime)
  loaders <- sort(all_names[grepl("^(get|dz_|accounts|carried)", all_names) &
                            vapply(all_names, function(name) {
                              is.function(base::get(name, envir = runtime))
                            }, logical(1))])
  if (!length(loaders)) stop("No loaders found.")
  cat(sprintf("Range: %s..%s (Europe/Moscow); loaders: %d\n",
              Sys.Date() - days, Sys.Date(), length(loaders)))

  frames_in <- function(value) {
    if (is.data.frame(value)) return(list(value))
    if (!is.list(value)) return(list())
    unlist(lapply(value, frames_in), recursive = FALSE)
  }
  failures <- empty <- nonfinite <- character()
  warning_details <- character()
  for (name in loaders) {
    loader <- base::get(name, envir = runtime)
    parameters <- names(formals(loader))
    call_args <- list()
    if ("startDate" %in% parameters) call_args$startDate <- start_date
    if ("endDate" %in% parameters) call_args$endDate <- end_date
    if (name %in% epoch_loaders) {
      call_args$startDate <- as.numeric(start_date) * 1000
      call_args$endDate <- as.numeric(end_date) * 1000
    }
    if ("date_type" %in% parameters) call_args$date_type <- "requests.created_at"
    started <- proc.time()[["elapsed"]]
    warnings <- character()
    result <- tryCatch({
      # Some loaders print messages; keep the output to shapes rather than row data.
      invisible(capture.output(value <- withCallingHandlers(
        do.call(loader, call_args),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(m) invokeRestart("muffleMessage")
      )))
      if (any(grepl("Closing open result set", warnings, fixed = TRUE))) {
        stop("A loader left a database result open; the next query cancelled it.")
      }
      if (identical(name, "getPriceIndicative") && anyDuplicated(value)) {
        stop("Price indicative repeats identical trip rows in the demo dataset.")
      }
      frames <- frames_in(value)
      if (!length(frames)) stop("Expected a data frame or list of data frames.")
      dimensions <- vapply(frames, function(frame) {
        sprintf("%dx%d", nrow(frame), ncol(frame))
      }, character(1))
      if (all(vapply(frames, nrow, integer(1)) == 0L)) empty <- c(empty, name)
      bad_numbers <- sum(vapply(frames, function(frame) {
        sum(vapply(frame, function(column) {
          if (is.numeric(column)) sum(is.infinite(column) | is.nan(column)) else 0L
        }, numeric(1)))
      }, numeric(1)))
      if (bad_numbers) nonfinite <- c(nonfinite, name)
      sprintf("OK %s%s%s%s", paste(dimensions, collapse = ", "),
              if (name %in% empty) " EMPTY" else "",
              if (bad_numbers) sprintf(" NONFINITE=%d", bad_numbers) else "",
              if (length(warnings)) sprintf(" WARNINGS=%d", length(warnings)) else "")
    }, error = function(error) {
      failures <<- c(failures, name)
      paste("ERROR", substr(gsub("[\r\n]+", " ", conditionMessage(error)), 1L, 600L))
    })
    cat(sprintf("%-39s %6.1fs %s\n", name,
                proc.time()[["elapsed"]] - started, result))
    if (length(warnings)) {
      warning_details <- c(warning_details, sprintf("%s: %s", name,
        substr(gsub("[\r\n]+", " ", unique(warnings)), 1L, 300L)))
    }
  }
  cat(sprintf("\nSUMMARY: %d/%d OK; %d errors; %d empty; %d with nonfinite numbers\n",
              length(loaders) - length(failures), length(loaders), length(failures),
              length(empty), length(nonfinite)))
  if (length(failures)) cat("ERRORS:", paste(failures, collapse = ", "), "\n")
  if (length(empty)) cat("EMPTY:", paste(empty, collapse = ", "), "\n")
  if (length(nonfinite)) cat("NONFINITE:", paste(nonfinite, collapse = ", "), "\n")
  if (length(warning_details)) {
    cat("WARNINGS (unique per loader, truncated to 300 characters):\n")
    cat(paste0("  ", warning_details, collapse = "\n"), "\n")
  }
  if (length(empty) || length(nonfinite)) {
    cat("Empty/nonfinite results need report-level review; they are not query exceptions.\n")
  }
  if (length(failures)) 1L else 0L
}

status <- tryCatch(main(), error = function(error) {
  cat("FATAL:", conditionMessage(error), "\n")
  1L
})
quit(save = "no", status = status)
