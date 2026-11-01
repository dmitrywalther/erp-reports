# Public container entry point; entrypoint.sh has already seeded the private DB.
online_port <- function(value = Sys.getenv("PORT", "3838")) {
  if (length(value) != 1L || is.na(value) || !grepl("^[0-9]{1,5}$", value)) {
    stop("PORT must be an integer from 1024 to 65535.", call. = FALSE)
  }
  port <- as.integer(value)
  if (port < 1024L || port > 65535L) stop("PORT must be an integer from 1024 to 65535.", call. = FALSE)
  port
}

online_database_settings <- function() {
  for (service in c("ERP", "LOADBOARD", "PRIVATE_AUTH", "PROFILES")) {
    prefix <- paste0("REPORTS_", service, "_PG_")
    values <- Sys.getenv(paste0(prefix, c("HOST", "PORT", "USER", "DB")))
    if (!identical(unname(values), c("127.0.0.1", "5432", "erp_demo_reader", "erp_demo"))) {
      stop("The online app requires the private read-only demo database.", call. = FALSE)
    }
    if (!nzchar(Sys.getenv(paste0(prefix, "PASSWORD")))) {
      stop("The generated demo connection credential is missing.", call. = FALSE)
    }
  }
  list(host = "127.0.0.1", port = 5432L, user = "erp_demo_reader", dbname = "erp_demo",
       password = Sys.getenv("REPORTS_ERP_PG_PASSWORD"))
}

run_online_app <- function() {
  port <- online_port()
  settings <- online_database_settings()
  runtime_dir <- Sys.getenv("ERP_REPORTS_RUNTIME_DIR")
  if (!nzchar(runtime_dir) || !dir.exists(file.path(runtime_dir, "health"))) {
    stop("The private runtime directory is missing.", call. = FALSE)
  }
  Sys.setenv(ERP_REPORTS_DEMO = "true", ERP_REPORTS_ONLINE = "true", TZ = "Europe/Moscow",
             ERP_BASE_URL = "", REPORTS_ELASTIC_URL = "http://elastic-stub",
             PGOPTIONS = "-c default_transaction_read_only=on -c statement_timeout=30000 -c lock_timeout=5000")
  options(shiny.maxRequestSize = 1024^2, shiny.sanitize.errors = TRUE,
          shiny.autoreload = FALSE, encoding = "UTF-8")
  probe <- do.call(DBI::dbConnect, c(list(drv = RPostgres::Postgres()), settings))
  on.exit(if (DBI::dbIsValid(probe)) DBI::dbDisconnect(probe), add = TRUE)
  identity <- DBI::dbGetQuery(probe, paste(
    "SELECT current_user AS app_user, current_database() AS db,",
    "current_setting('transaction_read_only') AS read_only,",
    "(SELECT rolsuper OR rolcreatedb OR rolcreaterole OR rolreplication FROM pg_roles",
    " WHERE rolname = current_user) AS elevated_role,",
    "has_table_privilege('erp_requests.requests', 'INSERT,UPDATE,DELETE,TRUNCATE') AS can_write,",
    "has_schema_privilege('public', 'CREATE') AS can_create,",
    "has_database_privilege(current_database(), 'TEMP') AS can_create_temp,",
    "(SELECT count(*) > 0 FROM erp_requests.requests) AS seeded"))
  if (identity$app_user != "erp_demo_reader" || identity$db != "erp_demo" ||
      identity$read_only != "on" || identity$elevated_role || identity$can_write ||
      identity$can_create || identity$can_create_temp || !identity$seeded) {
    stop("The synthetic read-only database is not ready.", call. = FALSE)
  }
  DBI::dbDisconnect(probe)
  health_dir <- normalizePath(file.path(runtime_dir, "health"), mustWork = TRUE)
  writeLines("ready", file.path(health_dir, "ready"))
  on.exit(unlink(file.path(health_dir, "ready")), add = TRUE)
  shiny::addResourcePath("health", health_dir)
  gc(full = TRUE)
  shiny::runApp(".", host = "0.0.0.0", port = port, launch.browser = FALSE)
}

run_online_app()
