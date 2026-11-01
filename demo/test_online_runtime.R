# Offline runtime-contract tests. Does not start processes or connect to a DB.
online_source <- file("demo/online/run_app.R", open = "rt", encoding = "UTF-8")
online_code <- parse(online_source)
close(online_source)
test_env <- new.env(parent = baseenv())
for (expression in as.list(online_code)) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-"))) eval(expression, test_env)
}
expect_failure <- function(expression) {
  stopifnot(inherits(tryCatch({ force(expression); NULL }, error = identity), "error"))
}
stopifnot(test_env$online_port("3838") == 3838L, test_env$online_port("10000") == 10000L)
for (value in list("0", "80", "65536", "3.5", "1;2", "", NA_character_, c("3838", "3839"))) {
  expect_failure(test_env$online_port(value))
}
fixture <- character()
for (service in c("ERP", "LOADBOARD", "PRIVATE_AUTH", "PROFILES")) {
  prefix <- paste0("REPORTS_", service, "_PG_")
  fixture[paste0(prefix, c("HOST", "PORT", "USER", "DB", "PASSWORD"))] <-
    c("127.0.0.1", "5432", "erp_demo_reader", "erp_demo", paste0("generated", "-fixture"))
}
test_env$Sys.getenv <- function(x, unset = "") {
  values <- fixture[x]
  values[is.na(values)] <- unset
  if (length(x) == 1L) unname(values) else values
}
settings <- test_env$online_database_settings()
stopifnot(settings$host == "127.0.0.1", settings$port == 5432L,
          settings$user == "erp_demo_reader", settings$dbname == "erp_demo")
fixture["REPORTS_PROFILES_PG_HOST"] <- "external.invalid"
expect_failure(test_env$online_database_settings())
fixture["REPORTS_PROFILES_PG_HOST"] <- "127.0.0.1"
fixture["REPORTS_ERP_PG_USER"] <- "postgres"
expect_failure(test_env$online_database_settings())
fixture["REPORTS_ERP_PG_USER"] <- "erp_demo_reader"
fixture["REPORTS_ERP_PG_PASSWORD"] <- ""
expect_failure(test_env$online_database_settings())

bootstrap <- paste(readLines("demo/online/entrypoint.sh", warn = FALSE), collapse = "\n")
stopifnot(grepl("mktemp -d /tmp/erp-reports-online.", bootstrap, fixed = TRUE),
          grepl("listen_addresses=127.0.0.1 -c port=5432", bootstrap, fixed = TRUE),
          grepl("REVOKE CREATE, TEMPORARY ON DATABASE erp_demo FROM PUBLIC", bootstrap, fixed = TRUE),
          grepl("GRANT SELECT ON ALL TABLES IN SCHEMA", bootstrap, fixed = TRUE),
          grepl("default_transaction_read_only = on", bootstrap, fixed = TRUE),
          grepl("ALTER ROLE erp_bootstrap NOLOGIN", bootstrap, fixed = TRUE),
          grepl("local erp_demo erp_demo_reader scram-sha-256", bootstrap, fixed = TRUE),
          grepl("PGHOST PGHOSTADDR", bootstrap, fixed = TRUE),
          grepl("R_CONFIG_ACTIVE R_CONFIG_FILE", bootstrap, fixed = TRUE),
          grepl('realpath -e -- "$runtime_dir"', bootstrap, fixed = TRUE),
          grepl('wait -n "$pg_pid" "$app_pid"', bootstrap, fixed = TRUE),
          !grepl("5433", bootstrap, fixed = TRUE))
loader_source <- file("demo/test_helpers.R", open = "rt", encoding = "UTF-8")
loader_code <- parse(loader_source)
close(loader_source)
target_definition <- Filter(function(expression) {
  is.call(expression) && identical(expression[[1]], as.name("<-")) &&
    identical(expression[[2]], as.name("demo_loader_target"))
}, as.list(loader_code))
stopifnot(length(target_definition) == 1L)
target_env <- new.env(parent = baseenv())
eval(target_definition[[1]], target_env)
loader_fixture <- c(ERP_REPORTS_DEMO = "true", ERP_REPORTS_ONLINE = "true",
                    ERP_REPORTS_RUNTIME_DIR = "/tmp/erp-reports-online.fixture1")
for (service in c("ERP", "LOADBOARD", "PRIVATE_AUTH", "PROFILES")) {
  prefix <- paste0("REPORTS_", service, "_PG_")
  loader_fixture[paste0(prefix, c("HOST", "PORT", "USER", "DB", "PASSWORD"))] <-
    c("127.0.0.1", "5432", "erp_demo_reader", "erp_demo", paste(rep("a", 64L), collapse = ""))
}
fixture_getenv <- function(x, unset = "") {
  values <- loader_fixture[x]
  values[is.na(values)] <- unset
  if (length(x) == 1L) unname(values) else values
}
target_args <- list(online = TRUE, getenv = fixture_getenv, os_type = "unix",
                    app_root = "/app", effective_user = "demo", exists = function(path) TRUE)
read_target <- function(args = target_args) do.call(target_env$demo_loader_target, args)
target <- read_target()
stopifnot(target$host == "127.0.0.1", target$port == 5432L,
          target$user == "erp_demo_reader", target$dbname == "erp_demo", target$online)
for (change in list(list(os_type = "windows"), list(app_root = "/other"),
                    list(effective_user = "root"), list(effective_user = ""),
                    list(exists = function(path) FALSE))) {
  expect_failure(read_target(modifyList(target_args, change)))
}
original_fixture <- loader_fixture
for (change in list(c(ERP_REPORTS_ONLINE = "false"), c(ERP_REPORTS_DEMO = "false"),
                    c(ERP_REPORTS_RUNTIME_DIR = "/tmp/other"),
                    c(REPORTS_ERP_PG_HOST = "external.invalid"),
                    c(REPORTS_LOADBOARD_PG_PORT = "5433"),
                    c(REPORTS_PROFILES_PG_USER = "postgres"),
                    c(REPORTS_PRIVATE_AUTH_PG_DB = "other_db"),
                    c(REPORTS_ERP_PG_PASSWORD = ""),
                    c(REPORTS_PROFILES_PG_PASSWORD = paste(rep("b", 64L), collapse = "")))) {
  loader_fixture <- original_fixture
  loader_fixture[names(change)] <- change
  expect_failure(read_target())
}
# The unchanged default ignores even inherited non-demo connection settings.
loader_fixture <- c(REPORTS_ERP_PG_HOST = "external.invalid", ERP_REPORTS_ONLINE = "true")
local_target <- read_target(modifyList(target_args, list(online = FALSE, effective_user = "root")))
stopifnot(local_target$host == "127.0.0.1", local_target$port == 5433L,
          local_target$user == "postgres", local_target$password == "", !local_target$online)
cat("Online runtime: PASS (ports, reader connections, bootstrap, process and loader-target guards).\n")
