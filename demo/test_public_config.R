# Offline checks for public-demo defaults and fail-closed configuration.
# Only selected definitions are evaluated; no app or database is started.
read_r_code <- function(path) {
  connection <- file(path, open = "rt", encoding = "UTF-8")
  on.exit(close(connection))
  parse(connection)
}
named_assignment <- function(code, name) {
  matches <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name(name))
  }, as.list(code))
  stopifnot(length(matches) == 1L)
  matches[[1]]
}
expect_config_error <- function(expression) {
  failed <- tryCatch({ force(expression); FALSE }, error = function(error) TRUE)
  stopifnot(failed)
}

config_env <- new.env(parent = baseenv())
eval(named_assignment(read_r_code("helpers.R"), "validate_report_config"), config_env)
connection <- list(host = "127.0.0.1", port = "5433", user = "postgres", dbname = "erp_demo")
settings <- setNames(rep(list(connection), 4), c("db_erp", "db_profiles", "db_tenders", "db_auth"))
settings$db_elastic <- list(url = "http://elastic-stub")
stopifnot(isTRUE(config_env$validate_report_config(settings, TRUE)))
stopifnot(isTRUE(config_env$validate_report_config(settings, FALSE)))
expect_config_error(config_env$validate_report_config(list(), FALSE))
broken <- settings
broken$db_auth$user <- ""
expect_config_error(config_env$validate_report_config(broken, FALSE))
broken <- settings
broken$db_elastic$url <- ""
expect_config_error(config_env$validate_report_config(broken, FALSE))
broken <- settings
broken$db_erp$port <- "0"
expect_config_error(config_env$validate_report_config(broken, TRUE))

global_code <- read_r_code("global.R")
config_env$DEMO_MODE <- TRUE
# A demo must not inherit a user's private ERP link or staff list from the shell.
config_env$Sys.getenv <- function(...) stop("Demo read a production environment setting.")
eval(named_assignment(global_code, "ERP_BASE_URL"), config_env)
eval(named_assignment(global_code, "SPOT_TEAM_CREATORS"), config_env)
stopifnot(identical(config_env$ERP_BASE_URL, ""),
          identical(config_env$SPOT_TEAM_CREATORS, sprintf("Логист %03d", 1:7)))

seed_code <- read_r_code("demo/seed_demo.R")
eval(named_assignment(seed_code, "make_names"), config_env)
eval(named_assignment(seed_code, "client_names"), config_env)
stopifnot(identical(config_env$make_names(3, "Логист"), sprintf("Логист %03d", 1:3)),
          identical(config_env$client_names, sprintf("Клиент %02d", 1:18)))

# The legacy notification script is deliberately absent from public exports.
if (file.exists("postman.R")) {
  postman_code <- read_r_code("postman.R")
  definitions_only <- vapply(as.list(postman_code), function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.call(expression[[3]]) && identical(expression[[3]][[1]], as.name("function"))
  }, logical(1))
  stopifnot(all(definitions_only))
  notify_env <- new.env(parent = baseenv())
  eval(named_assignment(postman_code, "notification_settings"), notify_env)
  notify_env$Sys.getenv <- function(name, unset = "") {
    values <- c(ERP_REPORTS_DEMO = "true", ERP_REPORTS_SEND_NOTIFICATIONS = "true")
    if (name %in% names(values)) unname(values[[name]]) else unset
  }
  expect_config_error(notify_env$notification_settings("MAIL_FROM"))
  notify_env$Sys.getenv <- function(name, unset = "") unset
  expect_config_error(notify_env$notification_settings("MAIL_FROM"))
}

for (path in c("global.R", "helpers.R", "demo/seed_demo.R", "demo/elastic_stub.R")) {
  text <- readLines(path, encoding = "UTF-8", warn = FALSE)
  # Generated UUIDs are allowed; fixed person/account identifiers are not.
  stopifnot(!any(grepl("[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}", text)))
}
cat("Public configuration: PASS (explicit connections, isolated demo defaults, synthetic labels, inert notifications)\n")
