# Run from the root: Rscript --vanilla -e "source('demo/test_public_support.R', encoding='UTF-8')"
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib"), .libPaths()))
source("demo/public_support.R", encoding = "UTF-8")
suppressPackageStartupMessages(library(dplyr))

local({
  scratch <- tempfile("erp-public-test-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE))
  user_file <- file.path(scratch, "users.csv")
  stopifnot(inherits(tryCatch(load_report_users(FALSE, user_file), error = identity), "error"))
  demo <- load_report_users(TRUE, user_file)
  stopifnot(identical(demo$user, "demo"), identical(demo$password, "demo"))
  fixture_users <- data.frame(user = "fixture-user", password = "demo",
                              permissions = "data_analyst", name = "Fixture analyst")
  write.csv(fixture_users, user_file, row.names = FALSE)
  stopifnot(identical(load_report_users(FALSE, user_file)$user, "fixture-user"),
            identical(load_report_users(TRUE, user_file)$user, "demo"))
  write.csv(fixture_users[c(1, 1), ], user_file, row.names = FALSE)
  stopifnot(inherits(tryCatch(load_report_users(FALSE, user_file), error = identity), "error"))
  write.csv(fixture_users[0, ], user_file, row.names = FALSE)
  stopifnot(inherits(tryCatch(load_report_users(FALSE, user_file), error = identity), "error"))
  fixture_users$password <- ""
  write.csv(fixture_users, user_file, row.names = FALSE)
  stopifnot(inherits(tryCatch(load_report_users(FALSE, user_file), error = identity), "error"))
  stopifnot(identical(report_erp_link("Request 1", "/#/requests/demo", ""), "Request 1"))

  fixture <- data.frame(Номер_заявки = c("DEMO-1", "DEMO-2"), Клиент = c("Клиент 01", "Клиент 02"),
                        Логист = "Логист 001", Перевозчик = "Перевозчик 001", Тип_груза = "Демо-груз",
                        Дата_загрузки_план = as.Date(c("2026-11-01", "2026-11-10")),
                        Дата_выгрузки_план = as.Date(c("2026-11-02", "2026-11-11")),
                        Пункт_отправления = "Город 1", Пункт_назначения = "Город 2", Стоимость_груза = c(100, 200))
  data <- demo_insurance_rows(fixture, as.Date("2026-11-01"), as.Date("2026-11-02"))
  stopifnot(nrow(data) == 1L, identical(data$Номер_заявки, "DEMO-1"))
  for (variant in c("Стандартный", "Сборный", "Упрощённый")) {
    wb <- create_demo_insurance_workbook(data, "2026-11-01", "2026-11-02", variant)
    path <- file.path(scratch, paste0(match(variant, c("Стандартный", "Сборный", "Упрощённый")), ".xlsx"))
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    restored <- openxlsx::read.xlsx(path, startRow = 6, detectDates = TRUE)
    stopifnot(nrow(restored) == 1L, identical(restored$Номер_заявки, "DEMO-1"),
              restored$Стоимость_груза == 100)
    entries <- unzip(path, list = TRUE)$Name
    stopifnot("docProps/core.xml" %in% entries)
    unzip(path, files = "docProps/core.xml", exdir = scratch)
    metadata <- paste(readLines(file.path(scratch, "docProps/core.xml"), warn = FALSE), collapse = "")
    stopifnot(grepl("ERP Reports Demo", metadata, fixed = TRUE))
  }
  empty <- demo_insurance_rows(fixture, "2027-01-01", "2027-01-02")
  stopifnot(nrow(empty) == 0L, identical(names(empty), names(data)))
  wb <- create_demo_insurance_workbook(empty, "2027-01-01", "2027-01-02")
  empty_path <- file.path(scratch, "empty.xlsx")
  openxlsx::saveWorkbook(wb, empty_path, overwrite = TRUE)
  stopifnot(file.exists(empty_path), file.info(empty_path)$size > 0)

  # Exercise the actual server reactive and download callback, without a DB or templates.
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
  server_code <- parse(con)
  close(con)
  test_env <- new.env(parent = environment())
  test_env$DEMO_MODE <- TRUE
  test_env$reactive <- shiny::reactive
  test_env$req <- shiny::req
  test_env$input <- shiny::reactiveValues(
    registersForInsurance_dateRangeInput = as.Date(c("2026-11-01", "2026-11-02")),
    type_registersForInsurance = "Стандартный")
  test_env$registersForInsuranceTable <- shiny::reactiveValues(data = fixture)
  test_env$getregistersForInsurance <- function(...) fixture
  test_env$saveWorkbook <- openxlsx::saveWorkbook
  test_env$loadWorkbook <- function(...) stop("Private template accessed in demo")
  eval(find_assignment(server_code, "selected_registersForInsuranceTable"), test_env)
  handler <- find_assignment(server_code, "output$downloadRegistersForInsurance")[[3]]
  content <- eval(handler[["content"]], test_env)
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)
  for (variant in c("Стандартный", "Сборный", "Упрощённый")) {
    test_env$input$type_registersForInsurance <- variant
    output_path <- file.path(scratch, "server-export.xlsx")
    shiny::withReactiveDomain(session, shiny::isolate(content(output_path)))
    restored <- openxlsx::read.xlsx(output_path, startRow = 6)
    stopifnot(nrow(restored) == 1L, identical(restored$Номер_заявки, "DEMO-1"))
  }

  # Public visitors cannot start an XLSX parser, including through hidden tabs.
  for (online in c(TRUE, FALSE)) {
    for (name in c("excel_data", "output$downloadLogisticianPlanFact", "output$downloadFactoringBills")) {
      upload_env <- new.env(parent = environment())
      upload_env$ONLINE_DEMO <- online
      upload_env$reactive <- shiny::reactive
      upload_env$req <- shiny::req
      upload_env$need <- shiny::need
      upload_env$input <- shiny::reactiveValues(
        cost_data_from_file = list(datapath = "synthetic-upload.xlsx"),
        planLogisticianInput = list(datapath = "synthetic-upload.xlsx"),
        factoringInput = list(datapath = "synthetic-upload.xlsx"))
      upload_env$read.xlsx <- function(...) {
        stop(structure(list(message = "XLSX parser reached", call = NULL),
                       class = c("xlsx_parser_reached", "error", "condition")))
      }
      assignment <- find_assignment(server_code, name)
      stopifnot(!is.null(assignment))
      if (name == "excel_data") {
        eval(assignment, upload_env)
        handler <- upload_env$excel_data
      } else {
        handler <- eval(assignment[[3]][["content"]], upload_env)
      }
      result <- tryCatch(shiny::isolate(handler()), error = identity)
      stopifnot(inherits(result, if (online) "shiny.silent.error" else "xlsx_parser_reached"))
    }
  }

  con <- file("global.R", open = "rt", encoding = "UTF-8")
  flag <- find_assignment(parse(con), "ONLINE_DEMO")
  close(con)
  stopifnot(!is.null(flag))
  previous_flag <- Sys.getenv("ERP_REPORTS_ONLINE", unset = NA_character_)
  on.exit(if (is.na(previous_flag)) Sys.unsetenv("ERP_REPORTS_ONLINE") else
            Sys.setenv(ERP_REPORTS_ONLINE = previous_flag), add = TRUE)
  for (demo in c(TRUE, FALSE)) for (online in c("true", "1", "yes", "false", "")) {
    Sys.setenv(ERP_REPORTS_ONLINE = online)
    flag_env <- list2env(list(DEMO_MODE = demo))
    eval(flag, flag_env)
    stopifnot(identical(flag_env$ONLINE_DEMO, demo && online %in% c("true", "1", "yes")))
  }
  find_online_ui <- function(expr) {
    if (missing(expr) || (!is.call(expr) && !is.expression(expr))) return(NULL)
    if (is.call(expr) && identical(expr[[1]], as.name("if")) &&
        identical(expr[[2]], as.name("ONLINE_DEMO"))) return(expr)
    for (part in as.list(expr)) {
      found <- find_online_ui(part)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  for (path in c("ui/client_health_ui.R", "ui/logistician_plan_fact_ui.R", "ui/factoring_ui.R")) {
    con <- file(path, open = "rt", encoding = "UTF-8")
    conditional <- find_online_ui(parse(con))
    close(con)
    stopifnot(!is.null(conditional))
    for (online in c(TRUE, FALSE)) {
      ui_env <- list2env(list(ONLINE_DEMO = online, helpText = shiny::helpText, fileInput = shiny::fileInput))
      html <- as.character(eval(conditional, ui_env))
      stopifnot(identical(grepl('type="file"', html, fixed = TRUE), !online))
      if (online) stopifnot(grepl("Загрузка Excel доступна при локальном запуске демо.", html, fixed = TRUE))
    }
  }

  active_assignment <- find_assignment(server_code, "actualRequests")
  stopifnot(!is.null(active_assignment))
  for (online in c(TRUE, FALSE)) {
    active_env <- new.env(parent = environment())
    active_env$ONLINE_DEMO <- online
    active_env$reactive <- shiny::reactive
    active_env$session <- new.env()
    active_env$query_count <- 0L
    active_env$poll_count <- 0L
    active_env$getActualRequests <- function() {
      active_env$query_count <- active_env$query_count + 1L
      data.frame(request = "DEMO-1")
    }
    active_env$reactivePoll <- function(intervalMillis, session, checkFunc, valueFunc) {
      stopifnot(!online, intervalMillis == 30000)
      active_env$poll_count <- active_env$poll_count + 1L
      shiny::reactive(valueFunc())
    }
    eval(active_assignment, active_env)
    stopifnot(identical(shiny::isolate(active_env$actualRequests()$request), "DEMO-1"),
              identical(shiny::isolate(active_env$actualRequests()$request), "DEMO-1"),
              active_env$query_count == 1L, active_env$poll_count == if (online) 0L else 1L)
  }
})
cat("Public support: PASS (fail-closed auth, demo isolation, plain ERP labels, actual template-free insurance downloads, empty export, neutral metadata, online XLSX uploads blocked, online active requests loaded once).\n")
