# Actual server initializers and report reactive, without DB or network access.
# Rscript --vanilla -e "source('demo/test_session_isolation.R', encoding='UTF-8')"
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib"), .libPaths()))
suppressPackageStartupMessages(library(shiny))
source("demo/public_support.R", encoding = "UTF-8")

local({
  con <- file("server.R", open = "rt", encoding = "UTF-8")
  code <- parse(con)
  close(con)
  server <- code[[length(code)]]
  stopifnot(is.call(server), identical(server[[1]], as.name("function")),
            identical(names(server[[2]]), c("input", "output", "session")))

  is_store_assignment <- function(expr) {
    is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.symbol(expr[[2]]) && is.call(expr[[3]]) &&
      identical(expr[[3]][[1]], as.name("reactiveValues"))
  }
  expected <- c(
    "carrier_prepayment_table", "client_health_table", "DZ_unconfirmed_receivables_table",
    "DZ_confirmed_receivables_table", "DZ_receivable_turnover_table", "health_dz_table",
    "overdue_dz_table", "unformed_bills_table", "funnel_NDZ_table", "dz_turnover_table",
    "logisticKPI", "RevenueProfitTable", "verifiedDocsTable", "billsTable", "actTable",
    "billsNewTable", "dzTable", "requestsTable", "requestsTable_specialDC", "kzTable",
    "breakdownsTable", "cancelingTableNew", "breakdownsTableNew", "newOldCarriersTable",
    "lateOrdersTable", "drivers_everyday", "carriers_everyday", "vehicles_everyday",
    "drivers_status_change", "carriers_status_change", "vehicles_status_change", "spotTable",
    "indicativeTable", "indicativeTable1", "indicativeTable2", "countRequestsTable",
    "averageTimeTable", "averageTimeTable1", "driverCountTable", "carrierCountTable",
    "cashTable", "registersForInsuranceTable", "metricTable", "metricTable1", "doubletsTable",
    "clientDirectionsTable", "requestProfitabilityTable")
  global_code <- code[-length(code)]
  stopifnot(!any(vapply(global_code, is_store_assignment, logical(1))))
  stores <- Filter(is_store_assignment, as.list(server[[3]])[-1])
  store_names <- vapply(stores, function(expr) as.character(expr[[2]]), character(1))
  stopifnot(setequal(store_names, expected), length(store_names) == length(expected))
  # Shared formatters/helpers must not close over session-owned report stores.
  stopifnot(length(intersect(all.names(global_code), expected)) == 0L)

  # Invoke the actual initializers in two ordinary server call frames. Both share
  # the same parent environment, just as sessions in one Shiny process do.
  initializer <- server
  initializer[[3]] <- as.call(c(list(as.name("{")), stores, list(quote(environment()))))
  shared <- new.env(parent = environment())
  shared$DEMO_MODE <- TRUE
  initialize_session <- eval(initializer, shared)
  sessions <- list(MockShinySession$new(), MockShinySession$new())
  on.exit(lapply(sessions, function(session) session$close()), add = TRUE)
  frames <- lapply(sessions, function(session) {
    withReactiveDomain(session, initialize_session(session$input, session$output, session))
  })
  a <- frames[[1]]
  b <- frames[[2]]
  stopifnot(identical(parent.env(a), shared), identical(parent.env(b), shared),
            !any(expected %in% ls(shared)))
  for (name in expected) {
    stopifnot(exists(name, a, inherits = FALSE), exists(name, b, inherits = FALSE),
              !identical(a[[name]], b[[name]]))
    initial_b <- isolate(reactiveValuesToList(b[[name]]))
    isolate(a[[name]]$data <- data.frame(visitor = "A", report = name))
    stopifnot(identical(isolate(reactiveValuesToList(b[[name]])), initial_b))
    isolate(b[[name]]$data <- data.frame(visitor = "B", report = name))
    stopifnot(identical(isolate(a[[name]]$data$visitor), "A"),
              identical(isolate(b[[name]]$data$visitor), "B"))
  }

  find_assignment <- function(expr, name) {
    if (missing(expr) || (!is.call(expr) && !is.expression(expr))) return(NULL)
    if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
        identical(expr[[2]], as.name(name))) return(expr)
    for (part in as.list(expr)) {
      found <- find_assignment(part, name)
      if (!is.null(found)) return(found)
    }
    NULL
  }
  selection <- find_assignment(server, "selected_registersForInsuranceTable")
  stopifnot(!is.null(selection))
  fixture <- data.frame(
    Номер_заявки = c("DEMO-A", "DEMO-B"), Клиент = "Клиент 01", Логист = "Логист 001",
    Перевозчик = "Перевозчик 001", Тип_груза = "Демо-груз",
    Дата_загрузки_план = as.Date(c("2026-11-01", "2026-11-02")),
    Дата_выгрузки_план = as.Date(c("2026-11-02", "2026-11-03")),
    Пункт_отправления = "Город 1", Пункт_назначения = "Город 2", Стоимость_груза = c(100, 200))
  for (i in seq_along(sessions)) {
    sessions[[i]]$setInputs(registersForInsurance_dateRangeInput = fixture$Дата_загрузки_план[c(i, i)])
    isolate(frames[[i]]$registersForInsuranceTable$data <- fixture)
    withReactiveDomain(sessions[[i]], eval(selection, frames[[i]]))
  }
  stopifnot(identical(isolate(a$selected_registersForInsuranceTable()$Номер_заявки), "DEMO-A"),
            identical(isolate(b$selected_registersForInsuranceTable()$Номер_заявки), "DEMO-B"))
  isolate(a$registersForInsuranceTable$data <- fixture[0, ])
  stopifnot(nrow(isolate(a$selected_registersForInsuranceTable())) == 0L,
            identical(isolate(b$selected_registersForInsuranceTable()$Номер_заявки), "DEMO-B"))
  sessions[[1]]$close()
  stopifnot(identical(isolate(b$selected_registersForInsuranceTable()$Номер_заявки), "DEMO-B"))
})
cat("Session isolation PASS: 47 local report stores, independent mutations, actual report/date filters and session close.\n")
