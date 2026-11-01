# Focused regression tests for actual KPI server reactives; no database access.
# Run from the root: Rscript --vanilla -e "source('demo/test_kpi.R', encoding='UTF-8')"
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib"), .libPaths()))
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})
source("production_calendar.R", encoding = "UTF-8")
source("report_utils.R", encoding = "UTF-8")

find_assignment <- function(expr, name) {
  if (missing(expr)) return(NULL)
  if (!is.call(expr) && !is.expression(expr)) return(NULL)
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(paste(deparse(expr[[2]]), collapse = ""), name)) return(expr)
  for (part in as.list(expr)) {
    found <- find_assignment(part, name)
    if (!is.null(found)) return(found)
  }
  NULL
}
server_source <- file("server.R", open = "rt", encoding = "UTF-8")
server_code <- parse(server_source)
close(server_source)
test_env <- new.env(parent = globalenv())
test_env$input <- reactiveValues(logistJobForLogisticProductivity = NULL,
                                logistGroupForLogisticProductivity = NULL,
                                clientForLogisticProductivity = NULL)
fixture <- data.frame(
  Дата = as.Date(c("2026-11-02", "2026-11-03", "2026-11-01")),
  Логист = c("Демо Логист", "Демо Логист", "Демо Выходной"),
  Группа = "Демо группа", Должность = "логист", Клиент = "Демо клиент",
  Рейсы = c(2, 3, 1), Оборот = c(100, 200, 0), Маржа = c(20, 30, 0),
  Договоры = c(1, 1, 1), stringsAsFactors = FALSE
)
test_env$logisticKPI <- reactiveValues(data = fixture)
test_env$DEMO_MODE <- TRUE
test_env$read.xlsx <- function(...) stop("Legacy Excel was accessed in demo mode")
for (name in c("selected_LogisticKPI_outcome", "selected_LogisticKPI_outcome1",
               "getLogisticPlanFact", "getLogisticPlanFact_total")) {
  assignment <- find_assignment(server_code, name)
  stopifnot(!is.null(assignment))
  eval(assignment, envir = test_env)
}

for (name in c("selected_LogisticKPI_outcome", "selected_LogisticKPI_outcome1")) {
  result <- isolate(test_env[[name]]())
  stopifnot(nrow(result) == 2L, sum(result$Рейсы) == 6,
            result$Рабочих_дней[result$Логист == "Демо Логист"] == 2,
            result$Выработка[result$Логист == "Демо Логист"] == 2.5,
            is.na(result$Выработка[result$Логист == "Демо Выходной"]))
  for (values in result[vapply(result, is.numeric, logical(1))]) {
    stopifnot(!any(is.nan(values)), !any(is.infinite(values)))
  }
}

bigint_fixture <- fixture
bigint_fixture$Рейсы <- bit64::as.integer64(bigint_fixture$Рейсы)
bigint_fixture$Договоры <- bit64::as.integer64(bigint_fixture$Договоры)
test_env$logisticKPI$data <- bigint_fixture
for (name in c("selected_LogisticKPI_outcome", "selected_LogisticKPI_outcome1")) {
  result <- isolate(test_env[[name]]())
  stopifnot(result$Выработка[result$Логист == "Демо Логист"] == 2.5,
            is.na(result$Выработка[result$Логист == "Демо Выходной"]))
}

missing_contract_fixture <- fixture
missing_contract_fixture$Договоры <- NA_integer_
test_env$logisticKPI$data <- missing_contract_fixture
for (name in c("selected_LogisticKPI_outcome", "selected_LogisticKPI_outcome1")) {
  result <- isolate(test_env[[name]]())
  stopifnot(all(result$Договоры == 0))
}

test_env$input$clientForLogisticProductivity <- "No matching client"
for (name in c("selected_LogisticKPI_outcome", "selected_LogisticKPI_outcome1")) {
  result <- isolate(test_env[[name]]())
  stopifnot(nrow(result) == 0L, all(c("Рейсы", "Рабочих_дней", "Выработка") %in% names(result)))
}

for (name in c("getLogisticPlanFact", "getLogisticPlanFact_total")) {
  err <- tryCatch(isolate(test_env[[name]]()), error = identity)
  stopifnot(inherits(err, "shiny.silent.error"))
}

# Exercise the actual JavaScript sent to reactable, including all-missing groups.
test_env$JS <- htmlwidgets::JS
for (name in c("kpi_productivity_mean", "kpi_productivity_cell")) {
  eval(find_assignment(server_code, name), envir = test_env)
}
js <- V8::v8()
js$eval(paste0("var average = (", as.character(test_env$kpi_productivity_mean), ");"))
js$eval(paste0("var cell = (", as.character(test_env$kpi_productivity_cell), ");"))
stopifnot(identical(js$eval("average([null, null]) === null"), "true"),
          identical(js$eval("average([]) === null"), "true"),
          identical(js$eval("average([2, 4, null]) === 3"), "true"),
          identical(enc2utf8(js$eval("cell({value: null})")), enc2utf8("–")),
          identical(enc2utf8(js$eval("cell({value: NaN})")), enc2utf8("–")),
          !grepl("NaN|не число", js$eval("cell({value: 2.5})")))

# The full package pipeline must keep aggregate values numeric until custom JS.
find_column <- function(expr, column) {
  if (missing(expr) || !is.call(expr)) return(NULL)
  if (identical(expr[[1]], as.name("list")) && column %in% names(expr)) return(expr[[column]])
  for (part in as.list(expr)) {
    found <- find_column(part, column)
    if (!is.null(found)) return(found)
  }
  NULL
}
test_env$colDef <- reactable::colDef
test_env$colFormat <- reactable::colFormat
test_env$def_rating_color <- function(value) "#ffffff"
test_env$logisticKPI$data <- fixture
test_env$input$clientForLogisticProductivity <- NULL
render_data <- data.frame(Группа = c("positive", "zero", "missing"),
                          Рейсы = c(5, 0, 1), Рабочих_дней = c(2, 1, 0),
                          Выработка = c(2.5, 0, NA_real_))
for (output_name in c("output$tableLogisticProductivity", "output$tableLogisticProductivity1")) {
  column <- eval(find_column(find_assignment(server_code, output_name), "Выработка"), test_env)
  widget <- isolate(reactable::reactable(render_data, groupBy = "Группа",
                                         columns = list(Выработка = column), static = TRUE))
  markup <- htmltools::renderTags(htmltools::as.tags(widget))$html
  document <- xml2::read_html(markup)
  group_rows <- xml2::xml_find_all(document, "//div[contains(@class,'rt-tbody')]/div[@class='rt-tr-group']/div[@role='row']")
  row_cells <- lapply(group_rows, function(row) xml2::xml_text(xml2::xml_find_all(row, "./div[@role='cell']")))
  positive <- tail(row_cells[[which(vapply(row_cells, function(x) grepl("positive", x[1]), logical(1)))]], 1)
  zero <- tail(row_cells[[which(vapply(row_cells, function(x) grepl("zero", x[1]), logical(1)))]], 1)
  absent <- tail(row_cells[[which(vapply(row_cells, function(x) grepl("missing", x[1]), logical(1)))]], 1)
  stopifnot(grepl("^2[,.]50?$", positive), grepl("^0([,.]00)?$", zero),
            identical(enc2utf8(absent), enc2utf8("–")))
}
cat("KPI regression checks passed: current dates, zero-day ratios, bigint counters, absent contracts, empty filters, group rendering, no legacy Excel in demo.\n")
