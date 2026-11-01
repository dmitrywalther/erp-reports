# No database: exercise the actual Spot reactives and browser aggregation code.
# Rscript --vanilla -e "source('demo/test_spot.R', encoding='UTF-8')"
.libPaths(c(file.path(Sys.getenv("LOCALAPPDATA"), "R", "erp-reports-lib"), .libPaths()))
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
source("report_utils.R", encoding = "UTF-8")
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
server_source <- file("server.R", open = "rt", encoding = "UTF-8")
server_code <- parse(server_source)
close(server_source)
test_env <- new.env(parent = globalenv())
test_env$input <- reactiveValues(aggregationSpot = "День", dateSpot = as.Date(c("2026-11-01", "2026-11-01")))
test_env$spotTable <- reactiveValues(data = data.frame(
  Дата = as.Date(rep("2026-11-01", 3)), Клиент = c("zero", "normal", "empty"),
  Заявки.x = c(0, 4, 0), Заявки.y = c(1, 2, 0)
))
for (name in c("selected_spotTable", "selected_spotTable_general_plot")) {
  eval(find_assignment(server_code, name), test_env)
}
result <- isolate(test_env$selected_spotTable())
stopifnot(is.na(result$Отношение[result$Клиент == "zero"]),
          is.na(result$Отношение[result$Клиент == "empty"]),
          result$Отношение[result$Клиент == "normal"] == 0.5,
          !any(is.nan(result$Отношение)), !any(is.infinite(result$Отношение)),
          isolate(test_env$selected_spotTable_general_plot())$Отношение == 0.75)
bigint_data <- isolate(test_env$spotTable$data)
bigint_data$Заявки.x <- bit64::as.integer64(bigint_data$Заявки.x)
bigint_data$Заявки.y <- bit64::as.integer64(bigint_data$Заявки.y)
test_env$spotTable$data <- bigint_data
result <- isolate(test_env$selected_spotTable())
stopifnot(is.na(result$Отношение[result$Клиент == "zero"]),
          result$Отношение[result$Клиент == "normal"] == 0.5)
test_env$input$dateSpot <- as.Date(c("2026-12-01", "2026-12-01"))
stopifnot(nrow(isolate(test_env$selected_spotTable())) == 0,
          nrow(isolate(test_env$selected_spotTable_general_plot())) == 0)

test_env$JS <- htmlwidgets::JS
for (name in c("spot_ratio_aggregate", "spot_ratio_cell")) {
  assignment <- find_assignment(server_code, name)
  stopifnot(!is.null(assignment))
  eval(assignment, test_env)
}
js <- V8::v8()
js$eval(paste0("var aggregate = (", as.character(test_env$spot_ratio_aggregate), ");"))
js$eval(paste0("var cell = (", as.character(test_env$spot_ratio_cell), ");"))
stopifnot(identical(js$eval("aggregate([], [{'Заявки.x': 0, 'Заявки.y': 1}]) === null"), "true"),
          identical(js$eval("aggregate([], []) === null"), "true"),
          identical(js$eval("aggregate([], [{'Заявки.x': 4, 'Заявки.y': 2}]) === 0.5"), "true"),
          identical(enc2utf8(js$eval("cell({value: null})")), enc2utf8("–")),
          identical(enc2utf8(js$eval("cell({value: Infinity})")), enc2utf8("–")),
          grepl("50", js$eval("cell({value: 0.5})")),
          grepl("200", js$eval("cell({value: 2})")))

# Exercise reactable's complete formatting pipeline, not only a raw callback.
# colFormat runs before custom JS renderers in the installed reactable version.
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
ratio_column <- eval(find_column(find_assignment(server_code, "output$spotTable"), "Отношение"), test_env)
render_data <- data.frame(Клиент = c("zero", "half", "missing"),
                          Заявки.x = c(1, 4, 0), Заявки.y = c(0, 2, 1),
                          Отношение = c(0, 0.5, NA_real_))
widget <- reactable::reactable(render_data, groupBy = "Клиент",
                               columns = list(Отношение = ratio_column), static = TRUE)
markup <- htmltools::renderTags(htmltools::as.tags(widget))$html
document <- xml2::read_html(markup)
group_rows <- xml2::xml_find_all(document, "//div[contains(@class,'rt-tbody')]/div[@class='rt-tr-group']/div[@role='row']")
row_cells <- lapply(group_rows, function(row) xml2::xml_text(xml2::xml_find_all(row, "./div[@role='cell']")))
zero_cell <- tail(row_cells[[which(vapply(row_cells, function(x) grepl("zero", x[1]), logical(1)))]], 1)
half_cell <- tail(row_cells[[which(vapply(row_cells, function(x) grepl("half", x[1]), logical(1)))]], 1)
missing_cell <- tail(row_cells[[which(vapply(row_cells, function(x) grepl("missing", x[1]), logical(1)))]], 1)
stopifnot(grepl("^0([,.]00)?[[:space:]]*%$", zero_cell),
          grepl("^50([,.]00)?[[:space:]]*%$", half_cell),
          identical(enc2utf8(missing_cell), enc2utf8("–")))
cat("Spot regression checks passed: zero denominators, bigint, empty period, valid ratios, group rendering.\n")
