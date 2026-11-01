# Helpers shared by the public demo and optional private deployments.
load_report_users <- function(demo_mode, path = "users.local.csv") {
  if (isTRUE(demo_mode)) {
    return(data.frame(user = "demo", password = "demo", permissions = "data_analyst",
                      name = "Демо-аналитик", stringsAsFactors = FALSE))
  }
  if (!file.exists(path)) {
    stop("Для обычного режима нужен users.local.csv. Для синтетического демо задайте ERP_REPORTS_DEMO=true.",
         call. = FALSE)
  }
  users <- read.csv(path, colClasses = "character", check.names = FALSE,
                    fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  required <- c("user", "password", "permissions", "name")
  if (!all(required %in% names(users)) || nrow(users) == 0L) {
    stop("users.local.csv должен содержать непустой список и колонки user, password, permissions, name.", call. = FALSE)
  }
  if (anyNA(users[required]) || any(vapply(users[required], function(x) any(!nzchar(trimws(x))), logical(1))) ||
      anyDuplicated(users$user)) {
    stop("users.local.csv содержит пустые поля или повторяющиеся логины.", call. = FALSE)
  }
  users[required]
}

report_erp_link <- function(label, path, base_url, title = NULL) {
  if (is.null(base_url) || !nzchar(base_url)) return(as.character(label))
  htmltools::tags$a(href = paste0(sub("/+$", "", base_url), path),
                    target = "_blank", rel = "noopener noreferrer", title = title, as.character(label))
}

demo_insurance_rows <- function(data, from, to) {
  columns <- c("Номер_заявки", "Клиент", "Логист", "Перевозчик", "Тип_груза",
               "Дата_загрузки_план", "Дата_выгрузки_план", "Пункт_отправления",
               "Пункт_назначения", "Стоимость_груза")
  stopifnot(is.data.frame(data), all(columns %in% names(data)))
  dates <- as.Date(data$Дата_загрузки_план)
  rows <- data[!is.na(dates) & dates >= as.Date(from) & dates <= as.Date(to), columns, drop = FALSE]
  data.frame(Номер = seq_len(nrow(rows)), rows, check.names = FALSE)
}

create_demo_insurance_workbook <- function(data, from, to, variant = "Стандартный") {
  stopifnot(is.data.frame(data), ncol(data) > 0L)
  wb <- openxlsx::createWorkbook(creator = "ERP Reports Demo")
  openxlsx::addWorksheet(wb, "Демо-реестр")
  openxlsx::writeData(wb, 1, "Демонстрационный реестр страхования — синтетические данные", startRow = 1)
  openxlsx::writeData(wb, 1, paste("Вариант:", variant), startRow = 2)
  openxlsx::writeData(wb, 1, paste("Период:", format(as.Date(from), "%d.%m.%Y"), "—", format(as.Date(to), "%d.%m.%Y")), startRow = 3)
  openxlsx::writeData(wb, 1, "Образец отчёта; не является страховым документом.", startRow = 4)
  openxlsx::writeData(wb, 1, data, startRow = 6, withFilter = nrow(data) > 0L)
  openxlsx::addStyle(wb, 1, openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCEAF7", wrapText = TRUE),
                    rows = 6, cols = seq_len(ncol(data)), gridExpand = TRUE)
  openxlsx::setColWidths(wb, 1, cols = seq_len(ncol(data)), widths = 23)
  openxlsx::freezePane(wb, 1, firstActiveRow = 7)
  if (nrow(data) > 0L) {
    dates <- which(vapply(data, inherits, logical(1), "Date"))
    if (length(dates)) openxlsx::addStyle(wb, 1, openxlsx::createStyle(numFmt = "dd.mm.yyyy"),
                                        rows = 7:(nrow(data) + 6), cols = dates, gridExpand = TRUE, stack = TRUE)
  }
  wb
}
