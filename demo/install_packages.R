# Установка пакетов для R 4.0.x без renv.
#
# CRAN уже не собирает бинарники под R 4.0, поэтому берём снимок
# Posit Package Manager от 2023-06-15 (дата последнего релиза проекта):
# на Windows ставятся готовые .zip, на Linux — бинарники под Ubuntu 20.04.
#
# Запуск:  Rscript demo/install_packages.R [путь_к_библиотеке]

args <- commandArgs(trailingOnly = TRUE)
lib <- if (length(args) >= 1) args[1] else Sys.getenv("R_LIBS_USER", .libPaths()[1])
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

snapshot <- "2023-06-15"
is_windows <- .Platform$OS.type == "windows"
repo <- if (is_windows) {
  sprintf("https://packagemanager.posit.co/cran/%s", snapshot)
} else {
  sprintf("https://packagemanager.posit.co/cran/__linux__/focal/%s", snapshot)
}
options(repos = c(P3M = repo), timeout = 600)
if (is_windows) options(pkgType = "win.binary")

pkgs <- c(
  "shiny", "shinydashboard", "shinydashboardPlus", "shinyWidgets", "shinythemes", "shinyjs",
  "shiny.router", "shinyauthr", "waiter",
  "DBI", "RPostgres", "config", "jsonlite",
  "tidyverse", "lubridate", "readxl", "writexl", "openxlsx",
  "DT", "reactable", "gt", "highcharter", "leaflet", "paletteer", "viridis", "xts", "htmltools",
  "sf", "bizdays", "sodium"
)
install.packages(pkgs, lib = lib)

# osrm пропал из снимка 2023-06 для R 4.0 — берём более ранний
install.packages("osrm", lib = lib,
                 repos = sub(snapshot, "2022-12-01", repo, fixed = TRUE))

# Только для боевого режима (рассылки, Teams); в демо не нужны
if (tolower(Sys.getenv("ERP_REPORTS_DEMO", "false")) %in% c("false", "0", "")) {
  install.packages(c("sendmailR", "teamr", "Microsoft365R"), lib = lib)
}

missing <- setdiff(c(pkgs, "osrm"), rownames(installed.packages(lib.loc = lib)))
if (length(missing) > 0) stop("не установились: ", paste(missing, collapse = ", "))
message("все пакеты установлены в ", lib)
