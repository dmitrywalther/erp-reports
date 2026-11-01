# Демо-режим: включается переменной окружения ERP_REPORTS_DEMO=true.
# В нём приложение работает с синтетическими данными в локальном PostgreSQL
# (см. demo/), а Elasticsearch заменён заглушкой demo/elastic_stub.R.
DEMO_MODE <- tolower(Sys.getenv("ERP_REPORTS_DEMO", "false")) %in% c("true", "1", "yes")
ONLINE_DEMO <- DEMO_MODE && tolower(Sys.getenv("ERP_REPORTS_ONLINE", "false")) %in% c("true", "1", "yes")
source("report_utils.R", encoding = "UTF-8")

library(shiny) # создание интерактивных веб-приложений
library(shinydashboard) # создание дашбордов
library(shinyWidgets)
library(shinythemes) # темы для приложений
library(DBI)
library(RPostgres)
if (DEMO_MODE) {
  source("demo/elastic_stub.R", encoding = "utf-8")
} else {
  library(elasticsearchr) # работа с Elasticsearch
}
library(tidyverse) # автоматизация рутинных задач по обработке данных (включает ggplot2)
library(DT) # интерактивные HTML-таблицы с фильтрацией, разбиением на страницы и сортировкой
library(openxlsx) # для записи в excel
library(leaflet)
library(config)
library(highcharter)
library(jsonlite)
library(shinydashboardPlus)
library(waiter)
library(gt)
library(paletteer)
library(xts)
library(reactable)
library(htmltools)
library(shinyjs)
library(lubridate)
library(shiny.router)
library(shinyauthr)
library(writexl)
library(bizdays)
library(readxl)
library(osrm) # маршруты для карт (публичный OSRM-сервер)
library(sf)
if (!DEMO_MODE) {
  library(sendmailR) # почтовые рассылки (postman.R)
  library(teamr) # уведомления в MS Teams (postman.R)
  library(Microsoft365R)
}

# Адрес ERP для ссылок из таблиц и список создателей заявок «спот-команды»
# (отчёт «Скорость закрытия рейса»); задаются переменными окружения.
ERP_BASE_URL <- if (DEMO_MODE) "" else Sys.getenv("ERP_BASE_URL", "")
SPOT_TEAM_CREATORS <- if (DEMO_MODE) sprintf("Логист %03d", 1:7) else trimws(strsplit(Sys.getenv("SPOT_TEAM_CREATORS", ""), ",")[[1]])
