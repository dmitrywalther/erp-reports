options(encoding = "UTF-8") 

Sys.setlocale("LC_TIME", "Russian")
Sys.setenv(TZ='Europe/Moscow')

source("demo/public_support.R", encoding = "UTF-8")
# Check authentication before opening any production data connections.
user_base <- load_report_users(DEMO_MODE)

# В файл helpers.R отдельно вынесены функции 
# для получения и предобработки данных
source("helpers.R", encoding="utf-8")
# В файл production_calendar.R отдельно вынесен
# производственный календарь (дата фрейм transition_table_workDay), 
# который используется для расчёта показателя выработки логиста
source("production_calendar.R", encoding="utf-8")


# Авансирование перевозчиков

carrier_prepayment_table <- reactiveValues(data = NULL)

# Здоровье клиента
client_health_table <- reactiveValues(data = NULL)

# НДЗ/ПДЗ
DZ_unconfirmed_receivables_table <- reactiveValues(data = NULL)
DZ_confirmed_receivables_table <- reactiveValues(data = NULL)
DZ_receivable_turnover_table <- reactiveValues(data = NULL)

# Дашборд ДЗ
health_dz_table <- reactiveValues(data = NULL)
overdue_dz_table <- reactiveValues(data = NULL)
unformed_bills_table <- reactiveValues(data = NULL)
funnel_NDZ_table <- reactiveValues(data = NULL)
dz_turnover_table <- reactiveValues(data = NULL)

# Выработка логистов по загруженным заявкам
logisticKPI <- reactiveValues(data = NULL)

# Фактические показатели по выгруженным заявкам
RevenueProfitTable <- reactiveValues(data = NULL)

# Обработанные заявки
verifiedDocsTable <- reactiveValues(data = NULL)
# Выставленные счета
billsTable <- reactiveValues(data = NULL)
# Проведённые акты
actTable <- reactiveValues(data = NULL)
billsNewTable <- reactiveValues(data = NULL)
# Контроль за дебиторской задолженностью
dzTable <- reactiveValues(data = NULL)

# Рейсы
requestsTable <- reactiveValues(data = NULL)

# Рейсы - Спец. РЦ
requestsTable_specialDC <- reactiveValues(data = NULL)

# Кредиторская задолженность
kzTable <- reactiveValues(data = NULL)

# Сорванные рейсы
breakdownsTable <- reactiveValues(data = NULL)
cancelingTableNew <- reactiveValues(data = NULL)
breakdownsTableNew <- reactiveValues(data = NULL)

# Конверсия обращений в БД перевозчиков
newOldCarriersTable <- reactiveValues(data = NULL)
# Просроченные заявки
lateOrdersTable <- reactiveValues(data = NULL)


# Служба безопасности 
drivers_everyday <- reactiveValues(data = data.frame(Дата = Sys.Date(), Сотрудник = "", Всего = 0, Создано = 0, Изменено = 0)) 
carriers_everyday <- reactiveValues(data = data.frame(Дата = Sys.Date(), Сотрудник = "", Всего = 0, Создано = 0, Изменено = 0)) 
vehicles_everyday <- reactiveValues(data = data.frame(Дата = Sys.Date(), Сотрудник = "", Всего = 0, Создано = 0, Изменено = 0)) 

drivers_status_change <- reactiveValues(data = NULL)
carriers_status_change <- reactiveValues(data = NULL)
vehicles_status_change <- reactiveValues(data = NULL)

# Спотовые заявки с площадок
spotTable <- reactiveValues(data = NULL)

# Индикатив цен
indicativeTable <- reactiveValues(data = NULL)
indicativeTable1 <- reactiveValues(data = NULL)
indicativeTable2 <- reactiveValues(data = NULL)

# Время наибольшей загруженности
countRequestsTable <- reactiveValues(data = NULL)

# Среднее время закрытия рейса
averageTimeTable <- reactiveValues(data = NULL)
averageTimeTable1 <- reactiveValues(data = NULL)

# Количество перевозчиков
driverCountTable <- reactiveValues(data = NULL)
carrierCountTable <- reactiveValues(data = NULL)

# Движения ДС
cashTable <- reactiveValues(data = NULL)

# Реестры для страховой
registersForInsuranceTable <- reactiveValues(data = NULL)

# Метрики
metricTable <- reactiveValues(data = NULL)
metricTable1 <- reactiveValues(data = NULL)

# Кругорейсы
doubletsTable <- reactiveValues(data = NULL)
clientDirectionsTable <- reactiveValues(data = NULL)

# Рейсы с низкой рентабельностью
requestProfitabilityTable <- reactiveValues(data = NULL)

# metricTable_completed <- reactiveValues(data = NULL)
# metricTable_canceling <- reactiveValues(data = NULL)
# metricTable_breakdowns <- reactiveValues(data = NULL)
# metricTable_expired <- reactiveValues(data = NULL)

# Оформление excel-таблиц
hs2 <- createStyle(
  fontColour = "#000000", fgFill = "#99ff99",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight",
  wrapText = TRUE
)

# Оформление excel-таблиц
hs2_yellow <- createStyle(
  fontColour = "#000000", fgFill = "#ffff00",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight",
  wrapText = TRUE
)

# Оформление таблиц Reactable
rating_column <- function(maxWidth = 55, ...) {
  colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 0), maxWidth = maxWidth, align = "center", class = "cell number", ...)
}

group_column <- function(class = NULL, ...) {
  colDef(cell = format_pct, maxWidth = 70, align = "center", class = paste("cell number", class), ...)
}

# knockout_column <- function(maxWidth = 70, class = NULL, ...) {
#   colDef(
#     cell = format_pct,
#     maxWidth = maxWidth,
#     class = paste("cell number", class),
#     style = function(value) {
#       # Lighter color for <1%
#       if (value < 1) {
#         list(color = "#aaa")
#       } else {
#         scaled <- (value - min(getManagerPlanFact()$Рентабельность_выполнение_общий)) / (max(result_plan_fact1$Рентабельность_выполнение_общий) - min(result_plan_fact1$Рентабельность_выполнение_общий))
#         list(color = "#111", background = knockout_pct_color(scaled))
#       }
#     },
#     ...
#   )
# }

format_pct <- function(value) {
  if (value == 0) "  \u2013 "    # en dash for 0%
  else if (value == 100) "\u2713"  # checkmark for 100%
  else if (value < 1) " <1%"
  else if (value > 99) ">99%"
  else formatC(paste0(round(value), "%"), width = 4)
}

# NA/NaN (например, рентабельность при нулевой ставке) красим как 0,
# иначе rgb() падает
safe_ramp_value <- function(x) {
  x[is.na(x) | !is.finite(x)] <- 0
  pmin(pmax(x, 0), 1)
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(safe_ramp_value(x)), maxColorValue = 255)
}

off_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 1.3)
def_rating_color <- make_color_pal(c("#ff2700", "#f8fcf8", "#44ab43"), bias = 0.6)
knockout_pct_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)

# Маршрут между двумя точками через публичный сервер OSRM. Если сервер
# недоступен (нет сети, прокси), рисуем прямую линию и оцениваем расстояние
# по формуле гаверсинуса с коэффициентом извилистости 1.3.
safe_osrm_route <- function(pts) {
  tryCatch(
    osrmRoute(loc = pts, returnclass = "sf", overview = "full"),
    error = function(e) {
      message("OSRM недоступен, строим прямую: ", conditionMessage(e))
      to_rad <- pi / 180
      dlat <- (pts$y[2] - pts$y[1]) * to_rad
      dlon <- (pts$x[2] - pts$x[1]) * to_rad
      a <- sin(dlat / 2)^2 + cos(pts$y[1] * to_rad) * cos(pts$y[2] * to_rad) * sin(dlon / 2)^2
      km <- 6371 * 2 * asin(sqrt(a)) * 1.3
      sf::st_sf(src = "start", dst = "finish", duration = km / 60 * 60, distance = km,
                geometry = sf::st_sfc(sf::st_linestring(cbind(pts$x, pts$y)), crs = 4326))
    }
  )
}

orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ff9500"))(safe_ramp_value(x)), maxColorValue = 255)
BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(safe_ramp_value(x)), maxColorValue = 255)


# Пользователи загружаются в начале файла; demo/demo доступен только в DEMO_MODE.

function(input, output, session){
  
  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  user_info <- reactive({
    credentials()$info
  })

  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("$('.tab-content').css('display', 'block');")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::runjs("$('.tab-content').css('display', 'none');")
      
    }
  })
  
  output$sidebarID <- renderMenu({
    req(credentials()$user_auth)
    if (user_info()$permissions == "admin"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Оборот и прибыль", tabName = "revenue_profit", icon = icon("calendar")),
        menuItem("НДЗ/ПДЗ", tabName = "DZ_new", icon = icon("credit-card")),
        menuItem("Документооборот", tabName = "DO", icon = icon("book")),
        menuItem("Кредиторская задолженность", tabName = "KZ", icon = icon("credit-card"))
      )
    } else if (user_info()$permissions == "senior_logistics_group"){
      sidebarMenu(
        id = "sidebarID"
      )
    } else if (user_info()$permissions == "document_management_officer"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("НДЗ/ПДЗ", tabName = "DZ_new", icon = icon("credit-card"))
      )
    } else if (user_info()$permissions == "jurist"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Реестры для страховой", tabName = "registersForInsurance", icon = icon("book"))
      ) 
    } else if (user_info()$permissions == "com"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Оборот и прибыль", tabName = "revenue_profit", icon = icon("calendar"))
      )
    } else if (user_info()$permissions == "security_officer"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Справочник перевозчика", tabName = "carriers", icon = icon("car")),
        menuItem("Время час пика", tabName = "count_requests", icon = icon("th")),
        menuItem("Скорость закрытия рейса", tabName = "averageTimeTabName", icon = icon("text-width"))
      )
    } else if (user_info()$permissions == "logistics_department_specialist"){
      sidebarMenu(
        id = "sidebarID"
      )
    } else if (user_info()$permissions == "logistics_department_kpi"){
        sidebarMenu(
          id = "sidebarID"
        )
    } else if (user_info()$permissions == "pricing_team"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Спот", tabName = "spot", icon = icon("adjust"))
      )
    } else if (user_info()$permissions == "disp"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Заявки и перевозки", tabName = "requests", icon = icon("tags"))
      )
    } else if (user_info()$permissions == "shareholder"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Оборот и прибыль", tabName = "revenue_profit", icon = icon("calendar"))
      ) 
    } else if (user_info()$permissions == "data_analyst"){
      sidebarMenu(
        id = "sidebarID",
        menuItem("Оборот и прибыль", tabName = "revenue_profit", icon = icon("calendar")),
        menuItem("Выработка логистов", tabName = "productivity", icon = icon("dashboard")),
        # menuItem("Движения ДС", tabName = "cash", icon = icon("plane")),
        # menuItem("Дебиторская задолженность", tabName = "DZ", icon = icon("credit-card")),
        menuItem("Авансирование", tabName = "carrier_prepayment", icon = icon("th")),
        menuItem("Здоровье клиента", tabName = "client_health", icon = icon("plane")),
        menuItem("Здоровье ДЗ", tabName = "DZ_dashboard", icon = icon("credit-card")),
        menuItem("НДЗ/ПДЗ", tabName = "DZ_new", icon = icon("credit-card")),
        menuItem("Документооборот", tabName = "DO", icon = icon("book")),
        menuItem("Заявки и перевозки", tabName = "requests", icon = icon("tags")),
        menuItem("Кредиторская задолженность", tabName = "KZ", icon = icon("credit-card")),
        menuItem("Открытые рейсы", tabName = "active_requests", icon = icon("hourglass")),
        menuItem("Рейсы -  низкая рент-ть", tabName = "low_profitability", icon = icon("hourglass")),
        menuItem("Рейсы - спец. РЦ", tabName = "specialDC", icon = icon("hourglass")),
        menuItem("Сорванные рейсы", tabName = "canceledOrders", icon = icon("remove")),
        menuItem("Отказы и срывы", tabName = "canceledOrders_proportion", icon = icon("remove")),
        menuItem("Потенциальные кругорейсы", tabName = "potentialDoublets", icon = icon("car")),
        # menuItem("Отсутствующие заявки", tabName = "missingDocs", icon = icon("minus")),
        # menuItem("План-факт по менеджерам", tabName = "manager_plan_fact", icon = icon("th")),
        # menuItem("План-факт по логистам", tabName = "logistician_plan_fact", icon = icon("th")),
        # menuItem("Проблемные документы", tabName = "problemDocs", icon = icon("warning")),
        menuItem("Справочник перевозчика", tabName = "carriers", icon = icon("car")),
        menuItem("СБ - статусы", tabName = "security_status", icon = icon("user")),
        menuItem("СБ - все изменения", tabName = "security", icon = icon("user")),
        menuItem("Спот", tabName = "spot", icon = icon("adjust")),
        # menuItem("Факторинг", tabName = "factoring", icon = icon("fire")),
        menuItem("Индикатив цен", tabName = "priceIndicative", icon = icon("dollar")),
        # menuItem("Время час пика", tabName = "count_requests", icon = icon("th")),
        # menuItem("Скорость закрытия рейса", tabName = "averageTimeTabName", icon = icon("text-width")),
        # menuItem("Водители, перевозчики", tabName = "drivers_carriers_vehicles", icon = icon("road")),
        menuItem("Реестры для страховой", tabName = "registersForInsurance", icon = icon("book"))
        # menuItem("Метрики логистики", tabName = "logisticMetrics", icon = icon("book"))
        # menuItem("Widgets", tabName = "widgets", icon = icon("th"))
        # downloadButton("downloadReports", "Экспорт в Excel"),
        # tags$style(type='text/css', "#downloadReports {margin-left: 50px;}")
      )
    }
  })
  
  # Данные загружаются при нажатии на соответствующие пункты меню
  observe({
    req(credentials()$user_auth)
    if(!is.null(input$sidebarID)){
    if(input$sidebarID == "revenue_profit") {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      startDate <- as.POSIXct(paste(input$dateRevenueProfit[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$dateRevenueProfit[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 

      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC" 
      
      if (input$dateTypeForRevenueProfit == "По созданным рейсам"){
        RevenueProfitTable$data <- getRevenueProfit(startDate, endDate)
      } else if (input$dateTypeForRevenueProfit == "По выгруженным рейсам"){
        RevenueProfitTable$data <- getRevenueProfit1(startDate, endDate)
      } 
      
       isolate(updateSelectizeInput(session, 'managerForRevenueProfit', choices = unique(RevenueProfitTable$data$Менеджер), server = FALSE))
       isolate(updateSelectizeInput(session, 'logistForRevenueProfit', choices = unique(RevenueProfitTable$data$Логист), server = FALSE))
      
      
      progress$inc(1, detail = "Данные успешно получены")

    } else if(input$sidebarID == "canceledOrders") {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      breakdownsTable$data <- getBreakdowns()

      progress$inc(1, detail = "Данные успешно получены")

    } else if(input$sidebarID == "spot"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      spotTable$data <- getSpot(startDate, endDate)

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "productivity"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      startDate <- as.POSIXct(paste(input$dateLogisticProductivity[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$dateLogisticProductivity[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
      
      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC" 

      if (input$dateTypeForLogisticProductivity == "По созданным рейсам"){
        logisticKPI$data <- getLogisticKPI(startDate, endDate)
      } else if (input$dateTypeForLogisticProductivity == "По загруженным рейсам"){
        logisticKPI$data <- getLogisticKPI1(startDate, endDate)
      } else if (input$dateTypeForLogisticProductivity == "По выгруженным рейсам"){
        logisticKPI$data <- getLogisticKPI2(startDate, endDate)
      }
      
      isolate(updateSelectizeInput(session, 'logistJobForLogisticProductivity', choices = unique(logisticKPI$data$Должность), server = FALSE))
      isolate(updateSelectizeInput(session, 'logistGroupForLogisticProductivity', choices = unique(logisticKPI$data$Группа), server = FALSE))
      isolate(updateSelectizeInput(session, 'clientForLogisticProductivity', choices = unique(logisticKPI$data$Клиент), server = FALSE))

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "DZ"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      dzTable$data <- dz_result()

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "DO"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(paste(input$date[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$date[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
      
      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC" 
      
      verifiedDocsTable$data <- getVerifiedDocs(startDate, endDate)
      billsTable$data <- getBills(startDate, endDate)
      actTable$data <- carriedOutActs(startDate, endDate)
      # billsNewTable$data <- getBillsNew()

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "factoring"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      billsTable$data <- getBillsForFactoring()

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "priceIndicative"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(input$dateIndicative[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$dateIndicative[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      indicativeTable$data <- getPriceIndicative(startDate, endDate)
      # indicativeTable1$data <- getPriceIndicative1(startDate, endDate)
      # indicativeTable2$data <- getPriceIndicative2(startDate, endDate)
      
      isolate(updateSelectizeInput(session, 'indicativeTypeOfBody', choices = unique(indicativeTable$data$Тип_ТС), server = FALSE))
      isolate(updateSelectizeInput(session, 'indicativeCapacityAndVolume', choices = unique(indicativeTable$data$Объём_тоннаж), server = FALSE))
      isolate(updateSelectizeInput(session, 'indicativeClient', choices = unique(indicativeTable$data$Клиент), server = FALSE))
      
      route_list <- reactive({
        indicativeTable$data %>%
        filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2]) %>%
        group_by(Маршрут) %>%
        summarise(Рейсы = n()) %>% 
        filter(Рейсы > 1) %>%
        arrange(desc(Рейсы)) %>%
        head(100)
      })
      
      # route_list1 <- reactive({
      #   indicativeTable1$data %>%
      #     filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2]) %>%
      #     group_by(Маршрут) %>%
      #     summarise(Рейсы = n()) %>%
      #     filter(Рейсы > 1) %>%
      #     arrange(desc(Рейсы)) %>%
      #     head(100)
      # })
      # 
      # route_list2 <- reactive({
      #   indicativeTable2$data %>%
      #     filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2]) %>%
      #     group_by(Маршрут) %>%
      #     summarise(Рейсы = n()) %>%
      #     filter(Рейсы > 1) %>%
      #     arrange(desc(Рейсы)) %>%
      #     head(100)
      # })
      
      # updateSelectizeInput(session, 'routeERP', choices = unique(route_list()$Маршрут), server = FALSE)
      # updateSelectizeInput(session, 'routeTransporeon', choices = unique(route_list1()$Маршрут), server = FALSE)
      # updateSelectizeInput(session, 'routeInbev', choices = unique(route_list2()$Маршрут), server = FALSE)

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "KZ"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      kzTable$data <- accountsPayable()

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "widgets"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      indicativeTable$data <- getPriceIndicative()

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "canceledOrders_proportion"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(input$date_canceledOrders_prop[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$date_canceledOrders_prop[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      isolate(updateSelectizeInput(session, 'client_canceling_breakdowns_dynamic', choices = c("Все", unique(getListOfClients()$short_title)), server = FALSE))
      
      result <- getBreakdowns_new(startDate, endDate)
      
      cancelingTableNew$data <- result$canceling
      breakdownsTableNew$data <- result$breakdowns

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "count_requests"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      countRequestsTable$data <- getCountRequests()

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "security"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      startDate <- (as.numeric(as.POSIXct(input$dateSB[1], format="%Y-%m-%d")) - 86399) * 1000
      endDate <- (as.numeric(as.POSIXct(input$dateSB[2], format="%Y-%m-%d")) + 86399) * 1000
      
      startDate1 <- as.POSIXct(paste(input$dateSB[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate1 <- as.POSIXct(paste(input$dateSB[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
      
      attr(startDate1, "tzone") <- "UTC" 
      attr(endDate1, "tzone") <- "UTC" 
      
      drivers_everyday$data <- getSB(startDate, endDate)
      carriers_everyday$data <- get_SB_carriers(startDate1, endDate1)
      vehicles_everyday$data <- get_SB_vehicles(startDate1, endDate1)

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "security_status"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- (as.numeric(as.POSIXct(input$dateSB_status[1], format="%Y-%m-%d")) - 86399) * 1000
      endDate <- (as.numeric(as.POSIXct(input$dateSB_status[2], format="%Y-%m-%d")) + 86399) * 1000
      
      startDate1 <- as.POSIXct(paste(input$dateSB_status[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate1 <- as.POSIXct(paste(input$dateSB_status[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
      
      attr(startDate1, "tzone") <- "UTC" 
      attr(endDate1, "tzone") <- "UTC" 
      
      drivers_status_change$data <- get_SB_drivers_status(startDate, endDate)
      carriers_status_change$data <- get_SB_carriers_status(startDate1, endDate1)
      vehicles_status_change$data <- get_SB_vehicles_status(startDate1, endDate1)
      
      progress$inc(1, detail = "Данные успешно получены")
      
    } else if(input$sidebarID == "averageTimeTabName"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      startDate <- as.POSIXct(input$averageTimeDate[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$averageTimeDate[2], format="%Y-%m-%d") + 24*60*60 - 1

      averageTimeTable$data <- getAverageTimeToCloseDeal(startDate, endDate)

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "active_requests"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      ######################
      ### Открытые рейсы ###
      ######################
      
      isolate(updateSelectizeInput(session, 'client_active_requests', choices = c("Все", unique(getActualRequests()$Клиент)), server = FALSE))

      # Run every 30 seconds
      actualRequests <- reactivePoll(30000, session,

                                     #A function whose values over time will be tested for equality; inequality indicates that the underlying value has changed and needs to be invalidated and re-read using valueFunc
                                     checkFunc = function(){
                                       if(!is.null(getActualRequests()))
                                         nrow(getActualRequests())
                                       else
                                         ""
                                     },
                                     valueFunc = function() {
                                       getActualRequests()
                                     })
      
      actualRequests_selectedByClient <- reactive({
        if (input$client_active_requests != "Все") {
          actualRequests() %>% 
            filter(Клиент == input$client_active_requests)
        } else {
          actualRequests()
        }
      })

      output$boxToday <- renderUI({
        box(title = paste0(nrow(actualRequests_selectedByClient() %>%
                                  filter(Дата_загрузки_план == Sys.Date()))," открытых рейсов - сегодня (", format(Sys.Date(), "%B %d, %Y"), ")"), status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 12, reactableOutput("activeRequestsTodayDataTable"))
      })

      output$boxTommorow <- renderUI({
        box(title = paste0(nrow(actualRequests_selectedByClient() %>%
                                  filter(Дата_загрузки_план == Sys.Date() + 1)), " открытых рейсов - завтра (", format(Sys.Date() + 1, "%B %d, %Y"), ")"), status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12, reactableOutput("activeRequestsTommorowDataTable"))
      })

      output$boxAfterTommorow <- renderUI({
        box(title = paste0(nrow(actualRequests_selectedByClient() %>%
                                  filter(Дата_загрузки_план == Sys.Date() + 2)), " открытых рейсов - послезавтра (", format(Sys.Date() + 2, "%B %d, %Y"), ")"), status = "purple", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12, reactableOutput("activeRequestsAfterTommorowDataTable"))
      })

      output$activeRequestsTodayDataTable <- renderReactable(
        reactable(actualRequests_selectedByClient() %>%
                    filter(Дата_загрузки_план == Sys.Date()) %>%
                    select(-4),
                  columns = list(
                    Номер_заявки = colDef(name = "Номер заявки"),
                    Клиент = colDef(minWidth = 150),
                    Ответственный_логист = colDef(name = "Логист", minWidth = 150),
                    Время_загрузки_план = colDef(name = "Время"),
                    Маршрут = colDef(minWidth = 150),
                    Груз = colDef(minWidth = 150),
                    ТС = colDef(show = TRUE, minWidth = 150),
                    Ставка_клиента = colDef(name = "Ставка клиента", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0))
                  ),

                  striped = TRUE,
                  bordered = TRUE,
                  borderless = FALSE,
                  highlight = TRUE,
                  resizable = FALSE,
                  pagination = FALSE

        )
      )

      output$activeRequestsTommorowDataTable <- renderReactable(
        reactable(actualRequests_selectedByClient() %>%
                    filter(Дата_загрузки_план == Sys.Date() + 1) %>%
                    select(-4),
                  columns = list(
                    Номер_заявки = colDef(name = "Номер заявки"),
                    Клиент = colDef(minWidth = 150),
                    Ответственный_логист = colDef(name = "Логист", minWidth = 150),
                    Время_загрузки_план = colDef(name = "Время"),
                    Маршрут = colDef(minWidth = 150),
                    Груз = colDef(minWidth = 150),
                    ТС = colDef(show = TRUE, minWidth = 150),
                    Ставка_клиента = colDef(name = "Ставка клиента", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0))
                  ),

                  striped = TRUE,
                  bordered = TRUE,
                  borderless = FALSE,
                  highlight = TRUE,
                  resizable = FALSE,
                  pagination = FALSE

        )
      )

      output$activeRequestsAfterTommorowDataTable <- renderReactable(
        reactable(actualRequests_selectedByClient() %>%
                    filter(Дата_загрузки_план == Sys.Date() + 2) %>%
                    select(-4),
                  columns = list(
                    Номер_заявки = colDef(name = "Номер заявки"),
                    Клиент = colDef(minWidth = 150),
                    Ответственный_логист = colDef(name = "Логист", minWidth = 150),
                    Время_загрузки_план = colDef(name = "Время"),
                    Маршрут = colDef(minWidth = 150),
                    Груз = colDef(minWidth = 150),
                    ТС = colDef(show = TRUE, minWidth = 150),
                    Ставка_клиента = colDef(name = "Ставка клиента", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0))
                  ),

                  striped = TRUE,
                  bordered = TRUE,
                  borderless = FALSE,
                  highlight = TRUE,
                  resizable = FALSE,
                  pagination = FALSE

        )
      )

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "requests"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)

      isolate(updateSelectizeInput(session, 'client', choices = unique(getListOfClients()$short_title), server = FALSE))
      isolate(updateSelectizeInput(session, 'logist', choices = unique(getListOfLogists()$Description), server = FALSE))
      isolate(updateSelectizeInput(session, 'manager', choices = expeditors_group$data.id[expeditors_group$Должность == "менеджер"], server = FALSE))

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "drivers_carriers_vehicles"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      driverCountTable$data <- getCountDrivers()
      carrierCountTable$data <- getCountCarriers()
      
      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "cash"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      cashTable$data <- getCash()

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "specialDC"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      isolate(updateSelectizeInput(session, 'logist_specialDC', choices = c("Все", unique(getListOfLogists()$Description)), server = FALSE))
      isolate(updateSelectizeInput(session, 'client_specialDC', choices = c("Все", unique(getListOfClients()$short_title)), server = FALSE))
      
      startDate <- as.POSIXct(input$date_specialDC[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$date_specialDC[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      if (input$dateType_specialDC == "По загрузке"){
        requestsTable_specialDC$data <- getSpecialDistributionCenters(startDate, endDate)
      } else if (input$dateType_specialDC == "По выгрузке"){
        requestsTable_specialDC$data <- getSpecialDistributionCenters1(startDate, endDate)
      }
      
      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "carriers"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      newOldCarriersTable$data <- getNewOldCarriers()
      lateOrdersTable$data <- getLateOrders()
      
      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "logisticMetrics"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(input$metricDateInput[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$metricDateInput[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      startDate1 <- as.POSIXct(input$metricDateInput1[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate1 <- as.POSIXct(input$metricDateInput1[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      metricTable$data <- getMetrics(startDate, endDate)
      
      metricTable1$data <- getMetrics1(startDate1, endDate1)
      
      # metricTable_completed$data <- result$completed
      # metricTable_canceling$data <- result$canceling
      # metricTable_breakdowns$data <- result$breakdowns
      # metricTable_expired$data <- result$expired
      
      isolate(updateSelectizeInput(session, 'metricClient', choices = unique(metricTable$data$Клиент), server = FALSE))
      isolate(updateSelectizeInput(session, 'metricLogistGroup', choices = unique(metricTable$data$Группа), server = FALSE))
      isolate(updateSelectizeInput(session, 'metricLogistJob', choices = unique(metricTable$data$Должность), server = FALSE))
      isolate(updateSelectizeInput(session, 'metricLogist', choices = unique(metricTable$data$Логист), server = FALSE))

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "potentialDoublets"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      doubletsTable$data <- getDoublets_result()
      isolate(updateSelectizeInput(session, 'doublets', choices = unique(doubletsTable$data$Кругорейс), server = FALSE))
      
      startDate <- as.POSIXct(input$clientDirectionsDateInput[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$clientDirectionsDateInput[2], format="%Y-%m-%d") + 24*60*60 - 1
      clientDirectionsTable$data <- getClientDirections(startDate, endDate)
      
      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "low_profitability"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(input$low_profitabilityDateInput[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$low_profitabilityDateInput[2], format="%Y-%m-%d") + 24*60*60 - 1
      requestProfitabilityTable$data <- getRequestProfitability(startDate, endDate)
      
      isolate(updateSelectizeInput(session, 'low_profitability_client', choices = unique(requestProfitabilityTable$data$Клиент), server = FALSE))
      
      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "DZ_new"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(paste(input$date_newDZ[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$date_newDZ[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
      
      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC" 
      
      DZ_unconfirmed_receivables_table$data <- getDZ_unconfirmed_receivables(startDate, endDate)

      DZ_confirmed_receivables_table$data <- getDZ_confirmed_receivables()
    
      DZ_receivable_turnover_table$data <- getDZ_receivable_turnover(startDate, endDate)

      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "DZ_dashboard"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(paste(input$turnover_date[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")

      attr(startDate, "tzone") <- "UTC" 

      unformed_bills_table$data <- get_unformed_bills()

      
      overdue_dz_table$data <- get_healthDZ()$prdz
      
      health_dz_table$data <- get_healthDZ()$health
      
      funnel_NDZ_table$data <- get_funnel_NDZ()
      
      dz_turnover_table$data <- get_dz_turnover(startDate)
      
      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "client_health"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(paste(input$client_health_choose1, "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$client_health_choose2, "23:59:59"), format="%Y-%m-%d %H:%M:%S")

      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC"
      
      if (!is.na(startDate) & !is.na(endDate)){
        client_health_table$data <- get_client_health(startDate, endDate)
        isolate(updateSelectizeInput(session, 'client_health_choose', choices = unique(client_health_table$data$Клиент), server = FALSE))
      }
      
      
      progress$inc(1, detail = "Данные успешно получены")
    } else if(input$sidebarID == "carrier_prepayment"){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Извлечение данных", value = 0.4)
      
      startDate <- as.POSIXct(paste(input$carrier_prepayment_date[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$carrier_prepayment_date[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S")
      
      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC"
      
      carrier_prepayment_table$data <- get_prepayment(startDate, endDate)
      isolate(updateSelectizeInput(session, 'carrier_prepayment_client_choose', choices = unique(carrier_prepayment_table$data$Клиент), server = FALSE))
      isolate(updateSelectizeInput(session, 'carrier_prepayment_logist_choose', choices = unique(carrier_prepayment_table$data$Логист), server = FALSE))
      isolate(updateSelectizeInput(session, 'carrier_prepayment_carrier_choose', choices = unique(carrier_prepayment_table$data$Перевозчик), server = FALSE))
      
      progress$inc(1, detail = "Данные успешно получены")
    }
    }
  })
  
  # СБ - статусы
  
  output$driversAllTable_status <- renderReactable({
    reactable(drivers_status_change$data,
              groupBy = c('Сотрудник', 'Дата изменения', 'Водитель'),
              columns = list(
                'Изменений' = colDef(aggregate = 'sum')
              )
              )
  })
  
  output$carriersAllTable_status <- renderReactable({
    reactable(carriers_status_change$data,
              groupBy = c('Сотрудник', 'Дата изменения', 'Перевозчик'),
              defaultSorted = c("Изменений"),
              columns = list(
                'Изменений' = colDef(defaultSortOrder = "desc", aggregate = 'sum')
              )
              )
  })
  
  
  output$vehiclesAllTable_status <- renderReactable({
    reactable(vehicles_status_change$data,
              groupBy = c('Сотрудник', 'Дата изменения', 'ТС'),
              defaultSorted = c("Изменений"),
              columns = list(
                'Изменений' = colDef(defaultSortOrder = "desc", aggregate = 'sum')
              )
    )
  })
  
  
  sb_staff_rating_data_status <- reactive({
    drivers <- drivers_status_change$data %>%
      group_by(Сотрудник) %>%
      summarise(`Изменено статусов водителей` = sum(Изменений))
    
    carriers <- carriers_status_change$data %>%
      group_by(Сотрудник) %>%
      summarise(`Изменено статусов перевозчиков`= sum(Изменений))
    
    vehicles <- vehicles_status_change$data %>%
      group_by(Сотрудник) %>%
      summarise(`Изменено статусов ТС`= sum(Изменений))
      
    result <- drivers %>%
      full_join(carriers, by = c('Сотрудник' = 'Сотрудник')) %>%
      full_join(vehicles, by = c('Сотрудник' = 'Сотрудник'))

    result[is.na(result)] <- 0
    
    result <- result %>%
      mutate(Итог = `Изменено статусов водителей` + `Изменено статусов перевозчиков` + `Изменено статусов ТС`) %>%
      arrange(desc(Итог))
    
    return(result)
  })
    
  
  output$sb_staff_rating_table_status <- renderReactable({
    if(!is.null(sb_staff_rating_data_status())){
      reactable(sb_staff_rating_data_status(),
                defaultColDef = colDef(class = "cell", headerClass = "header", footerStyle = list(fontWeight = "bold")),
                defaultSorted = c("Итог"),
                columns = list(
                  Сотрудник = colDef(
                    style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                    footer = "Всего",
                    minWidth = 270,
                    headerStyle = list(fontWeight = 700),
                    cell = function(value, index) {
                      div(
                        class = "manager",
                        if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/sb/%s.jpg", value)),
                        div(class = "manager-name", value)
                      )
                    }
                  ),

                  'Изменено статусов водителей' = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(sb_staff_rating_data_status()["Изменено статусов водителей"])) / (max(sb_staff_rating_data_status()["Изменено статусов водителей"]) - min(sb_staff_rating_data_status()["Изменено статусов водителей"]))
                    color <- off_rating_color(normalized)
                    list(background = color)
                  }, na = "–", aggregate = "sum",
                  format = colFormat(separators = TRUE)),

                
                  'Изменено статусов перевозчиков' = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(sb_staff_rating_data_status()["Изменено статусов перевозчиков"])) / (max(sb_staff_rating_data_status()["Изменено статусов перевозчиков"]) - min(sb_staff_rating_data_status()["Изменено статусов перевозчиков"]))
                    color <- orange_pal(normalized)
                    list(background = color)
                  }, na = "–", aggregate = "sum",
                  format = colFormat(separators = TRUE)),
                  
                  
                  'Изменено статусов ТС' = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(sb_staff_rating_data_status()["Изменено статусов ТС"])) / (max(sb_staff_rating_data_status()["Изменено статусов ТС"]) - min(sb_staff_rating_data_status()["Изменено статусов ТС"]))
                    color <- BuYlRd(normalized)
                    list(background = color)
                  }, na = "–", aggregate = "sum",
                  format = colFormat(separators = TRUE)),
                  
                  
                  Итог = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE))
                  
                )
      )
    }
  })
  
  
  output$downloadSB_status <- downloadHandler(
    filename = function() {
      paste("Изменение_статусов_водителей_перевозиков-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Итоги")
      
      addWorksheet(wb, "Водители")
      
      addWorksheet(wb, "Перевозчики")
      
      addWorksheet(wb, "ТС")
 
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateSB_status[1], "%d.%m.%Y"), "-", format(input$dateSB_status[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb,
                sheet = 1,
                x = sb_staff_rating_data_status(),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows",
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 2,
                x = paste("За период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb,
                sheet = 2,
                x = drivers_status_change$data,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows",
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 3,
                x = paste("За период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb,
                sheet = 3,
                x = carriers_status_change$data,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows",
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      
      writeData(wb,
                sheet = 4,
                x = paste("За период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb,
                sheet = 4,
                x = vehicles_status_change$data,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows",
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      setColWidths(wb, sheet = 1, cols = 1, widths = 25)
      setColWidths(wb, sheet = 2, cols = c(2, 3, 4, 5), widths = c(25, 25, 15, 25))
      setColWidths(wb, sheet = 3, cols = c(2, 3, 4, 5), widths = c(25, 25, 15, 25))
      setColWidths(wb, sheet = 4, cols = c(2, 3, 4, 5), widths = c(25, 25, 15, 25))
      
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  # СБ - главная таблица
  
  sb_staff_rating_data <- reactive({
    
    drivers <- drivers_everyday$data %>%
      group_by(Сотрудник) %>%
      summarise(Всего_водители = sum(Всего_водители),
                              Создано_водители = sum(Создано_водители),
                              Изменено_водители = sum(Изменено_водители)
                              )
    
    carriers <- carriers_everyday$data %>%
      group_by(Сотрудник) %>%
      summarise(Всего_перевозчики = sum(Всего_перевозчики),
                              Создано_перевозчики = sum(Создано_перевозчики),
                              Изменено_перевозчики = sum(Изменено_перевозчики)
      )
    
    vehicles <- vehicles_everyday$data %>%
      group_by(Сотрудник) %>%
      summarise(Всего_тс = sum(Всего_тс),
                                 Создано_тс = sum(Создано_тс),
                                 Изменено_тс = sum(Изменено_тс)
      ) 
    
    result <- drivers %>%
      full_join(carriers, by = c('Сотрудник' = 'Сотрудник')) %>%
      full_join(vehicles, by = c('Сотрудник' = 'Сотрудник'))
      
    result[is.na(result)] <- 0
    
    result <- result %>%
      mutate(Итог = Всего_водители + Всего_перевозчики + Всего_тс)
    
    return(result)
    
  })
  
  output$sb_staff_rating_table <- renderReactable({
    if(!is.null(sb_staff_rating_data())){
      reactable(sb_staff_rating_data(),
                columnGroups = list(
                  colGroup(name = "Водители", columns = c('Всего_водители', 'Создано_водители', 'Изменено_водители')),
                  colGroup(name = "Перевозчики", columns = c('Всего_перевозчики', 'Создано_перевозчики', 'Изменено_перевозчики')),
                  colGroup(name = "ТС", columns = c('Всего_тс', 'Создано_тс', 'Изменено_тс'))
                ),
                defaultColDef = colDef(class = "cell", headerClass = "header", footerStyle = list(fontWeight = "bold")),
                defaultSorted = c("Итог"),
                columns = list(
                  Сотрудник = colDef(
                    style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                    footer = "Всего",
                    minWidth = 270,
                    headerStyle = list(fontWeight = 700),
                    cell = function(value, index) {
                      div(
                        class = "manager",
                        if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/sb/%s.jpg", value)),
                        div(class = "manager-name", value)
                      )
                    }
                  ),
                  Создано_водители = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", maxWidth = 100, na = "–", name = "Создано", aggregate = "sum", 
                                   style = function(value) {
                                     normalized <- (value - min(sb_staff_rating_data()["Создано_водители"])) / (max(sb_staff_rating_data()["Создано_водители"]) - min(sb_staff_rating_data()["Создано_водители"]))
                                     color <- off_rating_color(normalized)
                                     list(background = color)
                                   },
                                   format = colFormat(separators = TRUE)),
                  Изменено_водители = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(sb_staff_rating_data()["Изменено_водители"])) / (max(sb_staff_rating_data()["Изменено_водители"]) - min(sb_staff_rating_data()["Изменено_водители"]))
                    color <- orange_pal(normalized)
                    list(background = color)
                  }, na = "–", name = "Изменено", aggregate = "sum",
                  format = colFormat(separators = TRUE)),
                  Всего_водители = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), show = TRUE, 
                                   style = function(value) {
                                     normalized <- (value - min(sb_staff_rating_data()["Всего_водители"])) / (max(sb_staff_rating_data()["Всего_водители"]) - min(sb_staff_rating_data()["Всего_водители"]))
                                     color <- BuYlRd(normalized)
                                     list(background = color)
                                   }, 
                                   na = "–", 
                                   name = "Всего действий",
                                   aggregate = "sum", 
                                   format = colFormat(separators = TRUE)),
                  
                  Создано_перевозчики = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", maxWidth = 100, na = "–", name = "Создано", aggregate = "sum", 
                                            style = function(value) {
                                              normalized <- (value - min(sb_staff_rating_data()["Создано_перевозчики"])) / (max(sb_staff_rating_data()["Создано_перевозчики"]) - min(sb_staff_rating_data()["Создано_перевозчики"]))
                                              color <- off_rating_color(normalized)
                                              list(background = color)
                                            },
                                            format = colFormat(separators = TRUE)),
                  Изменено_перевозчики = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(sb_staff_rating_data()["Изменено_перевозчики"])) / (max(sb_staff_rating_data()["Изменено_перевозчики"]) - min(sb_staff_rating_data()["Изменено_перевозчики"]))
                    color <- orange_pal(normalized)
                    list(background = color)
                  }, na = "–", name = "Изменено", aggregate = "sum",
                  format = colFormat(separators = TRUE)),
                  Всего_перевозчики = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), show = TRUE, 
                                          style = function(value) {
                                            normalized <- (value - min(sb_staff_rating_data()["Всего_перевозчики"])) / (max(sb_staff_rating_data()["Всего_перевозчики"]) - min(sb_staff_rating_data()["Всего_перевозчики"]))
                                            color <- BuYlRd(normalized)
                                            list(background = color)
                                          }, 
                                          na = "–", 
                                          name = "Всего действий",
                                          aggregate = "sum", 
                                          format = colFormat(separators = TRUE)),
                  
                  
                  
                  Создано_тс = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", maxWidth = 100, na = "–", name = "Создано", aggregate = "sum", 
                                               style = function(value) {
                                                 normalized <- (value - min(sb_staff_rating_data()["Создано_тс"])) / (max(sb_staff_rating_data()["Создано_тс"]) - min(sb_staff_rating_data()["Создано_тс"]))
                                                 color <- off_rating_color(normalized)
                                                 list(background = color)
                                               },
                                               format = colFormat(separators = TRUE)),
                  Изменено_тс = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(sb_staff_rating_data()["Изменено_тс"])) / (max(sb_staff_rating_data()["Изменено_тс"]) - min(sb_staff_rating_data()["Изменено_тс"]))
                    color <- orange_pal(normalized)
                    list(background = color)
                  }, na = "–", name = "Изменено", aggregate = "sum",
                  format = colFormat(separators = TRUE)),
                  Всего_тс = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), show = TRUE, 
                                             style = function(value) {
                                               normalized <- (value - min(sb_staff_rating_data()["Всего_тс"])) / (max(sb_staff_rating_data()["Всего_тс"]) - min(sb_staff_rating_data()["Всего_тс"]))
                                               color <- BuYlRd(normalized)
                                               list(background = color)
                                             }, 
                                             na = "–", 
                                             name = "Всего действий",
                                             aggregate = "sum", 
                                             format = colFormat(separators = TRUE)),
                  
                  
                  Итог = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE))
                  
                )
      )
    }
  })
  
  # Авансирование перевозчиков
  
  selected_carrier_prepayment_routes_dynamics <- reactive({
    
    aggregation <- switch(input$carrier_prepayment_aggregation, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    } 
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Рейсы = sum(Рейсы.x)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рейсы,
                  values_fill = 0)
    
    filtered1 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered1 <- filtered1 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered1 <- filtered1 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Рейсы = sum(Рейсы.y)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рейсы,
                  values_fill = 0)
    
    filtered2 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered2 <- filtered2 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered2 <- filtered2 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered2 <- filtered2 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered2 <- filtered2 %>% group_by(Дата) %>%
      summarise(Рейсы = sum(Рейсы.x) / sum(Рейсы.x + Рейсы.y) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рейсы,
                  values_fill = 0)
    
    df <- data.frame(Рейсы = c("С авансированием", "Без авансирования", 'Доля, %'))
    
    df1 <- rbind(filtered, filtered1, filtered2)
    
    result <- cbind(df, df1)
    
    return(result)

  })
  
  output$carrier_prepayment_routes <- renderReactable(
    reactable(selected_carrier_prepayment_routes_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", format = colFormat(separators = TRUE, digits = 0))
              )
  )
  
  selected_carrier_prepayment_revenue_dynamics <- reactive({
    
    aggregation <- switch(input$carrier_prepayment_aggregation, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Выручка = sum(Выручка.x)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Выручка,
                  values_fill = 0)
    
    filtered1 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered1 <- filtered1 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered1 <- filtered1 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Выручка = sum(Выручка.y)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Выручка,
                  values_fill = 0)
    
    filtered2 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered2 <- filtered2 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered2 <- filtered2 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered2 <- filtered2 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered2 <- filtered2 %>% group_by(Дата) %>%
      summarise(Выручка = sum(Выручка.x) / sum(Выручка.x + Выручка.y) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Выручка,
                  values_fill = 0)
    
    df <- data.frame('Выручка' = c("С авансированием, \U20BD", "Без авансирования, \U20BD", 'Доля, %'))
    
    df1 <- rbind(filtered, filtered1, filtered2)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  output$carrier_prepayment_revenue <- renderReactable(
    reactable(selected_carrier_prepayment_revenue_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = '', format = colFormat(separators = TRUE, digits = 0)),
              columns = list(
                'Выручка' = colDef(
                  name = 'Выручка'
                )
              )
              )
  )
  
  selected_carrier_prepayment_margin_dynamics <- reactive({
    
    aggregation <- switch(input$carrier_prepayment_aggregation, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Маржа = sum(Выручка.x - `Итоговая ставка`)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Маржа,
                  values_fill = 0)
    
    filtered1 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered1 <- filtered1 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered1 <- filtered1 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Маржа = sum(Маржа.y)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Маржа,
                  values_fill = 0)
    
    filtered2 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered2 <- filtered2 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered2 <- filtered2 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered2 <- filtered2 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered2 <- filtered2 %>% group_by(Дата) %>%
      summarise(Маржа = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x - `Итоговая ставка` + Маржа.y) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Маржа,
                  values_fill = 0)
    
    df <- data.frame(Маржа = c("С авансированием, \U20BD", "Без авансирования, \U20BD", 'Доля, %'))
    
    df1 <- rbind(filtered, filtered1, filtered2)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  output$carrier_prepayment_margin <- renderReactable(
    reactable(selected_carrier_prepayment_margin_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = '', format = colFormat(separators = TRUE, digits = 0)),
              columns = list(
                'Маржа' = colDef(
                  name = 'Маржа'
                )
              )
              )
  )
  
  selected_carrier_prepayment_profitability_dynamics <- reactive({
    
    aggregation <- switch(input$carrier_prepayment_aggregation, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Рентабельность = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рентабельность,
                  values_fill = 0)
    
    filtered1 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered1 <- filtered1 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered1 <- filtered1 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Рентабельность = sum(Маржа.y) / sum(Выручка.y) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рентабельность,
                  values_fill = 0)
    
    filtered2 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered2 <- filtered2 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered2 <- filtered2 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered2 <- filtered2 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered2 <- filtered2 %>% group_by(Дата) %>%
      summarise(Рентабельность = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x) * 100 - sum(Маржа.x) / sum(Выручка.x) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рентабельность,
                  values_fill = 0)
    
    df <- tibble(Рентабельность = c('С авансированием, %', 'В т.ч. за счёт комиссии, %', 'Без авансирования, %'))
    
    df1 <- rbind(filtered, filtered2, filtered1)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  output$carrier_prepayment_profitability <- renderReactable(
    reactable(selected_carrier_prepayment_profitability_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = '', format = colFormat(separators = TRUE, digits = 2)),
                          rowStyle = function(index) {
                            if (selected_carrier_prepayment_profitability_dynamics()[index, "Рентабельность"] == "В т.ч. за счёт комиссии, %") {
                              list(background = "rgba(187, 187, 187, 0.5)")
                            } 
                          },
              columns = list(
                'Рентабельность' = colDef(
                  name = 'Рентабельность'
                )
              )
    )
  )
  
  
  selected_carrier_prepayment_advances_dynamics <- reactive({
    
    aggregation <- switch(input$carrier_prepayment_aggregation, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Авансы = sum(Предоплата)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Авансы,
                  values_fill = 0)
    
    filtered1 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered1 <- filtered1 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered1 <- filtered1 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Доля = sum(Предоплата) / sum(`Итоговая ставка`) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Доля,
                  values_fill = 0)
    
    filtered2 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered2 <- filtered2 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered2 <- filtered2 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered2 <- filtered2 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered2 <- filtered2 %>% group_by(Дата) %>%
      summarise(Доля = sum(`Начальная ставка` - `Итоговая ставка`) / sum(`Начальная ставка`) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Доля,
                  values_fill = 0)
    
    filtered3 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered3 <- filtered3 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered3 <- filtered3 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered3 <- filtered3 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered3 <- filtered3 %>% group_by(Дата) %>%
      summarise(Доля = sum(`Начальная ставка` - `Итоговая ставка`)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Доля,
                  values_fill = 0)
    
    filtered4 <- carrier_prepayment_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered4 <- filtered4 %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered4 <- filtered4 %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered4 <- filtered4 %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    
    filtered4 <- filtered4 %>% group_by(Дата) %>%
      summarise(Средний_аванс = sum(Предоплата) / sum(Рейсы.x)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Средний_аванс,
                  values_fill = 0)
    
    df <- tibble('Быстрые предоплаты' = c('Выдано авансов, \U20BD', 'Средний аванс, \U20BD', 'Размер аванса, %', 'Комиссия перевозчика, %', 'Доход от авансирования, \U20BD'))
    
    df1 <- rbind(filtered, filtered4, filtered1, filtered2, filtered3)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  output$carrier_prepayment_advances <- renderReactable(
    reactable(selected_carrier_prepayment_advances_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = '', format = colFormat(separators = TRUE, digits = 2)),
              columns = list(
                'Быстрые предоплаты' = colDef(
                  name = 'Быстрые предоплаты'
                )
              )
    )
  )
  
  selected_carrier_prepayment_advances_by_client <- reactive({
    
    filtered <- carrier_prepayment_table$data
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    filtered %>%
      group_by(Клиент) %>%
      summarise(Рейсы = sum(Рейсы.x),
                     Рейсы_доля = sum(Рейсы.x) / sum(Рейсы.x + Рейсы.y),
                     Выручка = sum(Выручка.x),
                     Выручка_доля = sum(Выручка.x) / sum(Выручка.x + Выручка.y),
                     Маржа = sum(Выручка.x - `Итоговая ставка`),
                     Маржа_доля = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x - `Итоговая ставка` + Маржа.y),
                     Рентабельность = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x) * 100,
                     `Выдано авансов` = sum(Предоплата),
                     `Размер аванса` = sum(Предоплата) / sum(`Итоговая ставка`) * 100,
                     `Комиссия перевозчика` = sum(`Начальная ставка` - `Итоговая ставка`) / sum(`Начальная ставка`) * 100,
                     `Доход от авансирования` = sum(`Начальная ставка` - `Итоговая ставка`)
                     )
      # mutate(Рейсы_доля = Рейсы / sum(filtered$Рейсы.x),
      #        Выручка_доля = Выручка / sum(filtered$Выручка.x),
      #        Маржа_доля = Маржа / sum(filtered$Выручка.x - filtered$`Итоговая ставка`))
  })
  
  output$carrier_prepayment_advances_by_client <- renderReactable(
    reactable(selected_carrier_prepayment_advances_by_client(),
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              defaultSorted = 'Доход от авансирования',
              defaultPageSize = 5,
              columns = list(
                Клиент = colDef(style = list(fontWeight = 600)),
                Рейсы = colDef(cell = function(value, index) {
                                 share <- selected_carrier_prepayment_advances_by_client()$Рейсы_доля[index]
                                 share <- if (!is.na(share)) share else 0
                                 div(
                                   div(style = list(fontWeight = 600, fontSize = 18), format(value, big.mark=" ", digits = 2, scientific = FALSE)),
                                   div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                                 )
                               }),
                Выручка = colDef(cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_client()$Выручка_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Маржа = colDef(cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_client()$Маржа_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Рейсы_доля = colDef(show = FALSE),
                Выручка_доля = colDef(show = FALSE),
                Маржа_доля = colDef(show = FALSE),
                Рентабельность = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
                'Выдано авансов' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 2)),
                'Размер аванса' = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                'Комиссия перевозчика' = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
                'Доход от авансирования' = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0))
              )
              )
  )
  
  
  selected_carrier_prepayment_advances_by_logist <- reactive({
    
    filtered <- carrier_prepayment_table$data
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    filtered %>%
      group_by(Логист, data.name) %>%
      summarise(Рейсы = sum(Рейсы.x),
                     Рейсы_доля = sum(Рейсы.x) / sum(Рейсы.x + Рейсы.y),
                     Выручка = sum(Выручка.x),
                     Выручка_доля = sum(Выручка.x) / sum(Выручка.x + Выручка.y),
                     Маржа = sum(Выручка.x - `Итоговая ставка`),
                     Маржа_доля = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x - `Итоговая ставка` + Маржа.y),
                     Рентабельность = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x) * 100,
                     `Выдано авансов` = sum(Предоплата),
                     `Размер аванса` = sum(Предоплата) / sum(`Итоговая ставка`) * 100,
                     `Комиссия перевозчика` = sum(`Начальная ставка` - `Итоговая ставка`) / sum(`Начальная ставка`) * 100,
                     `Доход от авансирования` = sum(`Начальная ставка` - `Итоговая ставка`),
                     `Начальная ставка` = sum(`Начальная ставка`),
                     `Итоговая ставка` = sum(`Итоговая ставка`)
      ) 
      # mutate(Рейсы_доля = Рейсы / sum(filtered$Рейсы.x),
      #                  Выручка_доля = Выручка / sum(filtered$Выручка.x),
      #                  Маржа_доля = Маржа / sum(filtered$Выручка.x - filtered$`Итоговая ставка`))
  })
  
  output$carrier_prepayment_advances_by_logist <- renderReactable(
    reactable(selected_carrier_prepayment_advances_by_logist(),
              groupBy = 'data.name',
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              defaultSorted = 'Доход от авансирования',
              defaultPageSize = 5,
              columns = list(
                data.name = colDef(name = 'Группа'),
                Логист = colDef(style = list(fontWeight = 600)),
                Рейсы = colDef(format = colFormat(separators = TRUE), style = JS("{ fontSize: '18', fontWeight: 'bold' }"), aggregate = 'sum', cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_logist()$Рейсы_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), format(value, big.mark=" ", digits = 2, scientific = FALSE)),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Выручка = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), style = JS("{ fontSize: '18', fontWeight: 'bold' }"), aggregate = 'sum', cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_logist()$Выручка_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Маржа = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), style = JS("{ fontSize: '18', fontWeight: 'bold' }"), aggregate = 'sum', cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_logist()$Маржа_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Рейсы_доля = colDef(show = FALSE),
                Выручка_доля = colDef(show = FALSE),
                Маржа_доля = colDef(show = FALSE),
                'Начальная ставка' = colDef(show = FALSE),
                'Итоговая ставка' = colDef(show = FALSE),
                Рентабельность = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 2),
                                        aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа']
                                              totalUnits += row['Выручка']
                                            })
                                            var result = 0
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }")),
                'Выдано авансов' = colDef(aggregate = 'sum', format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 2)),
                'Размер аванса' = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выдано авансов']
                                              totalUnits += row['Итоговая ставка']
                                            })
                                            var result = 0
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                'Комиссия перевозчика' = colDef(                                        aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Доход от авансирования']
                                              totalUnits += row['Начальная ставка']
                                            })
                                            var result = 0
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
                'Доход от авансирования' = colDef(aggregate = 'sum', defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0))
              )
    )
  )
  
  
  selected_carrier_prepayment_advances_by_carrier <- reactive({
    
    filtered <- carrier_prepayment_table$data
    
    if (!is.null(input$carrier_prepayment_client_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$carrier_prepayment_client_choose)
    } 
    if (!is.null(input$carrier_prepayment_logist_choose)) {
      filtered <- filtered %>% filter(Логист %in% input$carrier_prepayment_logist_choose)
    } 
    if (!is.null(input$carrier_prepayment_carrier_choose)) {
      filtered <- filtered %>% filter(Перевозчик %in% input$carrier_prepayment_carrier_choose)
    }
    
    filtered %>%
      group_by(Перевозчик) %>%
      summarise(Рейсы = sum(Рейсы.x),
                     Рейсы_доля = sum(Рейсы.x) / sum(Рейсы.x + Рейсы.y),
                     Выручка = sum(Выручка.x),
                     Выручка_доля = sum(Выручка.x) / sum(Выручка.x + Выручка.y),
                     Маржа = sum(Выручка.x - `Итоговая ставка`),
                     Маржа_доля = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x - `Итоговая ставка` + Маржа.y),
                     Рентабельность = sum(Выручка.x - `Итоговая ставка`) / sum(Выручка.x) * 100,
                     `Выдано авансов` = sum(Предоплата),
                     `Размер аванса` = sum(Предоплата) / sum(`Итоговая ставка`) * 100,
                     `Комиссия перевозчика` = sum(`Начальная ставка` - `Итоговая ставка`) / sum(`Начальная ставка`) * 100,
                     `Доход от авансирования` = sum(`Начальная ставка` - `Итоговая ставка`)
      )
      # mutate(Рейсы_доля = Рейсы / sum(filtered$Рейсы.x),
      #                  Выручка_доля = Выручка / sum(filtered$Выручка.x),
      #                  Маржа_доля = Маржа / sum(filtered$Выручка.x - filtered$`Итоговая ставка`))
  })
  
  output$carrier_prepayment_advances_by_carrier <- renderReactable(
    reactable(selected_carrier_prepayment_advances_by_carrier(),
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              defaultSorted = 'Доход от авансирования',
              defaultPageSize = 5,
              columns = list(
                Перевозчик = colDef(style = list(fontWeight = 600)),
                Рейсы = colDef(cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_carrier()$Рейсы_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), format(value, big.mark=" ", digits = 2, scientific = FALSE)),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Выручка = colDef(cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_carrier()$Выручка_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Маржа = colDef(cell = function(value, index) {
                  share <- selected_carrier_prepayment_advances_by_carrier()$Маржа_доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Рейсы_доля = colDef(show = FALSE),
                Выручка_доля = colDef(show = FALSE),
                Маржа_доля = colDef(show = FALSE),
                Рентабельность = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
                'Выдано авансов' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 2)),
                'Размер аванса' = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                'Комиссия перевозчика' = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
                'Доход от авансирования' = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0))
              )
    )
  )
  
  
  # Здоровье клиента
  
  excel_data <- reactive({
    file_cost_data <- input$cost_data_from_file
    ext <- tools::file_ext(file_cost_data$datapath)
    
    req(file_cost_data)
    shiny::validate(need(ext == "xlsx", "Пожалуйста, загрузите файл xlsx"))
    excel_data <- read.xlsx(file_cost_data$datapath,
                            sheet = 1,
                            startRow = 1)
    
    aggregation <- switch(input$client_health_aggregation, 
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    db_data <- client_health_table$data 
      
    if (!is.null(input$client_health_choose)) {
      db_data <- db_data %>% 
        filter(Клиент %in% input$client_health_choose)
    } 
    
    db_data <- client_health_table$data %>%
      group_by(Дата) %>%
      summarise(Логисты = n_distinct(Логист),
                       Рейсы = sum(Рейсы),
                       Выручка = sum(Выручка),
                       Маржа = sum(`Маржа, расчёт`)
      ) %>%
      mutate(Дата_агр = format(Дата, aggregation),
                     Месяц = format(Дата, "%Y.%m")
      ) 
    
    
    
    result <- db_data %>%
      left_join(excel_data, by = c("Месяц" = "Месяц"))
    
    result[is.na(result)] <- 0
    
    result <- result %>% 
      group_by(Дата_агр) %>%
      summarise(Логисты = sum(Логисты),
                Рейсы = sum(Рейсы),
                Выручка = sum(Выручка),
                Маржа = sum(Маржа),
                Рентабельность = sum(Маржа) / sum(Выручка),
                ФОТ = sum(ФОТ),
                Штрафы = sum(Штрафы),
                Стоимость.денег = sum(Стоимость.денег),
                Мотивация = sum(Мотивация),
                Административные.расходы = sum(Административные.расходы),
                Разработка = sum(Разработка),
                США = sum(США),
                Авансирование = sum(Авансирование)
                       ) %>%
    
    mutate(Прибыль = Маржа + Авансирование - ФОТ - Штрафы -
      Стоимость.денег - Мотивация - Административные.расходы -
      Разработка - США,
           Прибыль_на_рейс = Прибыль / Рейсы,
           Прибыль_на_логиста = Прибыль / Логисты,
    Маржа1 = ФОТ + Штрафы + Стоимость.денег + Мотивация,
    Маржа1_на_рейс = Маржа1 / Рейсы,
    Маржа1_на_логиста = Маржа1 / Логисты,
    Маржа2 = Маржа1 + Административные.расходы + Разработка,
    Маржа2_на_рейс = Маржа2 / Рейсы,
    Маржа2_на_логиста = Маржа2 / Логисты,
    Маржа3 = Маржа2 + США,
    Маржа3_на_рейс = Маржа3 / Рейсы,
    Маржа3_на_логиста = Маржа3 / Логисты)
    
    
  })
  
  vc_data_df <- reactive({
  
  fot <- excel_data() %>%
    select(Дата_агр, ФОТ) %>%
    arrange(desc(Дата_агр)) %>%
    pivot_wider(names_from = Дата_агр,
                values_from = ФОТ,
                values_fill = 0)
  
  fines <- excel_data() %>%
    select(Дата_агр, Штрафы) %>%
    arrange(desc(Дата_агр)) %>%
    pivot_wider(names_from = Дата_агр,
                values_from = Штрафы,
                values_fill = 0)
  
  money_value <- excel_data() %>%
    select(Дата_агр, Стоимость.денег) %>%
    arrange(desc(Дата_агр)) %>%
    pivot_wider(names_from = Дата_агр,
                values_from = Стоимость.денег,
                values_fill = 0)
  
  motivation <- excel_data() %>%
    select(Дата_агр, Мотивация) %>%
    arrange(desc(Дата_агр)) %>%
    pivot_wider(names_from = Дата_агр,
                values_from = Мотивация,
                values_fill = 0)
  
  df <- tibble("Переменные издержки" = c("ФОТ", "Штрафы", "Стоимость денег", "Мотивация"))
  
  df1 <- rbind(fot, fines, money_value, motivation)
  
  result <- cbind(df, df1)
  
  return(result)
  
  })
  
  fc_data_df <- reactive({
    
    admin <- excel_data() %>%
      select(Дата_агр, Административные.расходы) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Административные.расходы,
                  values_fill = 0)
    
    developers <- excel_data() %>%
      select(Дата_агр, Разработка) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Разработка,
                  values_fill = 0)
    
    df <- tibble("Постоянные издержки" = c("Административные расходы", "Разработка"))
    
    df1 <- rbind(admin, developers)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  vc_payoff_data_df <- reactive({
    
    margin1 <- excel_data() %>%
      select(Дата_агр, Маржа1) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа1,
                  values_fill = 0)
    
    margin1_on_route <- excel_data() %>%
      select(Дата_агр, Маржа1_на_рейс) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа1_на_рейс,
                  values_fill = 0)
    
    margin1_on_logist <- excel_data() %>%
      select(Дата_агр, Маржа1_на_логиста) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа1_на_логиста,
                  values_fill = 0)
    
    df <- tibble("Окупаем VC" = c("Маржа", "Маржа на рейс", "Маржа на логиста"))
    
    df1 <- rbind(margin1, margin1_on_route, margin1_on_logist)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  
  fc_payoff_data_df <- reactive({
    
    margin2 <- excel_data() %>%
      select(Дата_агр, Маржа2) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа2,
                  values_fill = 0)
    
    margin2_on_route <- excel_data() %>%
      select(Дата_агр, Маржа2_на_рейс) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа2_на_рейс,
                  values_fill = 0)
    
    margin2_on_logist <- excel_data() %>%
      select(Дата_агр, Маржа2_на_логиста) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа2_на_логиста,
                  values_fill = 0)
    
    df <- tibble("Окупаем FC" = c("Маржа", "Маржа на рейс", "Маржа на логиста"))
    
    df1 <- rbind(margin2, margin2_on_route, margin2_on_logist)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  level3_df <- reactive({
    
    margin3 <- excel_data() %>%
      select(Дата_агр, Маржа3) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа3,
                  values_fill = 0)
    
    margin3_on_route <- excel_data() %>%
      select(Дата_агр, Маржа3_на_рейс) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа3_на_рейс,
                  values_fill = 0)
    
    margin3_on_logist <- excel_data() %>%
      select(Дата_агр, Маржа3_на_логиста) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Маржа3_на_логиста,
                  values_fill = 0)
    
    df <- tibble("Получаем прибыль" = c("Маржа", "Маржа на рейс", "Маржа на логиста"))
    
    df1 <- rbind(margin3, margin3_on_route, margin3_on_logist)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  
  profit_df <- reactive({
    
    profit <- excel_data() %>%
      select(Дата_агр, Прибыль) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Прибыль,
                  values_fill = 0)
    
    profit_on_route <- excel_data() %>%
      select(Дата_агр, Прибыль_на_рейс) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Прибыль_на_рейс,
                  values_fill = 0)
    
    profit_on_logist <- excel_data() %>%
      select(Дата_агр, Прибыль_на_логиста) %>%
      arrange(desc(Дата_агр)) %>%
      pivot_wider(names_from = Дата_агр,
                  values_from = Прибыль_на_логиста,
                  values_fill = 0)
    
    df <- tibble("Прибыль" = c("Общее значение", "На рейс", "На логиста"))
    
    df1 <- rbind(profit, profit_on_route, profit_on_logist)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  output$client_health_vc <- renderReactable(
    reactable(vc_data_df(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                'Переменные издержки' = colDef(
                  name = "Переменные издержки",
                  format = colFormat(suffix = "")
                )
              )
              )
  )
  
  output$client_health_fc <- renderReactable(
    reactable(fc_data_df(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                'Постоянные издержки' = colDef(
                  name = "Постоянные издержки",
                  format = colFormat(suffix = "")
                )
              )
    )
  )
  
  output$client_health_profit <- renderReactable(
    reactable(profit_df(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                'Прибыль' = colDef(
                  name = "Прибыль",
                  format = colFormat(suffix = "")
                )
              )
    )
  )
  
  output$client_health_vc_payoff <- renderReactable(
    reactable(vc_payoff_data_df(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                'Окупаем VC' = colDef(
                  name = "Окупаем VC",
                  format = colFormat(suffix = "")
                )
              )
    )
  )
  
  
  output$client_health_fc_payoff <- renderReactable(
    reactable(fc_payoff_data_df(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                'Окупаем FC' = colDef(
                  name = "Окупаем FC",
                  format = colFormat(suffix = "")
                )
              )
    )
  )
  
  output$client_health_level3 <- renderReactable(
    reactable(level3_df(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                'Получаем прибыль' = colDef(
                  name = "Получаем прибыль",
                  format = colFormat(suffix = "")
                )
              )
    )
  )
  
  # output$cost_table <- renderReactable(
  #   reactable(cost_data_df(),
  #             defaultColGroup = colGroup(headerClass = "group-header"),
  #             showSortIcon = FALSE,
  #             borderless = TRUE,
  #             class = "standings-table",
  #             columnGroups = list(
  #               colGroup(name = "Доходы", columns = c('Маржа', 'Авансирование')),
  #               colGroup(name = "Переменные издержки, VC", columns = c('ФОТ', 'Штрафы', 'Стоимость.денег', 'Мотивация')),
  #               colGroup(name = "Постоянные издержки, FC", columns = c('Административные.расходы', 'Разработка'))
  #             ),
  #             defaultColDef = colDef(class = "cell", headerClass = "header", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), style = list(fontWeight = 600, fontSize = 18)),
  #             columns = list(
  #               Стоимость.денег = colDef(name = 'Стоимость денег'),
  #               Административные.расходы = colDef(name = 'Административные расходы')
  #             )
  #             )
  # )
  # 
  # selected_income <- reactive({
  #   
  #   filtered <- client_health_table$data
  #   
  #   if (!is.null(input$client_health_choose)) {
  #     filtered <- filtered %>% filter(Клиент %in% input$client_health_choose)
  #   } 
  #   
  #   profit <- sum(filtered$`Маржа, расчёт`)
  #   
  #   revenue_share <- sum(filtered$Выручка) / sum(client_health_table$data$Выручка)
  #   
  #   df <- data.frame(
  #     Маржа = profit,
  #     Авансирование = input$advance_payment_input * revenue_share,
  #     ФОТ = input$fot_input * revenue_share,
  #     Штрафы = input$fines_input * revenue_share,
  #     Стоимость_денег = input$money_value_input * revenue_share,
  #     Мотивация = input$motivation_input * revenue_share,
  #     Админка = input$administrative_expenses_input * revenue_share,
  #     Разработка = input$development_input * revenue_share,
  #     Прибыль = profit + input$advance_payment_input * revenue_share - (input$fot_input + input$fines_input + input$money_value_input + input$motivation_input + input$administrative_expenses_input + input$development_input) * revenue_share
  #     )
  # })
  # 
  # output$client_health_income <- renderReactable(
  #   reactable(selected_income(),
  #             defaultColGroup = colGroup(headerClass = "group-header"),
  #             showSortIcon = FALSE,
  #             borderless = TRUE,
  #             class = "standings-table",
  #             columnGroups = list(
  #               colGroup(name = "Доходы", columns = c('Маржа', 'Авансирование')),
  #               colGroup(name = "Переменные издержки, VC", columns = c('ФОТ', 'Штрафы', 'Стоимость_денег', 'Мотивация')),
  #               colGroup(name = "Постоянные издержки, FC", columns = c('Админка', 'Разработка'))
  #             ),
  #             defaultColDef = colDef(class = "cell", headerClass = "header", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), style = list(fontWeight = 600, fontSize = 18)),
  #             columns = list(
  #               Стоимость_денег = colDef(name = 'Стоимость денег'),
  #               Админка = colDef(name = 'Административные расходы')
  #             )
  #             )
  # )
  # 
  # selected_VC_TC_chocolate <- reactive({
  #   
  #   filtered <- client_health_table$data
  #   
  #   if (!is.null(input$client_health_choose)) {
  #     filtered <- filtered %>% filter(Клиент %in% input$client_health_choose)
  #   } 
  #   
  #   routes <- sum(filtered$Рейсы)
  #   logists <- n_distinct(filtered$Логист)
  #   revenue <- sum(filtered$Выручка)
  #   
  #   revenue_share <- sum(filtered$Выручка) / sum(client_health_table$data$Выручка)
  #   
  #   profit_VC <- (input$fot_input + input$fines_input + input$money_value_input + input$motivation_input - input$advance_payment_input) * revenue_share
  #   profit_TC <- profit_VC + (input$administrative_expenses_input + input$development_input) * revenue_share
  #   profit_profit <- profit_TC + input$usa_input * revenue_share
  #   
  #   df <- data.frame(
  #     Уровень = c("Окупаем VC", "Окупаем TC", "Получаем прибыль"),
  #     Маржа = c(profit_VC, profit_TC, profit_profit),
  #     Маржа_рейс = c(profit_VC, profit_TC, profit_profit) / routes,
  #     Маржа_логист = c(profit_VC, profit_TC, profit_profit) / logists,
  #     Рентабельность = c(profit_VC, profit_TC, profit_profit) / revenue * 100
  #     )
  # })
  # 
  # output$client_health_levels <- renderReactable(
  #   reactable(selected_VC_TC_chocolate(),
  #             showSortIcon = FALSE,
  #             borderless = TRUE,
  #             class = "standings-table",
  #             rowStyle = function(index) {
  #               if (selected_VC_TC_chocolate()[index, "Уровень"] == "Окупаем VC") {
  #                 list(background = "rgba(255, 76, 91, 1)")
  #               } else if (selected_VC_TC_chocolate()[index, "Уровень"] == "Окупаем TC") {
  #                 list(background = "rgba(255, 161, 97, 1)")
  #               } else {
  #                 list(background = "rgba(144, 238, 144, 1)")
  #               }
  #             },
  #             defaultColDef = colDef(class = "cell", headerClass = "header", style = list(fontWeight = 600, fontSize = 18)),
  #             columns = list(
  #               Маржа = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
  #               Маржа_рейс = colDef(name = 'Маржа на рейс', format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
  #               Маржа_логист = colDef(name = 'Маржа на логиста', format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
  #               Рентабельность = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 2))
  #             )
  #             )
  # )
  
  selected_client_health_routes_dynamics <- reactive({
    
    aggregation <- switch(input$client_health_aggregation, 
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$client_health_choose)
    } 

    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Рейсы = sum(Рейсы)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рейсы,
                  values_fill = 0)
    
    filtered[mapply(is.infinite, filtered)] <- 0
    
    
    
    filtered1 <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Рейсы = sum(Рейсы) / n_distinct(Логист)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рейсы,
                  values_fill = 0)
    
    filtered1[mapply(is.infinite, filtered1)] <- 0
    
    df <- data.frame(Рейсы = c("Общее значение", "На логиста"))
    
    df1 <- rbind(filtered, filtered1)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  

  selected_client_health_revenue_dynamics <- reactive({
    
    aggregation <- switch(input$client_health_aggregation, 
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Выручка = sum(Выручка)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Выручка,
                  values_fill = 0)
    
    filtered[mapply(is.infinite, filtered)] <- 0
    
    
    filtered1 <- client_health_table$data %>%
      arrange(desc(Дата)) %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Выручка = sum(Выручка) / sum(Рейсы)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Выручка,
                  values_fill = 0)
    
    filtered1[mapply(is.infinite, filtered1)] <- 0
    
    
    filtered2 <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered2 <- filtered2 %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered2 <- filtered2 %>% group_by(Дата) %>%
      summarise(Выручка = sum(Выручка) / n_distinct(Логист)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Выручка,
                  values_fill = 0) 

    filtered2[mapply(is.infinite, filtered2)] <- 0
    
    df <- data.frame(Выручка = c("Общее значение", "На рейс", "На логиста"))
    
    df1 <- rbind(filtered, filtered1, filtered2)
    
    result <- cbind(df, df1)
    
    return(result)
  })
  
  
  selected_client_health_margin_dynamics <- reactive({
    
    aggregation <- switch(input$client_health_aggregation, 
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Маржа = sum(`Маржа, расчёт`)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Маржа,
                  values_fill = 0) 
    
    filtered[mapply(is.infinite, filtered)] <- 0
    
    
    filtered1 <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Маржа = sum(`Маржа, расчёт`) / sum(Рейсы)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Маржа,
                  values_fill = 0) 
    
    filtered1[mapply(is.infinite, filtered1)] <- 0
    
    
    filtered2 <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered2 <- filtered2 %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered2 <- filtered2 %>% group_by(Дата) %>%
      summarise(Маржа = sum(`Маржа, расчёт`) / n_distinct(Логист)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Маржа,
                  values_fill = 0) 
    
    filtered2[mapply(is.infinite, filtered2)] <- 0
    
    df <- data.frame(Маржа = c("Общее значение", "На рейс", "На логиста"))
    
    df1 <- rbind(filtered, filtered1, filtered2)
    
    result <- cbind(df, df1)
    
    return(result)
  })
  
  selected_client_health_routes_profitability <- reactive({
    
    aggregation <- switch(input$client_health_aggregation, 
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Рентабельность = sum(`Маржа, расчёт`) / sum(Выручка) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рентабельность,
                  values_fill = 0)
    
    filtered[mapply(is.infinite, filtered)] <- 0
    
    
    filtered1 <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$client_health_choose)) {
      filtered1 <- filtered1 %>% filter(Клиент %in% input$client_health_choose)
    } 
    
    
    filtered1 <- filtered1 %>% group_by(Дата) %>%
      summarise(Рентабельность = sum(`Маржа, ERP`) / sum(Оборот) * 100) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Рентабельность,
                  values_fill = 0)
    
    filtered1[mapply(is.infinite, filtered1)] <- 0
    
    df <- data.frame(Рентабельность = c("Очищенная", "ERP"))
    
    df1 <- rbind(filtered, filtered1)
    
    result <- cbind(df, df1)
    
    return(result)
    
  })
  
  selected_client_health_logist_dynamics <- reactive({

    aggregation <- switch(input$client_health_aggregation,
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")

    filtered <- client_health_table$data %>%
      mutate(Дата = format(Дата, aggregation))

    if (!is.null(input$client_health_choose)) {
      filtered <- filtered %>% filter(Клиент %in% input$client_health_choose)
    }


    filtered <- filtered %>% group_by(Дата) %>%
      summarise(Логисты = n_distinct(Логист)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Логисты,
                  values_fill = 0)

    filtered[mapply(is.infinite, filtered)] <- 0


    # filtered1 <- client_health_table$data %>%
    #   mutate(Дата = format(Дата, aggregation))
    # 
    # if (!is.null(input$client_health_choose)) {
    #   filtered1 <- filtered1 %>% filter(Клиент %in% input$client_health_choose)
    # }
    # 
    # 
    # filtered1 <- filtered1 %>% group_by(Дата) %>%
    #   summarise(Логисты = toString(unique(Логист))) %>%
    #   arrange(desc(Дата)) %>%
    #   pivot_wider(names_from = Дата,
    #               values_from = Логисты,
    #               values_fill = 0)
    # 
    # filtered1[mapply(is.infinite, filtered1)] <- 0



    df <- data.frame(Логисты = c("Количество"))

    df1 <- rbind(filtered)

    result <- cbind(df, df1)

    return(result)
  })
  
  output$client_health_routes <- renderReactable(
    reactable(selected_client_health_routes_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = ""),
              columns = list(
                Рейсы = colDef(
                  name = "Рейсы",
                  format = colFormat(suffix = "")
                )
              )
              )
  )
  
  output$client_health_logists <- renderReactable(
    reactable(selected_client_health_logist_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header")
              )
  )
  
  output$client_health_revenue <- renderReactable(
    reactable(selected_client_health_revenue_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                Выручка = colDef(
                  name = "Выручка",
                  format = colFormat(suffix = "")
                )
              )

    )
  )
  
  output$client_health_margin <- renderReactable(
    reactable(selected_client_health_margin_dynamics(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
              columns = list(
                Маржа = colDef(
                  name = "Маржа",
                  format = colFormat(suffix = "")
                )
              )
              
              )
  )
  
  output$client_health_profitability <- renderReactable(
    reactable(selected_client_health_routes_profitability(),
              defaultColDef = colDef(class = "cell", headerClass = "header", name = "", format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
              columns = list(
                Рентабельность = colDef(
                  name = "Рентабельность",
                  format = colFormat(suffix = "")
                )
              )
              )
  )
  
  # Дебиторская задолженность
  
  output$total_confirmed_receivables <- renderValueBox({
    valueBox(
      format(sum(type_dz_filter_df()$Сумма), big.mark=" "), 
      "Подтверждённая ДЗ", 
      icon = icon("ruble"),
      color = "purple"
    )
  })
  
  output$total_overdue_receivables <- renderValueBox({
    valueBox(
      format(sum(filter(type_dz_filter_df(), Категория == "ПрДЗ")$Сумма), big.mark=" "), 
      "Просроченная ДЗ", 
      icon = icon("ruble"),
      color = "yellow"
    )
  })
  
  output$total_unconfirmed_receivables <- renderValueBox({
    valueBox(
      format(sum(DZ_unconfirmed_receivables_table$data$Всего), big.mark=" "), 
      "Неподтверждённая ДЗ", 
      icon = icon("ruble"),
      color = "aqua"
    )
  })
  
  output$dz_receivable_turnover <- renderReactable(
    reactable(DZ_receivable_turnover_table$data,
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE,
              pagination = FALSE,
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold", position = "top")),
              columns = list(
                Контрагент = colDef(
                  footer = "Всего",
                  minWidth = 250,
                  style = list(fontWeight = 600)
                ),
                "Ожидание ТСД" = colDef(format = colFormat(separators = TRUE, digits = 2), footer = function(values) format(mean(values, na.rm = TRUE), big.mark=" ", digits = 2, scientific=FALSE)),
                "Проверка ТСД" = colDef(format = colFormat(separators = TRUE, digits = 2), footer = function(values) format(mean(values, na.rm = TRUE), big.mark=" ", digits = 2, scientific=FALSE)),
                "Формирование счёта" = colDef(format = colFormat(separators = TRUE, digits = 2), footer = function(values) format(mean(values, na.rm = TRUE), big.mark=" ", digits = 2, scientific=FALSE)),
                "Отправление счёта" = colDef(format = colFormat(separators = TRUE, digits = 2), footer = function(values) format(mean(values, na.rm = TRUE), big.mark=" ", digits = 2, scientific=FALSE)),
                "Доставка счёта" = colDef(format = colFormat(separators = TRUE, digits = 2), footer = function(values) format(mean(values, na.rm = TRUE), big.mark=" ", digits = 2, scientific=FALSE)),
                "Оплата счёта" = colDef(format = colFormat(separators = TRUE, digits = 2), footer = function(values) format(mean(values, na.rm = TRUE), big.mark=" ", digits = 2, scientific=FALSE)),
                "Оплата заявки" = colDef(format = colFormat(separators = TRUE, digits = 2), footer = function(values) format(mean(values, na.rm = TRUE), big.mark=" ", digits = 2, scientific=FALSE))
              )
              )
    )
  
  type_dz_filter_df <- reactive({
    filtered <- DZ_confirmed_receivables_table$data
    if (!is.null(input$type_dz_filter)) {
      filtered <- filtered %>% filter(Категория %in% input$type_dz_filter)
    }
    filtered <- filtered %>%
      group_by(Контрагент, `Номер счёта`, Сумма, 
               `Дата доставки`, `Отсрочка платежа`, `Скрытая отсрочка`, 
               `Плановая дата оплаты`, `Отклонение срока оплаты`) %>%
      summarise(Категория = first(Категория))
    return(filtered)
  })
  
  type_dz_filter_df1 <- reactive({
    filtered <- DZ_confirmed_receivables_table$data
    if (!is.null(input$type_dz_filter)) {
      filtered <- filtered %>% filter(Категория %in% input$type_dz_filter)
    }
    filtered <- filtered %>%
      select(`Номер счёта`, `Номер заявки`, `Ставка клиента`, request_id)
    return(filtered)
  })
  
  output$dz_confirmed_receivables <- renderReactable(
    reactable(type_dz_filter_df(),
              details = function(index) {
                nested_table <- type_dz_filter_df1()[type_dz_filter_df1()$`Номер счёта` == type_dz_filter_df()$`Номер счёта`[index], ]
                htmltools::div(style = "padding: 1rem",
                               reactable(nested_table, 
                                         outlined = TRUE,
                                         pagination = FALSE,
                                         columns = list(
                                           'Номер заявки' = colDef(cell = function(value, index) {
                                             report_erp_link(value, paste0("/#/requests/", nested_table[index, "request_id"]), ERP_BASE_URL)
                                           }),
                                           'Ставка клиента' = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                                           request_id = colDef(show = FALSE)
                                         )
                                         )
                )
              },
              groupBy = c("Контрагент"),
              defaultSorted = "Сумма",
              searchable = TRUE,
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE,
              pagination = FALSE,
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold", position = "top")),
              columns = list(
                Контрагент = colDef(
                  footer = "Всего",
                  minWidth = 250,
                  style = list(fontWeight = 600)
                ),
                "Номер счёта" = colDef(name = "Кол-во счетов", aggregate = "count"),
                Сумма = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                "Отсрочка платежа" = colDef(aggregate = "unique"),
                Категория = colDef(aggregate = "unique")
                )
              )
  )
  
  output$dz_PaymentSchedule <- renderReactable({
      reactable(type_dz_filter_df(),
                details = function(index) {
                  nested_table <- type_dz_filter_df1()[type_dz_filter_df1()$`Номер счёта` == type_dz_filter_df()$`Номер счёта`[index], ]
                  htmltools::div(style = "padding: 1rem",
                                 reactable(nested_table, 
                                           outlined = TRUE,
                                           pagination = FALSE,
                                           columns = list(
                                             'Номер заявки' = colDef(cell = function(value, index) {
                                               report_erp_link(value, paste0("/#/requests/", nested_table[index, "request_id"]), ERP_BASE_URL)
                                             }),
                                             'Ставка клиента' = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                                             request_id = colDef(show = FALSE)
                                           )
                                 )
                  )
                },
                defaultSorted = c("Плановая дата оплаты", "Сумма"),
                groupBy = c("Плановая дата оплаты", "Контрагент"),
                striped = TRUE,
                bordered = TRUE,
                borderless = FALSE,
                highlight = TRUE,
                resizable = FALSE,
                pagination = FALSE,
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold", position = "top")),
                columns = list(
                  "Плановая дата оплаты" = colDef(defaultSortOrder = "desc",                   footer = "Всего",
                                                  minWidth = 250,
                                                  style = list(fontWeight = 600)),
                  Сумма = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                  "Отклонение срока оплаты" = colDef(aggregate = "unique")
                ))
    
  })
  

  output$dz_unconfirmed_receivables <- renderReactable(
    reactable(DZ_unconfirmed_receivables_table$data,
              defaultSorted = "Всего",
              defaultPageSize = 10,
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE,
              pagination = TRUE,
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold", position = "top")),
              columns = list(
                Контрагент = colDef(
                  footer = "Всего",
                  minWidth = 250,
                  style = list(fontWeight = 600)
                ),
                'В ожидании.x' = colDef(name = "В ожидании",
                                        header = function(value) {
                                          report_erp_link(value, "/#/document-flow/requests/awaiting", ERP_BASE_URL, title = "Переход на вкладку «В ожидании» раздела «Счета» ERP")
                                        },
                                        cell = function(value, index) {
                  days <- DZ_unconfirmed_receivables_table$data$`В ожидании.y`[index]
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(days, big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }, defaultSortOrder = "desc", format = colFormat(separators = TRUE, digits = 2), 
                footer = function(values) {
                  days <- DZ_unconfirmed_receivables_table$data$`В ожидании.y`
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(sum(values), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(mean(days, na.rm = TRUE), big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }),
                Полученные.x = colDef(name = "Полученные", 
                                      header = function(value) {
                                        report_erp_link(value, "/#/document-flow/requests/received", ERP_BASE_URL, title = "Переход на вкладку «Полученные» раздела «Счета» ERP")
                                      },
                                      cell = function(value, index) {
                  days <- DZ_unconfirmed_receivables_table$data$`Полученные.y`[index]
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(days, big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }, format = colFormat(separators = TRUE, digits = 2),
                footer = function(values) {
                  days <- DZ_unconfirmed_receivables_table$data$`Полученные.y`
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(sum(values), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(mean(days, na.rm = TRUE), big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }
                ),
                Проверенные.x = colDef(name = "Проверенные",
                                       header = function(value) {
                                         report_erp_link(value, "/#/document-flow/requests/approved", ERP_BASE_URL, title = "Переход на вкладку «Проверенные» раздела «Счета» ERP")
                                       },
                                       cell = function(value, index) {
                  days <- DZ_unconfirmed_receivables_table$data$`Проверенные.y`[index]
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(days, big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }, format = colFormat(separators = TRUE, digits = 2), 
                footer = function(values) {
                  days <- DZ_unconfirmed_receivables_table$data$`Проверенные.y`
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(sum(values), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(mean(days, na.rm = TRUE), big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }
                ),
                'Проблемные (пер.).x' = colDef(name = "Проблемные (пер.)",
                                    header = function(value) {
                                      report_erp_link(value, "/#/document-flow/requests/problem", ERP_BASE_URL, title = "Переход на вкладку «Проблемы» раздела «Счета» ERP")
                                    },
                                    cell = function(value, index) {
                  days <- DZ_unconfirmed_receivables_table$data$`Проблемные (пер.).y`[index]
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(days, big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }, format = colFormat(separators = TRUE, digits = 2), 
                footer = function(values) {
                  days <- DZ_unconfirmed_receivables_table$data$`Проблемные (пер.).y`
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(sum(values), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(mean(days, na.rm = TRUE), big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }
                ),
                Сформированные.x = colDef(name = "Сформированные", 
                                          header = function(value) {
                                            report_erp_link(value, "/#/document-flow/bills/formed", ERP_BASE_URL, title = "Переход на вкладку «Сформированные» раздела «Счета» ERP")
                                          },
                                          cell = function(value, index) {
                  days <- DZ_unconfirmed_receivables_table$data$`Сформированные.y`[index]
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(days, big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }, format = colFormat(separators = TRUE, digits = 2), 
                footer = function(values) {
                  days <- DZ_unconfirmed_receivables_table$data$`Сформированные.y`
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(sum(values), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(mean(days, na.rm = TRUE), big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }
                ),
                Отправленные.x = colDef(name = "Отправленные", 
                                        header = function(value) {
                                          report_erp_link(value, "/#/document-flow/bills/sent", ERP_BASE_URL, title = "Переход на вкладку «Отправленные» раздела «Счета» ERP")
                                        },
                                        cell = function(value, index) {
                  days <- DZ_unconfirmed_receivables_table$data$`Отправленные.y`[index]
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(days, big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }, format = colFormat(separators = TRUE, digits = 2), 
                footer = function(values) {
                  days <- DZ_unconfirmed_receivables_table$data$`Отправленные.y`
                  days <- if (!is.na(days)) days else 0
                  div(
                    div(style = list(fontWeight = 600), paste(format(sum(values), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 12), paste(format(mean(days, na.rm = TRUE), big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                  )
                }
                ),
                'Проблемные (кл.).x' = colDef(name = "Проблемные (кл.)",
                                               header = function(value) {
                                                 report_erp_link(value, "/#/document-flow/bills/problem", ERP_BASE_URL, title = "Переход на вкладку «Проблемы» раздела «Счета» ERP")
                                               },
                                               cell = function(value, index) {
                                                 days <- DZ_unconfirmed_receivables_table$data$`Проблемные (кл.).y`[index]
                                                 days <- if (!is.na(days)) days else 0
                                                 div(
                                                   div(style = list(fontWeight = 600), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                                                   div(style = list(fontSize = 12), paste(format(days, big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                                                 )
                                               }, format = colFormat(separators = TRUE, digits = 2), 
                                               footer = function(values) {
                                                 days <- DZ_unconfirmed_receivables_table$data$`Проблемные (кл.).y`
                                                 days <- if (!is.na(days)) days else 0
                                                 div(
                                                   div(style = list(fontWeight = 600), paste(format(sum(values), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                                                   div(style = list(fontSize = 12), paste(format(mean(days, na.rm = TRUE), big.mark=" ", digits = 2, scientific = FALSE), "дней"))
                                                 )
                                               }
                ),
                'В ожидании.y' = colDef(show = FALSE),
                 Полученные.y = colDef(show = FALSE),
                 Проверенные.y = colDef(show = FALSE),
                 'Проблемные (пер.).y' = colDef(show = FALSE),
                 Сформированные.y = colDef(show = FALSE),
                 Отправленные.y = colDef(show = FALSE),
                'Проблемные (кл.).y' = colDef(show = FALSE),
                Всего = colDef(style = list(fontWeight = 600), defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))
              )
    )
  )
  
  output$download_newDZ <- downloadHandler(
    filename = function() {
      paste("ДЗ - ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Формирование excel-файла", value = 0.2)
      
      startDate <- as.POSIXct(paste(input$date_newDZ[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$date_newDZ[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
      
      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC" 
      
      progress$inc(0.6, detail = "Данные получены")
      
      progress$inc(0.8, detail = "Начало записи файла")
      
      write_xlsx(list("Неоплаченные заявки" = getDZ_unconfirmed_receivables_detail(startDate, endDate), "Неоплаченные счета" = getDZ_confirmed_receivables_detail()), file)
      
      progress$inc(1, detail = "Файл сформирован")
      

    }
  )
  
  
  ########################################
  ### Экономический отчёт (Дашборд ДЗ) ###
  ########################################
  
  # overdue_table_result <- reactive({
  #   overdue_dz_table$data %>%
  #     filter(Категория == "ПрДЗ") %>%
  #     mutate(Сроки = case_when(`Отклонение срока оплаты` < 0 & `Отклонение срока оплаты` >= -7 ~ "До 7 дней",
  #                              `Отклонение срока оплаты` < -7 & `Отклонение срока оплаты` >= -15 ~ "От 7 до 15 дней",
  #                              `Отклонение срока оплаты` < -15 & `Отклонение срока оплаты` >= -30 ~ "От 16 до 30 дней",
  #                              `Отклонение срока оплаты` < -30 ~ "Больше 30 дней",
  #     )) %>%
  #     group_by(Контрагент, Сроки) %>%
  #     
  #     summarise(Сумма = sum(Сумма)) %>%
  #     
  #     pivot_wider(names_from = Сроки,
  #                 values_from = Сумма,
  #                 values_fill = 0) %>%
  #     
  #     select(Контрагент, `До 7 дней`, `От 7 до 15 дней`, `От 16 до 30 дней`, `Больше 30 дней`) %>%
  #     
  #     mutate(Всего = `До 7 дней` + `От 7 до 15 дней` + `От 16 до 30 дней` + `Больше 30 дней`)
  # })
  
  # Просроченная ДЗ по клиентам по срокам
  output$overdue_table <- renderReactable(
    reactable(overdue_dz_table$data,
              defaultPageSize = 5,
              defaultColDef = colDef(class = "cell", headerClass = "header", footerStyle = list(fontWeight = "bold", position = "top")),
              defaultSorted = "Всего",
              columns = list(
                Контрагент = colDef(
                  footer = "Всего",
                  minWidth = 240,
                  style = list(fontWeight = 600)
                ),
                'До 7 дней' = colDef(style = function(value) {
                  normalized <- (value - min(overdue_dz_table$data$`До 7 дней`)) / (max(overdue_dz_table$data$`До 7 дней`) - min(overdue_dz_table$data$`До 7 дней`))
                  color <- BuYlRd(normalized)
                  list(background = color)
                }, format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                'От 7 до 15 дней' = colDef(style = function(value) {
                  normalized <- (value - min(overdue_dz_table$data$`От 7 до 15 дней`)) / (max(overdue_dz_table$data$`От 7 до 15 дней`) - min(overdue_dz_table$data$`От 7 до 15 дней`))
                  color <- BuYlRd(normalized)
                  list(background = color)
                }, format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                'От 16 до 30 дней' = colDef(style = function(value) {
                  normalized <- (value - min(overdue_dz_table$data$`От 16 до 30 дней`)) / (max(overdue_dz_table$data$`От 16 до 30 дней`) - min(overdue_dz_table$data$`От 16 до 30 дней`))
                  color <- BuYlRd(normalized)
                  list(background = color)
                }, format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                'Больше 30 дней' = colDef(style = function(value) {
                  normalized <- (value - min(overdue_dz_table$data$`Больше 30 дней`)) / (max(overdue_dz_table$data$`Больше 30 дней`) - min(overdue_dz_table$data$`Больше 30 дней`))
                  color <- BuYlRd(normalized)
                  list(background = color)
                }, format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                Всего = colDef(style = function(value) {
                  normalized <- (value - min(overdue_dz_table$data$Всего)) / (max(overdue_dz_table$data$Всего) - min(overdue_dz_table$data$Всего))
                  color <- BuYlRd(normalized)
                  list(background = color, fontWeight = 600)
                }, defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))
                
              )
              
    )
  )
  
  health_dz_general <- reactive({
    tibble(Всего = "Всего", 
                НДЗ = sum(health_dz_table$data$НДЗ),
                ПДЗ = sum(health_dz_table$data$ПДЗ),
                ПрДЗ = sum(health_dz_table$data$ПрДЗ),
                'НДЗ/ПДЗ' = sum(health_dz_table$data$НДЗ)/sum(health_dz_table$data$ПДЗ),
                'ПрДЗ/ПДЗ' = sum(health_dz_table$data$ПрДЗ)/sum(health_dz_table$data$ПДЗ),
                'Стоимость' = sum(health_dz_table$data$ПрДЗ) * (input$factoring_rate_numeric_input + 100) / 356
                
                # 'Оплата счёта, дни' = mean(health_dz_table$data$`Оплата счёта, дни`, na.rm = TRUE),
                # 'Оплата заявки, дни' = mean(health_dz_table$data$`Оплата заявки, дни`, na.rm = TRUE)                         
    )
  })
  
  output$health_DZ_table_general <- renderReactable(
    reactable(tibble(Всего = "Всего", 
                              НДЗ = sum(health_dz_table$data$НДЗ),
                              ПДЗ = sum(health_dz_table$data$ПДЗ),
                              ПрДЗ = sum(health_dz_table$data$ПрДЗ),
                              'НДЗ/ПДЗ' = sum(health_dz_table$data$НДЗ)/sum(health_dz_table$data$ПДЗ),
                              'ПрДЗ/ПДЗ' = sum(health_dz_table$data$ПрДЗ)/sum(health_dz_table$data$ПДЗ),
                              'Стоимость 1-го дня просрочки' = sum(health_dz_table$data$ПрДЗ) * (input$factoring_rate_numeric_input / 100) / 365,
                              'ПДЗ сейчас' = sum(dz_turnover_table$data$ПДЗ_сейчас),
                              'ПДЗ раньше' = sum(dz_turnover_table$data$ПДЗ_раньше),
                              'Оборот' = sum(dz_turnover_table$data$Оборот),
                              'Оборачиваемость' = as.numeric(Sys.Date() - input$turnover_date) / (sum(dz_turnover_table$data$Оборот) / ((sum(dz_turnover_table$data$ПДЗ_сейчас) + sum(dz_turnover_table$data$ПДЗ_раньше)) / 2))
                          ),
              defaultColDef = colDef(class = "cell", headerClass = "header", style = list(fontWeight = 600, fontSize = 18)),
              columns = list(
                Всего = colDef(
                  name = "",
                  minWidth = 200,
                  style = list(fontWeight = 600)
                ),
                'НДЗ' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПДЗ' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПрДЗ' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'НДЗ/ОДЗ' = colDef(format = colFormat(percent = TRUE, digits = 0)),
                'ПрДЗ/ПДЗ' = colDef(format = colFormat(percent = TRUE, digits = 0)),
                'НДЗ/ПДЗ' = colDef(format = colFormat(digits = 2), defaultSortOrder = "desc"),
                'Стоимость 1-го дня просрочки' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПДЗ сейчас' = colDef(name = paste("ПДЗ", format(Sys.Date(), "%d.%m.%Y")), format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПДЗ раньше' = colDef(name = paste("ПДЗ", format(input$turnover_date, "%d.%m.%Y")), format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'Оборот' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'Оборачиваемость' = colDef(format = colFormat(digits = 2), defaultSortOrder = "desc")
              )
              )
  )
  
  main_table <- reactive({
    
    health_dz_table$data %>% 
      mutate(Стоимость = ПрДЗ * (input$factoring_rate_numeric_input / 100) / 356) %>%
      left_join(dz_turnover_table$data, by = c("Контрагент" = "Контрагент"))
    
  })
  
  output$health_DZ_table_by_client <- renderReactable(
    reactable(main_table(),
              columnGroups = list(
                # colGroup(name = "Здоровье", columns = c('НДЗ', 'ПДЗ', 'ПрДЗ', 'НДЗ/ОДЗ'))
                colGroup(name = "Оборачиваемость", columns = c('ПДЗ_сейчас', 'ПДЗ_раньше', 'Оборот', 'Оборачиваемость'))
              ),
              defaultSorted = "НДЗ/ПДЗ",
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              columns = list(
                Контрагент = colDef(
                  minWidth = 200,
                  style = list(fontWeight = 600)
                ),
                'НДЗ' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПДЗ' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПрДЗ' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'НДЗ/ОДЗ' = colDef(format = colFormat(percent = TRUE, digits = 0)),
                'ПрДЗ/ПДЗ' = colDef(format = colFormat(percent = TRUE, digits = 0)),
                'НДЗ/ПДЗ' = colDef(format = colFormat(digits = 2), defaultSortOrder = "desc"),
                'Стоимость' = colDef(name = "Стоимость 1-го дня просрочки", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПДЗ_сейчас' = colDef(name = paste("ПДЗ", format(Sys.Date(), "%d.%m.%Y")), format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'ПДЗ_раньше' = colDef(name = paste("ПДЗ", format(input$turnover_date, "%d.%m.%Y")), format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'Оборот' = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                'Оборачиваемость' = colDef(format = colFormat(digits = 2), defaultSortOrder = "desc")
                # 'Оплата счёта, дни' = colDef(format = colFormat(separators = TRUE, digits = 2)),
                # 'Оплата заявки, дни' = colDef(format = colFormat(separators = TRUE, digits = 2)),

              )
    )
  )

  
  # Невыставленные счета
  output$unformed_bills_table <- renderReactable(
    reactable(
      unformed_bills_table$data,
      groupBy = c("Контрагент"),
      defaultSorted = "Всего",
      defaultPageSize = 5,
      defaultColDef = colDef(class = "cell", 
                             headerClass = "header", 
                             defaultSortOrder = "desc", 
                             format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                             aggregate = 'sum',
                             footerStyle = list(fontWeight = "bold"),
                             footer = function(values) if (is.numeric(values)) {format(sum(values), big.mark=" ", scientific=FALSE, digits = 0)}),
      columns = list(
        Контрагент = colDef(
          format = colFormat(suffix = ""),
          footer = "Всего",
          minWidth = 240,
          style = list(fontWeight = 600)
        ),
        Статус = colDef(format = colFormat(suffix = ""), aggregate = 'unique'),
        "Текущий год" = colDef(name = as.character(as.POSIXlt(Sys.time())$year + 1900),
                             style = function(value) {
        normalized <- (value - min(unformed_bills_table$data$`Текущий год`)) / (max(unformed_bills_table$data$`Текущий год`) - min(unformed_bills_table$data$`Текущий год`))
        color <- orange_pal(normalized)
        list(background = color)
        }
        ),
        Ранее = colDef(
          style = function(value) {
          normalized <- (value - min(unformed_bills_table$data$Ранее)) / (max(unformed_bills_table$data$Ранее) - min(unformed_bills_table$data$Ранее))
          color <- orange_pal(normalized)
          list(background = color)
        }
        ),
        Всего = colDef(
          style = function(value) {
          normalized <- (value - min(unformed_bills_table$data$Всего)) / (max(unformed_bills_table$data$Всего) - min(unformed_bills_table$data$Всего))
          color <- orange_pal(normalized)
          list(background = color)
        }
        )
        
        )
    )
  )
  
  # Воронка НДЗ
  
  output$funnel_ndz_table <- renderReactable(
    reactable(funnel_NDZ_table$data %>%
                mutate(ОК = case_when(Дни <= Цели ~ TRUE, Дни > Цели ~ FALSE)),
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              columns = list(
                Статус = colDef(cell = function(value, index) {
                  div(
                    class = "manager",
                    div(class = "manager-name", value)
                  )
                }),
                Сумма = colDef(style = function(value) {
                  normalized <- (value - min(funnel_NDZ_table$data$Сумма)) / (max(funnel_NDZ_table$data$Сумма) - min(funnel_NDZ_table$data$Сумма))
                  color <- orange_pal(normalized)
                  list(background = color)
                }, cell = function(value, index) {
                  share <- funnel_NDZ_table$data$Доля[index]
                  share <- if (!is.na(share)) share else 0
                  div(
                    div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
                    div(style = list(fontSize = 16), paste(format(share * 100, big.mark=" ", digits = 2, scientific = FALSE), "%"))
                  )
                }),
                Доля = colDef(show = FALSE),
                Рейсы = colDef(format = colFormat(separators = TRUE, digits = 0)),
                Дни = colDef(format = colFormat(separators = TRUE, digits = 2),
                             style = function(value) {
                  normalized <- (value - min(funnel_NDZ_table$data$Дни)) / (max(funnel_NDZ_table$data$Дни) - min(funnel_NDZ_table$data$Дни))
                  color <- BuYlRd(normalized)
                  list(background = color, fontWeight = 600, fontSize = 18)
                }),
                
                Цели = colDef(format = colFormat(separators = TRUE, digits = 2),
                             style = function(value) {
                               list(fontWeight = 600, fontSize = 18)
                             }),
                ОК = colDef(cell = function(value) {
                  if (value == FALSE) "\u274c Нет" else "\u2714\ufe0f Да"
                },
                style = function(value) {
                  list(fontWeight = 600, fontSize = 18)
                }
                )
              )
    )
  )
  
  output$average_payment_rate <- renderReactable(
    reactable(get_payment_rate() %>%
                mutate(ОК = case_when(`Скорость оплаты от даты выгрузки` <= Цели ~ TRUE, `Скорость оплаты от даты выгрузки` > Цели ~ FALSE)),
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              columns = list(
                "Скорость оплаты от даты выгрузки" = colDef(align = "center", format = colFormat(separators = TRUE, digits = 2),
                             style = function(value) {
                               list(fontWeight = 600, fontSize = 32)
                             }),
                Цели = colDef(align = "center", format = colFormat(separators = TRUE, digits = 2),
                                  style = function(value) {
                                    list(fontWeight = 600, fontSize = 32)
                                  }),
                ОК = colDef(cell = function(value) {
                  if (value == FALSE) "\u274c Нет" else "\u2714\ufe0f Да"
                },
                style = function(value) {
                  list(fontWeight = 600, fontSize = 32)
                }
                )
              )
              )
  )
  
  
  
  ############################
  ### Оборот и прибыль   ####  
  ###########################
  
  # Предобработка данных для отображения их на главной таблице отчёта
  selected_RevenueProfit_forTable <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      filtered <- RevenueProfitTable$data
      if (!is.null(input$typeForRevenueProfit)) {
        filtered <- filtered %>% filter(`Тип заявки` %in% input$typeForRevenueProfit)
      } 
      if (!is.null(input$geographyForRevenueProfit)) {
        filtered <- filtered %>% filter(География %in% input$geographyForRevenueProfit)
      }  
      if (!is.null(input$managerForRevenueProfit)) {
        filtered <- filtered %>% filter(Менеджер %in% input$managerForRevenueProfit)
      }  
      if (!is.null(input$logistForRevenueProfit)) {
        filtered <- filtered %>% filter(Логист %in% input$logistForRevenueProfit)
      } 
      filtered %>% group_by(Клиент, `Тип заявки`) %>%
        summarise(Оборот = sum(Оборот),
                  Маржа = sum(Маржа),
                  Рейсы = sum(Рейсы),
                  Рентабельность = report_ratio(sum(Маржа), sum(Оборот)) * 100,
                  "Стоимость рейса" = report_ratio(sum(Оборот), sum(Рейсы))
                  )
    }
  })
  
  
  selected_RevenueProfit_forTable1 <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      filtered <- RevenueProfitTable$data %>% 
        filter(`Тип заявки` %in% 'Спот')
      if (!is.null(input$geographyForRevenueProfit)) {
        filtered <- filtered %>% filter(География %in% input$geographyForRevenueProfit)
      }  
      if (!is.null(input$managerForRevenueProfit)) {
        filtered <- filtered %>% filter(Менеджер %in% input$managerForRevenueProfit)
      }  
      if (!is.null(input$logistForRevenueProfit)) {
        filtered <- filtered %>% filter(Логист %in% input$logistForRevenueProfit)
      } 
      filtered %>% group_by(Клиент, `Тип заявки`) %>%
        summarise(Оборот = sum(Оборот),
                        Маржа = sum(Маржа),
                        Рейсы = sum(Рейсы),
                        Рентабельность = report_ratio(sum(Маржа), sum(Оборот)) * 100,
                        "Стоимость рейса" = report_ratio(sum(Оборот), sum(Рейсы))
        )
    }
  })
  
  # Render a bar chart with a label on the left
  bar_chart <- function(label, width = "100%", height = "16px", fill = "#00bfc4", background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
  }
  
  # Render a bar chart with positive and negative values
  bar_chart_pos_neg <- function(label, value, max_value = 1, height = "16px",
                                pos_fill = "#005ab5", neg_fill = "#dc3220") {
    neg_chart <- div(style = list(flex = "1 1 0"))
    pos_chart <- div(style = list(flex = "1 1 0"))
    width <- paste0(abs(value / max_value) * 100, "%")
    
    if (value < 0) {
      bar <- div(style = list(marginLeft = "8px", background = neg_fill, width = width, height = height))
      chart <- div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
      neg_chart <- tagAppendChild(neg_chart, chart)
    } else {
      bar <- div(style = list(marginRight = "8px", background = pos_fill, width = width, height = height))
      chart <- div(style = list(display = "flex", alignItems = "center"), bar, label)
      pos_chart <- tagAppendChild(pos_chart, chart)
    }
    
    div(style = list(display = "flex"), neg_chart, pos_chart)
  }
  
  bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%", align = c("left", "right"), color = NULL) {
    align <- match.arg(align)
    if (align == "left") {
      position <- paste0(width * 100, "%")
      image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
    } else {
      position <- paste0(100 - width * 100, "%")
      image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
    }
    list(
      backgroundImage = image,
      backgroundSize = paste("100%", height),
      backgroundRepeat = "no-repeat",
      backgroundPosition = "center",
      color = color
    )
  }
  
  output$revenue_profit_table <- renderReactable(
    reactable(selected_RevenueProfit_forTable(),
              defaultSorted = "Оборот",
              groupBy = c("Клиент"),
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE,
              pagination = FALSE,
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              columns = list(
                Клиент = colDef(
                  minWidth = 270,
                  style = list(fontWeight = 600)
                ),
                "Тип заявки" = colDef(aggregate = "unique"),
                Оборот = colDef(defaultSortOrder = "desc", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum", align = "left",
                                            style = function(value) {
                                              bar_style(width = value / max(selected_RevenueProfit_forTable()$Оборот), fill = "#87cefa")
                                            },
                ),
                Маржа = colDef(
                  style = function(value) {
                    bar_style(width = value / max(selected_RevenueProfit_forTable()$Маржа), fill = "hsl(208, 70%, 90%)")
                  }, format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"),
                Рентабельность = colDef(format = colFormat(separators = TRUE, suffix = " %", digits = 2), aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа']
                                              totalUnits += row['Оборот']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }")
                ),
                Рейсы = colDef(
                  style = function(value) {
                    bar_style(width = value / max(selected_RevenueProfit_forTable()$Рейсы), height = "90%", align = "right")
                  },
                  aggregate = "sum"),
                "Стоимость рейса" = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Оборот']
                                              totalUnits += row['Рейсы']
                                            })
                                            return totalPrice / totalUnits
                                          }")
                )
              )
    )
  )
  
  selected_RevenueProfit_forTable_general <- reactive({
    tibble(
      Оборот = sum(selected_RevenueProfit_forTable()$Оборот),
      Маржа = sum(selected_RevenueProfit_forTable()$Маржа),
      Рейсы = sum(selected_RevenueProfit_forTable()$Рейсы),
      Рентабельность = report_ratio(sum(selected_RevenueProfit_forTable()$Маржа),
        sum(selected_RevenueProfit_forTable()$Оборот)) * 100,
      "Стоимость рейса" = report_ratio(sum(selected_RevenueProfit_forTable()$Оборот),
        sum(selected_RevenueProfit_forTable()$Рейсы))
    ) %>%
      mutate(
        Доля_оборот_спот = report_ratio(sum(selected_RevenueProfit_forTable1()$Оборот), Оборот) * 100,
        Доля_маржа_спот = report_ratio(sum(selected_RevenueProfit_forTable1()$Маржа), Маржа) * 100,
        Доля_рейсы_спот = report_ratio(sum(selected_RevenueProfit_forTable1()$Рейсы), Рейсы) * 100,
        Доля_рентабельность_спот = report_ratio(report_ratio(sum(selected_RevenueProfit_forTable1()$Маржа),
          sum(selected_RevenueProfit_forTable1()$Оборот)) * 100, Рентабельность) * 100,
        Доля_стоимость_рейса_спот = report_ratio(report_ratio(sum(selected_RevenueProfit_forTable1()$Оборот),
          sum(selected_RevenueProfit_forTable1()$Рейсы)), `Стоимость рейса`) * 100
      )
    
  })
  
  output$revenue_profit_table_general <- renderReactable(
    reactable(
      selected_RevenueProfit_forTable_general(),
      defaultColDef = colDef(class = "cell", headerClass = "header"),
      columns = list(
        Оборот = colDef(
          cell = function(value, index) {
            share <- selected_RevenueProfit_forTable_general()$Доля_оборот_спот[index]
            share <- if (!is.na(share)) share else 0
            div(
              div(style = list(fontWeight = 600, fontSize = 27), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
              div(style = list(fontSize = 16), paste(format(share, big.mark=" ", digits = 2, scientific = FALSE), "% *"))
            )
          },
          format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                        style = list(fontWeight = 600, fontSize = 43), align = 'center'),
        Маржа = colDef(
          cell = function(value, index) {
            share <- selected_RevenueProfit_forTable_general()$Доля_маржа_спот[index]
            share <- if (!is.na(share)) share else 0
            div(
              div(style = list(fontWeight = 600, fontSize = 27), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
              div(style = list(fontSize = 16), paste(format(share, big.mark=" ", digits = 2, scientific = FALSE), "% *"))
            )
          },
          format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                       style = list(fontWeight = 600, fontSize = 43), align = 'center'),
        Рейсы = colDef(
          cell = function(value, index) {
            share <- selected_RevenueProfit_forTable_general()$Доля_рейсы_спот[index]
            share <- if (!is.na(share)) share else 0
            div(
              div(style = list(fontWeight = 600, fontSize = 27), format(value, big.mark=" ", digits = 0, scientific = FALSE)),
              div(style = list(fontSize = 16), paste(format(share, big.mark=" ", digits = 2, scientific = FALSE), "% *"))
            )
          },
        format = colFormat(separators = TRUE), 
        style = list(fontWeight = 600, fontSize = 43), align = 'center'),
        Рентабельность = colDef(
          # cell = function(value, index) {
          #   share <- selected_RevenueProfit_forTable_general()$Доля_рентабельность_спот[index]
          #   share <- if (!is.na(share)) share else 0
          #   div(
          #     div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "%")),
          #     div(style = list(fontSize = 16), paste(format(share, big.mark=" ", digits = 2, scientific = FALSE), "%"))
          #   )
          # },
          format = colFormat(separators = TRUE, suffix = " %", digits = 2),
                                style = list(fontWeight = 600, fontSize = 27), align = 'center'),
        "Стоимость рейса" = colDef(
          # cell = function(value, index) {
          #   share <- selected_RevenueProfit_forTable_general()$Доля_стоимость_рейса_спот[index]
          #   share <- if (!is.na(share)) share else 0
          #   div(
          #     div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
          #     div(style = list(fontSize = 16), paste(format(share, big.mark=" ", digits = 2, scientific = FALSE), "%"))
          #   )
          # },
          format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                                   style = list(fontWeight = 600, fontSize = 27), align = 'center'),
        Доля_оборот_спот = colDef(show = FALSE),
        Доля_маржа_спот = colDef(show = FALSE),
        Доля_рейсы_спот = colDef(show = FALSE),
        Доля_рентабельность_спот = colDef(show = FALSE),
        Доля_стоимость_рейса_спот = colDef(show = FALSE)
      )
      )
  )
  
  output$factPlot <- renderHighchart({
    if(!is.null(selected_RevenueProfit_forTable())) {
      hchart(selected_RevenueProfit_forTable() %>%
               group_by(Клиент) %>%
               summarise(Маржа = sum(Маржа), Оборот = sum(Оборот), Рейсы = sum(Рейсы)), "scatter", hcaes(Маржа, Оборот, z = Рейсы, group = Клиент)) %>%
        hc_title(text = "Фактические показатели компании по клиентам",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("За период", format(input$dateRevenueProfit[1], "%d.%m.%Y"), "-", format(input$dateRevenueProfit[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx()) %>%
        
        hc_credits(enabled = T, text = "По выгруженным рейсам")
      
    }
  })
  
  
  output$ratePlotCurrentMonth <- renderHighchart({
    if(!is.null(selected_RevenueProfit_forTable())) {
      selected_RevenueProfit_forTable() %>%
        group_by(Клиент) %>%
        summarise(Оборот = sum(Оборот)) %>%
        arrange(desc(Оборот)) %>%
        hchart(
          "pie", hcaes(x = Клиент, y = Оборот),
          name = "Оборот"
        ) %>%
        hc_title(text = "Оборот",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("За период", format(input$dateRevenueProfit[1], "%d.%m.%Y"), "-", format(input$dateRevenueProfit[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx()) %>%
        
        hc_credits(enabled = T, text = "По выгруженным рейсам")
      
    }
  })
  
  output$profitPlotCurrentMonth <- renderHighchart({
    if(!is.null(selected_RevenueProfit_forTable())) {
      selected_RevenueProfit_forTable() %>%
        group_by(Клиент) %>%
        summarise(Маржа = round(sum(Маржа), digits = 2)) %>%
        arrange(desc(Маржа)) %>%
        hchart(type = "bar", 
               hcaes(x = Клиент, y = Маржа)) %>%
        
        hc_title(text = "Маржа очищенная",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("За период", format(input$dateRevenueProfit[1], "%d.%m.%Y"), "-", format(input$dateRevenueProfit[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx()) %>%
        
        hc_credits(enabled = T, text = "По выгруженным рейсам") %>%
        
        hc_plotOptions(bar = list(stacking = "normal"))
      
    }
  })
  
  output$profitabilityPlotCurrentMonth <- renderHighchart({
    if(!is.null(selected_RevenueProfit_forTable())) {
      selected_RevenueProfit_forTable() %>%
        group_by(Клиент) %>%
        summarise(Рентабельность = round(sum(Маржа) / sum(Оборот), digits = 2)) %>%
        arrange(desc(Рентабельность)) %>%
        hchart(type = "bar", 
               hcaes(x = Клиент, y = Рентабельность)) %>%
        
        hc_title(text = "Рентабельность",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("За период", format(input$dateRevenueProfit[1], "%d.%m.%Y"), "-", format(input$dateRevenueProfit[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx()) %>%
        
        hc_credits(enabled = T, text = "По выгруженным рейсам") %>%
        
        hc_plotOptions(bar = list(stacking = "normal"))
      
    }
  })
  
  output$routesPlotCurrentMonth <- renderHighchart({
    if(!is.null(selected_RevenueProfit_forTable())) {
      selected_RevenueProfit_forTable() %>%
        group_by(Клиент) %>%
        summarise(Рейсы = sum(Рейсы)) %>%
        hchart('treemap', hcaes(x = 'Клиент', value = 'Рейсы', color = 'Рейсы')) %>% 
        hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
        
        hc_title(text = "Рейсы",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("За период", format(input$dateRevenueProfit[1], "%d.%m.%Y"), "-", format(input$dateRevenueProfit[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx()) %>%
        
        hc_credits(enabled = T, text = "По выгруженным рейсам")
    }
  })
  
  selected_RevenueProfit_new <- reactive({
    
    aggregation <- switch(input$aggregationRevenueProfit, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- RevenueProfitTable$data %>%
      mutate(Дата = format(Дата, aggregation))
    if (!is.null(input$typeForRevenueProfit)) {
      filtered <- filtered %>% filter(`Тип заявки` %in% input$typeForRevenueProfit)
    } 
    if (!is.null(input$geographyForRevenueProfit)) {
      filtered <- filtered %>% filter(География %in% input$geographyForRevenueProfit)
    } 
    if (!is.null(input$managerForRevenueProfit)) {
      filtered <- filtered %>% filter(Менеджер %in% input$managerForRevenueProfit)
    } 
    filtered %>%
      group_by(Дата) %>%
      summarise(Оборот = sum(Оборот, na.rm = TRUE),
                      Маржа = sum(Маржа, na.rm = TRUE),
                      Рейсы = sum(Рейсы))
    
  })
  
  output$revenueDynamicsPlot <- renderHighchart({
    
    highchart() %>% 
      hc_chart(type ="spline",
               options3d = list(enabled = FALSE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = selected_RevenueProfit_new()$Дата) %>% 
      hc_add_series(data = selected_RevenueProfit_new()$Оборот, name = "Выручка, руб") %>%
      hc_add_series(data = selected_RevenueProfit_new()$Маржа, name = "Маржа, руб") %>%
      hc_add_series(data = selected_RevenueProfit_new()$Рейсы, name = "Рейсы", type = "column") %>%
      
      hc_title(text = "Динамика главных показателей компании",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$dateRevenueProfit[1], "%d.%m.%Y"), "-", format(input$dateRevenueProfit[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По выгруженным рейсам")
    
  })

  # Рейсы с низкой рентабельностью
  
  selected_low_profitability_dynamic <- reactive({
    
    aggregation <- switch(input$low_profitability_aggregation, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- requestProfitabilityTable$data %>%
      filter(Дата_создания_перевозки >= input$low_profitabilityDateInput[1] & Дата_создания_перевозки <= input$low_profitabilityDateInput[2])
    if (!is.null(input$low_profitabilityType)) {
      filtered <- filtered %>% filter(Тип_заявки %in% input$low_profitabilityType)
    }
    if (!is.null(input$low_profitabilityGeography)) {
      filtered <- filtered %>% filter(География %in% input$low_profitabilityGeography)
    }
    if (!is.null(input$low_profitability_client)) {
      filtered <- filtered %>% filter(Клиент %in% input$low_profitability_client)
    }
    # Рейсы с рентабельностью меньше 5 %
    requests_part_byDay <- filtered %>%
      filter(Рентабельность_расчёт  < input$low_profitabilityNumericInput) %>%
      mutate(Дата_создания_перевозки = format(Дата_создания_перевозки, aggregation)) %>%
      group_by(Дата_создания_перевозки) %>%
      summarise(Рейсы = n())
    
    # Все рейсы
    requests_all_byDay <- filtered %>%
      mutate(Дата_создания_перевозки = format(Дата_создания_перевозки, aggregation)) %>%
      group_by(Дата_создания_перевозки) %>%
      summarise(Рейсы = n())
    
    result1 <- requests_all_byDay %>%
      left_join(requests_part_byDay, by = c("Дата_создания_перевозки" = "Дата_создания_перевозки")) %>%
      mutate(Отношение = round(Рейсы.y/Рейсы.x * 100, digits = 2))
    
    result1$Рейсы.x[is.na(result1$Рейсы.x)] <- 0
    result1$Рейсы.y[is.na(result1$Рейсы.y)] <- 0
    result1$Отношение[is.na(result1$Отношение)] <- 0
    
    return(result1)
  })
  
  
  output$low_profitability_dynamic <- renderHighchart({
    
    highchart() %>% 
      hc_chart(type ="spline") %>%
      hc_xAxis(categories = selected_low_profitability_dynamic()$Дата_создания_перевозки) %>% 
      hc_add_series(data = selected_low_profitability_dynamic()$Рейсы.y, name = "Общее число") %>%
      hc_add_series(data = selected_low_profitability_dynamic()$Отношение, name = "Доля от общего числа, %") %>%
      hc_title(text = paste("Динамика рейсов с рентабельностью ниже", input$low_profitabilityNumericInput, "%"),
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      hc_subtitle(text = paste("За период", format(input$low_profitabilityDateInput[1], "%d.%m.%Y"), "-", format(input$low_profitabilityDateInput[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По созданным перевозкам")
    
  })
  
  
  selected_low_profitability_client <- reactive({
    filtered <- requestProfitabilityTable$data %>%
    filter(Дата_создания_перевозки >= input$low_profitabilityDateInput[1] & Дата_создания_перевозки <= input$low_profitabilityDateInput[2])
    if (!is.null(input$low_profitabilityType)) {
      filtered <- filtered %>% filter(Тип_заявки %in% input$low_profitabilityType)
    }
    if (!is.null(input$low_profitabilityGeography)) {
      filtered <- filtered %>% filter(География %in% input$low_profitabilityGeography)
    }
    # Рейсы с рентабельностью меньше 5 %
    requests_part_byDay <- filtered %>%
      filter(Рентабельность_расчёт  < input$low_profitabilityNumericInput) %>%
      group_by(Клиент, Тип_заявки) %>%
      summarise(Рейсы = n())
    
    # Все рейсы
    requests_all_byDay <- filtered %>%
      group_by(Клиент, Тип_заявки) %>%
      summarise(Рейсы = n())
    
    result1 <- requests_all_byDay %>%
      left_join(requests_part_byDay, by = c("Клиент" = "Клиент", "Тип_заявки" = "Тип_заявки")) %>%
      mutate(Отношение = Рейсы.y/Рейсы.x)
      
    result1$Рейсы.x[is.na(result1$Рейсы.x)] <- 0
    result1$Рейсы.y[is.na(result1$Рейсы.y)] <- 0
    result1$Отношение[is.na(result1$Отношение)] <- 0
    
    return(result1)
  })
  
  
  selected_low_profitability_logist <- reactive({
    filtered <- requestProfitabilityTable$data %>% 
      filter(Дата_создания_перевозки >= input$low_profitabilityDateInput[1] & Дата_создания_перевозки <= input$low_profitabilityDateInput[2])
    if (!is.null(input$low_profitabilityType)) {
      filtered <- filtered %>% filter(Тип_заявки %in% input$low_profitabilityType)
    }
    if (!is.null(input$low_profitabilityGeography)) {
      filtered <- filtered %>% filter(География %in% input$low_profitabilityGeography)
    }
    # Рейсы с рентабельностью меньше 5 %
    requests_part_byDay <- filtered %>%
      filter(Рентабельность_расчёт  < input$low_profitabilityNumericInput) %>%
      group_by(Логист, Группа) %>%
      summarise(Рейсы = n())
    
    # Все рейсы
    requests_all_byDay <- filtered %>%
      group_by(Логист, Группа) %>%
      summarise(Рейсы = n())
    
    result1 <- requests_all_byDay %>%
      left_join(requests_part_byDay, by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
      mutate(Отношение = Рейсы.y/Рейсы.x)
    
    result1$Рейсы.x[is.na(result1$Рейсы.x)] <- 0
    result1$Рейсы.y[is.na(result1$Рейсы.y)] <- 0
    result1$Отношение[is.na(result1$Отношение)] <- 0
    
    
    return(result1)
  })
  
  selected_low_profitability_source <- reactive({
    filtered <- requestProfitabilityTable$data %>% 
      filter(Дата_создания_перевозки >= input$low_profitabilityDateInput[1] & Дата_создания_перевозки <= input$low_profitabilityDateInput[2])
    if (!is.null(input$low_profitabilityType)) {
      filtered <- filtered %>% filter(Тип_заявки %in% input$low_profitabilityType)
    }
    if (!is.null(input$low_profitabilityGeography)) {
      filtered <- filtered %>% filter(География %in% input$low_profitabilityGeography)
    }
    # Рейсы с рентабельностью меньше 5 %
    requests_part_byDay <- filtered %>%
      filter(Рентабельность_расчёт  < input$low_profitabilityNumericInput)
  })
  
  output$low_profitabilityTable_client <- renderReactable(
    reactable(selected_low_profitability_client(),
              defaultSorted = "Отношение",
                              columns = list(
                                Тип_заявки = colDef(name = "Тип заявки"),
                                Клиент = colDef(footer = "Итог"),
                                Рейсы.x = colDef(name = "Всего", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                                Рейсы.y = colDef(name = paste("С рентабельностью ниже", input$low_profitabilityNumericInput, "%"), aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                                
                                Отношение = colDef(defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 2), 
                                                   aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы.y']
                                              totalUnits += row['Рейсы.x']
                                            })
                                            return totalPrice / totalUnits
                                          }"))
                              ),
                              groupBy = c("Клиент"),
                              striped = TRUE,
                              bordered = TRUE,
                              borderless = FALSE,
                              highlight = TRUE,
                              resizable = FALSE,
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    ))
  
  output$low_profitabilityTable_logist <- renderReactable(
    reactable(selected_low_profitability_logist(),
              defaultSorted = "Отношение",
              columns = list(
                Клиент = colDef(footer = "Итог"),
                Рейсы.x = colDef(name = "Всего", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                Рейсы.y = colDef(name = paste("С рентабельностью ниже", input$low_profitabilityNumericInput, "%"), aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                
                Отношение = colDef(defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 2), 
                                   aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы.y']
                                              totalUnits += row['Рейсы.x']
                                            })
                                            return totalPrice / totalUnits
                                          }"))
              ),
              groupBy = c("Группа"),
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE,
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    ))
  
  output$low_profitabilityTable_source <- renderReactable(
    reactable(
      selected_low_profitability_source(),
      defaultSorted = "Рентабельность_расчёт",
      columns = list(
        Тип_заявки = colDef(name = "Тип заявки"),
        Дата_создания_перевозки = colDef(name = "Дата создания перевозки", format = colFormat(date = TRUE)),
        Ставка_клиента_НДС = colDef(name = "Ставка клиента, с НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
        Ставка_клиента = colDef(name = "Ставка клиента, без НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
        Ставка_перевозчика_НДС = colDef(name = "Ставка перевозчика, с НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
        Ставка_перевозчика = colDef(name = "Ставка перевозчика, без НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
        Маржа_ERP = colDef(name = "Маржа, ERP", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
        Маржа_расчёт = colDef(name = "Маржа, расчёт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
        Рентабельность_расчёт = colDef(defaultSortOrder = "asc", name = "Рентабельность, расчёт", format = colFormat(separators = TRUE, suffix = " %", digits = 2))
        
      )
    )
    )
  
  output$low_profitability_totalRoutes <- renderText(
    paste("Всего рейсов:", format(sum(selected_low_profitability_client()$Рейсы.x), big.mark=" "))
  )
  
  output$low_profitability_partRoutes <- renderText(
    paste("Рейсов с рентабельностью ниже", input$low_profitabilityNumericInput, "%:", format(sum(selected_low_profitability_client()$Рейсы.y), big.mark=" "))
  )
  
  output$low_profitability_proportion <- renderText(
    paste("Доля от общего числа:", round(sum(selected_low_profitability_client()$Рейсы.y) / sum(selected_low_profitability_client()$Рейсы.x) * 100, digits = 2), "%")
  )
  # gh_set_api_url(API_URL)
  
  # The selectize input arrives after the data. Do not construct geometry until
  # the selected key identifies exactly one complete outward/return pair.
  selected_doublet_rows <- reactive({
    req(!is.null(doubletsTable$data), input$doublets, length(input$doublets) == 1L)
    rows <- doubletsTable$data %>% filter(Кругорейс == input$doublets)
    req(nrow(rows) == 2L)
    rows
  })

  selected_direct_route_a <- reactive({
    head(selected_doublet_rows(), 1)
  })

  selected_finish_route_a <- reactive({
    tail(selected_doublet_rows(), 1)
  })

  selected_direct_route <- reactive({
    direct <- selected_direct_route_a()
    safe_osrm_route(data.frame(
      x = c(direct$loading_longitude, direct$unloading_longitude),
      y = c(direct$loading_latitude, direct$unloading_latitude)))
  })

  selected_intermediate_route <- reactive({
    direct <- selected_direct_route_a()
    back <- selected_finish_route_a()
    safe_osrm_route(data.frame(
      x = c(direct$unloading_longitude, back$loading_longitude),
      y = c(direct$unloading_latitude, back$loading_latitude)))
  })

  selected_back_route <- reactive({
    back <- selected_finish_route_a()
    safe_osrm_route(data.frame(
      x = c(back$loading_longitude, back$unloading_longitude),
      y = c(back$loading_latitude, back$unloading_latitude)))
  })

  selected_start_point <- reactive({
    direct <- selected_direct_route_a()
    st_point(c(direct$loading_longitude, direct$loading_latitude))
  })

  selected_finish_point <- reactive({
    direct <- selected_direct_route_a()
    st_point(c(direct$unloading_longitude, direct$unloading_latitude))
  })

  selected_start_point1 <- reactive({
    back <- selected_finish_route_a()
    st_point(c(back$loading_longitude, back$loading_latitude))
  })

  selected_finish_point1 <- reactive({
    back <- selected_finish_route_a()
    st_point(c(back$unloading_longitude, back$unloading_latitude))
  })

  output$doublets_start_direct <- renderText(
    paste(selected_direct_route_a()$start_address, paste0("(", selected_direct_route_a()$loading_plan, ")"))
  )
  
  output$doublets_finish_direct <- renderText(
    paste(selected_direct_route_a()$finish_address, paste0("(", selected_direct_route_a()$unloading_plan, ")"))
  )
  
  output$doublets_start_back <- renderText(
    paste(selected_finish_route_a()$start_address, paste0("(", selected_finish_route_a()$loading_plan, ")"))
  )
  
  output$doublets_finish_back <- renderText(
    paste(selected_finish_route_a()$finish_address, paste0("(", selected_finish_route_a()$unloading_plan, ")"))
  )
  
  output$distance <- renderText(
    # paste(round(selected_intermediate_route()$distance / 1000, digits = 2), "км")
    paste(round(selected_intermediate_route()$distance, digits = 2), "км")
  )
  
  output$duration <- renderText(
    paste(round(selected_intermediate_route()$duration / 60, digits = 2), "ч")
    # paste(round(selected_intermediate_route()$time / 1000 / 3600, digits = 2), "ч"),
    
  )
  
  output$time_between_routes <- renderText(
    paste(selected_direct_route_a()$Часы_между_рейсами, "ч")
  )
  
  output$sum_revenue <- renderText(
    paste(format(selected_direct_route_a()$charterer_rate_amount + selected_finish_route_a()$charterer_rate_amount, digits = 2, big.mark=" ", scientific = FALSE), "руб")
  )
  
  output$sum_distance <- renderText(
    # paste(format(selected_direct_route()$distance / 1000 + selected_intermediate_route()$distance / 1000 + selected_back_route()$distance / 1000, digits = 2, big.mark=" ", scientific = FALSE), "км")
    paste(format(selected_direct_route()$distance + selected_intermediate_route()$distance + selected_back_route()$distance, digits = 2, big.mark=" ", scientific = FALSE), "км")
    
  )
  
  output$price_per_km <- renderText(
    # paste(format((selected_direct_route_a()$charterer_rate_amount + selected_finish_route_a()$charterer_rate_amount) / (selected_direct_route()$distance / 1000 + selected_intermediate_route()$distance / 1000 + selected_back_route()$distance / 1000), digits = 2, big.mark=" ", scientific = FALSE), "руб/км")
    paste(format((selected_direct_route_a()$charterer_rate_amount + selected_finish_route_a()$charterer_rate_amount) / (selected_direct_route()$distance + selected_intermediate_route()$distance + selected_back_route()$distance), digits = 2, big.mark=" ", scientific = FALSE), "руб/км")
    
  )
  
  carrier_rate_without_NDS <- reactive({
    carrier_rate_with_NDS <- (selected_direct_route_a()$charterer_rate_amount + selected_finish_route_a()$charterer_rate_amount) * (1 - input$profitability / 100)
    carrier_rate_without_NDS <- carrier_rate_with_NDS / 1.2
  })
  
  output$carrier_rate <- renderText(
    paste(format((selected_direct_route_a()$charterer_rate_amount + selected_finish_route_a()$charterer_rate_amount) * (1 - input$profitability / 100), digits = 2, big.mark=" ", scientific = FALSE), paste0("(", format(carrier_rate_without_NDS(), digits = 2, big.mark=" ", scientific = FALSE), ")"), "руб")
  )
  
  output$price_per_km_for_carrier <- renderText(
    # paste(format((selected_direct_route_a()$charterer_rate_amount + selected_finish_route_a()$charterer_rate_amount) * (1 - input$profitability / 100) / (selected_direct_route()$distance / 1000 + selected_intermediate_route()$distance / 1000 + selected_back_route()$distance / 1000), digits = 2, big.mark=" ", scientific = FALSE), paste0("(", format(carrier_rate_without_NDS()/(selected_direct_route()$distance / 1000 + selected_intermediate_route()$distance / 1000 + selected_back_route()$distance / 1000), digits = 2, big.mark=" ", scientific = FALSE), ")"), "руб/км")
    paste(format((selected_direct_route_a()$charterer_rate_amount + selected_finish_route_a()$charterer_rate_amount) * (1 - input$profitability / 100) / (selected_direct_route()$distance + selected_intermediate_route()$distance + selected_back_route()$distance), digits = 2, big.mark=" ", scientific = FALSE), paste0("(", format(carrier_rate_without_NDS()/(selected_direct_route()$distance + selected_intermediate_route()$distance + selected_back_route()$distance), digits = 2, big.mark=" ", scientific = FALSE), ")"), "руб/км")
    
  )
  
  icons_direct <- awesomeIcons(
    icon = 'home',
    iconColor = 'black',
    library = 'ion',
    markerColor = 'red',
    text = "1"
  )
  icons_direct1 <- awesomeIcons(
    icon = 'home',
    iconColor = 'black',
    library = 'ion',
    markerColor = 'red',
    text = "2"
  )
  
  icons_back <- awesomeIcons(
    icon = 'home',
    iconColor = 'white',
    library = 'ion',
    markerColor = "blue",
    text = "3"
  )
  
  icons_back1 <- awesomeIcons(
    icon = 'home',
    iconColor = 'white',
    library = 'ion',
    markerColor = "blue",
    text = "4"
  )
  
  output$mymap <- renderLeaflet({
    
    if(!is.null(selected_direct_route())){
      
    
    bbox <- st_bbox(selected_direct_route()) %>%
      as.vector()
    
    leaflet() %>%
      addTiles(group = "Карта") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Спутник") %>%
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
      addPolylines(data = selected_direct_route(), color = "red", popup = paste("Расстояние:", round(selected_direct_route()$distance, digits = 2), "км", "\n Продолжительность:", round(selected_direct_route()$duration / 60, digits = 2), "ч", "\n Стоимость:", round(selected_direct_route_a()$charterer_rate_amount / selected_direct_route()$distance, digits = 2), "\U20BD на км"), group = "Прямой рейс", label = "Прямой рейс") %>%
      addPolylines(data = selected_intermediate_route(), color = "green", popup = paste("Расстояние:", round(selected_intermediate_route()$distance, digits = 2), "км", "\n Продолжительность:", round(selected_intermediate_route()$duration / 60, digits = 2), "ч"), group = "Дополнительный рейс", label = "Дополнительный рейс") %>%
      addPolylines(data = selected_back_route(), color = "blue", popup = paste("Расстояние:", round(selected_back_route()$distance, digits = 2), "км", "\n Продолжительность:", round(selected_back_route()$duration / 60, digits = 2), "ч", "\n Стоимость:", round(selected_finish_route_a()$charterer_rate_amount / selected_back_route()$distance, digits = 2), "\U20BD на км"), label = "Обратный рейс", group = "Обратный рейс") %>%
      # addPolylines(data = selected_direct_route(), color = "red", popup = paste("Расстояние:", round(selected_direct_route()$distance / 1000, digits = 2), "км", "\n Продолжительность:", round(selected_direct_route()$time / 1000 / 3600, digits = 2), "ч", "\n Стоимость:", round(selected_direct_route_a()$charterer_rate_amount / (selected_direct_route()$distance / 1000), digits = 2), "\U20BD на км"), group = "Прямой рейс", label = "Прямой рейс") %>%
      # addPolylines(data = selected_intermediate_route(), color = "green", popup = paste("Расстояние:", round(selected_intermediate_route()$distance / 1000, digits = 2), "км", "\n Продолжительность:", round(selected_intermediate_route()$time / 1000 / 3600, digits = 2), "ч"), group = "Дополнительный рейс", label = "Дополнительный рейс") %>%
      # addPolylines(data = selected_back_route(), color = "blue", popup = paste("Расстояние:", round(selected_back_route()$distance / 1000, digits = 2), "км", "\n Продолжительность:", round(selected_back_route()$time / 1000 / 3600, digits = 2), "ч", "\n Стоимость:", round(selected_finish_route_a()$charterer_rate_amount / (selected_back_route()$distance / 1000), digits = 2), "\U20BD на км"), label = "Обратный рейс", group = "Обратный рейс") %>%
      addAwesomeMarkers(icon = icons_direct, data = selected_start_point(), group = "Прямой рейс", popup = paste("Адрес загрузки:", selected_direct_route_a()$start_address, "\n Время загрузки:", selected_direct_route_a()$loading_plan)) %>%
      addAwesomeMarkers(icon = icons_direct1, data = selected_finish_point(), group = "Прямой рейс", popup = paste("Адрес выгрузки:", selected_direct_route_a()$finish_address, "\n Время выгрузки:", selected_direct_route_a()$unloading_plan)) %>%
      addAwesomeMarkers(icon = icons_back, data = selected_start_point1(), group = "Обратный рейс", popup = paste("Адрес загрузки:", selected_finish_route_a()$start_address, "\n Время загрузки:", selected_finish_route_a()$loading_plan)) %>%
      addAwesomeMarkers(icon = icons_back1, data = selected_finish_point1(), group = "Обратный рейс", popup = paste("Адрес выгрузки:", selected_finish_route_a()$finish_address, "\n Время выгрузки:", selected_finish_route_a()$unloading_plan)) %>%
      addLayersControl(
        baseGroups = c("Карта", "Спутник"),
        overlayGroups = c("Прямой рейс", "Дополнительный рейс", "Обратный рейс"))
    
    }  
  })
  
  output$potentialDoubletsTable <- renderReactable(
    if(!is.null(doubletsTable$data)){
      
    reactable(doubletsTable$data,
              groupBy = "Кругорейс",
              # selection = "multiple",
              # onClick = "select",
              columns = list(
                Номер_прямого_рейса = colDef(name = "Номера заявок"),
                charterer_rate_amount = colDef(name = "Ставка клиента", aggregate = "sum", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                vehicle_requirements_carrying_capacity_and_volume = colDef(name = "Объём / Тоннаж ТС"),
                vehicle_requirements_types_of_body = colDef(name = "Тип ТС"),
                start_region = colDef(name = "Регион загрузки"),
                start_address = colDef(name = "Адрес загрузки"),
                finish_region = colDef(name = "Регион выгрузки"),
                finish_address = colDef(name = "Адрес выгрузки"),
                loading_plan = colDef(name = "Дата загрузки"),
                unloading_plan = colDef(name = "Дата выгрузки"),
                client = colDef(name = "Клиент"),
                loading_latitude = colDef(show = FALSE),
                loading_longitude = colDef(show = FALSE),
                unloading_latitude = colDef(show = FALSE),
                unloading_longitude = colDef(show = FALSE),
                Часы_между_рейсами = colDef(show = FALSE)

              ),
              filterable = FALSE,
              searchable = FALSE,
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE
              )
    }
  )
  
  selectedClientDirections <- reactive({
    
    clientDirectionsTable$data %>% 
      filter(Дата_выгрузки >= input$clientDirectionsDateInput[1] & Дата_выгрузки <= input$clientDirectionsDateInput[2]) %>%
      
      group_by(Клиент, Маршрут, Регион_загрузки, Регион_выгрузки, Тип_ТС, Объём_тоннаж) %>%
      
      summarise(Рейсы = n()) %>%
      
      arrange(desc(Рейсы))
  })
  
  output$clientDirectionsTable <- renderReactable(
    reactable(selectedClientDirections(),
              groupBy = c("Регион_загрузки", "Регион_выгрузки", "Клиент", "Маршрут"),
              defaultSorted = "Рейсы",
              filterable = TRUE,
              columns = list(
                Регион_загрузки = colDef(name = "Регион загрузки"),
                Регион_выгрузки = colDef(name = "Регион выгрузки"),
                Тип_ТС = colDef(name = "Тип ТС"),
                Объём_тоннаж = colDef(name = "Объём тоннаж"),
                Рейсы = colDef(defaultSortOrder = "desc", aggregate = "sum", format = colFormat(separators = TRUE))
              )
              )
  )
  
  
  # col_stops <- data.frame(
  #   q = c(0.15, 0.4, .8),
  #   c = c('#55BF3B', '#DDDF0D', '#DF5353'),
  #   stringsAsFactors = FALSE
  # )
  
  # plan_implementation <- data.frame(
  #   План = c("План с ГПН"),
  #   Выручка = c(sum(selected_LogisticKPI_outcome()$Выручка) / 250013150 * 100),
  #   Маржа = c(sum(selected_LogisticKPI_outcome()$Маржа) / 17882269 * 100),
  #   Рейсы = с(sum(selected_LogisticKPI_outcome()$Рейсы) /4641 * 100)
  # )
  
  selected_metrics <- reactive({
    filtered <- metricTable$data %>% 
      
      filter(Дата >= input$metricDateInput[1] & Дата <= input$metricDateInput[2]) %>%
    
    filter(Статус != "cancelled" & Статус != "transportationCancelled" & Статус != "active") %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
    
      if (!is.null(input$metricClient)) {
        filtered <- filtered %>% filter(Клиент %in% input$metricClient)
      }
      if (!is.null(input$metricLogistGroup)) {
        filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
      } 
      if (!is.null(input$metricLogistJob)) {
        filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
      } 
      if (!is.null(input$metricLogist)) {
        filtered <- filtered %>% filter(Логист %in% input$metricLogist)
      }
      if (!is.null(input$metricType)) {
        filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  selected_metrics_canceling <- reactive({
    filtered <- metricTable$data %>% 
      filter(Дата >= input$metricDateInput[1] & Дата <= input$metricDateInput[2]) %>%
      
      filter((Статус == "cancelled" | (Статус == "transportationCancelled" & stringr::str_detect(Заметки, 'Отказ клиента'))) & is.na(Связанная_заявка)) %>%
      
      anti_join(metricTable$data, by = c("Номер_заявки" = "Связанная_заявка")) %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
      
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  selected_metrics_breakdowns <- reactive({
    filtered <- metricTable$data %>% 
      filter(Дата >= input$metricDateInput[1] & Дата <= input$metricDateInput[2]) %>%
      
      filter((Статус == "transportationCancelled" & !stringr::str_detect(Заметки, 'Отказ клиента') & is.na(Связанная_заявка))) %>%
      
      anti_join(metricTable$data, by = c("Номер_заявки" = "Связанная_заявка")) %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
      
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  selected_metrics_expired <- reactive({
    filtered <- metricTable$data %>% 
      filter(Дата >= input$metricDateInput[1] & Дата <= input$metricDateInput[2]) %>%
      
      filter((is.na(Связанная_заявка) & Статус == "active" & (Sys.Date() - Дата_загрузки_план > 0))) %>%
      
      anti_join(metricTable$data, by = c("Номер_заявки" = "Связанная_заявка")) %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
      
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  selected_metrics_canceling_compare <- reactive({
    filtered <- metricTable1$data %>% 
      filter(Дата >= input$metricDateInput1[1] & Дата <= input$metricDateInput1[2]) %>%
      
      filter((Статус == "cancelled" | (Статус == "transportationCancelled" & stringr::str_detect(Заметки, 'Отказ клиента'))) & is.na(Связанная_заявка)) %>%
      
      anti_join(metricTable1$data, by = c("Номер_заявки" = "Связанная_заявка")) %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
    
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  selected_metrics_breakdowns_compare <- reactive({
    filtered <- metricTable1$data %>% 
      filter(Дата >= input$metricDateInput1[1] & Дата <= input$metricDateInput1[2]) %>%
      
      filter((Статус == "transportationCancelled" & !stringr::str_detect(Заметки, 'Отказ клиента') & is.na(Связанная_заявка))) %>%
      
      anti_join(metricTable1$data, by = c("Номер_заявки" = "Связанная_заявка")) %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
    
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  selected_metrics_expired_compare <- reactive({
    filtered <- metricTable1$data %>% 
      filter(Дата >= input$metricDateInput1[1] & Дата <= input$metricDateInput1[2]) %>%
      
      filter((is.na(Связанная_заявка) & Статус == "active" & (Sys.Date() - Дата_загрузки_план > 0))) %>%
      
      anti_join(metricTable1$data, by = c("Номер_заявки" = "Связанная_заявка")) %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
    
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  selected_metrics_compare <- reactive({
    filtered <- metricTable1$data %>% 
      filter(Дата >= input$metricDateInput1[1] & Дата <= input$metricDateInput1[2]) %>%
      
      filter(Статус != "cancelled" & Статус != "transportationCancelled" & Статус != "active") %>%
      
      group_by(Дата, Клиент, Логист, Должность, Тип) %>%
      
      summarise(Группа = first(Группа),
                      Рейсы = n(),
                      Выручка = sum(Ставка_клиента),
                      Маржа = sum(Маржа),
                      Рентабельность = sum(Маржа) / sum(Выручка),
                      Стоимость_рейса = sum(Выручка) / sum(Рейсы))
      
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  output$metricTotalLogists <- renderValueBox({
    valueBox(
      format(n_distinct(selected_metrics()$Логист), big.mark=" "), 
      paste("Логисты, рост на", format(n_distinct(selected_metrics()$Логист) / n_distinct(selected_metrics_compare()$Логист) * 100 - 100, big.mark=" ", digits = 0), "%"), 
      icon = icon("list-alt"),
      color = "teal"
    )
  })
  
  output$metricTotalRevenue <- renderValueBox({
    valueBox(
      paste(format(sum(selected_metrics()$Выручка), big.mark=" ", scientific = FALSE), " \U20BD") ,
      paste("Выручка, рост на", format(sum(selected_metrics()$Выручка) / sum(selected_metrics_compare()$Выручка) * 100 - 100, big.mark=" ", digits = 0), "%"), 
      icon = icon("ruble"),
      color = "purple"
    )
  })
  
  output$metricTotalProfit <- renderValueBox({
    valueBox(
      paste(format(sum(selected_metrics()$Маржа), big.mark=" ", scientific = FALSE), " \U20BD"), 
      paste("Маржа, рост на", format(sum(selected_metrics()$Маржа) / sum(selected_metrics_compare()$Маржа) * 100 - 100, big.mark=" ", digits = 0), "%"), 
      icon = icon("ruble"),
      color = "yellow"
    )
  })
  
  output$metricTotalRoutes <- renderValueBox({
    valueBox(
      format(sum(selected_metrics()$Рейсы), big.mark=" "),
      paste("Рейсы, рост на", format(sum(selected_metrics()$Рейсы) / sum(selected_metrics_compare()$Рейсы) * 100 - 100, big.mark=" ", digits = 0), "%"), 
      icon = icon("car"),
      color = "lime"
    )
  })
  
  output$metricTotalProfitability <- renderValueBox({
    valueBox(
      paste(round(sum(selected_metrics()$Маржа) / sum(selected_metrics()$Выручка) * 100, digits = 2), "%"), 
      paste("Рентабельность, рост на", round((sum(selected_metrics()$Маржа) / sum(selected_metrics()$Выручка) * 100) / (sum(selected_metrics_compare()$Маржа) / sum(selected_metrics_compare()$Выручка) * 100) * 100 - 100, digits = 2), "%"), 
      icon = icon("percent"),
      color = "aqua"
    )
  })
  
  output$metricTotalFlightPrice <- renderValueBox({
    valueBox(
      paste(format(sum(selected_metrics()$Выручка) / sum(selected_metrics()$Рейсы), big.mark=" ", digits = 2, scientific = FALSE), "\U20BD"), 
      paste("Стоимость рейса, рост на", format((sum(selected_metrics()$Выручка) / sum(selected_metrics()$Рейсы)) / (sum(selected_metrics_compare()$Выручка) / sum(selected_metrics_compare()$Рейсы)) * 100 - 100, big.mark=" ", digits = 2), "%"), 
      icon = icon("ruble"),
      color = "maroon"
    )
  })
  
  output$metricTotalCancelRequests <- renderValueBox({
    valueBox(
      paste(format(sum(selected_metrics_canceling()$Рейсы), big.mark=" "), "|", format(sum(selected_metrics_canceling()$Выручка), big.mark=" ", scientific = FALSE), "\U20BD"),
      paste("Отмены заявок, рост на", format(sum(selected_metrics_canceling()$Рейсы) / sum(selected_metrics_canceling_compare()$Рейсы) * 100 - 100, big.mark=" ", digits = 0), "%", "|", format(sum(selected_metrics_canceling()$Выручка) / sum(selected_metrics_canceling_compare()$Выручка) * 100 - 100, big.mark=" ", digits = 0), "%"), 
      icon = icon("car"),
      color = "red"
    )
  })
  
  output$metricTotalCancelTransportations <- renderValueBox({
    valueBox(
      paste(format(sum(selected_metrics_breakdowns()$Рейсы), big.mark=" "), "|", format(sum(selected_metrics_breakdowns()$Выручка), big.mark=" ", scientific = FALSE), "\U20BD"),
      paste("Отмены перевозок, рост на", format(sum(selected_metrics_breakdowns()$Рейсы) / sum(selected_metrics_breakdowns_compare()$Рейсы) * 100 - 100, big.mark=" ", digits = 0), "%", "|", format(sum(selected_metrics_breakdowns()$Выручка) / sum(selected_metrics_breakdowns_compare()$Выручка) * 100 - 100, big.mark=" ", digits = 0), "%"), 
      icon = icon("car"),
      color = "blue"
    )
  })
  
  output$metricTotalLateRequests <- renderValueBox({
    valueBox(
      paste(format(sum(selected_metrics_expired()$Рейсы), big.mark=" "), "|", format(sum(selected_metrics_expired()$Выручка), big.mark=" ", scientific = FALSE), "\U20BD"),
      paste("Просроченные заявки, рост на", format(sum(selected_metrics_expired()$Рейсы) / sum(selected_metrics_expired_compare()$Рейсы) * 100 - 100, big.mark=" ", digits = 0), "%", "|", format(sum(selected_metrics_expired()$Выручка) / sum(selected_metrics_expired_compare()$Выручка) * 100 - 100, big.mark=" ", digits = 0), "%"), 
      icon = icon("car"),
      color = "navy"
    )
  })
  
  
  selected_metrics_requests <- reactive({
    filtered <- metricTable$data %>% 
      
      filter(Дата >= input$metricDateInput[1] & Дата <= input$metricDateInput[2]) %>%
      
      select(Номер_заявки, Тип, Дата, Клиент, Группа, Логист, Должность, Ставка_клиента, Маржа, Рентабельность, Статус) %>%
      
      mutate(Статус = case_when(Статус == "carrierFound" ~ "Перевозчик найден",
                                      Статус == "paid" ~ "Клиент всё оплатил",
                                      Статус == "active" ~ "Заявка активна",
                                      Статус == "cancelled" ~ "Заявка отменена",
                                      Статус == "transportationCancelled" ~ "Перевозка отменена",
                                      Статус == "docsReceived" ~ "Документы получены клиентом",
                                      Статус == "docsApproved" ~ "Документы подтверждены клиентом",
                                      Статус == "docsGrouped" ~ "Документы готовы к отправке",
                                      Статус == "docsProblem" ~ "Есть проблемы с документами",
                                      Статус == "docsSent" ~ "Документы высланы клиенту",
                                                 TRUE ~ Статус))
    
    if (!is.null(input$metricClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$metricClient)
    }
    if (!is.null(input$metricLogistGroup)) {
      filtered <- filtered %>% filter(Группа %in% input$metricLogistGroup)
    } 
    if (!is.null(input$metricLogistJob)) {
      filtered <- filtered %>% filter(Должность %in% input$metricLogistJob) 
    } 
    if (!is.null(input$metricLogist)) {
      filtered <- filtered %>% filter(Логист %in% input$metricLogist)
    }
    if (!is.null(input$metricType)) {
      filtered <- filtered %>% filter(Тип %in% input$metricType)
    }
    filtered
  })
  
  
  output$requests <- renderReactable(
    reactable(selected_metrics_requests(),
              columns = list(
                Номер_заявки = colDef(name = "Номер заявки"),
                Дата = colDef(format = colFormat(date = TRUE)),
                Ставка_клиента = colDef(name = "Ставка клиента", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Маржа = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Рентабельность = colDef(name = "Рент-ть", format = colFormat(percent = TRUE, digits = 2))
              ),
              filterable = FALSE,
              searchable = TRUE,
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE
              )
  )
  
  getLogisticPlanFact_total <- reactive({
    req(!DEMO_MODE)
    data.frame(
      Выручка_план = sum(getLogisticPlanFact()$Выручка_план),
      Выручка_факт = sum(getLogisticPlanFact()$Выручка_факт),
      Выручка_выполнение = sum(getLogisticPlanFact()$Выручка_факт) / sum(getLogisticPlanFact()$Выручка_план) * 100,
      Маржа_план = sum(getLogisticPlanFact()$Маржа_план),
      Маржа_факт = sum(getLogisticPlanFact()$Маржа_факт),
      Маржа_выполнение = sum(getLogisticPlanFact()$Маржа_факт) / sum(getLogisticPlanFact()$Маржа_план) * 100,
      Рейсы_план = sum(getLogisticPlanFact()$Рейсы_план),
      Рейсы_факт = sum(getLogisticPlanFact()$Рейсы_факт),
      Рейсы_выполнение = sum(getLogisticPlanFact()$Рейсы_факт) / sum(getLogisticPlanFact()$Рейсы_план) * 100
    ) %>%
      mutate(
        Рентабельность_план = Маржа_план / Выручка_план * 100,
        Рентабельность_факт = Маржа_факт / Выручка_факт * 100,
        Рентабельность_выполнение = Рентабельность_факт / Рентабельность_план * 100,
        Стоимость_рейса_план = Выручка_план / Рейсы_план,
        Стоимость_рейса_факт = Выручка_факт / Рейсы_факт,
        Стоимость_рейса_выполнение = Стоимость_рейса_факт / Стоимость_рейса_план * 100
      )
  })
  
  output$planVika <- renderReactable(
    reactable(  
      getLogisticPlanFact_total(),
      defaultColDef = colDef(class = "cell", headerClass = "header"),
      columns = list(
      Выручка_план = colDef(cell = function(value, index) {
        fact <- getLogisticPlanFact_total()$Выручка_факт[index]
        div(
          div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
          div(style = list(fontSize = 16), paste(format(fact, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD"))
        )
      }, name = "Оборот, план/факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
      Выручка_факт = colDef(show = FALSE, name = "Выручка, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
      Выручка_выполнение = colDef(style = function(value) {
        list(fontWeight = 600, fontSize = 24)
      }, name = "Оборот, выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
      Маржа_план = colDef(cell = function(value, index) {
        fact <- getLogisticPlanFact_total()$Маржа_факт[index]
        div(
          div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
          div(style = list(fontSize = 16), paste(format(fact, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD"))
        )
      }, name = "Маржа, план/факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
      Маржа_факт = colDef(show = FALSE, name = "Маржа, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
      Маржа_выполнение = colDef(style = function(value) {
        list(fontWeight = 600, fontSize = 24)
      }, name = "Маржа, выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
      Рентабельность_план = colDef(cell = function(value, index) {
        fact <- getLogisticPlanFact_total()$Рентабельность_факт[index]
        div(
          div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "%")),
          div(style = list(fontSize = 16), paste(format(fact, big.mark=" ", digits = 2, scientific = FALSE), "%"))
        )
      }, name = "Рентабельность, план/факт", format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
      Рентабельность_факт = colDef(show = FALSE, name = "Рентабельность, факт", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
      Рентабельность_выполнение = colDef(style = function(value) {
        list(fontWeight = 600, fontSize = 24)
      }, name = "Рентабельность, выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
      Рейсы_план = colDef(cell = function(value, index) {
        fact <- getLogisticPlanFact_total()$Рейсы_факт[index]
        div(
          div(style = list(fontWeight = 600, fontSize = 18), format(value, big.mark=" ", digits = 2, scientific = FALSE)),
          div(style = list(fontSize = 16), format(fact, big.mark=" ", digits = 2, scientific = FALSE))
        )
      }, name = "Рейсы, план/факт", format = colFormat(separators = TRUE)),
      Рейсы_факт = colDef(show = FALSE, name = "Рейсы, факт", format = colFormat(separators = TRUE)),
      Рейсы_выполнение = colDef(style = function(value) {
        list(fontWeight = 600, fontSize = 24)
      }, name = "Рейсы, выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
      Стоимость_рейса_план = colDef(cell = function(value, index) {
        fact <- getLogisticPlanFact_total()$Стоимость_рейса_факт[index]
        div(
          div(style = list(fontWeight = 600, fontSize = 18), paste(format(value, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD")),
          div(style = list(fontSize = 16), paste(format(fact, big.mark=" ", digits = 2, scientific = FALSE), "\U20BD"))
        )
      }, name = "Стоимость рейса, план/факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
      Стоимость_рейса_факт = colDef(show = FALSE, name = "Стоимость рейса, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
      Стоимость_рейса_выполнение = colDef(style = function(value) {
        list(fontWeight = 600, fontSize = 24)
      }, name = "Стоимость рейса, выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0))
      
    )
    )
  )



  # Реестр для страховой
  
  selected_registersForInsuranceTable <- reactive({
    if (DEMO_MODE) {
      req(registersForInsuranceTable$data)
      return(demo_insurance_rows(registersForInsuranceTable$data,
                                input$registersForInsurance_dateRangeInput[1],
                                input$registersForInsurance_dateRangeInput[2]))
    }
    
    if(input$type_registersForInsurance == "Стандартный"){
      
      registersForInsuranceTable$data %>%
        
        filter(Клиент == "Архивный клиент 01" |
                       Клиент == "Архивный клиент 02") %>%
        
        filter(Дата_загрузки_план >= input$registersForInsurance_dateRangeInput[1] & Дата_загрузки_план <= input$registersForInsurance_dateRangeInput[2]) %>%
        
        transmute(Номер = row_number(),
          Период_страхования_начало = Дата_загрузки_план,
                                           Период_страхования_окончание = Дата_выгрузки_план,
                                           Наименование_груза = Тип_груза,
                                           Выгодоприобретатель = Клиент,
                                           Гос_номер_перевозочного_средства = Номер_заявки,
                                           Вид_транспорта = "Авто",
                                           Маршрут_пункт_отправления = Пункт_отправления,
                                           Маршрут_пункт_перегрузки = "",
                                           Маршрут_пункт_назначения = Пункт_назначения,
                                           Страховая_сумма = Стоимость_груза,
                                           Валюта = "Руб."
        )
      
    } else if (input$type_registersForInsurance == "Сборный"){
      
      registersForInsuranceTable$data %>%
        
        filter(Клиент != "Архивный клиент 01" |
                       Клиент != "Архивный клиент 02") %>%
        
        filter(Дата_загрузки_план >= input$registersForInsurance_dateRangeInput[1] & Дата_загрузки_план <= input$registersForInsurance_dateRangeInput[2]) %>%
        
        filter(Дата_выгрузки_план >= input$registersForInsurance_dateRangeInput[2] + 1) %>% 
        
        filter(Статус != "unloaded") %>% 
        
        transmute(Номер = row_number(),
          Комментарий = '',
          Рейс_ЕРП = Рейс_ЕРП,
          Логист = Логист,
          Перевозчик = Перевозчик,
          Период_страхования_начало = Дата_загрузки_план,
                                           Период_страхования_окончание = Дата_выгрузки_план,
          Время_выгрузки_план = Время_выгрузки_план,
                                           Наименование_груза = Тип_груза,
                                           Выгодоприобретатель = Клиент,
                                           Гос_номер_перевозочного_средства = paste(Марка_тягача, Номер_тягача, Марка_полуприцепа, Номер_полуприцепа),
                                           Вид_транспорта = "Авто",
                                           Кубатура = Кубатура,
                                           Маршрут_пункт_отправления = Пункт_отправления,
                                           Маршрут_пункт_перегрузки = "",
                                           Маршрут_пункт_назначения = Пункт_назначения,
                                           Страховая_сумма = Стоимость_груза,
                                           Валюта = "Руб."
        )
      
    } else {
      registersForInsuranceTable$data %>%
        
        filter(Клиент == "Архивный клиент 01" |
                       Клиент == "Архивный клиент 02") %>%
        
        filter(Дата_загрузки_план >= input$registersForInsurance_dateRangeInput[1] & Дата_загрузки_план <= input$registersForInsurance_dateRangeInput[2]) %>%
        
        transmute(Номер = row_number(),
                       Дата_экспедиторской_расписки = Дата_создания_перевозки,
                       Номер_экспедиторской_расписки = Номер_заявки,
                       Наименование_груза = Тип_груза,
                       Период_страхования_начало = Дата_загрузки_план,
                       Период_страхования_окончание = Дата_выгрузки_план,
                       Маршрут_пункт_отправления = Пункт_отправления,
                       Маршрут_пункт_назначения = Пункт_назначения,
                       Стоимость_груза = Стоимость_груза,
                       Страховая_сумма = Стоимость_груза,
                       Страховая_премия = Стоимость_груза * 0.00008
        )
    }

  }) 
  
  output$downloadRegistersForInsurance <- downloadHandler(
    filename = function() {
      paste("Декларация-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Формирование excel-файла", value = 0.2)
      
      progress$inc(0.4, detail = "Начало записи файла")
      
      progress$inc(0.6, detail = "Получение данных")
      
      startDate <- as.POSIXct(input$registersForInsurance_dateRangeInput[1], format="%Y-%m-%d") - 24*60*60 - 1
      endDate <- as.POSIXct(input$registersForInsurance_dateRangeInput[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      registersForInsuranceTable$data <- getregistersForInsurance(startDate, endDate)
      
      progress$inc(0.8, detail = "Данные получены")

      if (DEMO_MODE) {
        wb <- create_demo_insurance_workbook(selected_registersForInsuranceTable(),
                                             input$registersForInsurance_dateRangeInput[1],
                                             input$registersForInsurance_dateRangeInput[2],
                                             input$type_registersForInsurance)
        saveWorkbook(wb, file, overwrite = TRUE)
        return(invisible(NULL))
      }
      
      wb <- NULL
      
      if (input$type_registersForInsurance == "Стандартный"){
        wb <- loadWorkbook(Sys.getenv("REPORTS_INSURANCE_STANDARD_TEMPLATE", "insurance-standard.local.xlsx"))
        
        writeData(wb, sheet = 1, x = format(input$registersForInsurance_dateRangeInput[1], "%d.%m.%Y"), startCol = 8, startRow = 7)
        writeData(wb, sheet = 1, x = format(input$registersForInsurance_dateRangeInput[2], "%d.%m.%Y"), startCol = 10, startRow = 7)
        
        writeData(wb, sheet = 1, x = selected_registersForInsuranceTable(), startCol = 1, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Период_страхования_начало, startCol = 2, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Период_страхования_окончание, startCol = 3, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Наименование_груза, startCol = 4, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Выгодоприобретатель, startCol = 5, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Гос_номер_перевозочного_средства, startCol = 6, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Вид_транспорта, startCol = 7, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Маршрут_пункт_отправления, startCol = 8, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Маршрут_пункт_назначения, startCol = 10, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Страховая_сумма, startCol = 11, startRow = 15, colNames = F)
        # writeData(wb, sheet = 1, x = selected_registersForInsuranceTable()$Валюта, startCol = 12, startRow = 15, colNames = F)
        
        bodyStyle <- createStyle(fontSize = 10, fontName = "Arial", borderStyle = "thin", border = c("top", "bottom", "left", "right"), wrapText = TRUE)
        addStyle(wb, sheet = 1, bodyStyle, rows = 15:(nrow(selected_registersForInsuranceTable())+14), cols = 1:12, gridExpand = TRUE)
        
        dateStyle <- createStyle(numFmt = "DATE")
        addStyle(wb, sheet = 1, dateStyle, rows = 15:(nrow(selected_registersForInsuranceTable())+14), cols = 2:3, gridExpand = TRUE, stack = TRUE)
        
        moneyStyle <- createStyle(numFmt = "COMMA")
        addStyle(wb, sheet = 1, moneyStyle, rows = 15:(nrow(selected_registersForInsuranceTable())+14), cols = 13, gridExpand = TRUE, stack = TRUE)
        
        bodyStyle1 <- createStyle(fontSize = 10, fontName = "Arial", textDecoration = "bold", valign = "bottom", halign = "center")
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 19, cols = 2)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 23, cols = 2)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 16, cols = 10)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 16, cols = 11)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 19, cols = 9)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 23, cols = 9)
        
        mergeCells(wb, 1, cols = 2:4, rows = nrow(selected_registersForInsuranceTable()) + 19)
        writeData(wb, sheet = 1, x = "СТРАХОВЩИК", startCol = 2, startRow = nrow(selected_registersForInsuranceTable()) + 19)
        
        mergeCells(wb, 1, cols = 2:4, rows = nrow(selected_registersForInsuranceTable()) + 23)
        writeData(wb, sheet = 1, x = "_____________________ /________________/", startCol = 2, startRow = nrow(selected_registersForInsuranceTable()) + 23)
        
        writeData(wb, sheet = 1, x = "ИТОГО:", startCol = 10, startRow = nrow(selected_registersForInsuranceTable()) + 16)
        writeData(wb, sheet = 1, x = format(round(sum(selected_registersForInsuranceTable()$Страховая_сумма), digits = 2), big.mark=" "), startCol = 11, startRow = nrow(selected_registersForInsuranceTable()) + 16)
        
        mergeCells(wb, 1, cols = 9:11, rows = nrow(selected_registersForInsuranceTable()) + 19)
        writeData(wb, sheet = 1, x = "СТРАХОВАТЕЛЬ", startCol = 9, startRow = nrow(selected_registersForInsuranceTable()) + 19)
        
        mergeCells(wb, 1, cols = 9:12, rows = nrow(selected_registersForInsuranceTable()) + 23)
        writeData(wb, sheet = 1, x = "_____________________ /________________/", startCol = 9, startRow = nrow(selected_registersForInsuranceTable()) + 23)
        
      } else if (input$type_registersForInsurance == "Сборный"){
        wb <- loadWorkbook(Sys.getenv("REPORTS_INSURANCE_COMBINED_TEMPLATE", "insurance-combined.local.xlsx"))
        
        writeData(wb, sheet = 1, x = format(input$registersForInsurance_dateRangeInput[1], "%d.%m.%Y"), startCol = 12, startRow = 7)
        writeData(wb, sheet = 1, x = format(input$registersForInsurance_dateRangeInput[2], "%d.%m.%Y"), startCol = 14, startRow = 7)
        
        writeData(wb, sheet = 1, x = selected_registersForInsuranceTable(), startCol = 1, startRow = 15, colNames = F)
        
        bodyStyle <- createStyle(fontSize = 10, fontName = "Arial", borderStyle = "thin", border = c("top", "bottom", "left", "right"), wrapText = TRUE)
        addStyle(wb, sheet = 1, bodyStyle, rows = 15:(nrow(selected_registersForInsuranceTable())+14), cols = 1:18, gridExpand = TRUE)
        
        dateStyle <- createStyle(numFmt = "DATE")
        addStyle(wb, sheet = 1, dateStyle, rows = 15:(nrow(selected_registersForInsuranceTable())+14), cols = 6:7, gridExpand = TRUE, stack = TRUE)
        
        moneyStyle <- createStyle(numFmt = "COMMA")
        addStyle(wb, sheet = 1, moneyStyle, rows = 15:(nrow(selected_registersForInsuranceTable())+14), cols = 17, gridExpand = TRUE, stack = TRUE)
        
        bodyStyle1 <- createStyle(fontSize = 10, fontName = "Arial", textDecoration = "bold", valign = "bottom", halign = "center")
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 19, cols = 6)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 23, cols = 6)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 16, cols = 16)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 16, cols = 17)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 19, cols = 15)
        addStyle(wb, sheet = 1, bodyStyle1, rows = nrow(selected_registersForInsuranceTable()) + 23, cols = 15)
        
        mergeCells(wb, 1, cols = 6:9, rows = nrow(selected_registersForInsuranceTable()) + 19)
        writeData(wb, sheet = 1, x = "СТРАХОВЩИК", startCol = 6, startRow = nrow(selected_registersForInsuranceTable()) + 19)
        
        mergeCells(wb, 1, cols = 6:10, rows = nrow(selected_registersForInsuranceTable()) + 23)
        writeData(wb, sheet = 1, x = "_____________________ /________________/", startCol = 6, startRow = nrow(selected_registersForInsuranceTable()) + 23)
        
        writeData(wb, sheet = 1, x = "ИТОГО:", startCol = 16, startRow = nrow(selected_registersForInsuranceTable()) + 16)
        writeData(wb, sheet = 1, x = format(sum(selected_registersForInsuranceTable()$Страховая_сумма), digits = 2, big.mark=" "), startCol = 17, startRow = nrow(selected_registersForInsuranceTable()) + 16)
        
        mergeCells(wb, 1, cols = 15:17, rows = nrow(selected_registersForInsuranceTable()) + 19)
        writeData(wb, sheet = 1, x = "СТРАХОВАТЕЛЬ", startCol = 15, startRow = nrow(selected_registersForInsuranceTable()) + 19)
        
        mergeCells(wb, 1, cols = 15:18, rows = nrow(selected_registersForInsuranceTable()) + 23)
        writeData(wb, sheet = 1, x = "_____________________ /________________/", startCol = 15, startRow = nrow(selected_registersForInsuranceTable()) + 23)
        
        
      } else {
        
        wb <- loadWorkbook(Sys.getenv("REPORTS_INSURANCE_SIMPLE_TEMPLATE", "insurance-simple.local.xlsx"))
        
        writeData(wb, sheet = 1, x = paste("за период", format(input$registersForInsurance_dateRangeInput[1], "%d.%m.%Y"), "по", format(input$registersForInsurance_dateRangeInput[2], "%d.%m.%Y"), "были застрахованы следующие грузы:"), startCol = 2, startRow = 19)
        
        writeData(wb, sheet = 1, x = selected_registersForInsuranceTable(), startCol = 2, startRow = 23, colNames = F)
        
        bodyStyle <- createStyle(fontSize = 10, fontName = "Arial", borderStyle = "thin", border = c("top", "bottom", "left", "right"), wrapText = TRUE)
        addStyle(wb, sheet = 1, bodyStyle, rows = 23:(nrow(selected_registersForInsuranceTable())+22), cols = 2:12, gridExpand = TRUE)
        
        moneyStyle <- createStyle(numFmt = "COMMA")
        addStyle(wb, sheet = 1, moneyStyle, rows = 23:(nrow(selected_registersForInsuranceTable())+22), cols = 10:12, gridExpand = TRUE, stack = TRUE)
        
        dateStyle <- createStyle(numFmt = "DATE")
        addStyle(wb, sheet = 1, dateStyle, rows = 23:(nrow(selected_registersForInsuranceTable())+22), cols = c(3, 6, 7), gridExpand = TRUE, stack = TRUE)
        
        mergeCells(wb, 1, cols = 2:3, rows = nrow(selected_registersForInsuranceTable()) + 25)
        writeData(wb, sheet = 1, x = "От КЛИЕНТА:", startCol = 2, startRow = nrow(selected_registersForInsuranceTable()) + 25)
        
        mergeCells(wb, 1, cols = 2:4, rows = nrow(selected_registersForInsuranceTable()) + 27)
        writeData(wb, sheet = 1, x = "________________ /________________/", startCol = 2, startRow = nrow(selected_registersForInsuranceTable()) + 27)
        
        
        mergeCells(wb, 1, cols = 11:12, rows = nrow(selected_registersForInsuranceTable()) + 25)
        writeData(wb, sheet = 1, x = "От ЭКСПЕДИТОРА:", startCol = 11, startRow = nrow(selected_registersForInsuranceTable()) + 25)
        
        mergeCells(wb, 1, cols = 10:12, rows = nrow(selected_registersForInsuranceTable()) + 27)
        writeData(wb, sheet = 1, x = "________________ /________________/", startCol = 10, startRow = nrow(selected_registersForInsuranceTable()) + 27)
        
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      progress$inc(1, detail = "Файл сформирован")
      

    }
  )
  
  # Движения денежных средств
  output$cash <- renderReactable(
    reactable(cashTable$data,
              pagination = FALSE,
              defaultSorted = "Ставка_клиента",
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              groupBy = c("Клиент", "Статус"),
              columns = list(
                Клиент = colDef(footer = "Всего"),
                Номер_заявки = colDef( 
                                      name = "Номера заявок"),
                Количество = colDef(aggregate = "sum",
                                    footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)
                                    ),
                Ставка_клиента = colDef(aggregate = "sum",
                                        defaultSortOrder = "desc",
                                        name = "Сумма", 
                                        format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                                        footer = function(values) paste(format(sum(values), big.mark=" ", scientific=FALSE), "\U20BD") 
                                        ),
                Специалист = colDef(aggregate = "unique")
              )
              )
    )
  
  
  
  output$cash_do <- renderReactable(
    reactable(cashTable$data,
              pagination = FALSE,
              defaultSorted = "Ставка_клиента",
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              groupBy = c("Специалист", "Клиент", "Статус"),
              columns = list(
                Специалист = colDef(footer = "Всего"),
                Номер_заявки = colDef(name = "Номера заявок"),
                Количество = colDef(aggregate = "sum",
                                    footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)
                ),
                Ставка_клиента = colDef(aggregate = "sum",
                                        defaultSortOrder = "desc",
                                        name = "Сумма", 
                                        format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                                        footer = function(values) paste(format(sum(values), big.mark=" ", scientific=FALSE), "\U20BD") 
                ),
                Специалист = colDef(aggregate = "unique")
              )
    )
  )
  
  
  # Количество водителей
  output$countDriversTable <- renderReactable(
    reactable(driverCountTable$data,
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              columns = list(
                security_status_value = colDef(name = "Статус", footer = "Всего"),
                Количество = colDef(format = colFormat(separators = TRUE, digits = 0),
                footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                Доля = colDef(format = colFormat(percent = TRUE, digits = 2))
              )
    )
  )
  
  output$countDriversPlot <- renderHighchart({
    
    driverCountTable$data %>%
      hchart(
        "pie", hcaes(x = security_status_value, y = Количество),
        name = "Водители"
      ) %>%
      
      hc_title(text = "Водители и их статусы",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>% 
      hc_subtitle(text = paste("Всего за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
    
  })
  
  # Количество перевозчиков
  output$countCarriersTable <- renderReactable(
    reactable(carrierCountTable$data,
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              columns = list(
                data.securityStatus.statusValue = colDef(name = "Статус", footer = "Всего"),
                Количество = colDef(format = colFormat(separators = TRUE, digits = 0),
                footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                Доля = colDef(format = colFormat(percent = TRUE, digits = 2))
              )
    )
  )
  
  output$countCarriersPlot <- renderHighchart({
    
    carrierCountTable$data %>%
      hchart(
        "pie", hcaes(x = data.securityStatus.statusValue, y = Количество),
        name = "Перевозчики"
      ) %>%
      
      hc_title(text = "Перевозчики и их статусы",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>% 
      hc_subtitle(text = paste("Всего за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
    
  })
  
  # Скорость закрытия рейса - примитивный вариант
  
  selected_average_time_by_logist1 <- reactive({
    averageTimeTable$data %>%
      
      filter(Дата >= input$averageTimeDate[1] & Дата <= input$averageTimeDate[2]) %>%
      
#      group_by(Логист, Тип, География) %>%
      
      group_by(Логист, Тип) %>%
      
      summarise(Рейсы = n(),
                      Ставка = median(Ставка),
                      Рабочих_дней = n_distinct(Дата),
                      Скорость = Рейсы / Рабочих_дней / input$workingHours
      ) 
  })
  
  
  selected_average_time_by_day1 <- reactive({

    aggregation <- switch(input$aggregationAverageTime,
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")

    averageTimeTable$data %>%
      filter(Дата >= input$averageTimeDate[1] & Дата <= input$averageTimeDate[2]) %>%
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата, Логист) %>%
      summarise(Рейсы = n(),
                     # Логисты = n_distinct(Логист),
                     Скорость = Рейсы / input$workingHours
      ) %>%
      group_by(Дата) %>%
      summarise(Рейсы = sum(Рейсы),
                     Скорость = median(Скорость))
  })
  
  
  output$averageTimeByDayPlot1 <- renderHighchart({
    
    hc <- selected_average_time_by_day1() %>%
      hchart(
        "spline",
        hcaes(x = Дата, y = Скорость),
        name = "Скорость закрытия рейса, заявки/час"
      ) %>%
      hc_title(text = "Средняя скорость закрытия рейса по дням",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$averageTimeDate[1], "%d.%m.%Y"), "-", format(input$averageTimeDate[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По созданным перевозкам")
    
  })
  
  output$averageTime1 <- renderReactable(
    reactable(selected_average_time_by_logist1(),
              groupBy = c("Логист"),
              defaultSorted = c("Скорость", "Рейсы"),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              columns = list(
                responsible_expeditor_title = colDef(name = "Логист", footer = function(values) format(count(values), digits = 2, big.mark=" ", scientific = FALSE)),
                Рейсы = colDef(footer = function(values) format(sum(values), digits = 2, big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", name = "Рейсы", aggregate = "sum"),
                Рабочих_дней = colDef(footer = "Медианное значение по компании:", defaultSortOrder = "desc", name = "Рабочих дней"),
                Скорость = colDef(footer = function(values) format(median(values), digits = 2, big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", name = "Продуктивность, заявки/часы", aggregate = "median", format = colFormat(separators = TRUE, digits = 2)),
                Ставка = colDef(name = "Средняя стоимость рейса, \U20BD", footer = function(values) format(median(values), digits = 2, big.mark=" ", scientific=FALSE), aggregate = "median", format = colFormat(separators = TRUE, digits = 0))
              ),
              pagination = FALSE
              
    )
  )
  
  # Скорость закрытия рейса - исходные данные
  
  selected_average_time_by_initial <- reactive({
    averageTimeTable$data %>%
      
      filter(Дата >= input$averageTimeDate[1] & Дата <= input$averageTimeDate[2]) 
    
  })
  
  
  output$averageTime_initial <- renderReactable(
    reactable(selected_average_time_by_initial(),
              defaultSorted = c("Время"),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              columns = list(
                Дата = colDef(format = colFormat(date = TRUE), show = FALSE),
                Дата_заявки = colDef(name = "Дата заявки", format = colFormat(datetime = TRUE)),
                Дата_перевозки = colDef(name = "Дата перевозки", format = colFormat(datetime = TRUE)),
                Дата_загрузки = colDef(name = "Дата загрузки", format = colFormat(datetime = TRUE)),
                Логист = colDef(name = "Ответственный логист"),
                Создатель = colDef(name = "Кто создал"),
                Время = colDef(      footer = JS("function(colInfo) {
        let total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total.toFixed(2)
      }"), defaultSortOrder = "asc", name = "Затраченное время, часы", format = colFormat(separators = TRUE, digits = 2)),
                Ставка = colDef(name = "Ставка клиента, \U20BD", format = colFormat(separators = TRUE, digits = 0))
              ),
              pagination = TRUE,
              searchable = TRUE
              
    )
  )
  
  # Среднее время закрытия рейса - продвинутый вариант
  
  selected_average_time_by_logist <- reactive({
    averageTimeTable$data %>%
      filter(Дата >= input$averageTimeDate[1] & Дата <= input$averageTimeDate[2]) %>%
      group_by(Логист, Тип, География) %>%
      summarise(Рейсы = n(),
                Среднее_время = sum(Время)
                     )
  })
  
  selected_average_time_by_logist_ego <- reactive({
    averageTimeTable$data %>%
      
      filter(Создатель %in% SPOT_TEAM_CREATORS) %>%
      
      filter(Дата >= input$averageTimeDate[1] & Дата <= input$averageTimeDate[2]) %>%
      group_by(Логист, Тип, География) %>%
      summarise(Рейсы = n(),
                     Среднее_время = sum(Время)
      )
  })
  
  selected_average_time_by_day <- reactive({
    
    aggregation <- switch(input$aggregationAverageTime, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    averageTimeTable$data %>%
      filter(Дата >= input$averageTimeDate[1] & Дата <= input$averageTimeDate[2]) %>%
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата) %>%
       summarise(Рейсы = n(),
                      Среднее_время = sum(Время)
       )
      
      # group_by(Дата, Логист) %>%
      # summarise(Рейсы = n(),
      #                Среднее_время = median(Время)
      # )   %>%   
      # group_by(Дата) %>%
      # summarise(Рейсы = sum(Рейсы),
      #                Среднее_время = median(Среднее_время))
  })
  
  output$averageTimeByDayPlot <- renderHighchart({
    
    hc <- selected_average_time_by_day() %>%
      hchart(
        "spline",
        hcaes(x = Дата, y = Среднее_время),
        name = "Продуктивное время, часы"
      ) %>%
      hc_title(text = "Продуктивное время закрытия рейса по дням",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$averageTimeDate[1], "%d.%m.%Y"), "-", format(input$averageTimeDate[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По созданным перевозкам")
    
  })
  
  output$averageTime <- renderReactable(
    reactable(selected_average_time_by_logist(),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              groupBy = c("Логист", "Тип"),
              defaultSorted = c("Рейсы", "Среднее_время"),
              columns = list(
                Рейсы = colDef(footer = function(values) format(sum(values), digits = 2, big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", name = "Рейсы", aggregate = "sum"),
                Среднее_время = colDef(footer = function(values) format(sum(values), digits = 2, big.mark=" ", scientific=FALSE), defaultSortOrder = "asc", name = "Время, часы", aggregate = "sum", format = colFormat(separators = TRUE, digits = 2))
              )
    )
  )
  
  output$averageTime_ego <- renderReactable(
    reactable(selected_average_time_by_logist_ego(),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              groupBy = c("Логист", "Тип"),
              defaultSorted = c("Рейсы", "Среднее_время"),
              columns = list(
                Рейсы = colDef(footer = function(values) format(sum(values), digits = 2, big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", name = "Рейсы", aggregate = "sum"),
                Среднее_время = colDef(footer = function(values) format(sum(values), digits = 2, big.mark=" ", scientific=FALSE), defaultSortOrder = "asc", name = "Время, часы", aggregate = "sum", format = colFormat(separators = TRUE, digits = 2))
              )
    )
  )
  
  # Количество рейсов каждый день
  
  # output$countRequestsPlot <- renderHighchart({
  #   
  #   highchart(type = "stock") %>% 
  #     hc_add_series(xts(countRequestsTable$data[, -1], order.by = countRequestsTable$data$Время), type = "spline", name = "Рейсы") %>%
  #     hc_title(text = "Рейсы",
  #              style = list(fontWeight = "bold", fontSize = "14px"),
  #              align = "center") %>% 
  #     
  #     hc_subtitle(text = paste("За весь период наблюдения по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
  #                 style = list(fontWeight = "bold"),
  #                 align = "center")  %>%
  #     
  #     hc_add_theme(hc_theme_ffx()) %>%
  #     
  #     hc_credits(enabled = T, text = "По созданным заявкам")
  #   
  # })
  
  selected_countRequests <- reactive({
    table <- countRequestsTable$data %>%
      mutate(Дата = as.Date(strftime(Время, format="%Y-%m-%d")), День_недели = wday(strftime(Время, format="%Y-%m-%d")), Номер_недели = strftime(Время, format="%Y-%V")) %>%
      filter(День_недели != 1 & День_недели != 7) %>%
      mutate(Время = strftime(Время, format="%Y-%m-%d %H:%M")) %>%
      filter(Дата >= input$dateCountRequests[1] & Дата <= input$dateCountRequests[2]) 
  })
  
  selected_countRequests1 <- reactive({
    table <- countRequestsTable$data %>%
      mutate(Дата = as.Date(strftime(Время, format="%Y-%m-%d")), День_недели = wday(strftime(Время, format="%Y-%m-%d"))) %>%
      mutate(Время = strftime(Время, format="%H:%M")) %>%
      filter(День_недели != 1 & День_недели != 7) %>%
      mutate(День_недели = case_when(День_недели == 2 ~ "1. Понедельник",
                                                День_недели == 3 ~ "2. Вторник",
                                                День_недели == 4 ~ "3. Среда",
                                                День_недели == 5 ~ "4. Четверг ",
                                                День_недели == 6 ~ "5. Пятница"
      )) %>%
      arrange(desc(Заявки))  %>%
      group_by(Дата, День_недели) %>%
      summarise(Время_макс_значение = first(Время), Заявки = first(Заявки)) %>%
      filter(Дата >= input$dateCountRequests[1] & Дата <= input$dateCountRequests[2])
  })
  
  selected_countRequests2 <- reactive({
    table <- countRequestsTable$data %>%
      mutate(Дата = as.Date(strftime(Время, format="%Y-%m-%d")), День_недели = wday(strftime(Время, format="%Y-%m-%d"))) %>%
      mutate(Время = strftime(Время, format="%H:%M")) %>%
      filter(День_недели != 1 & День_недели != 7) %>%
      mutate(День_недели = case_when(День_недели == 2 ~ "1. Понедельник",
                                     День_недели == 3 ~ "2. Вторник",
                                     День_недели == 4 ~ "3. Среда",
                                     День_недели == 5 ~ "4. Четверг ",
                                     День_недели == 6 ~ "5. Пятница"
                                                )) %>%
      arrange(desc(Заявки))  %>%
      group_by(Дата, День_недели) %>%
      summarise(Время_макс_значение = first(Время), Заявки = first(Заявки)) %>%
      filter(Дата >= input$dateCountRequests[1] & Дата <= input$dateCountRequests[2]) %>%
    group_by(День_недели) %>%
      summarise(Время_макс_значение = format(mean(strptime(Время_макс_значение, "%H:%M")), "%H:%M"),
                                   Заявки = round(mean(Заявки), digits = 1)
      )
  })
  
  
  times <- data.frame(
    Время = c('00:00',
                '00:30',
                '01:00',
                '01:30',
                '02:00',
                '02:30',
                '03:00',
                '03:30',
                '04:00',
                '04:30',
                '05:00',
              '05:30',
              '06:00',
              '06:30',
              '07:00',
              '07:30',
              '08:00',
              '08:30',
              '09:00',
              '09:30',
              '10:00',
              '10:30',
              '11:00',
              '11:30',
              '12:00',
              '12:30',
              '13:00',
              '13:30',
              '14:00',
              '14:30',
              '15:00',
              '15:30',
              '16:00',
              '16:30',
              '17:00',
              '17:30',
              '18:00',
              '18:30',
              '19:00',
              '19:30',
              '20:00',
              '20:30',
              '21:00',
              '21:30',
              '22:00',
              '22:30',
              '23:00',
              '23:30'
              )
  )
  
  
  selected_countRequests3 <- reactive({
    table <- countRequestsTable$data %>%
      mutate(Дата = as.Date(strftime(Время, format="%Y-%m-%d")), День_недели = wday(strftime(Время, format="%Y-%m-%d"))) %>%
      mutate(Время = strftime(Время, format="%H:%M")) %>%
      filter(День_недели != 1 & День_недели != 7) %>%
      mutate(День_недели = case_when(День_недели == 2 ~ "1. Понедельник",
                                                День_недели == 3 ~ "2. Вторник",
                                                День_недели == 4 ~ "3. Среда",
                                                День_недели == 5 ~ "4. Четверг ",
                                                День_недели == 6 ~ "5. Пятница"
      )) %>%
      filter(Дата >= input$dateCountRequests[1] & Дата <= input$dateCountRequests[2]) %>%
      group_by(Время) %>%
      summarise(Заявки = round(mean(Заявки), digits = 1))
  })
  
  output$countRequestsTable <- renderReactable(
    reactable(selected_countRequests1(), 
              groupBy = "День_недели",
              columns = list(
                День_недели = colDef(name = "День недели"),
                Время_макс_значение = colDef(aggregate = "frequency", name = "Время пика"),
                Заявки = colDef(aggregate = "mean", format = colFormat(digits = 1))
              )
              )
  )
  
  output$countRequestsTable1 <- renderReactable(
    reactable(selected_countRequests2(), 
              columns = list(
                День_недели = colDef(name = "День недели"),
                Время_макс_значение = colDef(name = "Время пика")
              )
    )
  )

  output$countRequestsPlot1 <- renderHighchart({

    hc <- selected_countRequests3() %>%
      hchart(
        "spline",
        hcaes(x = Время, y = Заявки),
        name = "Заявки"
      ) %>%
      hc_title(text = "Среднее число заявок по 30-минутным интервалам",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%

      hc_subtitle(text = paste("За период", format(input$dateCountRequests[1], "%d.%m.%Y"), "-", format(input$dateCountRequests[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%

      hc_add_theme(hc_theme_ffx()) %>%

      hc_credits(enabled = T, text = "По созданным заявкам")

  })
  
  
  output$totalPeak <- renderText(
    format(mean(strptime(selected_countRequests2()$Время_макс_значение, "%H:%M")), "%H:%M")
  )
  
  output$periodCountRequests <- renderText(
    paste("За период", format(input$dateCountRequests[1], "%d.%m.%Y"), "-", format(input$dateCountRequests[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
  )
  
  # # Проведение актов
  # 
  # selected_acts <- reactive({
  #   actTable$data %>%
  #       filter(Дата_проверки >= input$date[1] & Дата_проверки <= input$date[2])
  # })
  
  # output$carriedOutActsTable <- renderReactable(
  #   reactable(selected_acts(),
  #             groupBy = c("Специалист", "Клиент"),
  #             columns = list(
  #               Дата_проверки = colDef(name = "Дата проверки"),
  #               Номер_заявки = colDef(name = "Номер заявки"),
  #               Ставка_клиента = colDef(name = "Ставка клиента", aggregate = "sum", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
  #               Сумма_счёта = colDef(name = "Сумма счёта", aggregate = "sum", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
  #               Номер_счёта = colDef(name = "Номер счёта"),
  #               Дата = colDef(name = "Дата счёта"),
  #               ДатаВыс = colDef(name = "Дата выставления")
  #             ),
  #             striped = TRUE,
  #             bordered = TRUE,
  #             borderless = FALSE,
  #             highlight = TRUE,
  #             resizable = FALSE
  #   )
  # )
  
  # Проведение актов
  
  selected_acts <- reactive({
    actTable$data %>%
      filter(Дата_проведения_акта_перевозчика >= input$date[1] & Дата_проведения_акта_перевозчика <= input$date[2])
  })
  
  output$carriedOutActsTable <- renderReactable(
    reactable(selected_acts(),
              defaultSorted = "Номер_акта_перевозчика",
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              groupBy = c("Специалист"),
              columns = list(
                Специалист = colDef(footer = "Всего"),
                Номер_акта_перевозчика = colDef(defaultSortOrder = "desc", name = "Номер акта перевозчика", aggregate = "count", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total.toFixed(0)
      }")),
                Номер_заявки = colDef(name = "Заявки"),
                Номер_акта_перевозчика = colDef(name = "Номер акта перевозчика"),
                Дата_акта_перевозчика = colDef(name = "Дата акта перевозчика"),
                Дата_проведения_акта_перевозчика = colDef(name = "Дата проведения акта")
                ),
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE
    )
  )
  
  # Доля отказов от рейсов
  
  selected_canceling_new <- reactive({
    
    aggregation <- switch(input$aggregationCanceledOrders, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    cancelingTableNew$data %>% 
      filter(Дата >= input$date_canceledOrders_prop[1] & Дата <= input$date_canceledOrders_prop[2]) %>% 
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата, Клиент) %>%
      summarise(Рейсы.x = sum(Рейсы.x), Рейсы.y = sum(Рейсы.y), Отношение = Рейсы.y/Рейсы.x)
    
  })
  
  selected_canceling_new_general_plot <- reactive({
    
    aggregation <- switch(input$aggregationCanceledOrders, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    if (input$client_canceling_breakdowns_dynamic == "Все"){
    cancelingTableNew$data %>% 
      filter(Дата >= input$date_canceledOrders_prop[1] & Дата <= input$date_canceledOrders_prop[2]) %>% 
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата) %>%
      summarise(Рейсы.x = sum(Рейсы.x), Рейсы.y = sum(Рейсы.y), Отношение = round(Рейсы.y/Рейсы.x * 100, digits = 2))
    } else {
      cancelingTableNew$data %>% 
        filter(Дата >= input$date_canceledOrders_prop[1] & Дата <= input$date_canceledOrders_prop[2]) %>% 
        filter(Клиент == input$client_canceling_breakdowns_dynamic) %>% 
        mutate(Дата = format(Дата, aggregation)) %>%
        group_by(Дата) %>%
        summarise(Рейсы.x = sum(Рейсы.x), Рейсы.y = sum(Рейсы.y), Отношение = round(Рейсы.y/Рейсы.x * 100, digits = 2))
    }
  })
  
  
  output$cancelingTableProportion <- 
    renderReactable(reactable(selected_canceling_new(),
                              columns = list(
                                Клиент = colDef(footer = "Итог"),
                                Рейсы.x = colDef(name = "Всего", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                                Рейсы.y = colDef(name = "Отказы", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                                Отношение = colDef(na = "-", format = colFormat(percent = TRUE, digits = 2), 
                                                   aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы.y']
                                              totalUnits += row['Рейсы.x']
                                            })
                                            return totalPrice / totalUnits
                                          }"))
                              ),
                              groupBy = c("Клиент"),
                              striped = TRUE,
                              bordered = TRUE,
                              borderless = FALSE,
                              highlight = TRUE,
                              resizable = FALSE,
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
                              ))
  
  # Доля срывов
  
  selected_breakdowns_new <- reactive({
    
    aggregation <- switch(input$aggregationCanceledOrders, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    breakdownsTableNew$data %>% 
      filter(Дата >= input$date_canceledOrders_prop[1] & Дата <= input$date_canceledOrders_prop[2]) %>% 
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата, Клиент) %>%
      summarise(Рейсы.x = sum(Рейсы.x), Рейсы.y = sum(Рейсы.y), Отношение = Рейсы.y/Рейсы.x, Рейсы.x.x = sum(Рейсы.x.x), Номера_заявок = toString(unique(Номера_заявок)), Рейсы.y.y = sum(Рейсы.y.y))
    
  })
  
  selected_breakdowns_new_general_plot <- reactive({
    
    aggregation <- switch(input$aggregationCanceledOrders, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    if (input$client_canceling_breakdowns_dynamic == "Все"){
    breakdownsTableNew$data %>% 
      filter(Дата >= input$date_canceledOrders_prop[1] & Дата <= input$date_canceledOrders_prop[2]) %>% 
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата) %>%
      summarise(Рейсы.x = sum(Рейсы.x), Рейсы.y = sum(Рейсы.y), Отношение = round(Рейсы.y/Рейсы.x * 100, digits = 1), Рейсы.x.x = sum(Рейсы.x.x), Рейсы.y.y = sum(Рейсы.y.y))
    } else {
      breakdownsTableNew$data %>% 
        filter(Дата >= input$date_canceledOrders_prop[1] & Дата <= input$date_canceledOrders_prop[2]) %>%
        filter(Клиент == input$client_canceling_breakdowns_dynamic) %>%
        mutate(Дата = format(Дата, aggregation)) %>%
        group_by(Дата) %>%
        summarise(Рейсы.x = sum(Рейсы.x), Рейсы.y = sum(Рейсы.y), Отношение = round(Рейсы.y/Рейсы.x * 100, digits = 1), Рейсы.x.x = sum(Рейсы.x.x), Рейсы.y.y = sum(Рейсы.y.y))
    }
  })
  
  output$breakdownsTableProportion <- 
    renderReactable(reactable(selected_breakdowns_new(),
                              columns = list(
                                Клиент = colDef(footer = "Итог"),
                                Рейсы.x = colDef(name = "Всего", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
        Рейсы.y = colDef(name = "Срывы и просроченные", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
        
        Рейсы.x.x = colDef(name = "Срывы", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
        Номера_заявок = colDef(name = "Номера срывов", aggregate = "unique"),
        Рейсы.y.y = colDef(name = "Просроченные", aggregate = "sum", footer = JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }")),
                                Отношение = colDef(na = "-", format = colFormat(percent = TRUE, digits = 2), 
                                                   aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы.y']
                                              totalUnits += row['Рейсы.x']
                                            })
                                            return totalPrice / totalUnits
                                          }"))
                              ),
                              groupBy = c("Клиент"),
                              striped = TRUE,
                              bordered = TRUE,
                              borderless = FALSE,
                              highlight = TRUE,
                              resizable = FALSE,
                              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    ))
  
  
  output$totalRoutes <- renderText(
    paste("Всего рейсов:", format(sum(selected_breakdowns_new()$Рейсы.x), big.mark=" "))
  )
  
  output$totalCanceling <- renderText(
    paste("Отказы от рейсов:", format(sum(selected_canceling_new()$Рейсы.y), big.mark=" "), "(", round(sum(selected_canceling_new()$Рейсы.y) / sum(selected_canceling_new()$Рейсы.x) * 100, digits = 2), "%)")
  )
  
  output$totalBreakdowns <- renderText(
    paste("Срывы:", format(sum(selected_breakdowns_new()$Рейсы.y), big.mark=" "), "(", round(sum(selected_breakdowns_new()$Рейсы.y) / sum(selected_canceling_new()$Рейсы.x) * 100, digits = 2), "%)")
  )
  
  output$periodBreakdowns <- renderText(
    paste("За период", format(input$date_canceledOrders_prop[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders_prop[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
  )
  
  output$canceling_breakdowns_dynamic_general <- renderHighchart({
    
    highchart() %>% 
      hc_chart(type ="spline") %>%
      hc_xAxis(categories = selected_canceling_new_general_plot()$Дата) %>% 
      hc_add_series(data = selected_canceling_new_general_plot()$Отношение, name = "Доля отказов, %") %>%
      hc_add_series(data = selected_breakdowns_new_general_plot()$Отношение, name = "Доля срывов, %") %>%
      hc_title(text = "Динамика отказов и срывов",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$date_canceledOrders_prop[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders_prop[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По созданным заявкам")
    
  })
  
  output$downloadCanceledOrders_prop <- downloadHandler(
    filename = function() {
      paste("Отказы и срывы - ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Отказы")
      addWorksheet(wb, "Срывы")
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$date_canceledOrders_prop[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders_prop[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = selected_canceling_new() %>% rename("Всего" = 3, "Отказы" = 4),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 2,
                x = paste("За период", format(input$date_canceledOrders_prop[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders_prop[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 2, 
                x = selected_breakdowns_new() %>% rename("Всего" = 3, "Срывы и просроченные" = 4, "Срывы" = 6, "Номера срывов" = 7, "Просроченные" = 8),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(selected_canceling_new()), widths = c(10, 40, 10, 10, 10))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 2, cols = 1:ncol(selected_breakdowns_new()), widths = c(10, 40, 10, 10, 10))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 60)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  # Индикатив цен
  
  output$indicative_selected_route_name <- renderText(
    paste("Маршрут:", selected_indicativeTable()[selected(), ]$Маршрут)
  )
  
  output$indicative_selected_route_distance <- renderText(
    # paste("Расстояние:", format(selected_indicative_route()$distance / 1000, big.mark = " ", scientific = FALSE, digits = 0), "км")
    paste("Расстояние:", format(selected_indicative_route()$distance, big.mark = " ", scientific = FALSE, digits = 0), "км")
    
  )
  
  output$carrier_rate_ind <- renderText(
    paste(format(selected_indicativeTable()[selected(), ]$Ставка_клиента_НДС * (1 - input$profitability_ind / 100), digits = 2, big.mark=" ", scientific = FALSE), paste0("(", format(selected_indicativeTable()[selected(), ]$Ставка_клиента * (1 - input$profitability_ind / 100), digits = 2, big.mark=" ", scientific = FALSE), ")"), "руб")
  )
  
  output$price_per_km_for_carrier_ind <- renderText(
    # paste(format(selected_indicativeTable()[selected(), ]$Ставка_клиента_НДС * (1 - input$profitability_ind / 100) / (selected_indicative_route()$distance / 1000), digits = 2, big.mark=" ", scientific = FALSE), paste0("(", format(selected_indicativeTable()[selected(), ]$Ставка_клиента * (1 - input$profitability_ind / 100) / (selected_indicative_route()$distance / 1000), digits = 2, big.mark=" ", scientific = FALSE), ")"), "руб")
    paste(format(selected_indicativeTable()[selected(), ]$Ставка_клиента_НДС * (1 - input$profitability_ind / 100) / (selected_indicative_route()$distance), digits = 2, big.mark=" ", scientific = FALSE), paste0("(", format(selected_indicativeTable()[selected(), ]$Ставка_клиента * (1 - input$profitability_ind / 100) / (selected_indicative_route()$distance), digits = 2, big.mark=" ", scientific = FALSE), ")"), "руб")
    
  )
  
  selected_indicativeTable <- reactive({
    if(!is.null(indicativeTable$data)){
      
    filtered <- indicativeTable$data %>% 
      filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2])
    
    if (!is.null(input$indicativeTypeOfBody)) {
      filtered <- filtered %>% filter(Тип_ТС %in% input$indicativeTypeOfBody)
    }
    if (!is.null(input$indicativeCapacityAndVolume)) {
      filtered <- filtered %>% filter(Объём_тоннаж %in% input$indicativeCapacityAndVolume)
    } 
    if (!is.null(input$indicativeClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$indicativeClient)
    } 
    
    filtered %>% 
      group_by(Регион_загрузки, Регион_выгрузки, Маршрут) %>%
      summarise(Рейсы = n(),
                     Ставка_клиента_НДС = median(Ставка_клиента_НДС),
                     Ставка_клиента = median(Ставка_клиента),
                     Ставка_перевозчика_НДС = median(Ставка_перевозчика_НДС),
                     Ставка_перевозчика = median(Ставка_перевозчика),
                     Маржа = median(Маржа),
                     loading_latitude = first(loading_latitude),
                     loading_longitude = first(loading_longitude),
                     unloading_latitude = last(unloading_latitude),
                     unloading_longitude = last(unloading_longitude)
      ) %>%
      
      arrange(desc(Рейсы))
    }
  })
  
  
  # selected_indicativeTable1 <- reactive({
  #   indicativeTable1$data %>% 
  #     filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2]) %>%
  #     group_by(Маршрут, Тип_ТС) %>%
  #     summarise(Рейсы = n(),
  #                         base_price_net = median(base_price_net),
  #                         base_price_gross = median(base_price_gross)
  #                         )
  # })
  # 
  # selected_indicativeTable2 <- reactive({
  #   indicativeTable2$data %>% 
  #     filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2]) %>%
  #     group_by(Маршрут, Тип_ТС) %>%
  #     summarise(Рейсы = n(),
  #                         base_price_net = median(base_price_net),
  #                         base_price_gross = median(base_price_gross)
  #                    )
  # })
  
  # пока строка в таблице не выбрана — показываем первый маршрут
  selected <- reactive({
    s <- getReactableState("indicativeTable", "selected")
    if (is.null(s) || length(s) == 0) 1 else s
  })
  
  output$indicativeTable <- renderReactable(
    if(!is.null(selected_indicativeTable())){

    reactable(selected_indicativeTable(),
              defaultSorted = c("Рейсы"),
              defaultSelected = 1,
              selection = "single", 
              onClick = "select",
              groupBy = c("Регион_загрузки", "Регион_выгрузки"),
              columns = list(
                Регион_загрузки = colDef(name = "Регион погрузки"),
                Регион_выгрузки = colDef(name = "Регион разгрузки"),
                # Тип_ТС = colDef(name = "Тип ТС"),
                # Объём_тоннаж = colDef(name = "Объём/Тоннаж"),
                # Дата_создания_заявки = colDef(name = "Дата создания заявки"),
                # Тип_ТС = colDef(name = "Тип ТС", aggregate = "unique"),
                Рейсы = colDef(defaultSortOrder = "desc", aggregate = "sum"),
                Ставка_клиента_НДС = colDef(name = "Ставка клиента, НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Ставка_клиента = colDef(name = "Ставка клиента", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Ставка_перевозчика = colDef(name = "Ставка перевозчика, без НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Ставка_перевозчика_НДС = colDef(name = "Ставка перевозчика, с НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Маржа = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                loading_latitude = colDef(show = FALSE),
                loading_longitude = colDef(show = FALSE),
                unloading_latitude = colDef(show = FALSE),
                unloading_longitude = colDef(show = FALSE)
                # boxplot = colDef(cell = function(value, index) {
                #   sparkline(selected_indicativeTable()$boxplot[[index]], type = "box")
                # })
              ),
              filterable = TRUE,
              searchable = TRUE,
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE,
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
              )
    }
  )
  
  output$indicativeTable_source <- renderReactable(
    if(!is.null(selected_indicativeTableERP())){
      
    reactable(selected_indicativeTableERP(),
              columns = list(
                Регион_загрузки = colDef(name = "Регион погрузки"),
                Регион_выгрузки = colDef(name = "Регион разгрузки"),
                Тип_ТС = colDef(name = "Тип ТС"),
                Объём_тоннаж = colDef(name = "Объём/Тоннаж"),
                Дата_создания_заявки = colDef(name = "Дата создания заявки", format = colFormat(date = TRUE)),
                Тип_ТС = colDef(name = "Тип ТС", aggregate = "unique"),
                Ставка_клиента_НДС = colDef(name = "Ставка клиента, с НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Ставка_клиента = colDef(name = "Ставка клиента, без НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Ставка_перевозчика = colDef(name = "Ставка перевозчика, без НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Ставка_перевозчика_НДС = colDef(name = "Ставка перевозчика, с НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                Маржа = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
                loading_latitude = colDef(show = FALSE),
                loading_longitude = colDef(show = FALSE),
                unloading_latitude = colDef(show = FALSE),
                unloading_longitude = colDef(show = FALSE)
              ),
              filterable = TRUE,
              searchable = TRUE,
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE
    )
    }
  )
  
  selected_routes_for_indicative_statistics <- reactive({
    filtered <- indicativeTable$data %>%
      filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2])

    if (!is.null(input$indicativeTypeOfBody)) {
      filtered <- filtered %>% filter(Тип_ТС %in% input$indicativeTypeOfBody)
    }
    if (!is.null(input$indicativeCapacityAndVolume)) {
      filtered <- filtered %>% filter(Объём_тоннаж %in% input$indicativeCapacityAndVolume)
    }
    if (!is.null(input$indicativeClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$indicativeClient)
    }
    
    distance = 1
    if(input$priceForKmCheckbox) {
    # distance <- selected_indicative_route()$distance / 1000
      distance <- selected_indicative_route()$distance
    } else {
      distance = 1
    }
    
    client <- filtered %>%
      filter(Маршрут == selected_indicativeTable()[selected(), ]$Маршрут) %>%
      group_by(Маршрут) %>%
      summarise(     
        Среднее = mean(Ставка_клиента),
        Медиана = median(Ставка_клиента),
        Минимум = min(Ставка_клиента),
        Максимум = max(Ставка_клиента)
      ) %>%
      transmute(
        Показатель = "Ставка клиента, без НДС",
        Среднее = Среднее / distance,
        Медиана = Медиана / distance,
        Минимум = Минимум / distance,
        Максимум = Максимум / distance
        )
    
    client_NDS <- filtered %>%
      filter(Маршрут == selected_indicativeTable()[selected(), ]$Маршрут) %>%
      group_by(Маршрут) %>%
      summarise(     
        Среднее = mean(Ставка_клиента_НДС),
        Медиана = median(Ставка_клиента_НДС),
        Минимум = min(Ставка_клиента_НДС),
        Максимум = max(Ставка_клиента_НДС)
      ) %>%
      transmute(
        Показатель = "Ставка клиента, с НДС",
        Среднее = Среднее / distance,
        Медиана = Медиана / distance,
        Минимум = Минимум / distance,
        Максимум = Максимум / distance
      )
    
    carriers <- filtered %>%
      filter(Маршрут == selected_indicativeTable()[selected(), ]$Маршрут) %>%
      group_by(Маршрут) %>%
      summarise(
                Среднее = mean(Ставка_перевозчика),
                Медиана = median(Ставка_перевозчика),
                Минимум = min(Ставка_перевозчика),
                Максимум = max(Ставка_перевозчика)
      ) %>%
      transmute(
        Показатель = "Ставка перевозчика, без НДС",
        Среднее = Среднее / distance,
        Медиана = Медиана / distance,
        Минимум = Минимум / distance,
        Максимум = Максимум / distance
      )
    
    carriers_NDS <- filtered %>%
      filter(Маршрут == selected_indicativeTable()[selected(), ]$Маршрут) %>%
      group_by(Маршрут) %>%
      summarise(
                          Среднее = mean(Ставка_перевозчика_НДС),
                          Медиана = median(Ставка_перевозчика_НДС),
                          Минимум = min(Ставка_перевозчика_НДС),
                          Максимум = max(Ставка_перевозчика_НДС)
      ) %>%
      transmute(
        Показатель = "Ставка перевозчика, с НДС",
        Среднее = Среднее / distance,
        Медиана = Медиана / distance,
        Минимум = Минимум / distance,
        Максимум = Максимум / distance
      )
    
    result = rbind(client_NDS, client, carriers_NDS, carriers)
  })
  
  output$indicativeTableDetailStatistics <- renderReactable(
    reactable(selected_routes_for_indicative_statistics(),
              columns = list(
                Среднее = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Медиана = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Минимум = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Максимум = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0))
              )
              )
  )
  
  # Маршрут - пространтсвенный объект
  selected_indicative_route <- reactive({
    if(!is.null(indicativeTable$data)){
      
      # start_point <- c(selected_indicativeTable()[selected(), ]$loading_latitude, selected_indicativeTable()[selected(), ]$loading_longitude)
      # end_point <- c(selected_indicativeTable()[selected(), ]$unloading_latitude, selected_indicativeTable()[selected(), ]$unloading_longitude)
      # 
      # route <- gh_get_route(list(start_point, end_point), profile='truck', elevation = FALSE, locale='ru-RU') %>%
      #   gh_as_sf()
      
      pts <- structure(
        list(x = c(selected_indicativeTable()[selected(), ]$loading_longitude, selected_indicativeTable()[selected(), ]$unloading_longitude),
             y = c(selected_indicativeTable()[selected(), ]$loading_latitude, selected_indicativeTable()[selected(), ]$unloading_latitude)),
        class = "data.frame", row.names = c(NA, -10L))
      route <- safe_osrm_route(pts)
      
      
    }  
  })
  
  # Точка загрузки
  selected_indicative_start_point <- reactive({
    if(!is.null(indicativeTable$data)){
      
      start_point <-  st_point(c(selected_indicativeTable()[selected(), ]$loading_longitude, selected_indicativeTable()[selected(), ]$loading_latitude))
    }  
  })
  
  # Точка выгрузки
  selected_indicative_finish_point <- reactive({
    if(!is.null(indicativeTable$data)){
      
      finish_point <-  st_point(c(selected_indicativeTable()[selected(), ]$unloading_longitude, selected_indicativeTable()[selected(), ]$unloading_latitude))
    }  
  })
  
  output$indicative_map <- renderLeaflet({
    
    if(!is.null(selected_indicative_route())){
      
      
      bbox <- st_bbox(selected_indicative_route()) %>%
        as.vector()
      
      leaflet() %>%
        addTiles(group = "Карта") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Спутник") %>%
        fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
        # addPolylines(data = selected_indicative_route(), color = "red", popup = paste("Расстояние:", round(selected_indicative_route()$distance / 1000, digits = 2), "км", "\n Продолжительность:", round(selected_indicative_route()$time / 1000 / 3600, digits = 2), "ч", "\n Стоимость:", round(median(selected_indicativeTableERP()$Ставка_клиента) / (selected_indicative_route()$distance / 1000), digits = 2), "\U20BD на км"), group = "Прямой рейс", label = "Маршрут") %>%
        addPolylines(data = selected_indicative_route(), color = "red", popup = paste("Расстояние:", round(selected_indicative_route()$distance, digits = 2), "км", "\n Продолжительность:", round(selected_indicative_route()$duration / 60, digits = 2), "ч", "\n Стоимость (Ставка клиента с НДС):", round(median(selected_indicativeTableERP()$Ставка_клиента_НДС) / (selected_indicative_route()$distance), digits = 2), "\U20BD на км"), group = "Прямой рейс", label = "Маршрут") %>%
        addAwesomeMarkers(icon = icons_direct, data = selected_indicative_start_point(), popup = "Точка загрузки", label = "Точка загрузки") %>%
        addAwesomeMarkers(icon = icons_direct1, data = selected_indicative_finish_point(), popup = "Точка выгрузки", label = "Точка выгрузки") %>%
        addLayersControl(
          baseGroups = c("Карта", "Спутник")
          )
      
    }  
  })
  
  
  # output$indicativeTable1 <- renderReactable(
  #   reactable(selected_indicativeTable1(),
  #             defaultSorted = c("Рейсы"),
  #             groupBy = c("Маршрут"),
  #             columns = list(
  #               Тип_ТС = colDef(name = "Тип ТС", aggregate = "unique"),
  #               Рейсы = colDef(defaultSortOrder = "desc", aggregate = "sum"),
  #               base_price_net = colDef(name = "Базовая ставка, без НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
  #               base_price_gross = colDef(name = "Базовая ставка, с НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median")
  #             ),
  #             searchable = TRUE,
  #             striped = TRUE,
  #             bordered = TRUE,
  #             borderless = FALSE,
  #             highlight = TRUE,
  #             resizable = FALSE,
  #   )
  # )
  # 
  # 
  # output$indicativeTable2 <- renderReactable(
  #   reactable(selected_indicativeTable2(),
  #             defaultSorted = c("Рейсы"),
  #             groupBy = c("Маршрут"),
  #             columns = list(
  #               Тип_ТС = colDef(name = "Тип ТС", aggregate = "unique"),
  #               Рейсы = colDef(defaultSortOrder = "desc", aggregate = "sum"),
  #               base_price_net = colDef(name = "Базовая ставка, без НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median"),
  #               base_price_gross = colDef(name = "Базовая ставка, с НДС", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "median")
  #               ),
  #             searchable = TRUE,
  #             striped = TRUE,
  #             bordered = TRUE,
  #             borderless = FALSE,
  #             highlight = TRUE,
  #             resizable = FALSE,
  #   )
  # )
  
  # Набор данных для отображения динамики
  selected_indicativeTableERP <- reactive({
    if(!is.null(indicativeTable$data)){
    filtered <- indicativeTable$data %>% 
      filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2])
    
    if (!is.null(input$indicativeTypeOfBody)) {
      filtered <- filtered %>% filter(Тип_ТС %in% input$indicativeTypeOfBody)
    }
    if (!is.null(input$indicativeCapacityAndVolume)) {
      filtered <- filtered %>% filter(Объём_тоннаж %in% input$indicativeCapacityAndVolume)
    } 
    if (!is.null(input$indicativeClient)) {
      filtered <- filtered %>% filter(Клиент %in% input$indicativeClient)
    } 
    
    filtered %>%
      filter(Маршрут == selected_indicativeTable()[selected(), ]$Маршрут)
    }
  })
  
  # selected_indicativeTableTransporeon <- reactive({
  #   indicativeTable1$data %>% 
  #     filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2]) %>%
  #     filter(Маршрут == input$routeTransporeon)
  # })
  # 
  # selected_indicativeTableInbev <- reactive({
  #   indicativeTable2$data %>% 
  #     filter(Дата_создания_заявки >= input$dateIndicative[1] & Дата_создания_заявки <= input$dateIndicative[2]) %>%
  #     filter(Маршрут == input$routeInbev)
  # })
  

  

  
  output$priceSplineERP <- renderHighchart({
    highchart() %>% 
      hc_chart(type ="spline") %>%
      hc_xAxis(categories = selected_indicativeTableERP()$Дата_создания_заявки) %>% 
      hc_add_series(data = selected_indicativeTableERP()$Ставка_клиента, name = "Ставка клиента") %>%
      hc_title(text = paste('Динамика ставки клиента по маршруту', input$routeERP),
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$dateIndicative[1], "%d.%m.%Y"), "-", format(input$dateIndicative[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По данным ERP")
  })
  
  # output$priceSplineTransporeon <- renderHighchart({
  #   highchart() %>% 
  #     hc_chart(type ="spline") %>%
  #     hc_xAxis(categories = selected_indicativeTableTransporeon()$Дата_создания_заявки) %>% 
  #     hc_add_series(data = selected_indicativeTableTransporeon()$base_price_net, name = "Ставка клиента") %>%
  #     hc_title(text = paste('Динамика ставки клиента по маршруту', input$routeTransporeon),
  #              style = list(fontWeight = "bold", fontSize = "14px"),
  #              align = "center") %>%
  #     
  #     hc_subtitle(text = paste("За период", format(input$dateIndicative[1], "%d.%m.%Y"), "-", format(input$dateIndicative[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
  #                 style = list(fontWeight = "bold"),
  #                 align = "center")  %>%
  #     
  #     hc_add_theme(hc_theme_ffx()) %>%
  #     
  #     hc_credits(enabled = T, text = "По данным torgtrans")
  # })
  # 
  # output$priceSplineInbev <- renderHighchart({
  #   highchart() %>% 
  #     hc_chart(type ="spline") %>%
  #     hc_xAxis(categories = selected_indicativeTableInbev()$Дата_создания_заявки) %>% 
  #     hc_add_series(data = selected_indicativeTableInbev()$base_price_net, name = "Ставка клиента") %>%
  #     hc_title(text = paste('Динамика ставки клиента по маршруту', input$routeInbev),
  #              style = list(fontWeight = "bold", fontSize = "14px"),
  #              align = "center") %>%
  #     
  #     hc_subtitle(text = paste("За период", format(input$dateIndicative[1], "%d.%m.%Y"), "-", format(input$dateIndicative[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
  #                 style = list(fontWeight = "bold"),
  #                 align = "center")  %>%
  #     
  #     hc_add_theme(hc_theme_ffx()) %>%
  #     
  #     hc_credits(enabled = T, text = "По данным inbev")
  # })
  
  
  # Сводный отчёт по споту
  
  selected_spotTable <- reactive({
    
    aggregation <- switch(input$aggregationSpot, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
      spotTable$data %>% 
        filter(Дата >= input$dateSpot[1] & Дата <= input$dateSpot[2]) %>% 
        mutate(Дата = format(Дата, aggregation)) %>%
       group_by(Дата, Клиент) %>%
        summarise(Заявки.x = sum(Заявки.x), Заявки.y = sum(Заявки.y), Отношение = report_ratio(Заявки.y, Заявки.x))

  })
  
  selected_spotTable_general_plot <- reactive({
    
    aggregation <- switch(input$aggregationSpot, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    spotTable$data %>% 
      filter(Дата >= input$dateSpot[1] & Дата <= input$dateSpot[2]) %>% 
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата) %>%
      summarise(Заявки.x = sum(Заявки.x), Заявки.y = sum(Заявки.y), Отношение = report_ratio(Заявки.y, Заявки.x))
    
  })
  
  selected_spotTable1 <- reactive({
    
    startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
    endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
    
    getDetailSpotForInbev(startDate, endDate) %>% 
      filter(Дата_торгов >= input$dateSpot[1] & Дата_торгов <= input$dateSpot[2])
    
  })
  
  selected_spotTable2 <- reactive({
    
    startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
    endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
    
    getDetailSpotForBaltika(startDate, endDate) %>% 
      filter(Дата_торгов >= input$dateSpot[1] & Дата_торгов <= input$dateSpot[2])
    
  })
  
  selected_spotTable3 <- reactive({
    
    startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
    endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
    
    getDetailSpotForTransporeon(startDate, endDate) %>% 
      filter(Дата_торгов >= input$dateSpot[1] & Дата_торгов <= input$dateSpot[2])
    
  })
  
  selected_spotTable4 <- reactive({
    
    startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
    endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
    
    getDetailSpotForPepsi(startDate, endDate) %>% 
      filter(Дата_торгов >= input$dateSpot[1] & Дата_торгов <= input$dateSpot[2])
    
  })
  
  selected_spotTable5 <- reactive({
    
    startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
    endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
    
    getDetailSpotForTorgtrans(startDate, endDate) %>% 
      filter(Дата_торгов >= input$dateSpot[1] & Дата_торгов <= input$dateSpot[2])
    
  })
  
  selected_spotTable6 <- reactive({
    
    startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
    endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
    
    getDetailSpotForMPK(startDate, endDate) %>% 
      filter(Дата_торгов >= input$dateSpot[1] & Дата_торгов <= input$dateSpot[2])
    
  })
  
  selected_spotTable7 <- reactive({
    
    startDate <- as.POSIXct(input$dateSpot[1], format="%Y-%m-%d") - 24*60*60 - 1
    endDate <- as.POSIXct(input$dateSpot[2], format="%Y-%m-%d") + 24*60*60 - 1
    
    getDetailSpotForUniconf(startDate, endDate) %>% 
      filter(Дата_торгов >= input$dateSpot[1] & Дата_торгов <= input$dateSpot[2])
    
  })
  
  spot_ratio_aggregate <- JS("function(values, rows) {
    var numerator = 0
    var denominator = 0
    rows.forEach(function(row) {
      numerator += Number(row['Заявки.y'])
      denominator += Number(row['Заявки.x'])
    })
    var ratio = numerator / denominator
    return Number.isFinite(ratio) ? ratio : null
  }")
  spot_ratio_cell <- JS("function(cellInfo) {
    var value = cellInfo.value
    if (value == null || !Number.isFinite(value)) return '\u2013'
    return (value * 100).toLocaleString('ru-RU', { minimumFractionDigits: 2, maximumFractionDigits: 2 }) + ' %'
  }")

  output$spotTable <- renderReactable(
    reactable(selected_spotTable(),
              columns = list(
                Заявки.x = colDef(name = "Спот от клиента", aggregate = "sum"),
                Заявки.y = colDef(name = "Спот из ERP по созданным заявкам", aggregate = "sum"),
                Отношение = colDef(na = "–",
                                   aggregate = spot_ratio_aggregate,
                                   cell = spot_ratio_cell,
                                   aggregated = spot_ratio_cell)
                                      ),
              groupBy = c("Клиент"),
              striped = TRUE,
              bordered = TRUE,
              borderless = FALSE,
              highlight = TRUE,
              resizable = FALSE
              )
  )
  

  output$spotPlot <- renderHighchart({

    highchart() %>% 
      hc_chart(type ="column") %>%
      hc_xAxis(categories = filter(selected_spotTable(), Клиент == 'Клиент 01')$Дата) %>%
      hc_add_series(data = filter(selected_spotTable(), Клиент == 'Клиент 01')$Заявки.x, name = "Спот от клиента") %>%
      hc_add_series(data = filter(selected_spotTable(), Клиент == 'Клиент 01')$Заявки.y, name = "Спот из ERP") %>%
      hc_title(text = 'Спотовые заявки Клиент 01',
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%

      hc_subtitle(text = paste("За период", format(input$dateSpot[1], "%d.%m.%Y"), "-", format(input$dateSpot[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%

      hc_add_theme(hc_theme_ffx()) %>%

      hc_credits(enabled = T, text = "По данным abi-loadboard.auth0.com")

  })
  
  output$spotPlot1 <- renderHighchart({
    
    highchart() %>% 
      hc_chart(type ="column") %>%
      hc_xAxis(categories = filter(selected_spotTable(), Клиент == 'Клиент 10')$Дата) %>%
      hc_add_series(data = filter(selected_spotTable(), Клиент == 'Клиент 10')$Заявки.x, name = "Спот от клиента") %>%
      hc_add_series(data = filter(selected_spotTable(), Клиент == 'Клиент 10')$Заявки.y, name = "Спот из ERP") %>%
      hc_title(text = 'Спотовые заявки Клиент 10',
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$dateSpot[1], "%d.%m.%Y"), "-", format(input$dateSpot[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По данным torgtrans.com")
    
  })
  
  output$spotPlot2 <- renderHighchart({
    
    highchart() %>% 
      hc_chart(type ="column") %>%
      hc_xAxis(categories = filter(selected_spotTable(), Клиент == 'Клиент 09')$Дата) %>%
      hc_add_series(data = filter(selected_spotTable(), Клиент == 'Клиент 09')$Заявки.x, name = "Спот от клиента") %>%
      hc_add_series(data = filter(selected_spotTable(), Клиент == 'Клиент 09')$Заявки.y, name = "Спот из ERP") %>%
      hc_title(text = 'Спотовые заявки Клиент 09',
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$dateSpot[1], "%d.%m.%Y"), "-", format(input$dateSpot[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По данным torgtrans.com")
    
  })
  
  output$spot_dynamic_general <- renderHighchart({
    
    highchart() %>% 
      hc_chart(type ="spline") %>%
      hc_xAxis(categories = selected_spotTable_general_plot()$Дата) %>% 
      hc_add_series(data = selected_spotTable_general_plot()$Заявки.y, name = "Спотовые заявки из ERP") %>%
      hc_title(text = "Динамика спотовых заявок",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$dateSpot[1], "%d.%m.%Y"), "-", format(input$dateSpot[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По созданным заявкам")
    
  })
  
  output$downloadSpot <- downloadHandler(
    filename = function() {
      paste("Спот - ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Формирование excel-файла", value = 0.2)

      wb <- createWorkbook()
      
      addWorksheet(wb, "Спот")
      addWorksheet(wb, "ИнБев")
      addWorksheet(wb, "Клиент 08")
      addWorksheet(wb, "Транспореон")
      addWorksheet(wb, "ПепсиКо")
      addWorksheet(wb, "Торгтранс")
      addWorksheet(wb, "МПК")
      addWorksheet(wb, "Кондитеры")
      
      
      progress$inc(0.4, detail = "Получение данных")
  
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateSpot[1], "%d.%m.%Y"), "-", format(input$dateSpot[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = selected_spotTable() %>% rename("Спот от клиента" = 3, "Спот из ERP по созданным заявкам" = 4) %>% mutate(Отношение = round(Отношение * 100, digits = 2)),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb, 
                sheet = 2, 
                x = selected_spotTable1() %>% rename("Дата торгов" = 2, "День недели" = 3), 
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb, 
                sheet = 3, 
                x = selected_spotTable2() %>% rename("Дата торгов" = 2, "День недели" = 3), 
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb, 
                sheet = 4, 
                x = selected_spotTable3() %>% rename("Дата торгов" = 2, "День недели" = 3),  
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb, 
                sheet = 5, 
                x = selected_spotTable4() %>% rename("Дата торгов" = 2, "День недели" = 3), 
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      
      writeData(wb, 
                sheet = 6, 
                x = selected_spotTable5() %>% rename("Дата торгов" = 2, "День недели" = 3), 
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb, 
                sheet = 7, 
                x = selected_spotTable6() %>% rename("Дата торгов" = 2, "День недели" = 3), 
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb, 
                sheet = 8, 
                x = selected_spotTable7() %>% rename("Дата торгов" = 2, "День недели" = 3), 
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      progress$inc(0.6, detail = "Данные получены")
      
      progress$inc(0.8, detail = "Начало записи файла")
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(selected_spotTable()), widths = c(10, 40, 10, 10, 10))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 2, cols = 1:ncol(selected_spotTable1()), widths = c(20, 10, 15, 30, 10, 10, 10, 15, 15, 50, 10, 10, 15, 15, 50, 10, 10, 10, 10, 10, 15, 10, 10, 10))
      setRowHeights(wb, sheet = 2, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 3, cols = 1:ncol(selected_spotTable2()), widths = c(50, 10, 15, 30, 10, 10, 10, 15, 15, 50, 10, 10, 15, 15, 50, 10, 10, 10, 10, 10, 15, 10, 10))
      setRowHeights(wb, sheet = 3, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 4, cols = 1:ncol(selected_spotTable3()), widths = c(50, 10, 15, 30, 10, 10, 10, 15, 15, 50, 10, 10, 15, 15, 50, 10, 10, 10, 10, 10, 15, 10, 10, 10))
      setRowHeights(wb, sheet = 4, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 5, cols = 1:ncol(selected_spotTable4()), widths = c(50, 10, 15, 30, 10, 10, 10, 15, 15, 50, 10, 10, 15, 15, 50, 10, 10, 10, 10, 10, 15, 10, 10))
      setRowHeights(wb, sheet = 5, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 6, cols = 1:ncol(selected_spotTable5()), widths = c(50, 10, 15, 30, 10, 10, 10, 15, 15, 50, 10, 10, 15, 15, 50, 10, 10, 10, 10, 10, 15, 10, 10))
      setRowHeights(wb, sheet = 6, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 7, cols = 1:ncol(selected_spotTable6()), widths = c(50, 10, 15, 30, 10, 10, 10, 15, 15, 50, 10, 10, 15, 15, 50, 10, 10, 10, 10, 10, 15, 10, 10))
      setRowHeights(wb, sheet = 7, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 8, cols = 1:ncol(selected_spotTable7()), widths = c(50, 10, 15, 30, 10, 10, 10, 15, 15, 50, 10, 10, 15, 15, 50, 10, 10, 10, 10, 10, 15, 10, 10))
      setRowHeights(wb, sheet = 8, rows = 2, heights = 60)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      progress$inc(1, detail = "Файл сформирован")
      
      
    }
  )
  
  # Выработка логистов
  
  selected_LogisticKPI_outcome <- reactive({
    filtered <- logisticKPI$data
    if (!is.null(input$logistJobForLogisticProductivity)) {
      filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
    } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    } 
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
    
    filtered <- filtered %>% 
      group_by(Дата, Логист, Группа) %>%
      summarise(
        Рейсы = sum(Рейсы),
        Оборот = sum(Оборот),
        # Выручка = sum(Выручка),
        Маржа = sum(Маржа),
        # Рентабельность = sum(Маржа) / sum(Выручка),
        Рентабельность = report_ratio(sum(Маржа), sum(Оборот)),
        Договоры = sum(Договоры, na.rm = TRUE),
      ) %>%
      inner_join(production_calendar_for_dates(filtered$Дата), by = c("Дата" = "Дата")) %>%
      group_by(Логист, Группа) %>%
      summarise(
        Рейсы = sum(Рейсы),
        Оборот = sum(Оборот),
        # Выручка = sum(Выручка),
        Маржа = sum(Маржа),
        # Рентабельность = sum(Маржа) / sum(Выручка),
        Рентабельность = report_ratio(sum(Маржа), sum(Оборот)),
        Договоры = sum(Договоры, na.rm = TRUE),
        Рабочих_дней = sum(Рабочих_дней),
        Выработка = report_ratio(sum(Рейсы), sum(Рабочих_дней))
      )
    
    return(filtered)
    
  })
  
  
  selected_LogisticKPI_outcome1 <- reactive({
    filtered <- logisticKPI$data
    if (!is.null(input$logistJobForLogisticProductivity)) {
        filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
      } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    } 
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
      
    filtered <- filtered %>% 
      group_by(Дата, Логист) %>%
      summarise(
        Рейсы = sum(Рейсы),
        Оборот = sum(Оборот),
        # Выручка = sum(Выручка),
        Маржа = sum(Маржа),
        # Рентабельность = sum(Маржа) / sum(Выручка),
        Рентабельность = report_ratio(sum(Маржа), sum(Оборот)),
        Договоры = sum(Договоры, na.rm = TRUE),
      ) %>%
    inner_join(production_calendar_for_dates(filtered$Дата), by = c("Дата" = "Дата")) %>%
      group_by(Логист) %>%
      summarise(
        Рейсы = sum(Рейсы),
        Оборот = sum(Оборот),
        # Выручка = sum(Выручка),
        Маржа = sum(Маржа),
        # Рентабельность = sum(Маржа) / sum(Выручка),
        Рентабельность = report_ratio(sum(Маржа), sum(Оборот)),
        Договоры = sum(Договоры, na.rm = TRUE),
        Рабочих_дней = sum(Рабочих_дней),
        Выработка = report_ratio(sum(Рейсы), sum(Рабочих_дней))
      )
    
    return(filtered)
    
  })
  
  
  
  selected_LogisticKPI_Routes_total <- reactive({
    filtered <- logisticKPI$data
      if (!is.null(input$logistJobForLogisticProductivity)) {
        filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
      } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    } 
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
      
    filtered %>% group_by(Логист) %>%
      summarise(Всего = sum(Рейсы))
  })
  
  selected_LogisticKPI_Routes_byDay <- reactive({
    
    aggregation <- switch(input$aggregation, 
                          "День" = "%d %b",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- logisticKPI$data %>%
        mutate(Дата = format(Дата, aggregation))
      
      if (!is.null(input$logistJobForLogisticProductivity)) {
        filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
      } 
      if (!is.null(input$logistGroupForLogisticProductivity)) {
        filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
      }
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
      
    filtered <- filtered %>% group_by(Дата, Логист, Группа) %>%
        summarise(Рейсы = sum(Рейсы)) %>%
        arrange(desc(Дата)) %>%
        pivot_wider(names_from = Дата,
                    values_from = Рейсы,
                    values_fill = 0) %>%
        left_join(selected_LogisticKPI_Routes_total(), by = c("Логист" = "Логист")) %>%
        arrange(desc(Всего))
      
      filtered[mapply(is.infinite, filtered)] <- 0
      
      return(filtered)
  })
  
  selected_LogisticKPI_Revenue_total <- reactive({
    filtered <- logisticKPI$data
    if (!is.null(input$logistJobForLogisticProductivity)) {
      filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
    } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    }
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
    
    filtered %>% group_by(Логист) %>%
      summarise(Всего = sum(Оборот))
  })
  
  selected_LogisticKPI_Revenue_byDay <- reactive({
    
    aggregation <- switch(input$aggregation, 
                          "День" = "%d %b",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- logisticKPI$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$logistJobForLogisticProductivity)) {
      filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
    } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    }
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
    
    filtered <- filtered %>% group_by(Дата, Логист, Группа) %>%
      summarise(Оборот = sum(Оборот)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Оборот,
                  values_fill = 0) %>%
      left_join(selected_LogisticKPI_Revenue_total(), by = c("Логист" = "Логист")) %>%
      arrange(desc(Всего))
    
    filtered[mapply(is.infinite, filtered)] <- 0
    
    return(filtered)
  })
  
  selected_LogisticKPI_Profit_total <- reactive({
    filtered <- logisticKPI$data
    
    if (!is.null(input$logistJobForLogisticProductivity)) {
      filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
    } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    }
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
    
    filtered %>% group_by(Логист) %>%
      summarise(Всего = sum(Маржа))
  })
  
  selected_LogisticKPI_Profit_byDay <- reactive({
    
    aggregation <- switch(input$aggregation, 
                          "День" = "%d %b",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    filtered <- logisticKPI$data %>%
      mutate(Дата = format(Дата, aggregation))
    
    if (!is.null(input$logistJobForLogisticProductivity)) {
      filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
    } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    }
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
    
    filtered <- filtered %>% group_by(Дата, Логист, Группа) %>%
      summarise(Маржа = sum(Маржа)) %>%
      arrange(desc(Дата)) %>%
      pivot_wider(names_from = Дата,
                  values_from = Маржа,
                  values_fill = 0) %>%
      left_join(selected_LogisticKPI_Profit_total(), by = c("Логист" = "Логист")) %>%
      arrange(desc(Всего))
    
    filtered[mapply(is.infinite, filtered)] <- 0
    
    return(filtered)
  })
  
  selectedFactOnUnloadedTable_manager <- reactive({
    RevenueProfitTable$data %>% 
      filter(Дата >= input$dateManagerPlanFact[1] & Дата <= input$dateManagerPlanFact[2])
  })
  
  selectedFactOnUnloadedTable_manager1 <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      RevenueProfitTable$data %>%
        filter(Дата >= input$dateManagerPlanFact[1] & Дата <= input$dateManagerPlanFact[2]) %>%
        group_by(Менеджер, Клиент) %>%
        summarise(Оборот = sum(Ставка_клиента_НДС), 
                        Маржа = sum(Маржа_расчёт),
                        Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        "Стоимость рейса" = round(Оборот / Рейсы, digits = 2)) %>%
        arrange(desc(Оборот))
    }
  })
  
  selectedFactOnUnloadedTable_logistician <- reactive({
    RevenueProfitTable$data %>% 
      filter(Дата >= input$dateLogisticianPlanFact[1] & Дата <= input$dateLogisticianPlanFact[2])
  })
  
  selected_RevenueProfit <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      RevenueProfitTable$data %>%
        filter(Дата >= input$dateRevenueProfit[1] & Дата <= input$dateRevenueProfit[2]) %>%
        group_by(Клиент) %>%
        summarise(Оборот = sum(Ставка_клиента_НДС), 
                        Маржа = sum(Маржа_расчёт),
                        Рентабельность = round(sum(Маржа_расчёт) / sum(Ставка_клиента) * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        "Стоимость рейса" = round(Оборот / Рейсы, digits = 2)) %>%
        arrange(desc(Оборот))
    }
  })
  
  selected_RevenueProfit1 <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      RevenueProfitTable$data %>%
        filter(Дата >= input$dateRevenueProfit[1] & Дата <= input$dateRevenueProfit[2])
    }
  })
  
  selected_fact_by_day <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      RevenueProfitTable$data %>%
        filter(Дата >= input$dateManagerPlanFact[1] & Дата <= input$dateManagerPlanFact[2]) %>%
        group_by(Дата) %>%
        summarise(Оборот = sum(Оборот), 
                        Маржа = sum(Маржа),
                        Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        "Стоимость рейса" = round(Оборот / Рейсы, digits = 2)) %>%
        arrange(Дата)
    }
  })
  
  selected_RevenueProfit2 <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      result_all <- RevenueProfitTable$data %>% 
        group_by(Дата) %>%
        summarize(Оборот = sum(Оборот, na.rm = TRUE)) 
      
      xts(result_all[, -1], order.by = result_all$Дата)
    }
  })
  
  selected_RevenueProfit3 <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      result_all <- RevenueProfitTable$data %>% 
        group_by(Дата) %>%
        summarize(Маржа = sum(Маржа, na.rm = TRUE)) 
      
      xts(result_all[, -1], order.by = result_all$Дата)
    }
  })
  
  selected_RevenueProfit4 <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      result_all <- RevenueProfitTable$data %>% 
        group_by(Дата) %>%
        summarize(Рейсы = sum(Рейсы, na.rm = TRUE)) 
      
      xts(result_all[, -1], order.by = result_all$Дата)
    }
  })
  
  selected_dz <- reactive({
    if(!is.null(dzTable$data)){
      dzTable$data %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        rename("Контрагент" = 1,
               "Менеджер ОП" = 2,
               "№ Договора с клиентом" = 3,
               "Номера заявок" = 4,
               "Номер счёта" = 5,
               "Дата счёта" = 6,
               "Дата выставления счёта" = 7,
               "Сумма счёта из 1С" = 8,
               "Сумма по ставкам клиента" = 9,
               "Разница по суммам" = 10,
               "Отсрочка платежа" = 11,
               "Сроки верификации" = 12,
               "Дата отправления" = 13,
               "Дата доставки" = 14,
               "Дата возврата" = 15,
               "Плановая дата верификации" = 16,
               "Фактическая дата верификации" = 17,
               "Просрочка по верификации" = 18,
               "Дата оплаты по договору" = 19,
               "Частично оплачен" = 20,
               "ДЗ/ПДЗ, дни" = 21,
               "Неделя для оплат" = 22,
               "Специалист ДО" = 23,
               "Категория" = 24)
    }
  })
  
  selected_dz_specialist_1 <- reactive({
    if(!is.null(dzTable$data)){
      
      table <- dzTable$data %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
      
        rename("Контрагент" = 1,
               "Менеджер ОП" = 2,
               "Номер счёта" = 3,
               "Дата счёта" = 4,
               "Сумма счёта" = 5,
               "Специалист ДО" = 6,
               # "Дата выставления счёта" = 7,
               "Дата отправления" = 7,
               "Дата доставки" = 8,
               "Отсрочка платежа" = 9,
               "Сроки верификации" = 10,
               "Фактическая дата верификации" = 11,
               "Просрочка по верификации" = 12,
               "Дата оплаты по договору" = 13
        )
      
      return(table)
    }
  })
  
  selected_dz1 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        group_by(Клиент, Категория) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE)) %>%
        arrange(desc(Сумма))
      
    }
  })
  
  
  selected_dz3 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        group_by(Категория) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE)) %>%
        arrange(desc(Сумма))
      
    }
  })
  
  selected_pdz <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория == "A. Дебиторская задолженность") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  selected_pdz1 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория == "B. До 7 дней") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  selected_pdz2 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория == "C. От 8 до 15 дней") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  selected_pdz3 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория == "D. От 16 до 30 дней") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  selected_pdz4 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория == "F. От 31 до 60 дней") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  selected_pdz5 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория == "G. От 61 до 90 дней") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  selected_pdz6 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория == "H. Свыше 90 дней") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  selected_pdz7 <- reactive({
    if(!is.null(dzTable$data)){
      
      dzTable$data %>%
        filter(Категория != "A. Дебиторская задолженность") %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE))
      
    }
  })
  
  getGTtable <- reactive({
    if(!is.null(dzTable$data)){
      
      pivot <- dzTable$data %>%
        filter(Дата_счёта >= input$date1[1] & Дата_счёта <= input$date1[2]) %>%
        group_by(Менеджер, Клиент, Категория) %>%
        summarise(Сумма = sum(Сумма_счёта, na.rm = TRUE)) %>%
        arrange(Категория, desc(Сумма)) %>%
        pivot_wider(names_from = Категория, 
                    values_from = Сумма, 
                    values_fill = 0) %>% 
        group_by(Менеджер, Клиент)  %>%
        summarise_if(is.numeric, sum) %>%
        gt(rowname_col = "Клиент") %>%
        summary_rows(groups = TRUE,
                     columns =  everything(),
                     fns = list(Итог = "sum"),
                     formatter = fmt_number,
                     use_seps = TRUE,
                     decimals = 0,
                     sep_mark = " ") %>%
        data_color(columns =  everything(),
                   colors = scales::col_numeric(
                     palette = paletteer::paletteer_d(
                       palette = 'palettetown::swablu') %>%
                       as.character(),
                     domain = NULL
                   )) %>%
        tab_options(summary_row.background.color = "#feebe2",
                    row_group.background.color = "#ffffcc",
                    heading.background.color = "#c6dbef",
                    column_labels.background.color = "#edf8fb",
                    stub.background.color = "#edf8fb",
                    table.font.color = "#323232",
                    table_body.hlines.color = "#989898",
                    table_body.border.top.color = "#989898",
                    heading.border.bottom.color = "#989898",
                    row_group.border.top.color = "#989898",
                    row_group.border.bottom.style = "none",
                    stub.border.style = "dashed",
                    stub.border.color = "#989898",
                    stub.border.width = "1px",
                    summary_row.border.color = "#989898",
                    table.width = "100%",
                    footnotes.background.color = "#edf8fb",
                    source_notes.background.color = "#f6eff7",
                    source_notes.font.size = "8px",
                    footnotes.font.size = "9px"
        ) %>%
        tab_style(style = list(
          cell_text(
            font = "Oswald",
            align = "center",
            weight = "bold"
          )
        ),
        locations = list(
          cells_title(groups = "title")
        )
        ) %>% 
        
        tab_style(style = list(
          cell_text(
            font = "Oswald",
            align = "center",
            weight = "bold"
          )
        ),
        locations = list(
          cells_title(groups = "subtitle")
        )
        ) %>%
        
        tab_style(style = list(
          cell_text(
            font = "Oswald",
            weight = "bold"
          )
        ),
        locations = list(
          cells_column_labels(everything())
        )
        ) %>%
        
        tab_style(style = list(
          cell_text(
            font = "Oswald",
            align = "center",
            weight = "bold"
          )
        ),
        locations = list(
          cells_row_groups(everything())
        )
        ) %>%
        
        tab_style(style = list(
          cell_text(
            font = "Oswald",
            align = "center",
            weight = "bold"
          )
        ),
        locations = cells_summary(groups = everything(), columns = everything())
        
        ) %>%
        
        opt_all_caps() %>%
        
        opt_table_outline() %>%
        
        tab_source_note("По данным ERP и 1С") %>%
        
        tab_header(
          title = "Дебиторская задолженность",
          subtitle = paste("За период", format(input$date1[1], "%d.%m.%Y"), "-", format(input$date1[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
        ) %>%
        fmt_number(columns = everything(),
                   sep_mark = " ",
                   decimals = 0)
      
    }
  })
  
  selected_data <- reactive({
    if(!is.null(verifiedDocsTable$data)) {
      verifiedDocsTable$data %>%
        filter(Дата >= input$date[1] & Дата <= input$date[2]) %>%
        group_by(Специалист) %>%
        summarise(Оригиналы = sum(Оригиналы, na.rm = TRUE),
                           "Номера оригиналов" = toString(unique(Номера_оригиналов)),
                           Сканы = sum(Сканы, na.rm = TRUE),
                           "Номера сканов" = toString(unique(Номера_сканов)),
                           Всего = sum(Всего, na.rm = TRUE)
        ) %>%
        arrange(desc(Всего))
      
    }
  })
  
  selected_data1 <- reactive({
    if(!is.null(verifiedDocsTable$data)) {
      verifiedDocsTable$data %>%
        filter(Дата >= input$date[1] & Дата <= input$date[2])
    }
  })
  
  selected_data2 <- reactive({
    selected_data1() %>%
      group_by(Дата, Специалист) %>%
      summarise(Всего = sum(Всего)) %>%
      mutate(Дата = format(Дата, "%d %b")) %>%
      pivot_wider(names_from = Дата, values_from = Всего, values_fill = 0) %>% 
      left_join(selected_data() %>% select(Специалист, Всего), by = c("Специалист" = "Специалист")) %>%
      arrange(desc(Всего))
  })
  
  selected_bills_for_factoring <- reactive({
    if(!is.null(billsTable$data)) {
      billsTable$data %>%
        filter(ДатаВыс >= input$dateFactoring[1] & ДатаВыс <= input$dateFactoring[2]) %>%
        arrange(ДатаВыс) %>%
        rename("Номер счёта" = 2,
               "Сумма счёта" = 3,
               "Дата счёта" = 4,
               "Дата выставления счёта" = 5,
               "Дата доставки" = 6,
               "Сроки верификации" = 7,
               "Плановая дата верификации" = 8,
               "Количество заявок" = 12,
               "Номера заявок" = 13,
               "Ответственный сотрудник ДО за клиента (дебитор)" = 9,
               "Менеджер" = 10,
               "Оплата" = 11)
    }
    
  })
  
  selected_bills <- reactive({
    if(!is.null(billsTable$data)) {
      billsTable$data %>%
        filter(ДатаВыс >= input$date[1] & ДатаВыс <= input$date[2])
    }
    
  })
  
  selected_bills1 <- reactive({
    if(!is.null(billsTable$data)) {
      
      billsTable$data %>%
        filter(ДатаВыс >= input$date[1] & ДатаВыс <= input$date[2]) %>%
        group_by(Специалист) %>%
        summarise(Специалист = first(Специалист),
                            Счета = n(),
                            Заявки = sum(Заявки, na.rm = TRUE),
                            Сумма = sum(Сумма_счёта, na.rm = TRUE)) %>%
        arrange(desc(Счета), desc(Заявки), desc(Сумма))
      
    }
    
  })
  
  selected_bills2 <- reactive({
    selected_bills() %>%
      group_by(ДатаВыс, Специалист) %>%
      summarise(Счета = n()) %>%
      mutate(ДатаВыс = format(ДатаВыс, "%d %b")) %>%
      pivot_wider(names_from = ДатаВыс, values_from = Счета, values_fill = 0) %>% 
      left_join(selected_bills1() %>% select(Специалист, Счета), by = c("Специалист" = "Специалист")) %>%
      arrange(desc(Счета))
  })

  
  selected_requests <- reactive({
    if(!is.null(RevenueProfitTable$data)) {
      filtered <- requestsTable$data
      if (!is.null(input$client)) {
        filtered <- filtered %>% filter(`Юридическое лицо клиента` %in% input$client)
      } 
      if (!is.null(input$manager)) {
        filtered <- filtered %>% filter(Менеджер %in% input$manager)
      }  
      if (!is.null(input$logist)) {
        filtered <- filtered %>% filter(`Ответственный логист` %in% input$logist)
      } 
      
      if(input$allStatusCheckbox){
        filtered
      } else {
        filtered %>%
          filter(`Статус заявки` != "Заявка активна" &
                   `Статус заявки` != "Заявка отменена" &   
                   `Статус заявки` != "Перевозка отменена" &
                   `Статус заявки` != "Заявка просрочена"
          )
      }
      
    }
  })
  
  selected_canceledOrders <- reactive({
    if(!is.null(breakdownsTable$data)){
      breakdownsTable$data %>%
        filter(Дата_создания_заявки >= input$date_canceledOrders[1] & Дата_создания_заявки <= input$date_canceledOrders[2]) %>%
        arrange(desc(Дата_создания_заявки)) %>%
        rename("Номер заявки" = 1,
               "Внешний номер заявки" = 2,
               "Дата создания заявки" = 3,
               "Клиент" = 4,
               "Тип заявки" = 5,
               "Статус заявки" = 6,
               "Статус обработки" = 7,
               "Наличие заявки клиента (прикреплена)" = 8,
               "Наличие подписанного договора-заявки с перевозчиком (прикреплен)" = 9,
               "Наличие подписанной заявки с перевозчиком (прикреплен)" = 10,
               "Место или город погрузки" = 11,
               "Место или город выгрузки" = 12,
               "Тип ТС реф, термос, тент из заявки Клиента" = 13,
               "Тоннаж ТС из заявки Клиента" = 14,
               "Объем ТС из заявки Клиента" = 15,
               "Ответственный логист" = 16,
               "Старший группы" = 17,
               "Перевозчик" = 18,
               "ИНН перевозчика" = 19,
               "Юр. адрес перевозчика" = 20,
               "Ставка клиента" = 21,
               "Ставка перевозчика" = 22,
               "Сумма предоплаты перевозчику" = 23,
               "Сумма оч.маржи." = 24,
               "НДС / без НДС (по перевозчику)" = 25,
               "Очищ. Рент., %" = 26,
               "Проблема с ТСД, дата" = 27,
               "Есть претензионная работа" = 28,
               "Наличие Торг-2" = 29,
               "Заметки" = 30,
               "Причина создания" = 31,
               "Связанная заявка" = 32
        )
    }
  })
  
  selected_canceledOrdersCommerce <- function(){
    if(!is.null(breakdownsTable$data)){
      breakdownsTable$data %>%
        # filter(Дата_создания_заявки >= (Sys.Date() - 6) & Дата_создания_заявки <= (Sys.Date()-1)) %>%
        filter(Дата_создания_заявки >= input$date_canceledOrders[1] & Дата_создания_заявки <= input$date_canceledOrders[2]) %>%
        select(Номер_заявки, Внешний_номер_заявки, Дата_создания_заявки, Клиент, Ответственный_логист, Старший_группы, Тип_заявки, Статус_заявки, Перевозчик) %>%
        arrange(desc(Дата_создания_заявки), Клиент) %>%
        rename("Номер заявки" = 1,
               "Внешний номер заявки" = 2,
               "Дата создания заявки" = 3,
               "Клиент" = 4,
               "Ответственный логист" = 5,
               "Старший группы" = 6,
               "Тип заявки" = 7,
               "Статус заявки" = 8,
               "Перевозчик" = 9
        )
    }
  }
  
  selected_canceledOrdersLogistics <- function(){
    if(!is.null(breakdownsTable$data)){
      breakdownsTable$data %>%
        # filter(Дата_создания_заявки >= (Sys.Date() - 6) & Дата_создания_заявки <= (Sys.Date()-1)) %>%
        filter(Дата_создания_заявки >= input$date_canceledOrders[1] & Дата_создания_заявки <= input$date_canceledOrders[2]) %>%
        select(Номер_заявки, Внешний_номер_заявки, Дата_создания_заявки, Ответственный_логист, Клиент, Тип_заявки, Статус_заявки, Старший_группы, Перевозчик) %>%
        arrange(desc(Дата_создания_заявки), Ответственный_логист) %>%
        rename("Номер заявки" = 1,
               "Внешний номер заявки" = 2,
               "Дата создания заявки" = 3,
               "Ответственный логист" = 4,
               "Клиент" = 5,
               "Тип заявки" = 6,
               "Статус заявки" = 7,
               "Старший группы" = 8,
               "Перевозчик" = 9
        )
    }
  }
  
  selected_canceledOrdersLawyers <- function(){
    if(!is.null(breakdownsTable$data)){
      breakdownsTable$data %>%
        # filter(Дата_создания_заявки >= (Sys.Date() - 8) & Дата_создания_заявки <= (Sys.Date()-1)) %>%
        filter(Дата_создания_заявки >= input$date_canceledOrders[1] & Дата_создания_заявки <= input$date_canceledOrders[2]) %>%
        select(-Статус_обработки, -Тип_ТС, -Тоннаж_ТС, -Объём_ТС, -ИНН_перевозчика, -Адрес_перевозчика, -Маржа, -Рентабельность) %>%
        arrange(Дата_создания_заявки) %>%
        rename("Номер заявки" = 1,
               "Внешний номер заявки" = 2,
               "Дата создания заявки" = 3,
               "Клиент" = 4,
               "Тип заявки" = 5,
               "Статус заявки" = 6,
               "Наличие заявки клиента (прикреплена)" = 7,
               "Наличие подписанного договора-заявки с перевозчиком (прикреплен)" = 8,
               "Наличие подписанной заявки с перевозчиком (прикреплен)" = 9,
               "Место или город погрузки" = 10,
               "Место или город выгрузки" = 11,
               "Ответственный логист" = 12,
               "Старший группы" = 13,
               "Перевозчик" = 14,
               "Ставка клиента" = 15,
               "Ставка перевозчика" = 16,
               "Сумма предоплаты перевозчику" = 17,
               "НДС / без НДС (по перевозчику)" = 18,
               "Проблема с ТСД, дата" = 19,
               "Наличие Торг-2" = 20,
               "Заметки" = 21,
               "Причина создания" = 22,
               "Связанная заявка" = 23
        )
    }
  }
  
  selected_canceledOrdersClient <- reactive({
    if(!is.null(breakdownsTable$data)){
      breakdownsTable$data %>%
        filter(Дата_создания_заявки >= input$date_canceledOrders[1] & Дата_создания_заявки <= input$date_canceledOrders[2]) %>%
        group_by(Клиент) %>%
        summarise(Заявки = n(),
                        Ставка_клиента = sum(Ставка_клиента, na.rm = TRUE)
        ) %>%
        arrange(desc(Заявки), desc(Ставка_клиента))
    }
  })
  
  selected_canceledOrdersCarrier <- reactive({
    if(!is.null(breakdownsTable$data)){
      breakdownsTable$data %>%
        filter(Дата_создания_заявки >= input$date_canceledOrders[1] & Дата_создания_заявки <= input$date_canceledOrders[2]) %>%
        group_by(Перевозчик) %>%
        summarise(Заявки = n(),
                        Ставка_клиента = sum(Ставка_клиента, na.rm = TRUE)
        ) %>%
        arrange(desc(Заявки), desc(Ставка_клиента))
    }
  })
  
  selected_canceledOrdersLogist <- reactive({
    if(!is.null(breakdownsTable$data)){
      breakdownsTable$data %>%
        filter(Дата_создания_заявки >= input$date_canceledOrders[1] & Дата_создания_заявки <= input$date_canceledOrders[2]) %>%
        group_by(Ответственный_логист) %>%
        summarise(Заявки = n(),
                        Ставка_клиента = sum(Ставка_клиента, na.rm = TRUE)
        ) %>%
        arrange(desc(Заявки), desc(Ставка_клиента))
    }
  })
  

  
  selected_drivers_all <- reactive({
    
    drivers_everyday$data %>%
      filter(Дата >= input$dateSB[1] & Дата <= input$dateSB[2]) %>%
      group_by(Сотрудник) %>%
      summarise(Всего_водители = sum(Всего_водители, na.rm = TRUE)) %>%
      arrange(desc(Всего_водители))
  })
  
  selected_drivers_everyday <- reactive({
    
    drivers_everyday$data %>%
      filter(Дата >= input$dateSB[1] & Дата <= input$dateSB[2])
  })
  
  selected_carriers_all <- reactive({
    
    carriers_everyday$data %>%
      filter(Дата >= input$dateSB[1] & Дата <= input$dateSB[2]) %>%
      group_by(Сотрудник) %>%
      summarise(Всего_перевозчики = sum(Всего_перевозчики, na.rm = TRUE)) %>%
      arrange(desc(Всего_перевозчики))
  })
  
  selected_carriers_everyday <- reactive({
    carriers_everyday$data %>%
      filter(Дата >= input$dateSB[1] & Дата <= input$dateSB[2])
  })
  
  selected_vehicles_all <- reactive({
    
    vehicles_everyday$data %>%
      filter(Дата >= input$dateSB[1] & Дата <= input$dateSB[2]) %>%
      group_by(Сотрудник) %>%
      summarise(Всего_тс = sum(Всего_тс, na.rm = TRUE)) %>%
      arrange(desc(Всего_тс))
  })
  
  selected_vehicles_everyday <- reactive({
    vehicles_everyday$data %>%
      filter(Дата >= input$dateSB[1] & Дата <= input$dateSB[2])
  })
  

  
  ##########################
  ### Выработка логистов ###
  ##########################

  kpi_productivity_mean <- JS("function(values) {
    var present = values.filter(function(value) { return Number.isFinite(value) })
    if (!present.length) return null
    return present.reduce(function(total, value) { return total + value }, 0) / present.length
  }")
  kpi_productivity_cell <- JS("function(cellInfo) {
    var value = cellInfo.value
    if (value == null || !Number.isFinite(value)) return '\u2013'
    return value.toLocaleString('ru-RU', { minimumFractionDigits: 2, maximumFractionDigits: 2 })
  }")
  
  output$tableLogisticProductivity <- renderReactable({
    if(!is.null(selected_LogisticKPI_outcome())){
      reactable(selected_LogisticKPI_outcome(),
                defaultSorted = c("Рейсы", "Оборот"),
                groupBy = c("Группа"),
                columns = list(
                  Группа = colDef(minWidth = 220),
                  Логист = colDef(minWidth = 200),
                  "Рейсы" = colDef(defaultSortOrder = "desc", maxWidth = 100, na = "–", name = "Рейсы", aggregate = "sum", 
                                   style = function(value) {
                                     normalized <- (value - min(selected_LogisticKPI_outcome()["Рейсы"])) / (max(selected_LogisticKPI_outcome()["Рейсы"]) - min(selected_LogisticKPI_outcome()["Рейсы"]))
                                     color <- off_rating_color(normalized)
                                     list(background = color)
                                   },
                                   format = colFormat(separators = TRUE)),
                  Оборот = colDef(defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(selected_LogisticKPI_outcome()["Оборот"])) / (max(selected_LogisticKPI_outcome()["Оборот"]) - min(selected_LogisticKPI_outcome()["Оборот"]))
                    color <- orange_pal(normalized)
                    list(background = color)
                  }, na = "–", name = "Оборот", aggregate = "sum",
                  format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                  # Выручка = colDef(defaultSortOrder = "desc", show = TRUE, style = function(value) {
                  #   normalized <- (value - min(selected_LogisticKPI_outcome()["Выручка"])) / (max(selected_LogisticKPI_outcome()["Выручка"]) - min(selected_LogisticKPI_outcome()["Выручка"]))
                  #   color <- orange_pal(normalized)
                  #   list(background = color)
                  # }, na = "–", name = "Выручка", aggregate = "sum",
                  # format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                  "Маржа" = colDef(show = TRUE, 
                                   style = function(value) {
                                     normalized <- (value - min(selected_LogisticKPI_outcome()["Маржа"])) / (max(selected_LogisticKPI_outcome()["Маржа"]) - min(selected_LogisticKPI_outcome()["Маржа"]))
                                     color <- BuYlRd(normalized)
                                     list(background = color)
                                   }, 
                                   na = "–", 
                                   name = "Маржа", 
                                   aggregate = "sum", 
                                   format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                  
                  "Рентабельность" = colDef(name = "Рент-ть",
                                            format = colFormat(percent = TRUE, digits = 2),
                                            na = "–",
                                            aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа']
                                              totalUnits += row['Оборот']
                                            })
                                            return totalUnits === 0 ? null : totalPrice / totalUnits
                                          }"),
                                            style = function(value) {
                                              normalized <- (value - min(selected_LogisticKPI_outcome()["Рентабельность"])) / (max(selected_LogisticKPI_outcome()["Рентабельность"]) - min(selected_LogisticKPI_outcome()["Рентабельность"]))
                                              color <- def_rating_color(normalized)
                                              list(background = color)
                                            }
                                            #           aggregated = JS("function(cellInfo) {
                                            #   return '$' + cellInfo.value <= 0 ? '\u2718' : '\u2713'
                                            # }"),
                                            
                  ),
                  
                  "Договоры" = colDef(na = "–", name = "Договоры", aggregate = "sum"),
                  
                  Рабочих_дней = colDef(show = FALSE),
                  
                  Выработка = colDef(name = "Выработка",
                                            format = list(cell = colFormat(separators = TRUE, digits = 2)),
                                            na = "–",
                                            aggregate = kpi_productivity_mean,
                                            aggregated = kpi_productivity_cell,
                                            style = function(value) {
                                              normalized <- (value - min(selected_LogisticKPI_outcome()["Выработка"])) / (max(selected_LogisticKPI_outcome()["Выработка"]) - min(selected_LogisticKPI_outcome()["Выработка"]))
                                              color <- def_rating_color(normalized)
                                              list(background = color)
                                            }
                                            
                  )
                  
                ),
                
                striped = TRUE,
                bordered = TRUE,
                borderless = FALSE,
                highlight = TRUE,
                resizable = FALSE
      )
    }
  })
  
  
  output$tableLogisticProductivity1 <- renderReactable({
    if(!is.null(selected_LogisticKPI_outcome1())){
      reactable(selected_LogisticKPI_outcome1(),
                defaultColDef = colDef(class = "cell", headerClass = "header", footerStyle = list(fontWeight = "bold")),
                defaultSorted = c("Рейсы", "Оборот"),
                columns = list(
                  Логист = colDef(
                    style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                    footer = "Всего",
                    minWidth = 270,
                    headerStyle = list(fontWeight = 700),
                    cell = function(value, index) {
                      div(
                        class = "manager",
                        if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/logist/%s.jpg", value)),
                        div(class = "manager-name", value)
                      )
                    }
                  ),
                  "Рейсы" = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", maxWidth = 100, na = "–", name = "Рейсы", aggregate = "sum", 
                                   style = function(value) {
                                     normalized <- (value - min(selected_LogisticKPI_outcome1()["Рейсы"])) / (max(selected_LogisticKPI_outcome1()["Рейсы"]) - min(selected_LogisticKPI_outcome1()["Рейсы"]))
                                     color <- off_rating_color(normalized)
                                     list(background = color)
                                   },
                                   format = colFormat(separators = TRUE)),
                  Оборот = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                    normalized <- (value - min(selected_LogisticKPI_outcome1()["Оборот"])) / (max(selected_LogisticKPI_outcome1()["Оборот"]) - min(selected_LogisticKPI_outcome1()["Оборот"]))
                    color <- orange_pal(normalized)
                    list(background = color)
                  }, na = "–", name = "Оборот", aggregate = "sum",
                  format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                  # Выручка = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), defaultSortOrder = "desc", show = TRUE, style = function(value) {
                  #   normalized <- (value - min(selected_LogisticKPI_outcome1()["Выручка"])) / (max(selected_LogisticKPI_outcome1()["Выручка"]) - min(selected_LogisticKPI_outcome1()["Выручка"]))
                  #   color <- orange_pal(normalized)
                  #   list(background = color)
                  # }, na = "–", name = "Выручка", aggregate = "sum",
                  # format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                  "Маржа" = colDef(footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE), show = TRUE, 
                                   style = function(value) {
                                     normalized <- (value - min(selected_LogisticKPI_outcome1()["Маржа"])) / (max(selected_LogisticKPI_outcome1()["Маржа"]) - min(selected_LogisticKPI_outcome1()["Маржа"]))
                                     color <- BuYlRd(normalized)
                                     list(background = color)
                                   }, 
                                   na = "–", 
                                   name = "Маржа", 
                                   aggregate = "sum", 
                                   format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                  
                  "Рентабельность" = colDef(footer = function(values) {
                    totals <- selected_LogisticKPI_outcome1()
                    value <- report_ratio(sum(totals$Маржа), sum(totals$Оборот))
                    if (is.na(value)) "–" else paste0(format(round(value * 100, 2), big.mark = " ", scientific = FALSE), " %")
                  }, name = "Рент-ть",
                                            format = colFormat(percent = TRUE, digits = 2),
                                            na = "–",
                                            aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа']
                                              totalUnits += row['Оборот']
                                            })
                                            return totalUnits === 0 ? null : totalPrice / totalUnits
                                          }"),
                                            style = function(value) {
                                              normalized <- (value - min(selected_LogisticKPI_outcome1()["Рентабельность"])) / (max(selected_LogisticKPI_outcome1()["Рентабельность"]) - min(selected_LogisticKPI_outcome1()["Рентабельность"]))
                                              color <- def_rating_color(normalized)
                                              list(background = color)
                                            }

                                            
                  ),
                  
                  "Договоры" = colDef(footer = function(values) format(sum(values, na.rm = TRUE), big.mark=" ", scientific=FALSE), na = "–", name = "Договоры", aggregate = "sum"),
                  
                  Рабочих_дней = colDef(show = FALSE),
                  
                  "Выработка" = colDef(footer = function(values) {
                    totals <- selected_LogisticKPI_outcome1()
                    value <- report_ratio(sum(totals$Рейсы), sum(totals$Рабочих_дней))
                    if (is.na(value)) "–" else format(round(value, 2), big.mark = " ", scientific = FALSE)
                  }, name = "Выработка",
                                            format = list(cell = colFormat(separators = TRUE, digits = 2)),
                                            na = "–",
                                            aggregated = kpi_productivity_cell,
                                            aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы']
                                              totalUnits += row['Рабочих_дней']
                                            })
                                            return totalUnits === 0 ? null : totalPrice / totalUnits
                                          }"),
                                            style = function(value) {
                                              normalized <- (value - min(selected_LogisticKPI_outcome1()["Выработка"])) / (max(selected_LogisticKPI_outcome1()["Выработка"]) - min(selected_LogisticKPI_outcome1()["Выработка"]))
                                              color <- def_rating_color(normalized)
                                              list(background = color)
                                            }
                                            
                                            
                  )
                  
                ),
                
                pagination = FALSE
      )
    }
  })
  

  output$tableLogisticProductivityRoutes <- renderReactable(
    reactable(selected_LogisticKPI_Routes_byDay(),
              pagination = FALSE,
              defaultSorted = "Всего",
              groupBy = c("Группа"),
              defaultColDef = colDef(aggregate = "sum", class = "cell", headerClass = "header",
              #                        style = function(value) {
              #   if (!is.numeric(value)) return()
              #   if (value <= 3) {
              #     color <- "#fb607f"
              #   } else if (value > 3 & value <= 6) {
              #     color <- "#ffffe0"
              #   } else if (value > 6) {
              #     color <- "#90ee90"
              #   }
              #   list(background = color)
              # },
              style = function(value) {
                list(fontWeight = 600, fontSize = 18)
              },
              footerStyle = list(fontWeight = "bold"),
              footer = function(values) if (is.numeric(values)) {format(sum(values), big.mark=" ", scientific=FALSE)}
              ),
              columns = list(
                Логист = colDef(
                  aggregate = "count",
                  style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                  footer = "Всего",
                  minWidth = 270,
                  headerStyle = list(fontWeight = 700),
                  cell = function(value, index) {
                    div(
                      class = "manager",
                      if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/logist/%s.jpg", value)),
                      div(class = "manager-name", value)
                    )
                  }
                ),
                Всего = colDef(style = list(fontWeight = 600, position = "sticky", right = 0, background = "#fff", zIndex = 1),
                               aggregate = "sum", defaultSortOrder = "desc", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))
              )
    )
  )
  
  output$tableLogisticProductivityRevenue <- renderReactable(
    reactable(selected_LogisticKPI_Revenue_byDay(),
              pagination = FALSE,
              defaultSorted = "Всего",
              groupBy = c("Группа"),
              defaultColDef = colDef(format = colFormat(separators = TRUE, digits = 0), aggregate = "sum", class = "cell", headerClass = "header",
              #                        style = function(value) {
              #   if (!is.numeric(value)) return()
              #   if (value <= 100000) {
              #     color <- "#fb607f"
              #   } else if (value > 100000 & value <= 300000) {
              #     color <- "#ffffe0"
              #   } else if (value > 300000) {
              #     color <- "#90ee90"
              #   }
              #   list(background = color)
              # },
              style = function(value) {
                list(fontWeight = 600, fontSize = 18)
              },
              footerStyle = list(fontWeight = "bold"),
              footer = function(values) if (is.numeric(values)) {format(sum(values), big.mark=" ", scientific=FALSE)}
              ),
              columns = list(
                Логист = colDef(
                  aggregate = "count",
                  style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                  footer = "Всего",
                  minWidth = 270,
                  headerStyle = list(fontWeight = 700),
                  cell = function(value, index) {
                    div(
                      class = "manager",
                      if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/logist/%s.jpg", value)),
                      div(class = "manager-name", value)
                    )
                  }
                ),
                Всего = colDef(style = list(fontWeight = 600, position = "sticky", right = 0, background = "#fff", zIndex = 1),
                               aggregate = "sum", defaultSortOrder = "desc", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))
              )
    )
  )
  
  output$tableLogisticProductivityProfit <- renderReactable(
    reactable(selected_LogisticKPI_Profit_byDay(),
              pagination = FALSE,
              defaultSorted = "Всего",
              groupBy = c("Группа"),
              defaultColDef = colDef(format = colFormat(separators = TRUE, digits = 0), aggregate = "sum", class = "cell", headerClass = "header",                   
              #                        style = function(value) {
              #   if (!is.numeric(value)) return()
              #   if (value <= 10000) {
              #     color <- "#fb607f"
              #   } else if (value > 10000 & value <= 30000) {
              #     color <- "#ffffe0"
              #   } else if (value > 30000) {
              #     color <- "#90ee90"
              #   }
              #   list(background = color)
              # },
              style = function(value) {
                list(fontWeight = 600, fontSize = 18)
              },
              footerStyle = list(fontWeight = "bold"),
              footer = function(values) if (is.numeric(values)) {format(sum(values), big.mark=" ", scientific=FALSE, digits = 0)}
              ),
              columns = list(
                Логист = colDef(
                  aggregate = "count",
                  style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                  footer = "Всего",
                  minWidth = 270,
                  headerStyle = list(fontWeight = 700),
                  cell = function(value, index) {
                    div(
                      class = "manager",
                      if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/logist/%s.jpg", value)),
                      div(class = "manager-name", value)
                    )
                  }
                ),
                Всего = colDef(style = list(fontWeight = 600, position = "sticky", right = 0, background = "#fff", zIndex = 1),
                               aggregate = "sum", defaultSortOrder = "desc", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))
              )
    )
  )
  
  #################################
  ### Дебиторская задолженность ###
  #################################
  
  output$dz <- renderReactable(
    reactable(selected_dz_specialist_1(),
              groupBy = c("Менеджер ОП", "Контрагент", "Категория"),
              defaultSorted = c("Менеджер ОП", "Контрагент"),
              columns = list(
                "Менеджер ОП" = colDef(minWidth = 200, defaultSortOrder = "asc"),
                "Контрагент" = colDef(minWidth = 300, defaultSortOrder = "asc"),
                "Сумма счёта" = colDef(format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"))
              )
  )
  
  output$dzPlot <- renderHighchart({
    if(!is.null(selected_dz3())) {
      selected_dz3() %>%
        hchart(
          "pie", hcaes(x = Категория, y = Сумма),
          name = "Сумма"
        ) %>%
        hc_title(text = "ДЗ/ПДЗ",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>%
        
        hc_subtitle(text = paste("За период", format(input$date1[1], "%d.%m.%Y"), "-", format(input$date1[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx())
      
    }
    
  })
  

  output$totalDZ <- renderText(
    paste(format(sum(selected_dz1()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ <- renderText(
    paste(format(sum(selected_pdz()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ1 <- renderText(
    paste(format(sum(selected_pdz1()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ2 <- renderText(
    paste(format(sum(selected_pdz2()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ3 <- renderText(
    paste(format(sum(selected_pdz3()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ4 <- renderText(
    paste(format(sum(selected_pdz4()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ5 <- renderText(
    paste(format(sum(selected_pdz5()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ6 <- renderText(
    paste(format(sum(selected_pdz6()$Сумма), big.mark=" "), "руб")
  )
  
  output$totalPDZ7 <- renderText(
    paste(format(sum(selected_pdz7()$Сумма), big.mark=" "), "руб")
  )
  
  output$periodDZ <- renderText(
    paste("За период", format(input$date1[1], "%d.%m.%Y"), "-", format(input$date1[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
  )
  
  output$gtTable <- render_gt(
    expr = getGTtable() 
  )
  
  #################################
  #################################
  #################################
  
  #################################
  ###Кредиторская задолженность####
  #################################
  
  selected_kz_table <- reactive({
    if(!is.null(kzTable$data)){
      kzTable$data %>%
        filter(Дата_оплаты_перевозчику_расчёт >= input$dateKZ[1] & Дата_оплаты_перевозчику_расчёт <= input$dateKZ[2]) 
    }
  })
  
  selected_kz_plot <- reactive({
    if(!is.null(kzTable$data)){
      
      kzTable$data %>%
        filter(Дата_оплаты_перевозчику_расчёт >= input$dateKZ[1] & Дата_оплаты_перевозчику_расчёт <= input$dateKZ[2]) %>%
        group_by(Категория) %>%
        summarise(Сумма = sum(С_учётом_предоплаты, na.rm = TRUE)) %>%
        arrange(desc(Сумма))
      
    }
  })
  
  selected_kz_plot1 <- reactive({
    if(!is.null(kzTable$data)){
      
      kzTable$data %>%
        filter(Дата_оплаты_перевозчику_расчёт >= input$dateKZ[1] & Дата_оплаты_перевозчику_расчёт <= input$dateKZ[2]) %>%
        group_by(Перевозчик) %>%
        summarise(Сумма = sum(С_учётом_предоплаты, na.rm = TRUE)) %>%
        arrange(desc(Сумма))
      
    }
  })
  
  selected_kz_plot2 <- reactive({
    if(!is.null(kzTable$data)){
      
      kzTable$data %>%
        filter(Дата_оплаты_перевозчику_расчёт >= input$dateKZ[1] & Дата_оплаты_перевозчику_расчёт <= input$dateKZ[2]) %>%
        group_by(Категория) %>%
        summarise(Рейсы = n()) %>%
        arrange(desc(Рейсы))
      
    }
  })
  
  selected_kz_plot3 <- reactive({
    if(!is.null(kzTable$data)){
      
      kzTable$data %>%
        filter(Дата_оплаты_перевозчику_расчёт >= input$dateKZ[1] & Дата_оплаты_перевозчику_расчёт <= input$dateKZ[2]) %>%
        group_by(Перевозчик) %>%
        summarise(Рейсы = n()) %>%
        arrange(desc(Рейсы))
      
    }
  })
  
  output$kzPlot <- renderHighchart({
    if(!is.null(selected_kz_plot())) {
      selected_kz_plot() %>%
        hchart(
          "pie", hcaes(x = Категория, y = Сумма),
          name = "Сумма"
        ) %>%
        hc_title(text = "КЗ/ПКЗ",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>%
        
        hc_subtitle(text = paste("За период", format(input$dateKZ[1], "%d.%m.%Y"), "-", format(input$dateKZ[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx())
      
    }
    
  })
  
  output$kzPlot1 <- renderHighchart({
    if(!is.null(selected_kz_plot1())) {
      
      head(selected_kz_plot1(), 20) %>% hchart('bar', hcaes(x = Перевозчик, y = Сумма), name = "Сумма") %>%
        hc_title(text = "Топ-20 по суммам",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>%
        
        hc_subtitle(text = paste("За период", format(input$dateKZ[1], "%d.%m.%Y"), "-", format(input$dateKZ[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx())
    }
  })
  
  
  output$kzPlot2 <- renderHighchart({
    if(!is.null(selected_kz_plot2())) {
      selected_kz_plot2() %>%
        hchart(
          "pie", hcaes(x = Категория, y = Рейсы),
          name = "Рейсы"
        ) %>%
        hc_title(text = "КЗ/ПКЗ",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>%
        
        hc_subtitle(text = paste("За период", format(input$dateKZ[1], "%d.%m.%Y"), "-", format(input$dateKZ[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx())
      
    }
    
  })
  
  output$kzPlot3 <- renderHighchart({
    if(!is.null(selected_kz_plot3())) {
      
      head(selected_kz_plot3(), 20) %>% hchart('bar', hcaes(x = Перевозчик, y = Рейсы), name = "Рейсы") %>%
        hc_title(text = "Топ-20 по рейсам",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>%
        
        hc_subtitle(text = paste("За период", format(input$dateKZ[1], "%d.%m.%Y"), "-", format(input$dateKZ[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>%
        
        hc_add_theme(hc_theme_ffx())
    }
  })
  
  
  
  output$reactableKZ <- renderReactable({
    if(!is.null(selected_kz_table())){
      reactable(selected_kz_table(),
                defaultSorted = c("С_учётом_предоплаты", "Номер_заявки"),
                groupBy = c("Перевозчик"),
                columns = list(
                  Номер_заявки = colDef(defaultSortOrder = "desc", name = "Количество заявок", aggregate = "count"),
                  Ставка_перевозчика = colDef(name = "Ставка перевозчика", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"),
                  Сумма_предоплаты = colDef(name = "Сумма предоплаты перевозчику", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"),
                  С_учётом_предоплаты = colDef(defaultSortOrder = "desc", name = "Сумма задолженности", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"),
                  Дата_оплаты_перевозчику_расчёт = colDef(name = "Дата оплаты перевозчику - расчетная"),
                  Статус_заявки = colDef(name = "Статус заявки"),
                  ПKЗ = colDef(name = "Просрочено дней")
                ))
    }
  })
  
  output$reactablePaymentSchedule <- renderReactable({
    if(!is.null(selected_kz_table())){
      reactable(selected_kz_table(),
                defaultSorted = c("Дата_оплаты_перевозчику_расчёт", "С_учётом_предоплаты"),
                groupBy = c("Дата_оплаты_перевозчику_расчёт", "Перевозчик"),
                columns = list(
                  Номер_заявки = colDef(name = "Количество заявок", aggregate = "count"),
                  Ставка_перевозчика = colDef(name = "Ставка перевозчика", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"),
                  Сумма_предоплаты = colDef(name = "Сумма предоплаты перевозчику", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"),
                  С_учётом_предоплаты = colDef(defaultSortOrder = "desc", name = "Сумма задолженности", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0), aggregate = "sum"),
                  Дата_оплаты_перевозчику_расчёт = colDef(defaultSortOrder = "desc", name = "Дата оплаты перевозчику - расчетная"),
                  Статус_заявки = colDef(name = "Статус заявки"),
                  ПKЗ = colDef(name = "Просрочено дней")
                ))
    }
  })
  
  #################################################
  ### Обработанные заявки и выставленные счета ####
  #################################################
  
  output$total <- renderText(
    format(sum(selected_data()$Всего), big.mark=" ")
  )
  
  output$totalBills <- renderText(
    format(sum(selected_bills1()$Счета), big.mark=" ")
  )
  
  output$totalBills1 <- renderText(
    format(sum(selected_bills1()$Заявки), big.mark=" ")
  )
  
  output$totalBills2 <- renderText(
    paste(format(sum(selected_bills1()$Сумма), big.mark=" "), "руб")
  )
  
  output$period <- renderText(
    paste("За период", format(input$date[1], "%d.%m.%Y"), "-", format(input$date[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
  )
  
  output$period1 <- renderText(
    paste("За период", format(input$date[1], "%d.%m.%Y"), "-", format(input$date[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
  )
  
  output$verifyDocs <- renderReactable(
    reactable(selected_data1(),
              pagination = FALSE,
              defaultSorted = "Всего",
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              groupBy = c("Специалист", "Клиент"),
              columns = list(
                Специалист = colDef(footer = "Всего"),
                Оригиналы = colDef(aggregate = "sum", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                Номера_оригиналов = colDef(name = "Номера оригиналов"),
                Сканы = colDef(aggregate = "sum", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE)),
                Номера_сканов = colDef(name = "Номера сканов"),
                Всего = colDef(aggregate = "sum", defaultSortOrder = "desc", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))              
                )
              )
  )
  
  output$verifyDocs1 <- renderReactable(
    reactable(selected_data2(),
              pagination = FALSE,
              defaultSorted = "Всего",
              defaultColDef = colDef(class = "cell", headerClass = "header",
                                     style = function(value) {
                if (!is.numeric(value)) return()
                if (value <= 20) {
                  color <- "#fb607f"
                } else if (value > 20 & value <= 35) {
                  color <- "#ffffe0"
                } else if (value > 35) {
                  color <- "#90ee90"
                }
                list(background = color)
              },
              footerStyle = list(fontWeight = "bold"),
              footer = function(values) if (is.numeric(values)) {format(sum(values), big.mark=" ", scientific=FALSE)}
              ),
              columns = list(
                Специалист = colDef(
                  style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                  footer = "Всего",
                  minWidth = 270,
                  headerStyle = list(fontWeight = 700),
                  cell = function(value, index) {
                    div(
                      class = "manager",
                      if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/do/%s.jpg", value)),
                      div(class = "manager-name", value)
                    )
                  }
                ),
                Всего = colDef(style = list(fontWeight = 600, position = "sticky", right = 0, background = "#fff", zIndex = 1),
                                     aggregate = "sum", defaultSortOrder = "desc", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))
              )
    )
  )
  
  output$billsTable <- renderReactable(
    reactable(selected_bills(),
              pagination = FALSE,
              defaultSorted = c("Количество_счетов", "Сумма_счёта", "Заявки"),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              groupBy = c("Специалист", "Клиент"),
              columns = list(
                Специалист = colDef(footer = "Всего"),
                Номер_счёта = colDef(name = "Номер счёта"),
                Сумма_счёта = colDef(defaultSortOrder = "desc", name = "Сумма счёта", aggregate = "sum",
                                     format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                                     footer = function(values) paste(format(sum(values), digits = 0, big.mark=" ", scientific=FALSE), "\U20BD")),
                Дата = colDef(name = "Дата счёта"),
                ДатаВыс = colDef(name = "Дата выставления счёта"),
                Заявки = colDef(defaultSortOrder = "desc", name = "Количество заявок", aggregate = "sum", format = colFormat(separators = TRUE, digits = 0),
                                footer = function(values) format(sum(values), digits = 0, big.mark=" ", scientific=FALSE)),
                Номера_заявок = colDef(name = "Номера заявок"),
                Количество_счетов = colDef(defaultSortOrder = "desc", name = "Количество счетов", aggregate = "sum", format = colFormat(separators = TRUE, digits = 0),
                                           footer = function(values) format(sum(values), digits = 0, big.mark=" ", scientific=FALSE))
              )    
              )
  )
  
  output$billsTable1 <- renderReactable(
    reactable(selected_bills2(),
              pagination = FALSE,
              defaultSorted = "Счета",
              defaultColDef = colDef(class = "cell", headerClass = "header",
              #                        style = function(value) {
              #   if (!is.numeric(value)) return()
              #   if (value <= 20) {
              #     color <- "#fb607f"
              #   } else if (value > 20 & value <= 35) {
              #     color <- "#ffffe0"
              #   } else if (value > 35) {
              #     color <- "#90ee90"
              #   }
              #   list(background = color)
              # },
              footerStyle = list(fontWeight = "bold"),
              footer = function(values) if (is.numeric(values)) {format(sum(values), big.mark=" ", scientific=FALSE)}
              ),
              columns = list(
                Специалист = colDef(
                  style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1),
                  footer = "Всего",
                  minWidth = 270,
                  headerStyle = list(fontWeight = 700),
                  cell = function(value, index) {
                    div(
                      class = "manager",
                      if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/do/%s.jpg", value)),
                      div(class = "manager-name", value)
                    )
                  }
                ),
                Счета = colDef(style = list(fontWeight = 600, position = "sticky", right = 0, background = "#fff", zIndex = 1),
                                     name = "Всего", aggregate = "sum", defaultSortOrder = "desc", footer = function(values) format(sum(values), big.mark=" ", scientific=FALSE))
              )
    )
  )
  
  #################################################
  #################################################
  #################################################
  
  #############################
  ### Проблемные документы ###
  #############################
  
  observeEvent(input$getProblemDocsButton, {
    problemDocsTable = getProblemDocsTable()
    problemDocsTableStatic = problemDocsTable$static
    problemDocsTableDynamics = problemDocsTable$dynamics
    
    output$problemDocsPlotStatic <- renderHighchart({
      
      problemDocsTableStatic %>%
        hchart('bar', hcaes(x = Логист, y = Ставка)) %>%
        
        hc_title(text = "Проблемные документы",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("По состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>% 
        
        hc_add_theme(hc_theme_ffx())
      
      
    })
    
    output$problemDocsPlotDynamics <- renderHighchart({
      
      problemDocsTableDynamics %>% 
        
        hchart('spline', hcaes(x = 'Дата', y = 'Ставка', group = "Логист")) %>%
        
        hc_title(text = "Проблемные документы",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("По дням по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>% 
        
        hc_add_theme(hc_theme_ffx())
      
    })
    
    output$docsProblemDataTable <- renderDataTable(
      problemDocsTableStatic
    )
    
  })
  
  #############################
  #############################
  #############################
  
  ############################
  #### Служба безопасноти ###
  ###########################
  
  observeEvent(input$getSBButton, {
    startDate <- (as.numeric(as.POSIXct(input$dateSB[1], format="%Y-%m-%d")) - 86399) * 1000
    endDate <- (as.numeric(as.POSIXct(input$dateSB[2], format="%Y-%m-%d")) + 86399) * 1000
    drivers_everyday$data <- getSB(startDate, endDate)$drivers_everyday
    carriers_everyday$data <- getSB(startDate, endDate)$carriers_everyday
    vehicles_everyday$data <- getSB(startDate, endDate)$vehicles_everyday
    
  })
  
  output$driversAllPlot <- renderHighchart({
    
    selected_drivers_all() %>%
      hchart(
        "pie", hcaes(x = Сотрудник, y = Всего_водители),
        name = "Водители"
      ) %>%
      
      hc_title(text = "Создание и изменение водителей",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>% 
      hc_subtitle(text = paste("Всего за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
    
  })
  
  output$driversAllTable <- renderReactable(
    reactable(selected_drivers_everyday(),
              defaultSorted = c("Всего_водители"),
              groupBy = "Сотрудник",
              columns = list(
                Всего_водители = colDef(name = 'Всего', defaultSortOrder = "desc", aggregate = "sum"),
                Создано_водители = colDef(name = 'Создано', aggregate = "sum"),
                Изменено_водители = colDef(name = 'Изменено', aggregate = "sum")
              )
              )
  )
  
  output$driversEveryDayPlot <- renderHighchart({
    selected_drivers_everyday() %>%
      hchart(
        type = "spline", hcaes(x = Дата, y = Всего_водители, group = Сотрудник)      ) %>%
      
      hc_title(text = "Создание и изменение водителей",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("По дням за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
  })
  
  output$carriersAllPlot <- renderHighchart({
    selected_carriers_all() %>%
      hchart('treemap', hcaes(x = 'Сотрудник', value = Всего_перевозчики, color = Всего_перевозчики)) %>% 
      hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
      
      hc_title(text = "Создание и изменение перевозчиков",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>% 
      hc_subtitle(text = paste("Всего за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      hc_add_theme(hc_theme_ffx())
  })
  
  output$carriersAllTable <- renderReactable(
    reactable(selected_carriers_everyday(),
              defaultSorted = c("Всего_перевозчики"),
              groupBy = "Сотрудник",
              columns = list(
                Всего_перевозчики = colDef(name = 'Всего', defaultSortOrder = "desc", aggregate = "sum"),
                Создано_перевозчики = colDef(name = 'Создано', aggregate = "sum"),
                Изменено_перевозчики = colDef(name = 'Изменено', aggregate = "sum")
              )
              )
  )
  
  output$carriersEveryDayPlot <- renderHighchart({
    selected_carriers_everyday() %>%
      hchart(
        type = "spline", hcaes(x = Дата, y = Всего_перевозчики, group = Сотрудник)      ) %>%
      
      hc_title(text = "Создание и изменение перевозчиков",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("По дням за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
  })
  
  output$vehiclesAllPlot <- renderHighchart({
    selected_vehicles_all() %>%
      hchart(
        "bar", hcaes(x = Сотрудник, y = Всего_тс),
        name = "ТС"
      ) %>%
      
      hc_title(text = "Создание и изменение ТС",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>% 
      
      hc_subtitle(text = paste("За весь период наблюдения по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold", fontSize = "14px"),
                  align = "center") %>% 
      hc_subtitle(text = paste("Всего за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      hc_add_theme(hc_theme_ffx())
  })
  
  output$vehiclesAllTable <- renderReactable(
    reactable(selected_vehicles_everyday(),
              defaultSorted = c("Всего_тс"),
              groupBy = "Сотрудник",
              columns = list(
                Всего_тс = colDef(name = 'Всего', defaultSortOrder = "desc", aggregate = "sum"),
                Создано_тс = colDef(name = 'Создано', aggregate = "sum"),
                Изменено_тс = colDef(name = 'Изменено', aggregate = "sum")
              )
              )
  )
  
  output$vehiclesEveryDayPlot <- renderHighchart({
    selected_vehicles_everyday() %>%
      hchart(
        type = "spline", hcaes(x = Дата, y = Всего_тс, group = Сотрудник)      ) %>%
      
      hc_title(text = "Создание и изменение ТС",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("По дням за период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
  })
  
  ############################
  ############################  
  ###########################
  
  #####################################
  ### Отменённые рейсы ###
  #####################################
  
  # observeEvent(input$getCanceledOrdersButton, {
  #   breakdownsTable$data <- getBreakdowns()
  #   
  # })
  
  output$canceledOrdersClientPlot <- renderHighchart({
    highchart() %>%
      hc_chart(type ="bar",
               options3d = list(enabled = FALSE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = selected_canceledOrdersClient()$Клиент) %>%
      hc_add_series(data = selected_canceledOrdersClient()$Заявки, name = "Отменённые рейсы") %>%
      hc_add_series(data = selected_canceledOrdersClient()$Ставка_клиента, name = "Ставка клиент") %>%
      hc_title(text = "Отменённые рейсы",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
  })
  
  output$canceledOrdersCarrierPlot <- renderHighchart({
    highchart() %>%
      hc_chart(type ="bar",
               options3d = list(enabled = FALSE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = selected_canceledOrdersCarrier()$Перевозчик) %>%
      hc_add_series(data = selected_canceledOrdersCarrier()$Заявки, name = "Отменённые рейсы") %>%
      hc_add_series(data = selected_canceledOrdersCarrier()$Ставка_клиента, name = "Ставка клиент") %>%
      hc_title(text = "Отменённые рейсы",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
  })
  
  output$canceledOrdersLogistPlot <- renderHighchart({
    highchart() %>%
      hc_chart(type ="bar",
               options3d = list(enabled = FALSE, beta = 15, alpha = 15)) %>%
      hc_xAxis(categories = selected_canceledOrdersLogist()$Ответственный_логист) %>%
      hc_add_series(data = selected_canceledOrdersLogist()$Заявки, name = "Отменённые рейсы") %>%
      hc_add_series(data = selected_canceledOrdersLogist()$Ставка_клиента, name = "Ставка клиент") %>%
      hc_title(text = "Отменённые рейсы",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx())
  })
  
  #####################################
  #####################################
  #####################################
  
  ##########################################
  #### Отсутствующие заявки и договора ####
  ##########################################
  
  observeEvent(input$getMissingDocsButton, {
    
    missingDocsTable = getMissingDocsTable()
    missingRequestsTable = missingDocsTable$requests
    missingContractsTable = missingDocsTable$contracts
    missingRequestsTableDay <- filter(missingRequestsTable, Дата == Sys.Date() - 1)
    missingContractsTableDay <- filter(missingContractsTable, Дата == Sys.Date() - 1)
    
    output$missingDocsPlotCurrentDay <- renderHighchart(
      highchart() %>%
        hc_chart(type ="column",
                 options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>%
        hc_xAxis(categories = missingRequestsTableDay$Логист) %>%
        hc_add_series(data = missingRequestsTableDay$Всего, name = "Всего заявок") %>%
        hc_add_series(data = missingRequestsTableDay$Отсутствует, name = "Отсутствует заявка клиента") %>%
        hc_add_series(data = missingContractsTableDay$Отсутствует, name = "Отсутствует договор перевозчика") %>%
        
        hc_title(text = "Отсутствующие документы",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste0("За вчерашний день (", format(Sys.Date() - 1, "%d %B %Y"), ")"),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>% 
        
        hc_add_theme(hc_theme_ffx())
    )
    
    output$missingRequestsPlot <- renderHighchart(
      missingRequestsTable %>% 
        
        hchart('column', hcaes(x = 'Дата', y = 'Отсутствует', group = "Логист")) %>%
        
        hc_title(text = "Отсутствующие заявки клиента",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("По дням по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>% 
        
        hc_add_theme(hc_theme_ffx())
    )
    
    output$missingContractsPlot <- renderHighchart(
      missingContractsTable %>% 
        
        hchart('column', hcaes(x = 'Дата', y = 'Отсутствует', group = "Логист")) %>%
        
        hc_title(text = "Отсутствующие договоры перевозчика",
                 style = list(fontWeight = "bold", fontSize = "14px"),
                 align = "center") %>% 
        
        hc_subtitle(text = paste("По дням по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    style = list(fontWeight = "bold"),
                    align = "center")  %>% 
        
        hc_add_theme(hc_theme_ffx())
    )
    
  })
  
  #############################################
  ### Конверсия обращений в БД перевозчиков ###
  #############################################
  
  # Изначально данные загружались при нажатии на кнопку
  # observeEvent(input$getCarriersButton, {
  #   newOldCarriersTable$data <- getNewOldCarriers()
  # })
  
  selected_lateOrders <- reactive({
    lateOrdersTable$data %>% 
      filter(Дата >= input$date_carriers[1] & Дата <= input$date_carriers[2])
  })
  
  selected_lateOrders_byDay <- reactive({
    
    aggregation <- switch(input$aggregation_carriers, 
                          "День" = "%Y.%m.%d",
                          "Неделя" = "%Y.%V",
                          "Месяц" = "%Y.%m",
                          "Год" = "%Y")
    
    lateOrdersTable$data %>% 
      filter(Дата >= input$date_carriers[1] & Дата <= input$date_carriers[2]) %>%
      mutate(Дата = format(Дата, aggregation)) %>%
      group_by(Дата) %>%
      summarise(Заявки = n())
  })
  
  selected_newOldCarriers <- reactive({
    if(!is.null(newOldCarriersTable$data)){
      newOldCarriersTable$data %>% 
        filter(Дата >= input$date_carriers[1] & Дата <= input$date_carriers[2]) %>%
        group_by(Логист, Статус) %>%
        summarise(Рейсы = length(unique(Перевозчик))) %>%
        # summarise(Рейсы = n()) %>%
        pivot_wider(names_from = Статус, 
                    values_from = Рейсы,
                    values_fill = 0) %>%
        mutate(Всего = Новый + Существующий,
                    Конверсия = round(Существующий/Всего * 100, digits = 2)) %>%
        arrange(desc(Конверсия))    
    }
    
  })
  
  selected_newOldCarriers_byDay <- reactive({
    if(!is.null(newOldCarriersTable$data)){
      
      aggregation <- switch(input$aggregation_carriers, 
                            "День" = "%Y.%m.%d",
                            "Неделя" = "%Y.%V",
                            "Месяц" = "%Y.%m",
                            "Год" = "%Y")
      
      newOldCarriersTable$data %>% 
        filter(Дата >= input$date_carriers[1] & Дата <= input$date_carriers[2]) %>%
        mutate(Дата = format(Дата, aggregation)) %>%
        group_by(Дата, Статус) %>%
        summarise(Рейсы = length(unique(Перевозчик))) %>%
        # summarise(Рейсы = n()) %>%
        pivot_wider(names_from = Статус, 
                    values_from = Рейсы,
                    values_fill = 0) %>%
        mutate(Всего = Новый + Существующий,
                    Конверсия = round(Существующий/Всего * 100, digits = 2)) %>%
        arrange(Дата)    
    }
    
  })
  
  selected_newOldCarriers1 <- reactive({
    if(!is.null(newOldCarriersTable$data)){
      newOldCarriersTable$data %>% 
        filter(Дата >= input$date_carriers[1] & Дата <= input$date_carriers[2])
    }
  })
  
  output$totalNewOldCarriers <- renderValueBox({
    valueBox(
      format(sum(selected_newOldCarriers()$Всего), big.mark=" "), 
      "Всего перевозчиков", 
      icon = icon("car"),
      color = "purple"
    )
  })
  
  output$totalNewCarriers <- renderValueBox({
    valueBox(
      format(sum(selected_newOldCarriers()$Новый), big.mark=" "), 
      "Новых перевозчиков", 
      icon = icon("car"),
      color = "yellow"
    )
  })
  
  output$totalConversionCarriers <- renderValueBox({
    valueBox(
      round(sum(selected_newOldCarriers()$Существующий) / sum(selected_newOldCarriers()$Всего) * 100, digits = 2), 
      "Переиспользование перевозчиков", 
      icon = icon("percent"),
      color = "lime"
    )
  })
  
  output$conversion_dynamic_general <- renderHighchart({

    highchart() %>%
      hc_chart(type ="spline") %>%
      hc_xAxis(categories = selected_newOldCarriers_byDay()$Дата) %>%
      hc_add_series(data = selected_newOldCarriers_byDay()$Конверсия, name = "Конверсия") %>%
      hc_title(text = "Динамика переиспользования базы данных перевозчиков",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%

      hc_subtitle(text = paste("За период", format(input$date_carriers[1], "%d.%m.%Y"), "-", format(input$date_carriers[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%

      hc_add_theme(hc_theme_ffx()) %>%

      hc_credits(enabled = T, text = "По созданным заявкам")

  })
  
  output$lateOrders_dynamic_general <- renderHighchart({
    
    highchart() %>%
      hc_chart(type ="spline") %>%
      hc_xAxis(categories = selected_lateOrders_byDay()$Дата) %>%
      hc_add_series(data = selected_lateOrders_byDay()$Заявки, name = "Просроченные заявки") %>%
      hc_title(text = "Динамика просроченных заявок",
               style = list(fontWeight = "bold", fontSize = "14px"),
               align = "center") %>%
      
      hc_subtitle(text = paste("За период", format(input$date_carriers[1], "%d.%m.%Y"), "-", format(input$date_carriers[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                  style = list(fontWeight = "bold"),
                  align = "center")  %>%
      
      hc_add_theme(hc_theme_ffx()) %>%
      
      hc_credits(enabled = T, text = "По созданным заявкам")
    
  })
  

  output$conversionDataTable <- renderReactable(
    if(!is.null(selected_newOldCarriers())){
      reactable(selected_newOldCarriers(),
                columns = list(
                  Новый = colDef(name = "Новых перевозчиков"),
                  Существующий = colDef(name = "Старых перевозчиков"),
                  Конверсия = colDef(name = "Конверсия, %")
                  )
                )
    }
  )
  
  output$lateOrders_reactableTable <- renderReactable(
    if(!is.null(selected_lateOrders())){
      reactable(selected_lateOrders(),
                defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
                groupBy = "Маршрут",
                defaultSorted = c("Номер"),
                columns = list(
                  Маршрут = colDef(footer = "Итог"),
                  Номер = colDef(name = "Рейсы", defaultSortOrder = "desc", aggregate = "count",
                  footer = JS("function(colInfo) {
        let total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total.toFixed(0)
      }")),
                  Ставка_клиента = colDef(name = "Ставка клиента",
                                          aggregate = "sum",
                                          format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0),
                                          footer = function(values) paste(format(sum(values), digits = 0, big.mark=" ", scientific=FALSE), "\U20BD")),
                  Место_погрузки = colDef(name = "Место погрузки"),
                  Место_выгрузки = colDef(name = "Место выгрузки")
                )
                )
    }
  )
  
  # План-факт по менеджерам
  
  warranty_cols <- c("Выручка_план_гарантия",
                     "Выручка_факт_гарантия",
                     "Выручка_выполнение_гарантия",
                     "Маржа_план_гарантия",
                     "Маржа_факт_гарантия",
                     "Маржа_выполнение_гарантия",
                     "Рентабельность_план_гарантия",
                     "Рентабельность_факт_гарантия",
                     "Рентабельность_выполнение_гарантия",
                     "Рейсы_план_гарантия",
                     "Рейсы_факт_гарантия",
                     "Рейсы_выполнение_гарантия",
                     "Стоимость_рейса_план_гарантия",
                     "Стоимость_рейса_факт_гарантия",
                     "Стоимость_рейса_выполнение_гарантия")
  
  spot_cols <- c("Выручка_план_спот",
                 "Выручка_факт_спот",
                 "Выручка_выполнение_спот",
                 "Маржа_план_спот",
                 "Маржа_факт_спот",
                 "Маржа_выполнение_спот",
                 "Рентабельность_план_спот",
                 "Рентабельность_факт_спот",
                 "Рентабельность_выполнение_спот",
                 "Рейсы_план_спот",
                 "Рейсы_факт_спот",
                 "Рейсы_выполнение_спот",
                 "Стоимость_рейса_план_спот",
                 "Стоимость_рейса_факт_спот",
                 "Стоимость_рейса_выполнение_спот")
  
  general_cols <- c("Выручка_план_общий",
                    "Выручка_факт_общий",
                    "Выручка_выполнение_общий",
                    "Маржа_план_общий",
                    "Маржа_факт_общий",
                    "Маржа_выполнение_общий",
                    "Рентабельность_план_общий",
                    "Рентабельность_факт_общий",
                    "Рентабельность_выполнение_общий",
                    "Рейсы_план_общий",
                    "Рейсы_факт_общий",
                    "Рейсы_выполнение_общий",
                    "Стоимость_рейса_план_общий",
                    "Стоимость_рейса_факт_общий",
                    "Стоимость_рейса_выполнение_общий")
  
  getManagerPlanFact <- function(){
    plan <- read.xlsx(Sys.getenv("REPORTS_LEGACY_PLAN_1", "plan-1.local.xlsx"),
                      sheet = 1,
                      startRow = 5) %>%
      select(-2, -4, -5, -6) %>%
      rename("Менеджер" = 1,
             "Клиент" = 2,
             "Выручка_план_гарантия" = 3,
             "Маржа_план_гарантия" = 4,
             "Рентабельность_план_гарантия" = 5,
             "Рейсы_план_гарантия" = 6,
             "Стоимость_рейса_план_гарантия" = 7,
             "Выручка_план_спот" = 8,
             "Маржа_план_спот" = 9,
             "Рентабельность_план_спот" = 10,
             "Рейсы_план_спот" = 11,
             "Стоимость_рейса_план_спот" = 12,
             "Выручка_план_общий" = 13,
             "Маржа_план_общий" = 14,
             "Рентабельность_план_общий" = 15,
             "Рейсы_план_общий" = 16,
             "Стоимость_рейса_план_общий" = 17
      ) %>%
      filter(!is.na(Клиент))
    
    # plan <- plan[-c(1, 2, 3), ]
    
    fact <- selectedFactOnUnloadedTable_manager() %>%
      group_by(Клиент, Менеджер, Тип_заявки) %>%
      summarise(Оборот = sum(Ставка_клиента_НДС), 
                      Маржа = sum(Маржа_расчёт),
                      Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                      Рейсы = sum(Рейсы),
                      Стоимость_рейса = round(Оборот / Рейсы, digits = 2))
    
    fact_company_group <- fact %>% 
      filter(Клиент == "Архивный клиент 03" |
                     Клиент == "Архивный клиент 21" |
                     Клиент == "Архивный клиент 04") %>%
      group_by(Менеджер, Тип_заявки) %>%
      summarise(Оборот = sum(Оборот), 
                      Маржа = sum(Маржа),
                      Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                      Рейсы = sum(Рейсы),
                      Стоимость_рейса = round(Оборот / Рейсы, digits = 2)) %>%
      mutate(Клиент = "Красный октябрь/Бабаевский/Русский шоколад")
    
    fact_company_group1 <- fact %>% 
      filter(Клиент == "Архивный клиент 05" |
                     Клиент == "Архивный клиент 06") %>%
      group_by(Менеджер, Тип_заявки) %>%
      summarise(Оборот = sum(Оборот), 
                      Маржа = sum(Маржа),
                      Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                      Рейсы = sum(Рейсы),
                      Стоимость_рейса = round(Оборот / Рейсы, digits = 2)) %>%
      mutate(Клиент = 'Ривьера АО/"ЕВРОПАРК" ООО/"Ландшафт-строй" ООО')
    
    
    fact_company_group2 <- fact %>% 
      filter(Клиент == "Архивный клиент 07" |
                     Клиент == "Архивный клиент 08" |
                     Клиент == "Архивный клиент 09") %>%
      group_by(Менеджер, Тип_заявки) %>%
      summarise(Оборот = sum(Оборот), 
                      Маржа = sum(Маржа),
                      Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                      Рейсы = sum(Рейсы),
                      Стоимость_рейса = round(Оборот / Рейсы, digits = 2)) %>%
      mutate(Клиент = 'СИТИМАФ/АДАНАТ/"Городские проекты" ООО')
    
    fact <- rbind(filter(fact, Клиент != "Архивный клиент 03" |
                           Клиент != "Архивный клиент 21" |
                           Клиент != "Архивный клиент 04" |
                           Клиент != "Архивный клиент 05" |
                           Клиент != "Архивный клиент 06"|
                           Клиент != "Архивный клиент 07"|
                           Клиент != "Архивный клиент 08"|
                           Клиент != "Архивный клиент 09"), fact_company_group, fact_company_group1, fact_company_group2)
    
    fact_warranty <- fact %>%
      filter(Тип_заявки == "Гарантия") %>%
      select(-Тип) %>%
      rename("Выручка_факт_гарантия" = 3,
             "Маржа_факт_гарантия" = 4,
             "Рентабельность_факт_гарантия" = 5,
             "Рейсы_факт_гарантия" = 6,
             "Стоимость_рейса_факт_гарантия" = 7)
    
    fact_spot <- fact %>%
      filter(Тип_заявки == "Спот") %>%
      select(-Тип) %>%
      rename("Выручка_факт_спот" = 3,
             "Маржа_факт_спот" = 4,
             "Рентабельность_факт_спот" = 5,
             "Рейсы_факт_спот" = 6,
             "Стоимость_рейса_факт_спот" = 7)
    
    fact_all <- fact_warranty %>% 
      full_join(fact_spot, by = c("Клиент" = "Клиент", "Менеджер" = "Менеджер"))
    
    result <- plan %>%
      left_join(fact_all, by = c("Клиент" = "Клиент", "Менеджер" = "Менеджер"))
    
    result[is.na(result)] <- 0
    
    result_plan_fact <- result %>%
      transmute(Менеджер = Менеджер,
                        Клиент = Клиент,
                        Выручка_план_гарантия = as.numeric(Выручка_план_гарантия),
                        Выручка_факт_гарантия = as.numeric(Выручка_факт_гарантия),
                        Выручка_выполнение_гарантия = round(Выручка_факт_гарантия / Выручка_план_гарантия * 100, digits = 2),
                        Маржа_план_гарантия = as.numeric(Маржа_план_гарантия),
                        Маржа_факт_гарантия = as.numeric(Маржа_факт_гарантия),
                        Маржа_выполнение_гарантия = round(Маржа_факт_гарантия / Маржа_план_гарантия * 100, digits = 2),
                        Рентабельность_план_гарантия = as.numeric(Рентабельность_план_гарантия),
                        Рентабельность_факт_гарантия = as.numeric(Рентабельность_факт_гарантия),
                        Рентабельность_выполнение_гарантия = round(Рентабельность_факт_гарантия / Рентабельность_план_гарантия * 100, digits = 2),
                        Рейсы_план_гарантия = as.numeric(Рейсы_план_гарантия),
                        Рейсы_факт_гарантия = as.numeric(Рейсы_факт_гарантия),
                        Рейсы_выполнение_гарантия = round(Рейсы_факт_гарантия / Рейсы_план_гарантия * 100, digits = 2),
                        Стоимость_рейса_план_гарантия = as.numeric(Стоимость_рейса_план_гарантия),
                        Стоимость_рейса_факт_гарантия = as.numeric(Стоимость_рейса_факт_гарантия),
                        Стоимость_рейса_выполнение_гарантия = round(Стоимость_рейса_факт_гарантия / Стоимость_рейса_план_гарантия * 100, digits = 2),

                        Выручка_план_спот = as.numeric(Выручка_план_спот),
                        Выручка_факт_спот = as.numeric(Выручка_факт_спот),
                        Выручка_выполнение_спот = round(Выручка_факт_спот / Выручка_план_спот * 100, digits = 2),
                        Маржа_план_спот = as.numeric(Маржа_план_спот),
                        Маржа_факт_спот = as.numeric(Маржа_факт_спот),
                        Маржа_выполнение_спот = round(Маржа_факт_спот / Маржа_план_спот * 100, digits = 2),
                        Рентабельность_план_спот = as.numeric(Рентабельность_план_спот),
                        Рентабельность_факт_спот = as.numeric(Рентабельность_факт_спот),
                        Рентабельность_выполнение_спот = round(Рентабельность_факт_спот / Рентабельность_план_спот * 100, digits = 2),
                        Рейсы_план_спот = as.numeric(Рейсы_план_спот),
                        Рейсы_факт_спот = as.numeric(Рейсы_факт_спот),
                        Рейсы_выполнение_спот = round(Рейсы_факт_спот / Рейсы_план_спот * 100, digits = 2),
                        Стоимость_рейса_план_спот = as.numeric(Стоимость_рейса_план_спот),
                        Стоимость_рейса_факт_спот = as.numeric(Стоимость_рейса_факт_спот),
                        Стоимость_рейса_выполнение_спот = round(Стоимость_рейса_факт_спот / Стоимость_рейса_план_спот * 100, digits = 2),

                        Выручка_план_общий = as.numeric(Выручка_план_общий),
                        Выручка_факт_общий = Выручка_факт_гарантия + Выручка_факт_спот,
                        Выручка_выполнение_общий = round(Выручка_факт_общий / Выручка_план_общий * 100, digits = 2),
                        Маржа_план_общий = as.numeric(Маржа_план_общий),
                        Маржа_факт_общий = Маржа_факт_гарантия + Маржа_факт_спот,
                        Маржа_выполнение_общий = round(Маржа_факт_общий / Маржа_план_общий * 100, digits = 2),
                        Рентабельность_план_общий = as.numeric(Рентабельность_план_общий),
                        Рентабельность_факт_общий = round(Маржа_факт_общий / Выручка_факт_общий * 100, digits = 2),
                        Рентабельность_выполнение_общий = round(Рентабельность_факт_общий / Рентабельность_план_общий * 100, digits = 2),
                        Рейсы_план_общий = as.numeric(Рейсы_план_общий),
                        Рейсы_факт_общий = Рейсы_факт_гарантия + Рейсы_факт_спот,
                        Рейсы_выполнение_общий = round(Рейсы_факт_общий / Рейсы_план_общий * 100, digits = 2),
                        Стоимость_рейса_план_общий = as.numeric(Стоимость_рейса_план_общий),
                        Стоимость_рейса_факт_общий = round(Выручка_факт_общий / Рейсы_факт_общий, digits = 2),
                        Стоимость_рейса_выполнение_общий = round(Стоимость_рейса_факт_общий / Стоимость_рейса_план_общий * 100, digits = 2)

      ) %>%
      arrange(Менеджер, Клиент)

    result_plan_fact[is.na(result_plan_fact)] <- 0
    result_plan_fact[mapply(is.infinite, result_plan_fact)] <- 0
    
    result_plan_fact1 <- result %>%
      group_by(Менеджер) %>%
      summarise(
        Выручка_план_гарантия = sum(as.numeric(Выручка_план_гарантия)),
        Выручка_факт_гарантия = sum(as.numeric(Выручка_факт_гарантия)),
        Выручка_выполнение_гарантия = round(Выручка_факт_гарантия / Выручка_план_гарантия * 100, digits = 2),
        Маржа_план_гарантия = sum(as.numeric(Маржа_план_гарантия)),
        Маржа_факт_гарантия = sum(as.numeric(Маржа_факт_гарантия)),
        Маржа_выполнение_гарантия = round(Маржа_факт_гарантия / Маржа_план_гарантия * 100, digits = 2),
        Рентабельность_план_гарантия = round(Маржа_план_гарантия / Выручка_план_гарантия * 100, digits = 2),
        Рентабельность_факт_гарантия = round(Маржа_факт_гарантия / Выручка_факт_гарантия * 100, digits = 2),
        Рентабельность_выполнение_гарантия = round(Рентабельность_факт_гарантия / Рентабельность_план_гарантия * 100, digits = 2),
        Рейсы_план_гарантия = sum(as.numeric(Рейсы_план_гарантия)),
        Рейсы_факт_гарантия = sum(as.numeric(Рейсы_факт_гарантия)),
        Рейсы_выполнение_гарантия = round(Рейсы_факт_гарантия / Рейсы_план_гарантия * 100, digits = 2),
        Стоимость_рейса_план_гарантия = round(Выручка_план_гарантия / Рейсы_план_гарантия, digits = 2),
        Стоимость_рейса_факт_гарантия = round(Выручка_факт_гарантия / Рейсы_факт_гарантия, digits = 2),
        Стоимость_рейса_выполнение_гарантия = round(Стоимость_рейса_факт_гарантия / Стоимость_рейса_план_гарантия * 100, digits = 2),
        
        Выручка_план_спот = sum(as.numeric(Выручка_план_спот)),
        Выручка_факт_спот = sum(as.numeric(Выручка_факт_спот)),
        Выручка_выполнение_спот = round(Выручка_факт_спот / Выручка_план_спот * 100, digits = 2),
        Маржа_план_спот = sum(as.numeric(Маржа_план_спот)),
        Маржа_факт_спот = sum(as.numeric(Маржа_факт_спот)),
        Маржа_выполнение_спот = round(Маржа_факт_спот / Маржа_план_спот * 100, digits = 2),
        Рентабельность_план_спот = round(Маржа_план_спот / Выручка_план_спот * 100, digits = 2),
        Рентабельность_факт_спот = round(Маржа_факт_спот / Выручка_факт_спот * 100, digits = 2),
        Рентабельность_выполнение_спот = round(Рентабельность_факт_спот / Рентабельность_план_спот * 100, digits = 2),
        Рейсы_план_спот = sum(as.numeric(Рейсы_план_спот)),
        Рейсы_факт_спот = sum(as.numeric(Рейсы_факт_спот)),
        Рейсы_выполнение_спот = round(Рейсы_факт_спот / Рейсы_план_спот * 100, digits = 2),
        Стоимость_рейса_план_спот = round(Выручка_план_спот / Рейсы_план_спот, digits = 2),
        Стоимость_рейса_факт_спот = round(Выручка_факт_спот / Рейсы_факт_спот, digits = 2),
        Стоимость_рейса_выполнение_спот = round(Стоимость_рейса_факт_спот / Стоимость_рейса_план_спот * 100, digits = 2),
        
        Выручка_план_общий = sum(as.numeric(Выручка_план_общий)),
        Выручка_факт_общий = Выручка_факт_гарантия + Выручка_факт_спот,
        Выручка_выполнение_общий = round(Выручка_факт_общий / Выручка_план_общий * 100, digits = 2),
        Маржа_план_общий = sum(as.numeric(Маржа_план_общий)),
        Маржа_факт_общий = Маржа_факт_гарантия + Маржа_факт_спот,
        Маржа_выполнение_общий = round(Маржа_факт_общий / Маржа_план_общий * 100, digits = 2),
        Рентабельность_план_общий = round(Маржа_план_общий / Выручка_план_общий * 100, digits = 2),
        Рентабельность_факт_общий = round(Маржа_факт_общий / Выручка_факт_общий * 100, digits = 2),
        Рентабельность_выполнение_общий = round(Рентабельность_факт_общий / Рентабельность_план_общий * 100, digits = 2),
        Рейсы_план_общий = sum(as.numeric(Рейсы_план_общий)),
        Рейсы_факт_общий = Рейсы_факт_гарантия + Рейсы_факт_спот,
        Рейсы_выполнение_общий = round(Рейсы_факт_общий / Рейсы_план_общий * 100, digits = 2),
        Стоимость_рейса_план_общий = round(Выручка_план_общий / Рейсы_план_общий, digits = 2),
        Стоимость_рейса_факт_общий = round(Выручка_факт_общий / Рейсы_факт_общий, digits = 2),
        Стоимость_рейса_выполнение_общий = round(Стоимость_рейса_факт_общий / Стоимость_рейса_план_общий * 100, digits = 2)
        
      ) %>%
      
      arrange(Менеджер)
    
    generalTable <- reactable(result_plan_fact1[, c("Менеджер", general_cols)],
              pagination = FALSE,
              defaultColGroup = colGroup(headerClass = "group-header"),
              columnGroups = list(
                colGroup(name = "Общий", columns = general_cols)
              ),
              defaultColDef = colDef(class = "cell", headerClass = "header"),
              
              columns = list(
                Менеджер = colDef(
                  defaultSortOrder = "asc",
                  minWidth = 270,
                  headerStyle = list(fontWeight = 700), 
                  cell = function(value, index) {
                    div(
                      class = "manager",
                      if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/com/%s.png", value)),
                      div(class = "manager-name", value)
                    )
                  }
                ),
                
                Выручка_план_общий = colDef(name = "Выручка, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Выручка_факт_общий = colDef(
                  style = function(value) {
                    normalized <- (value - min(result_plan_fact1$Выручка_факт_общий)) / (max(result_plan_fact1$Выручка_факт_общий) - min(result_plan_fact1$Выручка_факт_общий))
                    color <- orange_pal(normalized)
                    list(background = color)
                  },
                  name = "Выручка, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Выручка_выполнение_общий = rating_column(
                  name = "Выручка, выполнение",
                  cell = function(value) {
                    scaled <- (value - min(result_plan_fact1$Выручка_выполнение_общий)) / (max(result_plan_fact1$Выручка_выполнение_общий) - min(result_plan_fact1$Выручка_выполнение_общий))
                    color <- off_rating_color(scaled)
                    value <- format(round(value, 1), nsmall = 1)
                    div(class = "spi-rating", style = list(background = color), value)
                  }
                ),
                
                Маржа_план_общий = colDef(name = "Маржа, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Маржа_факт_общий = colDef(
                  style = function(value) {
                    normalized <- (value - min(result_plan_fact1$Маржа_факт_общий)) / (max(result_plan_fact1$Маржа_факт_общий) - min(result_plan_fact1$Маржа_факт_общий))
                    color <- orange_pal(normalized)
                    list(background = color)
                  },
                  name = "Маржа, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Маржа_выполнение_общий = rating_column(
                  cell = function(value) {
                    scaled <- (value - min(result_plan_fact1$Маржа_выполнение_общий)) / (max(result_plan_fact1$Маржа_выполнение_общий) - min(result_plan_fact1$Маржа_выполнение_общий))
                    color <- off_rating_color(scaled)
                    value <- format(round(value, 1), nsmall = 1)
                    div(class = "spi-rating", style = list(background = color), value)
                  },
                  name = "Маржа, выполнение"),
                
                Рентабельность_план_общий = colDef(name = "Рентабельность, план", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                Рентабельность_факт_общий = colDef(
                  style = function(value) {
                    normalized <- (value - min(result_plan_fact1$Рентабельность_факт_общий)) / (max(result_plan_fact1$Рентабельность_факт_общий) - min(result_plan_fact1$Рентабельность_факт_общий))
                    color <- orange_pal(normalized)
                    list(background = color)
                  },
                  name = "Рентабельность, факт", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                Рентабельность_выполнение_общий = rating_column(
                  cell = function(value) {
                    scaled <- (value - min(result_plan_fact1$Рентабельность_выполнение_общий)) / (max(result_plan_fact1$Рентабельность_выполнение_общий) - min(result_plan_fact1$Рентабельность_выполнение_общий))
                    color <- off_rating_color(scaled)
                    value <- format(round(value, 1), nsmall = 1)
                    div(class = "spi-rating", style = list(background = color), value)
                  },
                  name = "Рентабельность, выполнение"),
                
                Рейсы_план_общий = colDef(name = "Рейсы, план", format = colFormat(separators = TRUE)),
                Рейсы_факт_общий = colDef(
                  style = function(value) {
                    normalized <- (value - min(result_plan_fact1$Рейсы_факт_общий)) / (max(result_plan_fact1$Рейсы_факт_общий) - min(result_plan_fact1$Рейсы_факт_общий))
                    color <- orange_pal(normalized)
                    list(background = color)
                  },
                  name = "Рейсы, факт", format = colFormat(separators = TRUE)),
                Рейсы_выполнение_общий = rating_column(
                  cell = function(value) {
                    scaled <- (value - min(result_plan_fact1$Рейсы_выполнение_общий)) / (max(result_plan_fact1$Рейсы_выполнение_общий) - min(result_plan_fact1$Рейсы_выполнение_общий))
                    color <- off_rating_color(scaled)
                    value <- format(round(value, 1), nsmall = 1)
                    div(class = "spi-rating", style = list(background = color), value)
                  },
                  name = "Рейсы, выполнение"),
                
                Стоимость_рейса_план_общий = colDef(name = "Стоимость рейса, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Стоимость_рейса_факт_общий = colDef(
                  style = function(value) {
                    normalized <- (value - min(result_plan_fact1$Стоимость_рейса_факт_общий)) / (max(result_plan_fact1$Стоимость_рейса_факт_общий) - min(result_plan_fact1$Стоимость_рейса_факт_общий))
                    color <- orange_pal(normalized)
                    list(background = color)
                  },
                  name = "Стоимость рейса, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                Стоимость_рейса_выполнение_общий = rating_column(
                  cell = function(value) {
                    scaled <- (value - min(result_plan_fact1$Стоимость_рейса_выполнение_общий)) / (max(result_plan_fact1$Стоимость_рейса_выполнение_общий) - min(result_plan_fact1$Стоимость_рейса_выполнение_общий))
                    color <- off_rating_color(scaled)
                    value <- format(round(value, 1), nsmall = 1)
                    div(class = "spi-rating", style = list(background = color), value)
                  },
                  name = "Стоимость рейса, выполнение")
                
              ),
              
              rowClass = JS("
    function(rowInfo, state) {
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'Клиент') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"
              ),
              showSortIcon = FALSE,
              borderless = TRUE,
              class = "standings-table"
              
    )
    
    detailTable <- reactable(result_plan_fact,
                             groupBy = 'Менеджер',
                                pagination = FALSE,
                                defaultColGroup = colGroup(headerClass = "group-header"),
                                columnGroups = list(
                                  colGroup(name = "Гарантия", columns = warranty_cols),
                                  colGroup(name = "Спот", columns = spot_cols),
                                  colGroup(name = "Общий", columns = general_cols)
                                ),
                                defaultColDef = colDef(class = "cell", headerClass = "header"),
                                
                                columns = list(
                                  Менеджер = colDef(
                                    defaultSortOrder = "asc",
                                    minWidth = 270,
                                    headerStyle = list(fontWeight = 700), 
                                    cell = function(value, index) {
                                      div(
                                        class = "manager",
                                        if (!DEMO_MODE) img(class = "portrait", alt = paste(value, "portrait"), src = sprintf("images/%s.png", value)),
                                        div(class = "manager-name", value)
                                      )
                                    }
                                  ),
                                  
                                  # Клиент = colDef(defaultSortOrder = "asc", align = "center", maxWidth = 150,
                                  #                class = "cell group", headerStyle = list(fontWeight = 700)),
                                  
                                  Выручка_план_общий = colDef(aggregate = "sum", name = "Выручка, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Выручка_факт_общий = colDef(aggregate = "sum", name = "Выручка, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Выручка_выполнение_общий = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_общий']
                                              totalUnits += row['Выручка_план_общий']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"),
                                    name = "Выручка, выполнение"
                                  ),
                                  
                                  Маржа_план_общий = colDef(aggregate = "sum", name = "Маржа, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Маржа_факт_общий = colDef(aggregate = "sum",

                                    name = "Маржа, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Маржа_выполнение_общий = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_общий']
                                              totalUnits += row['Маржа_план_общий']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"),
                                    name = "Маржа, выполнение"),
                                  
                                  Рентабельность_план_общий = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_план_общий']
                                              totalUnits += row['Выручка_план_общий']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рентабельность, план", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                  Рентабельность_факт_общий = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_общий']
                                              totalUnits += row['Выручка_факт_общий']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"),
                                    name = "Рентабельность, факт", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                  Рентабельность_выполнение_общий = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            var totalPrice1 = 0
                                            var totalUnits1 = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_общий']
                                              totalUnits += row['Выручка_факт_общий'] 
                                              totalPrice1 += row['Маржа_план_общий']
                                              totalUnits1 += row['Выручка_план_общий'] 
                                            })
                                            result = (totalPrice / totalUnits) / (totalPrice1 / totalUnits1) * 100
                                            return result 
                                          }"),
                                          name = "Рентабельность, выполнение"),
                                  
                                  Рейсы_план_общий = colDef(aggregate = "sum", name = "Рейсы, план", format = colFormat(separators = TRUE)),
                                  Рейсы_факт_общий = colDef(aggregate = "sum", name = "Рейсы, факт", format = colFormat(separators = TRUE)),
                                  Рейсы_выполнение_общий = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы_факт_общий']
                                              totalUnits += row['Рейсы_план_общий']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рейсы, выполнение"),
                                  
                                  Стоимость_рейса_план_общий = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_план_общий']
                                              totalUnits += row['Рейсы_план_общий']
                                            })
                                            return totalPrice / totalUnits 
                                          }"), name = "Стоимость рейса, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Стоимость_рейса_факт_общий = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_общий']
                                              totalUnits += row['Рейсы_факт_общий']
                                            })
                                            return totalPrice / totalUnits 
                                          }"), name = "Стоимость рейса, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Стоимость_рейса_выполнение_общий = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            var totalPrice1 = 0
                                            var totalUnits1 = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_общий'] 
                                              totalUnits += row['Рейсы_факт_общий'] 
                                              totalPrice1 += row['Выручка_план_общий'] 
                                              totalUnits1 += row['Рейсы_план_общий']
                                            })
                                            result = (totalPrice / totalUnits) / (totalPrice1 / totalUnits1) * 100
                                            return result
                                          }"), name = "Стоимость рейса, выполнение"),
                                  
                                  Выручка_план_гарантия = colDef(aggregate = "sum", name = "Выручка, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Выручка_факт_гарантия = colDef(aggregate = "sum",
                                                              name = "Выручка, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Выручка_выполнение_гарантия = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_гарантия']
                                              totalUnits += row['Выручка_план_гарантия']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Выручка, выполнение"
                                  ),
                                  
                                  Маржа_план_гарантия = colDef(aggregate = "sum", name = "Маржа, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Маржа_факт_гарантия = colDef(aggregate = "sum", name = "Маржа, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Маржа_выполнение_гарантия = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_гарантия']
                                              totalUnits += row['Маржа_план_гарантия']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Маржа, выполнение"),
                                  
                                  Рентабельность_план_гарантия = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_план_гарантия']
                                              totalUnits += row['Выручка_план_гарантия']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рентабельность, план", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                  Рентабельность_факт_гарантия = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_гарантия']
                                              totalUnits += row['Выручка_факт_гарантия']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рентабельность, факт", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                  Рентабельность_выполнение_гарантия = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            var totalPrice1 = 0
                                            var totalUnits1 = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_гарантия']
                                              totalUnits += row['Выручка_факт_гарантия'] 
                                              totalPrice1 += row['Маржа_план_гарантия']
                                              totalUnits1 += row['Выручка_план_гарантия'] 
                                            })
                                            result = (totalPrice / totalUnits) / (totalPrice1 / totalUnits1) * 100
                                            return result 
                                          }"), name = "Рентабельность, выполнение"),
                                  
                                  Рейсы_план_гарантия = colDef(aggregate = "sum", name = "Рейсы, план", format = colFormat(separators = TRUE)),
                                  Рейсы_факт_гарантия = colDef(aggregate = "sum", name = "Рейсы, факт", format = colFormat(separators = TRUE)),
                                  Рейсы_выполнение_гарантия = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы_факт_гарантия']
                                              totalUnits += row['Рейсы_план_гарантия']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рейсы, выполнение"),
                                  
                                  Стоимость_рейса_план_гарантия = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_план_гарантия']
                                              totalUnits += row['Рейсы_план_гарантия']
                                            })
                                            return totalPrice / totalUnits 
                                          }"), name = "Стоимость рейса, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Стоимость_рейса_факт_гарантия = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_гарантия']
                                              totalUnits += row['Рейсы_факт_гарантия']
                                            })
                                            return totalPrice / totalUnits 
                                          }"),
                                          name = "Стоимость рейса, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Стоимость_рейса_выполнение_гарантия = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            var totalPrice1 = 0
                                            var totalUnits1 = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_гарантия']
                                              totalUnits += row['Рейсы_факт_гарантия'] 
                                              totalPrice1 += row['Выручка_план_гарантия']
                                              totalUnits1 += row['Рейсы_план_гарантия'] 
                                            })
                                            result = (totalPrice / totalUnits) / (totalPrice1 / totalUnits1) * 100
                                            return result 
                                          }"),
                                          name = "Стоимость рейса, выполнение"),
                                  
                                  Выручка_план_спот = colDef(aggregate = "sum", name = "Выручка, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Выручка_факт_спот = colDef(aggregate = "sum",
                                                                 name = "Выручка, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Выручка_выполнение_спот = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_спот']
                                              totalUnits += row['Выручка_план_спот']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Выручка, выполнение"
                                  ),
                                  
                                  Маржа_план_спот = colDef(aggregate = "sum", name = "Маржа, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Маржа_факт_спот = colDef(aggregate = "sum", name = "Маржа, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Маржа_выполнение_спот = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_спот']
                                              totalUnits += row['Маржа_план_спот']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Маржа, выполнение"),
                                  
                                  Рентабельность_план_спот = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_план_спот']
                                              totalUnits += row['Выручка_план_спот']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рентабельность, план", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                  Рентабельность_факт_спот = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_спот']
                                              totalUnits += row['Выручка_факт_спот']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рентабельность, факт", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                  Рентабельность_выполнение_спот = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            var totalPrice1 = 0
                                            var totalUnits1 = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт_спот']
                                              totalUnits += row['Выручка_факт_спот'] 
                                              totalPrice1 += row['Маржа_план_спот']
                                              totalUnits1 += row['Выручка_план_спот'] 
                                            })
                                            result = (totalPrice / totalUnits) / (totalPrice1 / totalUnits1) * 100
                                            return result 
                                          }"), name = "Рентабельность, выполнение"),
                                  
                                  Рейсы_план_спот = colDef(aggregate = "sum", name = "Рейсы, план", format = colFormat(separators = TRUE)),
                                  Рейсы_факт_спот = colDef(aggregate = "sum", name = "Рейсы, факт", format = colFormat(separators = TRUE)),
                                  Рейсы_выполнение_спот = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы_факт_спот']
                                              totalUnits += row['Рейсы_план_спот']
                                            })
                                            return totalPrice / totalUnits * 100
                                          }"), name = "Рейсы, выполнение"),
                                  
                                  Стоимость_рейса_план_спот = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_план_спот']
                                              totalUnits += row['Рейсы_план_спот']
                                            })
                                            return totalPrice / totalUnits 
                                          }"), name = "Стоимость рейса, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Стоимость_рейса_факт_спот = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_спот']
                                              totalUnits += row['Рейсы_факт_спот']
                                            })
                                            return totalPrice / totalUnits 
                                          }"), name = "Стоимость рейса, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                  Стоимость_рейса_выполнение_спот = rating_column(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            var totalPrice1 = 0
                                            var totalUnits1 = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт_спот']
                                              totalUnits += row['Рейсы_факт_спот'] 
                                              totalPrice1 += row['Выручка_план_спот']
                                              totalUnits1 += row['Рейсы_план_спот'] 
                                            })
                                            result = (totalPrice / totalUnits) / (totalPrice1 / totalUnits1) * 100
                                            return result 
                                          }"), name = "Стоимость рейса, выполнение")
                                  
                                ),
                                
                                rowClass = JS("
    function(rowInfo, state) {
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'Клиент') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"
                                ),
                                showSortIcon = FALSE,
                                borderless = TRUE,
                                class = "standings-table"
                                
    )
    
    resultList <- list('general' = generalTable, 'detail' = detailTable)
     
    return(resultList)
  }
  
  
  output$reactableManagerPlanFactGeneral <- renderReactable(
    getManagerPlanFact()$general
  )
  
  output$reactableManagerPlanFactDetail <- renderReactable(
    getManagerPlanFact()$detail
  )
  
  output$downloadManagerPlanFact <- downloadHandler(
    filename = function() {
      paste("План-факт, менеджеры-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      plan <- read.xlsx(Sys.getenv("REPORTS_LEGACY_PLAN_1", "plan-1.local.xlsx"),
                        sheet = 1,
                        startRow = 5) %>%
        select(-2, -4, -5, -6) %>%
        rename("Менеджер" = 1,
               "Клиент" = 2,
               "Выручка_план_гарантия" = 3,
               "Маржа_план_гарантия" = 4,
               "Рентабельность_план_гарантия" = 5,
               "Рейсы_план_гарантия" = 6,
               "Стоимость_рейса_план_гарантия" = 7,
               "Выручка_план_спот" = 8,
               "Маржа_план_спот" = 9,
               "Рентабельность_план_спот" = 10,
               "Рейсы_план_спот" = 11,
               "Стоимость_рейса_план_спот" = 12,
               "Выручка_план_общий" = 13,
               "Маржа_план_общий" = 14,
               "Рентабельность_план_общий" = 15,
               "Рейсы_план_общий" = 16,
               "Стоимость_рейса_план_общий" = 17
        ) %>%
        filter(!is.na(Клиент))
      
      # plan <- plan[-c(1, 2, 3), ]
      
      fact <- selectedFactOnUnloadedTable_manager() %>%
        group_by(Клиент, Менеджер, Тип_заявки) %>%
        summarise(Оборот = sum(Ставка_клиента_НДС), 
                        Маржа = sum(Маржа_расчёт),
                        Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        Стоимость_рейса = round(Оборот / Рейсы, digits = 2))
      
      fact_company_group <- fact %>% 
        filter(Клиент == "Архивный клиент 03" |
                       Клиент == "Архивный клиент 21" |
                       Клиент == "Архивный клиент 04") %>%
        group_by(Менеджер, Тип) %>%
        summarise(Оборот = sum(Оборот), 
                        Маржа = sum(Маржа),
                        Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        Стоимость_рейса = round(Оборот / Рейсы, digits = 2)) %>%
        mutate(Клиент = "Красный октябрь/Бабаевский/Русский шоколад")
      
      fact_company_group1 <- fact %>% 
        filter(Клиент == "Архивный клиент 05" |
                       Клиент == "Архивный клиент 06") %>%
        group_by(Менеджер, Тип) %>%
        summarise(Оборот = sum(Оборот), 
                        Маржа = sum(Маржа),
                        Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        Стоимость_рейса = round(Оборот / Рейсы, digits = 2)) %>%
        mutate(Клиент = 'Ривьера АО/"ЕВРОПАРК" ООО/"Ландшафт-строй" ООО')
      
      
      fact_company_group2 <- fact %>% 
        filter(Клиент == "Архивный клиент 07" |
                       Клиент == "Архивный клиент 08" |
                       Клиент == "Архивный клиент 09") %>%
        group_by(Менеджер, Тип) %>%
        summarise(Оборот = sum(Оборот), 
                        Маржа = sum(Маржа),
                        Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        Стоимость_рейса = round(Оборот / Рейсы, digits = 2)) %>%
        mutate(Клиент = 'СИТИМАФ/АДАНАТ/"Городские проекты" ООО')
      
      fact <- rbind(filter(fact, Клиент != "Архивный клиент 03" |
                             Клиент != "Архивный клиент 21" |
                             Клиент != "Архивный клиент 04" |
                             Клиент != "Архивный клиент 05" |
                             Клиент != "Архивный клиент 06"|
                             Клиент != "Архивный клиент 07"|
                             Клиент != "Архивный клиент 08"|
                             Клиент != "Архивный клиент 09"), fact_company_group, fact_company_group1, fact_company_group2)
      
      fact_warranty <- fact %>%
        filter(Тип == "Гарантия") %>%
        select(-Тип) %>%
        rename("Выручка_факт_гарантия" = 3,
               "Маржа_факт_гарантия" = 4,
               "Рентабельность_факт_гарантия" = 5,
               "Рейсы_факт_гарантия" = 6,
               "Стоимость_рейса_факт_гарантия" = 7)
      
      fact_spot <- fact %>%
        filter(Тип == "Спот") %>%
        select(-Тип) %>%
        rename("Выручка_факт_спот" = 3,
               "Маржа_факт_спот" = 4,
               "Рентабельность_факт_спот" = 5,
               "Рейсы_факт_спот" = 6,
               "Стоимость_рейса_факт_спот" = 7)
      
      fact_all <- fact_warranty %>% 
        full_join(fact_spot, by = c("Клиент" = "Клиент", "Менеджер" = "Менеджер"))
      
      result <- plan %>%
        left_join(fact_all, by = c("Клиент" = "Клиент", "Менеджер" = "Менеджер"))
      
      result[is.na(result)] <- 0
      
      result_plan_fact <- result %>%
        transmute(Менеджер = Менеджер,
                          Клиент = Клиент,
                          Выручка_план_гарантия = as.numeric(Выручка_план_гарантия),
                          Выручка_факт_гарантия = as.numeric(Выручка_факт_гарантия),
                          Выручка_выполнение_гарантия = round(Выручка_факт_гарантия / Выручка_план_гарантия * 100, digits = 2),
                          Маржа_план_гарантия = as.numeric(Маржа_план_гарантия),
                          Маржа_факт_гарантия = as.numeric(Маржа_факт_гарантия),
                          Маржа_выполнение_гарантия = round(Маржа_факт_гарантия / Маржа_план_гарантия * 100, digits = 2),
                          Рентабельность_план_гарантия = as.numeric(Рентабельность_план_гарантия),
                          Рентабельность_факт_гарантия = as.numeric(Рентабельность_факт_гарантия),
                          Рентабельность_выполнение_гарантия = round(Рентабельность_факт_гарантия / Рентабельность_план_гарантия * 100, digits = 2),
                          Рейсы_план_гарантия = as.numeric(Рейсы_план_гарантия),
                          Рейсы_факт_гарантия = as.numeric(Рейсы_факт_гарантия),
                          Рейсы_выполнение_гарантия = round(Рейсы_факт_гарантия / Рейсы_план_гарантия * 100, digits = 2),
                          Стоимость_рейса_план_гарантия = as.numeric(Стоимость_рейса_план_гарантия),
                          Стоимость_рейса_факт_гарантия = as.numeric(Стоимость_рейса_факт_гарантия),
                          Стоимость_рейса_выполнение_гарантия = round(Стоимость_рейса_факт_гарантия / Стоимость_рейса_план_гарантия * 100, digits = 2),
                          
                          Выручка_план_спот = as.numeric(Выручка_план_спот),
                          Выручка_факт_спот = as.numeric(Выручка_факт_спот),
                          Выручка_выполнение_спот = round(Выручка_факт_спот / Выручка_план_спот * 100, digits = 2),
                          Маржа_план_спот = as.numeric(Маржа_план_спот),
                          Маржа_факт_спот = as.numeric(Маржа_факт_спот),
                          Маржа_выполнение_спот = round(Маржа_факт_спот / Маржа_план_спот * 100, digits = 2),
                          Рентабельность_план_спот = as.numeric(Рентабельность_план_спот),
                          Рентабельность_факт_спот = as.numeric(Рентабельность_факт_спот),
                          Рентабельность_выполнение_спот = round(Рентабельность_факт_спот / Рентабельность_план_спот * 100, digits = 2),
                          Рейсы_план_спот = as.numeric(Рейсы_план_спот),
                          Рейсы_факт_спот = as.numeric(Рейсы_факт_спот),
                          Рейсы_выполнение_спот = round(Рейсы_факт_спот / Рейсы_план_спот * 100, digits = 2),
                          Стоимость_рейса_план_спот = as.numeric(Стоимость_рейса_план_спот),
                          Стоимость_рейса_факт_спот = as.numeric(Стоимость_рейса_факт_спот),
                          Стоимость_рейса_выполнение_спот = round(Стоимость_рейса_факт_спот / Стоимость_рейса_план_спот * 100, digits = 2),
                          
                          Выручка_план_общий = as.numeric(Выручка_план_общий),
                          Выручка_факт_общий = Выручка_факт_гарантия + Выручка_факт_спот,
                          Выручка_выполнение_общий = round(Выручка_факт_общий / Выручка_план_общий * 100, digits = 2),
                          Маржа_план_общий = as.numeric(Маржа_план_общий),
                          Маржа_факт_общий = Маржа_факт_гарантия + Маржа_факт_спот,
                          Маржа_выполнение_общий = round(Маржа_факт_общий / Маржа_план_общий * 100, digits = 2),
                          Рентабельность_план_общий = as.numeric(Рентабельность_план_общий),
                          Рентабельность_факт_общий = round(Маржа_факт_общий / Выручка_факт_общий * 100, digits = 2),
                          Рентабельность_выполнение_общий = round(Рентабельность_факт_общий / Рентабельность_план_общий * 100, digits = 2),
                          Рейсы_план_общий = as.numeric(Рейсы_план_общий),
                          Рейсы_факт_общий = Рейсы_факт_гарантия + Рейсы_факт_спот,
                          Рейсы_выполнение_общий = round(Рейсы_факт_общий / Рейсы_план_общий * 100, digits = 2),
                          Стоимость_рейса_план_общий = as.numeric(Стоимость_рейса_план_общий),
                          Стоимость_рейса_факт_общий = round(Выручка_факт_общий / Рейсы_факт_общий, digits = 2),
                          Стоимость_рейса_выполнение_общий = round(Стоимость_рейса_факт_общий / Стоимость_рейса_план_общий * 100, digits = 2)
                          
        ) %>%
        arrange(Менеджер, Клиент)
      
      result_plan_fact[is.na(result_plan_fact)] <- 0
      
      result_plan_fact1 <- result %>%
        group_by(Менеджер) %>%
        summarise(
          Выручка_план_гарантия = sum(as.numeric(Выручка_план_гарантия)),
          Выручка_факт_гарантия = sum(as.numeric(Выручка_факт_гарантия)),
          Выручка_выполнение_гарантия = round(Выручка_факт_гарантия / Выручка_план_гарантия * 100, digits = 2),
          Маржа_план_гарантия = sum(as.numeric(Маржа_план_гарантия)),
          Маржа_факт_гарантия = sum(as.numeric(Маржа_факт_гарантия)),
          Маржа_выполнение_гарантия = round(Маржа_факт_гарантия / Маржа_план_гарантия * 100, digits = 2),
          Рентабельность_план_гарантия = round(Маржа_план_гарантия / Выручка_план_гарантия * 100, digits = 2),
          Рентабельность_факт_гарантия = round(Маржа_факт_гарантия / Выручка_факт_гарантия * 100, digits = 2),
          Рентабельность_выполнение_гарантия = round(Рентабельность_факт_гарантия / Рентабельность_план_гарантия * 100, digits = 2),
          Рейсы_план_гарантия = sum(as.numeric(Рейсы_план_гарантия)),
          Рейсы_факт_гарантия = sum(as.numeric(Рейсы_факт_гарантия)),
          Рейсы_выполнение_гарантия = round(Рейсы_факт_гарантия / Рейсы_план_гарантия * 100, digits = 2),
          Стоимость_рейса_план_гарантия = round(Выручка_план_гарантия / Рейсы_план_гарантия, digits = 2),
          Стоимость_рейса_факт_гарантия = round(Выручка_факт_гарантия / Рейсы_факт_гарантия, digits = 2),
          Стоимость_рейса_выполнение_гарантия = round(Стоимость_рейса_факт_гарантия / Стоимость_рейса_план_гарантия * 100, digits = 2),
          
          Выручка_план_спот = sum(as.numeric(Выручка_план_спот)),
          Выручка_факт_спот = sum(as.numeric(Выручка_факт_спот)),
          Выручка_выполнение_спот = round(Выручка_факт_спот / Выручка_план_спот * 100, digits = 2),
          Маржа_план_спот = sum(as.numeric(Маржа_план_спот)),
          Маржа_факт_спот = sum(as.numeric(Маржа_факт_спот)),
          Маржа_выполнение_спот = round(Маржа_факт_спот / Маржа_план_спот * 100, digits = 2),
          Рентабельность_план_спот = round(Маржа_план_спот / Выручка_план_спот * 100, digits = 2),
          Рентабельность_факт_спот = round(Маржа_факт_спот / Выручка_факт_спот * 100, digits = 2),
          Рентабельность_выполнение_спот = round(Рентабельность_факт_спот / Рентабельность_план_спот * 100, digits = 2),
          Рейсы_план_спот = sum(as.numeric(Рейсы_план_спот)),
          Рейсы_факт_спот = sum(as.numeric(Рейсы_факт_спот)),
          Рейсы_выполнение_спот = round(Рейсы_факт_спот / Рейсы_план_спот * 100, digits = 2),
          Стоимость_рейса_план_спот = round(Выручка_план_спот / Рейсы_план_спот, digits = 2),
          Стоимость_рейса_факт_спот = round(Выручка_факт_спот / Рейсы_факт_спот, digits = 2),
          Стоимость_рейса_выполнение_спот = round(Стоимость_рейса_факт_спот / Стоимость_рейса_план_спот * 100, digits = 2),
          
          Выручка_план_общий = sum(as.numeric(Выручка_план_общий)),
          Выручка_факт_общий = Выручка_факт_гарантия + Выручка_факт_спот,
          Выручка_выполнение_общий = round(Выручка_факт_общий / Выручка_план_общий * 100, digits = 2),
          Маржа_план_общий = sum(as.numeric(Маржа_план_общий)),
          Маржа_факт_общий = Маржа_факт_гарантия + Маржа_факт_спот,
          Маржа_выполнение_общий = round(Маржа_факт_общий / Маржа_план_общий * 100, digits = 2),
          Рентабельность_план_общий = round(Маржа_план_общий / Выручка_план_общий * 100, digits = 2),
          Рентабельность_факт_общий = round(Маржа_факт_общий / Выручка_факт_общий * 100, digits = 2),
          Рентабельность_выполнение_общий = round(Рентабельность_факт_общий / Рентабельность_план_общий * 100, digits = 2),
          Рейсы_план_общий = sum(as.numeric(Рейсы_план_общий)),
          Рейсы_факт_общий = Рейсы_факт_гарантия + Рейсы_факт_спот,
          Рейсы_выполнение_общий = round(Рейсы_факт_общий / Рейсы_план_общий * 100, digits = 2),
          Стоимость_рейса_план_общий = round(Выручка_план_общий / Рейсы_план_общий, digits = 2),
          Стоимость_рейса_факт_общий = round(Выручка_факт_общий / Рейсы_факт_общий, digits = 2),
          Стоимость_рейса_выполнение_общий = round(Стоимость_рейса_факт_общий / Стоимость_рейса_план_общий * 100, digits = 2)
          
        ) %>%
        
        arrange(Менеджер)
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "По менеджерам и клиентам")
      addWorksheet(wb, "По менеджерам")
      addWorksheet(wb, "Факт по менеджерам и клиентам")
      addWorksheet(wb, "Факт по дням")
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateManagerPlanFact[1], "%d.%m.%Y"), "-", format(input$dateManagerPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = result_plan_fact %>% rename("Выручка план., гаррантия" = 3,
                                                "Выручка факт., гарантия" = 4,
                                                "Выручка выполнение, гарантия" = 5,
                                                
                                                "Маржа план., гаррантия" = 6,
                                                "Маржа факт., гарантия" = 7,
                                                "Маржа выполнение, гарантия" = 8,
                                                
                                                "Рентабельность план., гаррантия" = 9,
                                                "Рентабельность факт., гарантия" = 10,
                                                "Рентабельность выполнение, гарантия" = 11,
                                                
                                                "Рейсы план., гаррантия" = 12,
                                                "Рейсы факт., гарантия" = 13,
                                                "Рейсы выполнение, гарантия" = 14,
                                                
                                                "Стоимость рейса план., гаррантия" = 15,
                                                "Стоимость рейса факт., гарантия" = 16,
                                                "Стоимость рейса выполнение, гарантия" = 17,
                                                
                                                
                                                "Выручка план., спот" = 18,
                                                "Выручка факт., спот" = 19,
                                                "Выручка выполнение, спот" = 20,
                                                
                                                "Маржа план., спот" = 21,
                                                "Маржа факт., спот" = 22,
                                                "Маржа выполнение, спот" = 23,
                                                
                                                "Рентабельность план., спот" = 24,
                                                "Рентабельность факт., спот" = 25,
                                                "Рентабельность выполнение, спот" = 26,
                                                
                                                "Рейсы план., спот" = 27,
                                                "Рейсы факт., спот" = 28,
                                                "Рейсы выполнение, спот" = 29,
                                                
                                                
                                                "Стоимость рейса план., спот" = 30,
                                                "Стоимость рейса факт., спот" = 31,
                                                "Стоимость рейса выполнение, спот" = 32,
                                                
                                                
                                                "Выручка план., общий" = 33,
                                                "Выручка факт., общий" = 34,
                                                "Выручка выполнение, общий" = 35,
                                                
                                                "Маржа план., общий" = 36,
                                                "Маржа факт., общий" = 37,
                                                "Маржа выполнение, общий" = 38,
                                                
                                                "Рентабельность план., общий" = 39,
                                                "Рентабельность факт., общий" = 40,
                                                "Рентабельность выполнение, общий" = 41,
                                                
                                                "Рейсы план., общий" = 42,
                                                "Рейсы факт., общий" = 43,
                                                "Рейсы выполнение, общий" = 44,
                                                
                                                "Стоимость рейса план., общий" = 45,
                                                "Стоимость рейса факт., общий" = 46,
                                                "Стоимость рейса выполнение, общий" = 47
                ),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 2,
                x = paste("За период", format(input$dateManagerPlanFact[1], "%d.%m.%Y"), "-", format(input$dateManagerPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 2, 
                x = result_plan_fact1 %>% rename("Выручка план., гаррантия" = 2,
                                                 "Выручка факт., гарантия" = 3,
                                                 "Выручка выполнение, гарантия" = 4,
                                                 
                                                 "Маржа план., гаррантия" = 5,
                                                 "Маржа факт., гарантия" = 6,
                                                 "Маржа выполнение, гарантия" = 7,
                                                 
                                                 "Рентабельность план., гаррантия" = 8,
                                                 "Рентабельность факт., гарантия" = 9,
                                                 "Рентабельность выполнение, гарантия" = 10,
                                                 
                                                 "Рейсы план., гаррантия" = 11,
                                                 "Рейсы факт., гарантия" = 12,
                                                 "Рейсы выполнение, гарантия" = 13,
                                                 
                                                 "Стоимость рейса план., гаррантия" = 14,
                                                 "Стоимость рейса факт., гарантия" = 15,
                                                 "Стоимость рейса выполнение, гарантия" = 16,
                                                 
                                                 "Выручка план., спот" = 17,
                                                 "Выручка факт., спот" = 18,
                                                 "Выручка выполнение, спот" = 19,
                                                 
                                                 "Маржа план., спот" = 20,
                                                 "Маржа факт., спот" = 21,
                                                 "Маржа выполнение, спот" = 22,
                                                 
                                                 "Рентабельность план., спот" = 23,
                                                 "Рентабельность факт., спот" = 24,
                                                 "Рентабельность выполнение, спот" = 25,
                                                 
                                                 "Рейсы план., спот" = 26,
                                                 "Рейсы факт., спот" = 27,
                                                 "Рейсы выполнение, спот" = 28,
                                                 
                                                 
                                                 "Стоимость рейса план., спот" = 29,
                                                 "Стоимость рейса факт., спот" = 30,
                                                 "Стоимость рейса выполнение, спот" = 31,
                                                 
                                                 
                                                 "Выручка план., общий" = 32,
                                                 "Выручка факт., общий" = 33,
                                                 "Выручка выполнение, общий" = 34,
                                                 
                                                 "Маржа план., общий" = 35,
                                                 "Маржа факт., общий" = 36,
                                                 "Маржа выполнение, общий" = 37,
                                                 
                                                 "Рентабельность план., общий" = 38,
                                                 "Рентабельность факт., общий" = 39,
                                                 "Рентабельность выполнение, общий" = 40,
                                                 
                                                 "Рейсы план., общий" = 41,
                                                 "Рейсы факт., общий" = 42,
                                                 "Рейсы выполнение, общий" = 43,
                                                 
                                                 "Стоимость рейса план., общий" = 44,
                                                 "Стоимость рейса факт., общий" = 45,
                                                 "Стоимость рейса выполнение, общий" = 46
                ),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 3,
                x = paste("За период", format(input$dateManagerPlanFact[1], "%d.%m.%Y"), "-", format(input$dateManagerPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 3, 
                x = selectedFactOnUnloadedTable_manager1() %>% arrange(Менеджер, Клиент),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 4,
                x = paste("За период", format(input$dateManagerPlanFact[1], "%d.%m.%Y"), "-", format(input$dateManagerPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 4, 
                x = selected_fact_by_day(),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      groupColumns(wb, sheet = 1, cols = 3:32, hidden = FALSE)
      groupColumns(wb, sheet = 2, cols = 2:31, hidden = FALSE)
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 5, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 8, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 11, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 14, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 17, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 20, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 23, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 26, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 29, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 32, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 35, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 38, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 41, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 44, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 47, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 4, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 7, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 10, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 13, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 16, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 19, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 22, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 25, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 28, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 31, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 34, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 37, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 40, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 43, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 46, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      
      headerStyle <- createStyle(
        fontColour = "#000000", fgFill = "#87cefa",
        halign = "center", valign = "center", textDecoration = "bold",
        border = "TopBottomLeftRight",
        wrapText = TRUE
      )
      
      headerStyle1 <- createStyle(
        fontColour = "#000000", fgFill = "#ffffe0",
        halign = "center", valign = "center", textDecoration = "bold",
        border = "TopBottomLeftRight",
        wrapText = TRUE
      )
      
      headerStyle2 <- createStyle(
        fontColour = "#000000", fgFill = "#c8a2c8",
        halign = "center", valign = "center", textDecoration = "bold",
        border = "TopBottomLeftRight",
        wrapText = TRUE
      )
      
      addStyle(wb, sheet = 1, headerStyle, rows = 2, cols = 3:17, gridExpand = TRUE)
      addStyle(wb, sheet = 1, headerStyle1, rows = 2, cols = 18:32, gridExpand = TRUE)
      addStyle(wb, sheet = 1, headerStyle2, rows = 2, cols = 33:47, gridExpand = TRUE)
      
      addStyle(wb, sheet = 2, headerStyle, rows = 2, cols = 2:16, gridExpand = TRUE)
      addStyle(wb, sheet = 2, headerStyle1, rows = 2, cols = 17:31, gridExpand = TRUE)
      addStyle(wb, sheet = 2, headerStyle2, rows = 2, cols = 32:46, gridExpand = TRUE)
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(result_plan_fact), widths = c(20, 50, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 2, cols = 1:ncol(result_plan_fact1), widths = c(20, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 2, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 3, cols = 1:ncol(selectedFactOnUnloadedTable_manager1()), widths = c(20, 50, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 3, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 4, cols = 1:ncol(selected_fact_by_day()), widths = c(10, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 4, rows = 2, heights = 100)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  
  # План-факт по логистам
  
  getLogisticPlanFact <- function(){
    # The historical Excel plan contains real people and is not a demo fixture.
    req(!DEMO_MODE)
    plan <- read.xlsx(Sys.getenv("REPORTS_LEGACY_PLAN_2", "plan-2.local.xlsx"),
                      sheet = 2,
                      startRow = 8) %>%
      select(3, 5, 6, 9, 12, 15, 18) %>%
      rename(
             "Логист" = 1,
             "Клиент" = 2,
             "Выручка_план" = 3,
             "Маржа_план" = 4,
             "Рейсы_план" = 5,
             "Рентабельность_план" = 6,
             "Стоимость_рейса_план" = 7
      )  %>%
      filter(!is.na(Клиент)) %>%
      filter(!is.na(Рейсы_план)) %>%
      
      filter(!grepl("^(Группа|Вакант)", trimws(Логист)))
    
    fact <- logisticKPI$data %>%
      group_by(Логист, Клиент) %>%
      summarise(Оборот = sum(Оборот), 
                      Маржа = sum(Маржа),
                      Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                      Рейсы = sum(Рейсы),
                      Стоимость_рейса = round(Оборот / Рейсы, digits = 2))
    
    
    result <- plan %>%
      full_join(fact, by = c("Логист" = "Логист","Клиент" = "Клиент")) %>%
      
      left_join(expeditors_group, by = c("Логист" = "data.id"))
      
    
    result[is.na(result)] <- 0
    
    result_plan_fact_detail <- result %>%
      transmute(Логист = Логист,
                      Группа = data.name,
                      Клиент = Клиент,
                      Выручка_план = as.numeric(Выручка_план),
                      Выручка_факт = as.numeric(Оборот),
                      Выручка_выполнение = round(Выручка_факт / Выручка_план * 100, digits = 2),
                      Маржа_план = as.numeric(Маржа_план),
                      Маржа_факт = as.numeric(Маржа),
                      Маржа_выполнение = round(Маржа_факт / Маржа_план * 100, digits = 2),
                      Рейсы_план = as.numeric(Рейсы_план),
                      Рейсы_факт = as.numeric(Рейсы),
                      Рейсы_выполнение = round(Рейсы_факт / Рейсы_план * 100, digits = 2),
                      Рентабельность_план = as.numeric(Рентабельность_план),
                      Рентабельность_факт = as.numeric(Рентабельность),
                      Рентабельность_выполнение = round(Рентабельность_факт / Рентабельность_план * 100, digits = 2),
                      Стоимость_рейса_план = as.numeric(Стоимость_рейса_план),
                      Стоимость_рейса_факт = as.numeric(Стоимость_рейса),
                      Стоимость_рейса_выполнение = round(Стоимость_рейса_факт / Стоимость_рейса_план * 100, digits = 2)
                      
      ) %>% 
      arrange(Группа, Логист, Клиент)
    
    result_plan_fact_detail[mapply(is.infinite, result_plan_fact_detail)] <- 0
    result_plan_fact_detail[is.na(result_plan_fact_detail)] <- 0
    
    filtered <- result_plan_fact_detail
    if (!is.null(input$logistJobForLogisticProductivity)) {
      filtered <- filtered %>% filter(Должность %in% input$logistJobForLogisticProductivity)
    } 
    if (!is.null(input$logistGroupForLogisticProductivity)) {
      filtered <- filtered %>% filter(Группа %in% input$logistGroupForLogisticProductivity)
    } 
    
    if (!is.null(input$clientForLogisticProductivity)) {
      filtered <- filtered %>% filter(Клиент %in% input$clientForLogisticProductivity)
    } 
    
    
    return(filtered)
    
  }
  

  output$reactableLogisticPlanFact_detail <- renderReactable(reactable(getLogisticPlanFact(),
                                                                       defaultColGroup = colGroup(headerClass = "group-header"),
                                                                       rowClass = JS("function(rowInfo, state) {
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'Клиент') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.row.group !== nextRow.group) {
          return 'group-last'
        }
      }
    }"),
                                                                       showSortIcon = FALSE,
                                                                       borderless = TRUE,
                                                                       class = "standings-table",
                                                                       columnGroups = list(
                                                                         colGroup(name = "Оборот", columns = c('Выручка_план', 'Выручка_факт', 'Выручка_выполнение')),
                                                                         colGroup(name = "Маржа", columns = c('Маржа_план', 'Маржа_факт', 'Маржа_выполнение')),
                                                                         colGroup(name = "Рейсы", columns = c('Рейсы_план', 'Рейсы_факт', 'Рейсы_выполнение')),
                                                                         colGroup(name = "Рентабельность", columns = c('Рентабельность_план', 'Рентабельность_факт', 'Рентабельность_выполнение'))
                                                                       ),
                                                                       groupBy = c("Группа", "Логист"),
                                                                       defaultColDef = colDef(class = "cell", headerClass = "header"),
                                                                       columns = list(
                                                                         Выручка_план = colDef(style = JS("{ fontSize: '16', fontWeight: 'bold' }"), aggregate = "sum", name = "План", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                                                          Выручка_факт = colDef(style = JS("{ fontSize: '18', fontFamily: 'monospace' }"), aggregate = "sum", name = "Факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                                                          Выручка_выполнение = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт']
                                              totalUnits += row['Выручка_план']
                                            })
                                            
                                            var result = 0
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), name = "Выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                                                          
                                                                          Маржа_план = colDef(style = JS("{ fontSize: '16', fontWeight: 'bold' }"), aggregate = "sum", name = "План", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                                                          Маржа_факт = colDef(style = JS("{ fontSize: '18', fontFamily: 'monospace' }"), aggregate = "sum", name = "Факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                                                          Маржа_выполнение = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт']
                                              totalUnits += row['Маржа_план']
                                            })
                                            var result = 0
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), name = "Выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                                                          
                                                                          Рентабельность_план = colDef(style = JS("{ fontSize: '16', fontWeight: 'bold' }"), show = TRUE, aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_план']
                                              totalUnits += row['Выручка_план']
                                            })
                                            var result = 0
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), name = "План", format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
                                                                          Рентабельность_факт = colDef(show = TRUE, aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт']
                                              totalUnits += row['Выручка_факт']
                                            })
                                            var result = 0
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), name = "Факт", format = colFormat(separators = TRUE, suffix = " %", digits = 2)),
                                                                          Рентабельность_выполнение = colDef(show = TRUE, aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            
                                            var totalPrice1 = 0
                                            var totalUnits1 = 0
                                            
                                            rows.forEach(function(row) {
                                              totalPrice += row['Маржа_факт']
                                              totalUnits += row['Маржа_план']
                                              
                                              totalPrice1 += row['Выручка_факт']
                                              totalUnits1 += row['Выручка_план']
                                            })
                                            var result = 0
                                            if (totalUnits == 0 || totalPrice == 0) {
                                                result = 0
                                            } else {
                                                result = (totalPrice / totalUnits) / (totalPrice1 / totalUnits1) * 100
                                            }
                                            return result
                                          }"), name = "Выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                                                          
                                                                          Рейсы_план = colDef(style = JS("{ fontSize: '16', fontWeight: 'bold' }"), aggregate = "sum", name = "План", format = colFormat(separators = TRUE)),
                                                                          Рейсы_факт = colDef(style = JS("{ fontSize: '18', fontFamily: 'monospace' }"), aggregate = "sum", name = "Факт", format = colFormat(separators = TRUE)),
                                                                          Рейсы_выполнение = colDef(aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Рейсы_факт']
                                              totalUnits += row['Рейсы_план']
                                            })
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), name = "Выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0)),
                                                                          
                                                                          Стоимость_рейса_план = colDef(show = FALSE, aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_план']
                                              totalUnits += row['Рейсы_план']
                                            })
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits
                                            }
                                            return result
                                          }"), name = "Стоимость рейса, план", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                                                          Стоимость_рейса_факт = colDef(show = FALSE, aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Выручка_факт']
                                              totalUnits += row['Рейсы_факт']
                                            })
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits
                                            }
                                            return result
                                          }"), name = "Стоимость рейса, факт", format = colFormat(separators = TRUE, suffix = " \U20BD", digits = 0)),
                                                                          Стоимость_рейса_выполнение = colDef(show = FALSE, aggregate = JS("function(values, rows) {
                                            var totalPrice = 0
                                            var totalUnits = 0
                                            rows.forEach(function(row) {
                                              totalPrice += row['Стоимость_рейса_факт']
                                              totalUnits += row['Стоимость_рейса_план']
                                            })
                                            if (totalUnits == 0) {
                                                result = 0
                                            } else {
                                                result = totalPrice / totalUnits * 100
                                            }
                                            return result
                                          }"), name = "Стоимость рейса, выполнение", format = colFormat(separators = TRUE, suffix = " %", digits = 0))
                                                                          
                                                                       )
  ))
  
  output$downloadLogisticianPlanFact <- downloadHandler(
    filename = function() {
      paste("План-факт, логисты-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      file_plan <- input$planLogisticianInput
      ext <- tools::file_ext(file_plan$datapath)
      
      req(file_plan)
      shiny::validate(need(ext == "xlsx", "Пожалуйста, загрузите файл xlsx"))
      plan <- read.xlsx(file_plan$datapath,
                        sheet = 2,
                        startRow = 8) %>%
        select(4, 6, 7, 10, 13, 16, 19) %>%
        rename(
          "Логист" = 1,
          "Клиент" = 2,
          "Выручка_план" = 3,
          "Маржа_план" = 4,
          "Рейсы_план" = 5,
          "Рентабельность_план" = 6,
          "Стоимость_рейса_план" = 7
        )  %>%
        filter(!is.na(Клиент)) %>%
        
        filter(!grepl("^(Группа|Вакант)", trimws(Логист)))
      
      fact <- selectedFactOnUnloadedTable_logistician() %>%
        group_by(Логист, Клиент) %>%
        summarise(Оборот = sum(Ставка_клиента_НДС), 
                        Маржа = sum(Маржа_расчёт),
                        Рентабельность = round(Маржа / Оборот * 100, digits = 2), 
                        Рейсы = sum(Рейсы),
                        Стоимость_рейса = round(Оборот / Рейсы, digits = 2))
      
      
      result <- plan %>%
        full_join(fact, by = c("Логист" = "Логист","Клиент" = "Клиент")) %>%
        
        left_join(expeditors_group, by = c("Логист" = "data.id"))
      
      
      result[is.na(result)] <- 0
      
      result_plan_fact <- result %>%
        transmute(Логист = Логист,
                        Группа = data.name,
                        Клиент = Клиент,
                        Выручка_план = as.numeric(Выручка_план),
                        Выручка_факт = as.numeric(Оборот),
                        Выручка_выполнение = round(Выручка_факт / Выручка_план * 100, digits = 2),
                        Маржа_план = as.numeric(Маржа_план),
                        Маржа_факт = as.numeric(Маржа),
                        Маржа_выполнение = round(Маржа_факт / Маржа_план * 100, digits = 2),
                        Рентабельность_план = as.numeric(Рентабельность_план),
                        Рентабельность_факт = as.numeric(Рентабельность),
                        Рентабельность_выполнение = round(Рентабельность_факт / Рентабельность_план * 100, digits = 2),
                        Рейсы_план = as.numeric(Рейсы_план),
                        Рейсы_факт = as.numeric(Рейсы),
                        Рейсы_выполнение = round(Рейсы_факт / Рейсы_план * 100, digits = 2),
                        Стоимость_рейса_план = as.numeric(Стоимость_рейса_план),
                        Стоимость_рейса_факт = as.numeric(Стоимость_рейса),
                        Стоимость_рейса_выполнение = round(Стоимость_рейса_факт / Стоимость_рейса_план * 100, digits = 2)
                        
        ) %>% rename("Выручка план." = 4,
                     "Выручка факт." = 5,
                     "Выручка выполнение" = 6,
                     
                     "Маржа план." = 7,
                     "Маржа факт." = 8,
                     "Маржа выполнение" = 9,
                     
                     "Рентабельность план." = 10,
                     "Рентабельность факт." = 11,
                     "Рентабельность выполнение" = 12,
                     
                     "Рейсы план." = 13,
                     "Рейсы факт." = 14,
                     "Рейсы выполнение" = 15,
                     
                     "Стоимость рейса план." = 16,
                     "Стоимость рейса факт." = 17,
                     "Стоимость рейса выполнение" = 18
                     
        ) %>% 
        
        arrange(Группа, Логист, Клиент)
      
      
      result_plan_fact1 <- result %>%
        group_by(data.name) %>%
        summarise(
          Выручка_план = sum(as.numeric(Выручка_план)),
          Выручка_факт = sum(as.numeric(Оборот)),
          Выручка_выполнение = round(Выручка_факт / Выручка_план * 100, digits = 2),
          Маржа_план = sum(as.numeric(Маржа_план)),
          Маржа_факт = sum(as.numeric(Маржа)),
          Маржа_выполнение = round(Маржа_факт / Маржа_план * 100, digits = 2),
          Рентабельность_план = round(Маржа_план / Выручка_план * 100, digits = 2),
          Рентабельность_факт = round(Маржа_факт / Выручка_факт * 100, digits = 2),
          Рентабельность_выполнение = round(Рентабельность_факт / Рентабельность_план * 100, digits = 2),
          Рейсы_план = sum(as.numeric(Рейсы_план)),
          Рейсы_факт = sum(as.numeric(Рейсы)),
          Рейсы_выполнение = round(Рейсы_факт / Рейсы_план * 100, digits = 2),
          Стоимость_рейса_план = round(Выручка_план / Рейсы_план, digits = 2),
          Стоимость_рейса_факт = round(Выручка_факт / Рейсы_факт, digits = 2),
          Стоимость_рейса_выполнение = round(Стоимость_рейса_факт / Стоимость_рейса_план * 100, digits = 2)
          
        ) %>% rename("Выручка план." = 2,
                     "Выручка факт." = 3,
                     "Выручка выполнение" = 4,
                     
                     "Маржа план." = 5,
                     "Маржа факт." = 6,
                     "Маржа выполнение" = 7,
                     
                     "Рентабельность план." = 8,
                     "Рентабельность факт." = 9,
                     "Рентабельность выполнение" = 10,
                     
                     "Рейсы план." = 11,
                     "Рейсы факт." = 12,
                     "Рейсы выполнение" = 13,
                     
                     "Стоимость рейса план." = 14,
                     "Стоимость рейса факт." = 15,
                     "Стоимость рейса выполнение" = 16
                     
        ) %>% 
        
        arrange(data.name)
      
      
      result_plan_fact2 <- result %>%
        group_by(data.name, Клиент) %>%
        summarise(
          Выручка_план = sum(as.numeric(Выручка_план)),
          Выручка_факт = sum(as.numeric(Оборот)),
          Выручка_выполнение = round(Выручка_факт / Выручка_план * 100, digits = 2),
          Маржа_план = sum(as.numeric(Маржа_план)),
          Маржа_факт = sum(as.numeric(Маржа)),
          Маржа_выполнение = round(Маржа_факт / Маржа_план * 100, digits = 2),
          Рентабельность_план = round(Маржа_план / Выручка_план * 100, digits = 2),
          Рентабельность_факт = round(Маржа_факт / Выручка_факт * 100, digits = 2),
          Рентабельность_выполнение = round(Рентабельность_факт / Рентабельность_план * 100, digits = 2),
          Рейсы_план = sum(as.numeric(Рейсы_план)),
          Рейсы_факт = sum(as.numeric(Рейсы)),
          Рейсы_выполнение = round(Рейсы_факт / Рейсы_план * 100, digits = 2),
          Стоимость_рейса_план = round(Выручка_план / Рейсы_план, digits = 2),
          Стоимость_рейса_факт = round(Выручка_факт / Рейсы_факт, digits = 2),
          Стоимость_рейса_выполнение = round(Стоимость_рейса_факт / Стоимость_рейса_план * 100, digits = 2)
          
        ) %>% rename("Выручка план." = 3,
                     "Выручка факт." = 4,
                     "Выручка выполнение" = 5,
                     
                     "Маржа план." = 6,
                     "Маржа факт." = 7,
                     "Маржа выполнение" = 8,
                     
                     "Рентабельность план." = 9,
                     "Рентабельность факт." = 10,
                     "Рентабельность выполнение" = 11,
                     
                     "Рейсы план." = 12,
                     "Рейсы факт." = 13,
                     "Рейсы выполнение" = 14,
                     
                     "Стоимость рейса план." = 15,
                     "Стоимость рейса факт." = 16,
                     "Стоимость рейса выполнение" = 17
                     
        ) %>% 
        
        arrange(data.name, Клиент)
      
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "По логистам")
      addWorksheet(wb, "По группам логистов и клиентам")
      addWorksheet(wb, "По группам логистов")
      addWorksheet(wb, "Факт по логистам")
      
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateLogisticianPlanFact[1], "%d.%m.%Y"), "-", format(input$dateLogisticianPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = result_plan_fact,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      
      writeData(wb,
                sheet = 2,
                x = paste("За период", format(input$dateLogisticianPlanFact[1], "%d.%m.%Y"), "-", format(input$dateLogisticianPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 2, 
                x = result_plan_fact2,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 3,
                x = paste("За период", format(input$dateLogisticianPlanFact[1], "%d.%m.%Y"), "-", format(input$dateLogisticianPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 3, 
                x = result_plan_fact1,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 4,
                x = paste("За период", format(input$dateLogisticianPlanFact[1], "%d.%m.%Y"), "-", format(input$dateLogisticianPlanFact[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 4, 
                x = fact %>% arrange(data.name, Клиент) %>% rename("Стоимость рейса" = 8),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 6, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 9, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 12, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 15, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 18, rows = 2:(nrow(result_plan_fact)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 5, rows = 2:(nrow(result_plan_fact2)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 8, rows = 2:(nrow(result_plan_fact2)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 11, rows = 2:(nrow(result_plan_fact2)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 14, rows = 2:(nrow(result_plan_fact2)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 17, rows = 2:(nrow(result_plan_fact2)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      
      conditionalFormatting(wb, sheet = 3,
                            cols = 4, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 3,
                            cols = 7, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 3,
                            cols = 10, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 3,
                            cols = 13, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 3,
                            cols = 16, rows = 2:(nrow(result_plan_fact1)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(result_plan_fact), widths = c(20, 30, 50, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 2, cols = 1:ncol(result_plan_fact2), widths = c(30, 50, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 2, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 3, cols = 1:ncol(result_plan_fact1), widths = c(30, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 3, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 4, cols = 1:ncol(fact), widths = c(20, 50, 30, 10, 10, 10, 10, 10))
      setRowHeights(wb, sheet = 4, rows = 2, heights = 100)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  
  output$downloadFactoringBills <- downloadHandler(
    filename = function() {
      paste("Выставленные счета + факторинг - ", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      file_factoring <- input$factoringInput
      ext <- tools::file_ext(file_factoring$datapath)
      
      req(file_factoring)
      shiny::validate(need(ext == "xlsx", "Пожалуйста, загрузите файл xlsx"))
      factoringBills <- read.xlsx(file_factoring$datapath,
                        sheet = 1,
                        startRow = 2) %>%
        select(Номер, статус)
      
      result <- selected_bills_for_factoring() %>%
        left_join(factoringBills, by = c("Номер счёта" = "Номер"))


      wb <- createWorkbook()
      
      addWorksheet(wb, "Выставленные счета")
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateFactoring[1], "%d.%m.%Y"), "-", format(input$dateFactoring[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = result,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(result), widths = c(40, 12, 10, 10, 10, 10, 10, 10, 20, 20, 10, 10, 50, 5))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 60)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  
  ##########################
  ### Выработка логистов ###
  ##########################
  
  output$downloadLogisticProductivity <- downloadHandler(
    filename = function() {
      paste("Выработка_логисты-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Выработка, все статусы")
      addWorksheet(wb, "Выработка, перевозчик найден")
      
      if(input$aggregation == "День") {
        result_LogisticProductivity = selected_LogisticProductivity_everyday() %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
        result_LogisticProductivity_carrierFound = selected_LogisticProductivity_everyday_carrierFound() %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all_carrierFound(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
      } else {
        result_LogisticProductivity = selected_LogisticProductivity_everyday() %>%
          select(-Заявки, -Рабочих_дней) %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
        result_LogisticProductivity_carrierFound = selected_LogisticProductivity_everyday_carrierFound() %>%
          select(-Заявки, -Рабочих_дней) %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all_carrierFound(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
      }
      colnames(result_LogisticProductivity)[which(names(result_LogisticProductivity) == "Заявки")] <- "Заявки, всего"
      colnames(result_LogisticProductivity_carrierFound)[which(names(result_LogisticProductivity_carrierFound) == "Заявки")] <- "Заявки, всего"
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateLogisticProductivity[1], "%d.%m.%Y"), "-", format(input$dateLogisticProductivity[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = result_LogisticProductivity,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateLogisticProductivity[1], "%d.%m.%Y"), "-", format(input$dateLogisticProductivity[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 2, 
                x = result_LogisticProductivity_carrierFound,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      setColWidths(wb, sheet = 1, cols = 1:2, widths = c(30, 30))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 2, cols = 1:2, widths = c(30, 30))
      setRowHeights(wb, sheet = 2, rows = 2, heights = 100)
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 1:(ncol(result_LogisticProductivity)-1), rows = 2:(nrow(result_LogisticProductivity)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 1:(ncol(result_LogisticProductivity_carrierFound)-1), rows = 2:(nrow(result_LogisticProductivity_carrierFound)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  ##########################
  ### Выработка логистов в отдельной вкладке###
  ##########################
  
  output$downloadLogisticProductivity_1 <- downloadHandler(
    filename = function() {
      paste("Выработка_логисты-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Выработка, все статусы")
      addWorksheet(wb, "Выработка, перевозчик найден")
      
      if(input$aggregation_1 == "День") {
        result_LogisticProductivity = selected_LogisticProductivity_everyday_1() %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all_1(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
        result_LogisticProductivity_carrierFound = selected_LogisticProductivity_everyday_carrierFound_1() %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all_carrierFound_1(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
      } else {
        result_LogisticProductivity = selected_LogisticProductivity_everyday_1() %>%
          select(-Заявки, -Рабочих_дней) %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all_1(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
        result_LogisticProductivity_carrierFound = selected_LogisticProductivity_everyday_carrierFound_1() %>%
          select(-Заявки, -Рабочих_дней) %>%
          pivot_wider(names_from = Дата, values_from = Выработка, values_fill = 0) %>% 
          left_join(selected_LogisticProductivity_all_carrierFound_1(), by = c("Логист" = "Логист", "Группа" = "Группа")) %>%
          arrange(Группа, Логист)
        
      }
      colnames(result_LogisticProductivity)[which(names(result_LogisticProductivity) == "Заявки")] <- "Заявки, всего"
      colnames(result_LogisticProductivity_carrierFound)[which(names(result_LogisticProductivity_carrierFound) == "Заявки")] <- "Заявки, всего"
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateLogisticProductivity_1[1], "%d.%m.%Y"), "-", format(input$dateLogisticProductivity_1[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = result_LogisticProductivity,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 2,
                x = paste("За период", format(input$dateLogisticProductivity_1[1], "%d.%m.%Y"), "-", format(input$dateLogisticProductivity_1[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 2, 
                x = result_LogisticProductivity_carrierFound,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      setColWidths(wb, sheet = 1, cols = 1:2, widths = c(30, 30))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
      
      setColWidths(wb, sheet = 2, cols = 1:2, widths = c(30, 30))
      setRowHeights(wb, sheet = 2, rows = 2, heights = 100)
      
      conditionalFormatting(wb, sheet = 1,
                            cols = 1:(ncol(result_LogisticProductivity)-1), rows = 2:(nrow(result_LogisticProductivity)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 1:(ncol(result_LogisticProductivity_carrierFound)-1), rows = 2:(nrow(result_LogisticProductivity_carrierFound)+2),
                            style = c("red", "yellow", "green"),
                            type = "colourScale"
      )
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  output$downloadDO <- downloadHandler(
    filename = function() {
      paste("creditors_debtors-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Кредиторы")
      addWorksheet(wb, "Кредиторы по дням")
      addWorksheet(wb, "Дебиторы")
      addWorksheet(wb, "Проведённые акты")
      
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$date[1], "%d.%m.%Y"), "-", format(input$date[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = selected_data(),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      result_do = selected_data1() %>%
        group_by(Дата, Специалист) %>%
        summarise(Всего = sum(Всего)) %>%
        pivot_wider(names_from = Дата, values_from = Всего, values_fill = 0) %>% 
        left_join(selected_data() %>% select(Специалист, Всего), by = c("Специалист" = "Специалист")) %>%
        arrange(desc(Всего))
      
      writeData(wb, 
                sheet = 2, 
                x = result_do,
                startCol = 1,
                startRow = 1,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 3,
                x = paste("За период", format(input$date[1], "%d.%m.%Y"), "-", format(input$date[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 3, 
                x = selected_bills() %>% rename("Номер счёта" = 2,
                                                    "Сумма счёта" = 3,
                                                    "Дата счёта" = 4,
                                                    "Дата выставления счёта" = 5,
                                                    "Количество заявок" = 7,
                                                    "Номера заявок" = 8,
                                                    "Ответственный сотрудник ДО за клиента (дебитор)" = 6),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 4,
                x = paste("За период", format(input$date[1], "%d.%m.%Y"), "-", format(input$date[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 4, 
                x = selected_acts() %>% arrange(Дата_проведения_акта_перевозчика) %>% 
                  rename("Номер акта перевозчика" = 1, "Номера заявок" = 2, "Дата акта" = 3, "Дата проведения акта" = 4, "Специалист ДО" = 5),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      # bodyStyle <- createStyle(wrapText = TRUE, borderStyle = "dashed")
      # addStyle(wb, sheet = 1, bodyStyle, rows = 3:(nrow(selected_data())+2), cols = 3, gridExpand = TRUE)
      
      # addStyle(wb, sheet = 1, bodyStyle, rows = 3:(nrow(selected_data())+2), cols = 5, gridExpand = TRUE)
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(selected_data()), widths = c(30, 20, 50, 20, 50, 20))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 2, cols = 1, widths = 50)
      
      negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
      posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 2:(ncol(result_do)-1), rows = 2:(nrow(result_do) + 1),
                            rule = "<35", style = negStyle
                            
      )
      
      conditionalFormatting(wb, sheet = 2,
                            cols = 2:(ncol(result_do)-1), rows = 2:(nrow(result_do) + 1),
                            rule = ">=35", style = posStyle
                            
      )
      
      setColWidths(wb, sheet = 3, cols = 1:ncol(selected_bills()), widths = c(50, 20, 20, 20, 20, 20, 20, 50))
      setRowHeights(wb, sheet = 3, rows = 2, heights = 60)
      
      setColWidths(wb, sheet = 4, cols = 1:ncol(selected_acts()), widths = c(20, 20, 10, 11, 30))
      setRowHeights(wb, sheet = 4, rows = 2, heights = 60)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      
    }
  )
  
  output$downloadDZ <- downloadHandler(
    filename = function() {
      paste("ДЗ-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Неоплаченные счета")

      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$date1[1], "%d.%m.%Y"), "-", format(input$date1[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = selected_dz_specialist_1(),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      # writeData(wb,
      #           sheet = 2,
      #           x = getDZ_new_flow(),
      #           startCol = 1,
      #           startRow = 2,
      #           colNames = TRUE,
      #           rowNames = FALSE,
      #           borders = "rows",
      #           borderColour = "black",
      #           headerStyle = hs2, borderStyle = "dashed")
      


      setColWidths(wb, sheet = 1, cols = 1:ncol(selected_dz_specialist_1()), widths = c(40, 20, 15, 10, 10, 20, 10, 10, 10, 10, 10, 10, 10, 20, 20))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 60)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      
    }
  )
  
  
  selectedByDate_specialDC <- reactive({
    selectedDateType <- switch(input$dateType_specialDC, 
                               "По загрузке" = requestsTable_specialDC$data$Дата_загрузки_план,
                               "По выгрузке" = requestsTable_specialDC$data$Дата_выгрузки_план)
    
    
    requestsTable_specialDC$data <- requestsTable_specialDC$data %>%
      filter(selectedDateType >= input$date_specialDC[1] & selectedDateType <= input$date_specialDC[2])
    
  })
  
  
  selectedByClientRequests_specialDC <- reactive({
    if (input$client_specialDC != "Все") {
      selectedByDate_specialDC() %>% 
        filter(Клиент == input$client_specialDC)
    } else {
      selectedByDate_specialDC()
    }
  })
  
  
  selected_requests_specialDC <- reactive({
    if (input$logist_specialDC != "Все") {
      selectedByClientRequests_specialDC() %>% 
        filter(Ответственный_логист == input$logist_specialDC)
    } else {
      selectedByClientRequests_specialDC()
    }
  })
  
  output$tablespecialDC <- renderReactable(
    reactable(selected_requests_specialDC() %>% select(1, 4, 5, 19, 20, 21, 7, 10, 13, 17),
              pagination = FALSE,
              columns = list(
                Номер_заявки = colDef(name = "Номер заявки"),
                Ответственный_логист = colDef(name = "Логист"),
                Номер_телефона = colDef(name = "Номер телефона"),
                Дата_загрузки_план = colDef(name = "Дата начала рейса", format = colFormat(date = TRUE)),
                Дата_выгрузки_план = colDef(name = "Дата завершения рейса", format = colFormat(date = TRUE))
              )
    )
  )
  
  
  # Скачать отчёт со списком рейсов с загрузкой/выгрузкой на Спец.РЦ
  output$downloadRequests_specialDC <- downloadHandler(
    filename = function() {
      paste("Рейсы_спец_РЦ-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Формирование excel-файла", value = 0.2)
      
      # startDate <- as.POSIXct(input$date_specialDC[1], format="%Y-%m-%d")
      # endDate <- as.POSIXct(input$date_specialDC[2], format="%Y-%m-%d") + 24*60*60 - 1
      
      progress$inc(0.4, detail = "Получение данных")
      # requestsTable_specialDC$data <- switch(input$dateType_specialDC, 
      #                              "По загрузке" = getSpecialDistributionCenters(startDate, endDate),
      #                              "По выгрузке" = getSpecialDistributionCenters1(startDate, endDate))
      
      progress$inc(0.6, detail = "Данные получены")
      
      progress$inc(0.8, detail = "Начало записи файла")
      
      start.time <- Sys.time()
      print("Beginning of forming an excel file")
      wb <- createWorkbook()
      
      addWorksheet(wb, "Рейсы")
      print("Created sheet in excel file")
      
      print("Start data recording")
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$date_specialDC[1], "%d.%m.%Y"), "-", format(input$date_specialDC[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = selected_requests_specialDC() %>% rename("Номер заявки" = 1,
                                                   "Внешний номер заявки" = 2,
                                                   "Дата создания заявки" = 3,
                                                   "Клиент" = 4,
                                                   "Ответственный логист" = 5,
                                                   "Грузополучатель" = 6,
                                                   "Дата загрузки, план" = 7,
                                                   "Время загрузки, план" = 8,
                                                   "Дата загрузки, факт" = 9,
                                                   "Дата выгрузки, план" = 10,
                                                   "Время выгрузки, план" = 11,
                                                   "Дата выгрузки, факт" = 12,
                                                   "Маршрут" = 13,
                                                   "Адрес загрузки, Спец РЦ" = 14,
                                                   "Адрес выгрузки, Спец РЦ" = 15,
                                                   "Менеджер" = 16,
                                                   "Особенность" = 17,
                                                   "Старший группы" = 18,
                                                   "Перевозчик" = 19,
                                                   "Водитель" = 20,
                                                   "Номер телефона" = 21,
                                                   "Номер тягача" = 22,
                                                   "Номер полуприцепа" = 23),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      print("End of data recording")
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(selected_requests_specialDC()), widths = c(10, 15, 10, 20, 20, 20, 10, 10, 20, 10, 10, 20, 100, 50, 50, 20, 30, 30, 30, 30, 15, 10, 10))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      progress$inc(1, detail = "Файл сформирован")
      
      print("File saved")
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      print(paste("Time taken to save the file:", round(time.taken, digits = 2), "sec"))
      
    }
  )
  
  # Скачать классический отчёт по рейсам
  output$downloadRequests <- downloadHandler(
    filename = function() {
      paste("Рейсы-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Формирование excel-файла", value = 0.2)
      
      startDate <- as.POSIXct(paste(input$date_requests[1], "00:00:00"), format="%Y-%m-%d %H:%M:%S")
      endDate <- as.POSIXct(paste(input$date_requests[2], "23:59:59"), format="%Y-%m-%d %H:%M:%S") 
      
      attr(startDate, "tzone") <- "UTC" 
      attr(endDate, "tzone") <- "UTC" 
      
      progress$inc(0.4, detail = "Получение данных")

      requestsTable$data <- switch(input$dateType, 
                                   "По созданию заявки" = getRequests_slave(startDate, endDate, "requests.created_at"),
                                   "По созданию перевозки" = getRequests_slave(startDate, endDate, "transportations.created_at"),
                                   "По загрузке" = getRequests_slave(startDate, endDate, "first_loading.from_at"),
                                   "По выгрузке" = getRequests_slave(startDate, endDate, "last_unloading.from_at"))
      progress$inc(0.6, detail = "Данные получены")
      
      progress$inc(0.8, detail = "Начало записи файла")
      
      write_xlsx(selected_requests(), file)
      
      requestsTable$data <- NULL

      progress$inc(1, detail = "Файл сформирован")
      
      
    }
  )
  
  observeEvent(input$allDataCheckbox, {
    if (input$allDataCheckbox){
      shinyjs::disable("date_canceledOrders")

    }
    else{
      shinyjs::enable("date_canceledOrders")
    }
    
  })
  
  # Скачивание отчёта «Отменённые рейсы» в формате excel
  
  observeEvent(input$allDataCheckbox, {
    if (input$allDataCheckbox){
      selected_data <- breakdownsTable$data %>% 
        arrange(desc(Дата_создания_заявки)) %>%
        rename("Номер заявки" = 1,
               "Внешний номер заявки" = 2,
               "Дата создания заявки" = 3,
               "Клиент" = 4,
               "Тип заявки" = 5,
               "Статус заявки" = 6,
               "Статус обработки" = 7,
               "Наличие заявки клиента (прикреплена)" = 8,
               "Наличие подписанного договора-заявки с перевозчиком (прикреплен)" = 9,
               "Наличие подписанной заявки с перевозчиком (прикреплен)" = 10,
               "Место или город погрузки" = 11,
               "Место или город выгрузки" = 12,
               "Тип ТС реф, термос, тент из заявки Клиента" = 13,
               "Тоннаж ТС из заявки Клиента" = 14,
               "Объем ТС из заявки Клиента" = 15,
               "Ответственный логист" = 16,
               "Старший группы" = 17,
               "Перевозчик" = 18,
               "ИНН перевозчика" = 19,
               "Юр. адрес перевозчика" = 20,
               "Ставка клиента" = 21,
               "Ставка перевозчика" = 22,
               "Сумма предоплаты перевозчику" = 23,
               "Сумма оч.маржи." = 24,
               "НДС / без НДС (по перевозчику)" = 25,
               "Очищ. Рент., %" = 26,
               "Проблема с ТСД, дата" = 27,
               "Наличие Торг-2" = 28,
               "Заметки" = 29,
               "Причина создания" = 30,
               "Связанная заявка" = 31)
      
      output$downloadCanceledOrders <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = "Формирование excel-файла", value = 0.4)
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Общий отчёт")
          addWorksheet(wb, "Отчёт по необработанным заявкам")
          addWorksheet(wb, "Отчёт по обработанным заявкам")
          
          not_flt = !stringr::str_detect(selected_data$Заметки, 'Обработано|обработано') # готовим фильтр для строк
          yes_flt = stringr::str_detect(selected_data$Заметки, 'Обработано|обработано') # готовим фильтр для строк
          
          writeData(wb,
                    sheet = 1,
                    x = "За весь период существования компании",
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = selected_data,
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          writeData(wb,
                    sheet = 2,
                    x = "За весь период существования компании",
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 2, 
                    x = selected_data %>% dplyr::filter(not_flt),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          writeData(wb,
                    sheet = 3,
                    x = "За весь период существования компании",
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 3, 
                    x = selected_data %>% dplyr::filter(yes_flt),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:ncol(selected_data), widths = c(10, 10, 10, 30, 10, 15, 15, 10, 10, 10, 10, 10, 10, 10, 10, 30, 30, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          setColWidths(wb, sheet = 2, cols = 1:ncol(selected_data), widths = c(10, 10, 10, 30, 10, 15, 15, 10, 10, 10, 10, 10, 10, 10, 10, 30, 30, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10))
          setRowHeights(wb, sheet = 2, rows = 2, heights = 100)
          
          setColWidths(wb, sheet = 3, cols = 1:ncol(selected_data), widths = c(10, 10, 10, 30, 10, 15, 15, 10, 10, 10, 10, 10, 10, 10, 10, 30, 30, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10))
          setRowHeights(wb, sheet = 3, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          
          progress$inc(1, detail = "Файл успешно сформирован")
          
        }
      )
      
    }
    else{
      
      output$downloadCanceledOrders <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = "Формирование excel-файла", value = 0.4)
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Общий отчёт")
          addWorksheet(wb, "Отчёт по необработанным заявкам")
          addWorksheet(wb, "Отчёт по обработанным заявкам")
          
          not_flt = !stringr::str_detect(selected_canceledOrders()$Заметки, 'Обработано|обработано') # готовим фильтр для строк
          yes_flt = stringr::str_detect(selected_canceledOrders()$Заметки, 'Обработано|обработано') # готовим фильтр для строк
          
          writeData(wb,
                    sheet = 1,
                    x = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = selected_canceledOrders(),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          writeData(wb,
                    sheet = 2,
                    x = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 2, 
                    x = selected_canceledOrders() %>% dplyr::filter(not_flt),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          writeData(wb,
                    sheet = 3,
                    x = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 3, 
                    x = selected_canceledOrders() %>% dplyr::filter(yes_flt),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:ncol(selected_canceledOrders()), widths = c(10, 10, 10, 30, 10, 15, 15, 10, 10, 10, 10, 10, 10, 10, 10, 30, 30, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          setColWidths(wb, sheet = 2, cols = 1:ncol(selected_canceledOrders()), widths = c(10, 10, 10, 30, 10, 15, 15, 10, 10, 10, 10, 10, 10, 10, 10, 30, 30, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10))
          setRowHeights(wb, sheet = 2, rows = 2, heights = 100)
          
          setColWidths(wb, sheet = 3, cols = 1:ncol(selected_canceledOrders()), widths = c(10, 10, 10, 30, 10, 15, 15, 10, 10, 10, 10, 10, 10, 10, 10, 30, 30, 30, 10, 30, 10, 10, 10, 10, 10, 10, 10, 10, 10, 30, 10, 10))
          setRowHeights(wb, sheet = 3, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          
          progress$inc(1, detail = "Файл успешно сформирован")
          
          
        }
      )
      
    }
    
  })
  
  
  observeEvent(input$allDataCheckbox, {
    if (input$allDataCheckbox){

      output$downloadCanceledOrdersCommerce <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы_коммерция-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Коммерция")
          
          writeData(wb,
                    sheet = 1,
                    x = "За весь период существования компании",
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = breakdownsTable$data %>%
                      select(Номер_заявки, Внешний_номер_заявки, Дата_создания_заявки, Клиент, Ответственный_логист, Старший_группы, Тип_заявки, Статус_заявки, Перевозчик) %>%
                      arrange(desc(Дата_создания_заявки), Клиент) %>%
                      rename("Номер заявки" = 1,
                             "Внешний номер заявки" = 2,
                             "Дата создания заявки" = 3,
                             "Клиент" = 4,
                             "Ответственный логист" = 5,
                             "Старший группы" = 6,
                             "Тип заявки" = 7,
                             "Статус заявки" = 8,
                             "Перевозчик" = 9),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2_yellow, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:9, widths = c(10, 10, 10, 35, 35, 35, 10, 20, 35))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          # subject <- "Отчёт по сорванным рейсам, отдел продаж"
          # body <- paste("Сорванные рейсы", "за период", format(Sys.Date() - 6, "%d.%m.%Y"), "-", format(Sys.Date() - 1, "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
          # attachmentObject <- file
          # bodyWithAttachment <- list(body, mime_part(attachmentObject, name = paste("Отменённые_рейсы_коммерция-", Sys.Date(), ".xlsx", sep="")))
          # sendmail(from, 
          #          to, 
          #          subject, 
          #          bodyWithAttachment, 
          #          control=list(smtpServer= "smtp.example.com", smtpPort="25"))
          
        }
      )
      
    }
    else{

      output$downloadCanceledOrdersCommerce <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы_коммерция-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Коммерция")
          
          writeData(wb,
                    sheet = 1,
                    x = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = selected_canceledOrdersCommerce(),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2_yellow, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:ncol(selected_canceledOrdersCommerce()), widths = c(10, 10, 10, 35, 35, 35, 10, 20, 35))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          # subject <- "Отчёт по сорванным рейсам, отдел продаж"
          # body <- paste("Сорванные рейсы", "за период", format(Sys.Date() - 6, "%d.%m.%Y"), "-", format(Sys.Date() - 1, "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
          # attachmentObject <- file
          # bodyWithAttachment <- list(body, mime_part(attachmentObject, name = paste("Отменённые_рейсы_коммерция-", Sys.Date(), ".xlsx", sep="")))
          # sendmail(from, 
          #          to, 
          #          subject, 
          #          bodyWithAttachment, 
          #          control=list(smtpServer= "smtp.example.com", smtpPort="25"))
          
        }
      )
    
    }
    
  })
  

  observeEvent(input$allDataCheckbox, {
    if (input$allDataCheckbox){

      output$downloadCanceledOrdersLogistics <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы_логистика-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Логистика")
          
          writeData(wb,
                    sheet = 1,
                    x = "За весь период существования компании",
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = breakdownsTable$data %>%
                      select(Номер_заявки, Внешний_номер_заявки, Дата_создания_заявки, Ответственный_логист, Клиент, Тип_заявки, Статус_заявки, Старший_группы, Перевозчик) %>%
                      arrange(desc(Дата_создания_заявки), Ответственный_логист) %>%
                      rename("Номер заявки" = 1,
                             "Внешний номер заявки" = 2,
                             "Дата создания заявки" = 3,
                             "Ответственный логист" = 4,
                             "Клиент" = 5,
                             "Тип заявки" = 6,
                             "Статус заявки" = 7,
                             "Старший группы" = 8,
                             "Перевозчик" = 9
                      ),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2_yellow, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:9, widths = c(10, 10, 10, 35, 35, 10, 20, 35, 35))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          # subject <- "Отчёт по сорванным рейсам, отдел логистики"
          # body <- paste("Сорванные рейсы", "за период", format(Sys.Date() - 6, "%d.%m.%Y"), "-", format(Sys.Date() - 1, "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
          # attachmentObject <- file
          # bodyWithAttachment <- list(body, mime_part(attachmentObject, name = paste("Отменённые_рейсы_логистика-", Sys.Date(), ".xlsx", sep="")))
          # sendmail(from, 
          #          to, 
          #          subject, 
          #          bodyWithAttachment, 
          #          control=list(smtpServer= "smtp.example.com", smtpPort="25"))
          
        }
      )
      
    }
    else{

      output$downloadCanceledOrdersLogistics <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы_логистика-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Логистика")
          
          writeData(wb,
                    sheet = 1,
                    x = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = selected_canceledOrdersLogistics(),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2_yellow, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:ncol(selected_canceledOrdersLogistics()), widths = c(10, 10, 10, 35, 35, 10, 20, 35, 35))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          # subject <- "Отчёт по сорванным рейсам, отдел логистики"
          # body <- paste("Сорванные рейсы", "за период", format(Sys.Date() - 6, "%d.%m.%Y"), "-", format(Sys.Date() - 1, "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
          # attachmentObject <- file
          # bodyWithAttachment <- list(body, mime_part(attachmentObject, name = paste("Отменённые_рейсы_логистика-", Sys.Date(), ".xlsx", sep="")))
          # sendmail(from, 
          #          to, 
          #          subject, 
          #          bodyWithAttachment, 
          #          control=list(smtpServer= "smtp.example.com", smtpPort="25"))
          
        }
      )
      
    }
    
  })  
  

  observeEvent(input$allDataCheckbox, {
    if (input$allDataCheckbox){

      output$downloadCanceledOrdersLawyers <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы_юристы-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Юристы")
          
          writeData(wb,
                    sheet = 1,
                    x = "За весь период существования компании",
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = breakdownsTable$data %>%
                      select(-Статус_обработки, -Тип_ТС, -Тоннаж_ТС, -Объём_ТС, ИНН_перевозчика, -Адрес_перевозчика, -Маржа, -Рентабельность) %>%
                      arrange(Дата_создания_заявки) %>%
                      rename("Номер заявки" = 1,
                             "Внешний номер заявки" = 2,
                             "Дата создания заявки" = 3,
                             "Клиент" = 4,
                             "Тип заявки" = 5,
                             "Статус заявки" = 6,
                             "Наличие заявки клиента (прикреплена)" = 7,
                             "Наличие подписанного договора-заявки с перевозчиком (прикреплен)" = 8,
                             "Наличие подписанной заявки с перевозчиком (прикреплен)" = 9,
                             "Место или город погрузки" = 10,
                             "Место или город выгрузки" = 11,
                             "Ответственный логист" = 12,
                             "Старший группы" = 13,
                             "Перевозчик" = 14,
                             "Ставка клиента" = 15,
                             "Ставка перевозчика" = 16,
                             "Сумма предоплаты перевозчику" = 17,
                             "НДС / без НДС (по перевозчику)" = 18,
                             "Проблема с ТСД, дата" = 19,
                             "Наличие Торг-2" = 20,
                             "Заметки" = 21,
                             "Причина создания" = 22,
                             "Связанная заявка" = 23
                      ),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:23, widths = c(10, 10, 10, 35, 10, 20, 10, 10, 10, 10, 10, 35, 35, 35, 10, 10, 10, 10, 10, 10, 35, 10, 10))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          # subject <- "Отчёт по сорванным рейсам, юридический отдел"
          # body <- paste("Сорванные рейсы", "за период", format(Sys.Date() - 8, "%d.%m.%Y"), "-", format(Sys.Date() - 1, "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
          # attachmentObject <- file
          # bodyWithAttachment <- list(body, mime_part(attachmentObject, name = paste("Отменённые_рейсы_юристы-", Sys.Date(), ".xlsx", sep="")))
          # sendmail(from, 
          #          to, 
          #          subject, 
          #          bodyWithAttachment, 
          #          control=list(smtpServer= "smtp.example.com", smtpPort="25"))
          
        }
      )
      
    }
    else{
      
      output$downloadCanceledOrdersLawyers <- downloadHandler(
        filename = function() {
          paste("Сорванные_рейсы_юристы-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          
          wb <- createWorkbook()
          
          addWorksheet(wb, "Юристы")
          
          writeData(wb,
                    sheet = 1,
                    x = paste("За период", format(input$date_canceledOrders[1], "%d.%m.%Y"), "-", format(input$date_canceledOrders[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                    startCol = 1,
                    startRow = 1)
          
          writeData(wb, 
                    sheet = 1, 
                    x = selected_canceledOrdersLawyers(),
                    startCol = 1,
                    startRow = 2,
                    colNames = TRUE,
                    rowNames = FALSE,
                    borders = "rows", 
                    borderColour = "black",
                    headerStyle = hs2, borderStyle = "dashed")
          
          setColWidths(wb, sheet = 1, cols = 1:ncol(selected_canceledOrdersLawyers()), widths = c(10, 10, 10, 35, 10, 20, 10, 10, 10, 10, 10, 35, 35, 35, 10, 10, 10, 10, 10, 10, 35, 10, 10))
          setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
          
          saveWorkbook(wb, file, overwrite = TRUE)
          
          
          # Для отправки писем на почту при нажатии на кнопку
          # subject <- "Отчёт по сорванным рейсам, юридический отдел"
          # body <- paste("Сорванные рейсы", "за период", format(Sys.Date() - 8, "%d.%m.%Y"), "-", format(Sys.Date() - 1, "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y"))
          # attachmentObject <- file
          # bodyWithAttachment <- list(body, mime_part(attachmentObject, name = paste("Отменённые_рейсы_юристы-", Sys.Date(), ".xlsx", sep="")))
          # sendmail(from, 
          #          to, 
          #          subject, 
          #          bodyWithAttachment, 
          #          control=list(smtpServer= "smtp.example.com", smtpPort="25"))
          
        }
      )
      
    }
    
  })
  
  
  output$downloadCarriers <- downloadHandler(
    filename = function() {
      paste("Конверсия-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Конверсия")
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$date_carriers[1], "%d.%m.%Y"), "-", format(input$date_carriers[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb, 
                sheet = 1, 
                x = selected_newOldCarriers1(),
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows", 
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      setColWidths(wb, sheet = 1, cols = 1:ncol(selected_newOldCarriers1()), widths = c(20, 20, 50, 50, 20))
      setRowHeights(wb, sheet = 1, rows = 2, heights = 100)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
  output$downloadSB <- downloadHandler(
    filename = function() {
      paste("Выработка_СБ_по_сущностям-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Водители")
      
      addWorksheet(wb, "Перевозчики")
      
      addWorksheet(wb, "ТС")
      
      
      result_drivers = selected_drivers_everyday() %>%
        select(Дата, Сотрудник, Всего_водители) %>%
        pivot_wider(names_from = Дата, values_from = Всего_водители) %>% 
        left_join(selected_drivers_all(), by = c("Сотрудник" = "Сотрудник")) %>%
        arrange(desc(Всего_водители))
      
      result_carriers = selected_carriers_everyday() %>%
        select(Дата, Сотрудник, Всего_перевозчики) %>%
        pivot_wider(names_from = Дата, values_from = Всего_перевозчики) %>% 
        left_join(selected_carriers_all(), by = c("Сотрудник" = "Сотрудник")) %>%
        arrange(desc(Всего_перевозчики))
      
      result_vehicles = selected_vehicles_everyday() %>%
        select(Дата, Сотрудник, Всего_тс) %>%
        pivot_wider(names_from = Дата, values_from = Всего_тс) %>% 
        left_join(selected_vehicles_all(), by = c("Сотрудник" = "Сотрудник")) %>%
        arrange(desc(Всего_тс))
      
      writeData(wb,
                sheet = 1,
                x = paste("За период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb,
                sheet = 1,
                x = result_drivers,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows",
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 2,
                x = paste("За период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb,
                sheet = 2,
                x = result_carriers,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows",
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      writeData(wb,
                sheet = 3,
                x = paste("За период", format(input$dateSB[1], "%d.%m.%Y"), "-", format(input$dateSB[2], "%d.%m.%Y"), "по состоянию на", format(Sys.time(), "%H:%M:%S %d %B %Y")),
                startCol = 1,
                startRow = 1)
      
      writeData(wb,
                sheet = 3,
                x = result_vehicles,
                startCol = 1,
                startRow = 2,
                colNames = TRUE,
                rowNames = FALSE,
                borders = "rows",
                borderColour = "black",
                headerStyle = hs2, borderStyle = "dashed")
      
      setColWidths(wb, sheet = 1, cols = 1, widths = 50)
      setColWidths(wb, sheet = 2, cols = 1, widths = 50)
      setColWidths(wb, sheet = 3, cols = 1, widths = 50)
      
      
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
}
