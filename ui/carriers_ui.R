carriers_tabItem <- tabItem(tabName = "carriers",
        fluidRow(
          
          box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
              dateRangeInput(inputId = "date_carriers",
                             label = "Период",
                             start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                             end = Sys.Date(),
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              selectInput(inputId = "aggregation_carriers",
                          label = "Агрегация",
                          choices = c("День",
                                      "Неделя",
                                      "Месяц",
                                      "Год"),
                          selected = "День")
              #                                   helpText("Для начала работы с отчётом «Конверсия» необходимо в первый раз нажать
              #                                        на кнопку «Получить данные». Это сделано для ускорения работы всего приложения.
              #                                        После загрузки даннных Вы можете выбирать период наблюдений и экспортировать результаты отчёта в формате Excel-таблицы"),
              #                                   actionButton("getCarriersButton", "Получить данные"),
              #                                   tags$style(type='text/css', "#getCarriersButton {    margin-left: auto;
              # margin-right: auto; margin-bottom: 10px; display: block; width: 10em}"),
              #                                   downloadButton("downloadCarriers", "Экспорт в Excel"),
              #                                   tags$style(type='text/css', "#downloadCarriers {    margin-left: auto;
              # margin-right: auto; display: block; width: 10em}")
          ),
          
          valueBoxOutput(width = 4, "totalNewOldCarriers"),
          valueBoxOutput(width = 4, "totalNewCarriers"),
          valueBoxOutput(width = 4, "totalConversionCarriers"),
          
          box(title = "Динамика", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              highchartOutput(outputId = "conversion_dynamic_general")
          ),
          
          box(title = "Конверсия по логистам", status = "info", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              reactableOutput(outputId = "conversionDataTable")
          ),
          
          box(title = "Динамика просроченных заявок", status = "purple", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              highchartOutput(outputId = "lateOrders_dynamic_general")
          ),
          
          box(title = "Просроченные заявки по маршрутам", status = "danger", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              reactableOutput(outputId = "lateOrders_reactableTable")
          )
          
        ))