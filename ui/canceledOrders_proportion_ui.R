canceledOrders_proportion_tabItem <- tabItem(tabName = "canceledOrders_proportion",
        fluidRow(
          box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 6,
              dateRangeInput(inputId = "date_canceledOrders_prop",
                             label = "Период",
                             start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                             end = Sys.Date(),
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              selectInput(inputId = "aggregationCanceledOrders",
                          label = "Агрегация",
                          choices = c("День",
                                      "Неделя",
                                      "Месяц",
                                      "Год"),
                          selected = "День"),
              downloadButton("downloadCanceledOrders_prop", "Скачать отчёт"),
              tags$style(type='text/css', "#downloadCanceledOrders_prop {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
          ),
          
          box(title = "Итог", status = "danger", solidHeader = TRUE,
              collapsible = TRUE, width = 6,
              h3(textOutput(outputId = "totalRoutes")),
              h3(textOutput(outputId = "totalCanceling")),
              h3(textOutput(outputId = "totalBreakdowns")),
              h4(textOutput(outputId = "periodBreakdowns"))
          ),
          
          box(title = "Динамика отказов и срывов", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              selectInput(inputId = "client_canceling_breakdowns_dynamic",
                          label = "Клиент",
                          choices = NULL),
              highchartOutput(outputId = "canceling_breakdowns_dynamic_general")
          ),
          
          box(title = "Отказы от рейсов по клиентам", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              helpText("Отказы - рейсы со статусом «Заявка отменена» по причине «Отказ клиента» при отсутствии связанной заявки"),
              reactableOutput(outputId = "cancelingTableProportion")
          ),
          
          box(title = "Срывы рейсов по клиентам", status = "purple", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              helpText("Срывы - рейсы со статусом «Заявка отменена» по причине «Не нашли перевозчика», со статусом «Перевозка отменена», а также просроченные заявки - все при отсутствии связанной заявки"),
              reactableOutput(outputId = "breakdownsTableProportion")
          )
          
        ))