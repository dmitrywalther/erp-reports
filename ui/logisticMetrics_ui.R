logisticMetrics_tabName <- tabItem(tabName = "logisticMetrics",
        fluidRow(
          
          box(title = "Система фильтров", status = "success", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              
              column(12, dateRangeInput(inputId = "metricDateInput",
                                        label = "Наблюдаемый период",
                                        start = Sys.Date() - 7,
                                        end = Sys.Date(),
                                        language = "ru",
                                        weekstart = 1,
                                        format = "dd.mm.yyyy")),
              
              column(4, dateRangeInput(inputId = "metricDateInput1",
                                       label = "Сравнить с периодом",
                                       start = Sys.Date() - 15,
                                       end = Sys.Date() - 8,
                                       language = "ru",
                                       weekstart = 1,
                                       format = "dd.mm.yyyy")),
              
              column(4, selectInput(inputId = "metricClient",
                                    label = "Клиент",
                                    choices = NULL,
                                    multiple = TRUE
              )),
              
              column(4, selectInput(inputId = "metricType",
                                    label = "Тип заявки",
                                    choices = c("Гарантия", "Спот"),
                                    multiple = TRUE
              )),
              
              
              column(4, selectInput(inputId = "metricLogistGroup",
                                    label = "Группа",
                                    choices = NULL,
                                    multiple = TRUE
              )),
              
              column(4, selectInput(inputId = "metricLogistJob",
                                    label = "Должность",
                                    choices = NULL,
                                    multiple = TRUE
              )),
              
              column(4, selectInput(inputId = "metricLogist",
                                    label = "Ответственный логист",
                                    choices = NULL,
                                    multiple = TRUE
              ))
              
          ),
          
          valueBoxOutput(width = 4, "metricTotalLogists"),
          valueBoxOutput(width = 4, "metricTotalRevenue"),
          valueBoxOutput(width = 4, "metricTotalProfit"),
          valueBoxOutput(width = 4, "metricTotalRoutes"),
          valueBoxOutput(width = 4, "metricTotalProfitability"),
          valueBoxOutput(width = 4, "metricTotalFlightPrice"),
          valueBoxOutput(width = 4, "metricTotalCancelRequests"),
          valueBoxOutput(width = 4, "metricTotalCancelTransportations"),
          valueBoxOutput(width = 4, "metricTotalLateRequests"),
          
          column(12, reactableOutput(outputId = "requests"))
          
        )
)