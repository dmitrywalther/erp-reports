low_profitability_tabName <- tabItem(tabName = "low_profitability",
        fluidRow(
          
          column(width = 4,
                 wellPanel(
                   helpText("Отчёт позволяет определять долю рейсов с рентабельностью ниже указанного значения"),
                   h4(textOutput(outputId = "low_profitability_totalRoutes")),
                   h4(textOutput(outputId = "low_profitability_partRoutes")),
                   h4(textOutput(outputId = "low_profitability_proportion")),
                   dateRangeInput(inputId = "low_profitabilityDateInput",
                                  label = "За период",
                                  start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                                  end = Sys.Date(),
                                  language = "ru",
                                  weekstart = 1,
                                  format = "dd.mm.yyyy"),
                   numericInput("low_profitabilityNumericInput", "Рентабельность, %:", 5, min = -100, max = 100),
                   selectInput(inputId = "low_profitabilityType",
                               label = "Тип заявки",
                               choices = c("Гарантия", "Спот"),
                               multiple = TRUE),
                   selectInput(inputId = "low_profitabilityGeography",
                               label = "География",
                               choices = c("Город", "Межгород"),
                               multiple = TRUE)
                 )
          ),
          column(width = 8,
                 wellPanel(
                   column(width = 6, 
                          selectInput(inputId = "low_profitability_aggregation",
                                      label = "Агрегация",
                                      choices = c("День",
                                                  "Неделя",
                                                  "Месяц",
                                                  "Год"),
                                      selected = "День")),
                   column(width = 6,
                          selectInput(inputId = "low_profitability_client",
                                      label = "Клиент",
                                      choices = NULL,
                                      multiple = TRUE
                          )
                   ),
                   highchartOutput(outputId = "low_profitability_dynamic")
                   
                 )
          ),
          column(width = 12, 
                 wellPanel(
                   tags$h3("По клиентам"),
                   reactableOutput(outputId = "low_profitabilityTable_client"),
                   tags$h3("По группам и специалистам отдела логистики"),
                   reactableOutput(outputId = "low_profitabilityTable_logist"),
                   tags$h3("Исходные заявки"),
                   reactableOutput(outputId = "low_profitabilityTable_source")
                 )
          )
          
          
        )
)