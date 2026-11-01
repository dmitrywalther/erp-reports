KZ_tabName <- tabItem(tabName = "KZ", fluidRow(
  
  box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
      dateRangeInput(inputId = "dateKZ",
                     label = "Период",
                     start = "2020-01-01",
                     end = Sys.Date(),
                     language = "ru",
                     weekstart = 1,
                     format = "dd.mm.yyyy")
  ),
  
  box(title = "Суммы по категориям", status = "purple", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      highchartOutput(outputId = "kzPlot")),
  
  
  box(title = "Перевозчики по суммам", status = "teal", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      highchartOutput(outputId = "kzPlot1")),
  
  box(title = "Рейсы по категориям", status = "orange", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      highchartOutput(outputId = "kzPlot2")),
  
  
  box(title = "Перевозчики по рейсам", status = "maroon", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      highchartOutput(outputId = "kzPlot3")),
  
  box(title = "Сводная таблица", status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12, reactableOutput("reactableKZ")),
  
  box(title = "Платёжный календарь", status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12, reactableOutput("reactablePaymentSchedule"))
  
)
)