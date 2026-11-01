spot_tabItem <- tabItem(tabName = "spot", fluidRow(
  
  box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
      dateRangeInput(inputId = "dateSpot",
                     label = "Период",
                     start = paste0(as.POSIXlt(Sys.time())$year + 1900, '-', as.POSIXlt(Sys.time())$mon + 1, "-01"),
                     end = Sys.Date(),
                     language = "ru",
                     weekstart = 1,
                     format = "dd.mm.yyyy"),
      selectInput(inputId = "aggregationSpot",
                  label = "Агрегация",
                  choices = c("День",
                              "Неделя",
                              "Месяц",
                              "Год"),
                  selected = "День"),
      downloadButton("downloadSpot", "Скачать отчёт"),
      tags$style(type='text/css', "#downloadSpot {margin-left: auto;
    margin-right: auto; display: block; width: 23em}")
      
  ),
  
  box(title = "Динамика спотовых заявок", status = "purple", solidHeader = TRUE,
      collapsible = TRUE, width = 12,
      highchartOutput(outputId = "spot_dynamic_general")
  ),
  
  box(title = "Сводный отчёт по спотовым рейсам", status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12, reactableOutput(outputId = "spotTable")),
  
  box(title = 'Клиент 01', status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12, highchartOutput(outputId = "spotPlot")),
  
  box(title = 'Клиент 10', status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12, highchartOutput(outputId = "spotPlot1")),
  
  box(title = 'Клиент 09', status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12, highchartOutput(outputId = "spotPlot2"))
  
)
)