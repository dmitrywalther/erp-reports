security_tabName <- tabItem(tabName = "security", fluidRow(
  column(width = 12, wellPanel(
  column(width = 6,
      dateRangeInput(inputId = "dateSB",
                     label = "Период",
                     start = Sys.Date() - 1,
                     end = Sys.Date(),
                     language = "ru",
                     weekstart = 1,
                     format = "dd.mm.yyyy"),
      #                           actionButton("getSBButton", "Получить данные"),
      #                           tags$style(type='text/css', "#getSBButton {    margin-left: auto;
      # margin-right: auto; margin-bottom: 10px; display: block; width: 10em}"),
      downloadButton("downloadSB", "Экспорт в Excel"),
      tags$style(type='text/css', "#downloadSB {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
      
  ),
  
  tabsetPanel(
    tabPanel("Абсолютные итоги",
             reactableOutput(outputId = "sb_staff_rating_table")
    ),
    
    tabPanel("Диаграммы",
             box(title = "Водители", status = "navy", solidHeader = TRUE,
                 collapsible = TRUE, width = 6,
                 highchartOutput(outputId = "driversAllPlot")
             ),
             
             box(title = "Водители", status = "navy", solidHeader = TRUE,
                 collapsible = TRUE, width = 6,
                 highchartOutput(outputId = "driversEveryDayPlot")
             ),
             
             box(title = "Водители", status = "navy", solidHeader = TRUE,
                 collapsible = TRUE, width = 12,
                 reactableOutput(outputId = "driversAllTable")
             ),
             
             
             box(title = "Перевозчики", status = "danger", solidHeader = TRUE,
                 collapsible = TRUE, width = 6,
                 highchartOutput(outputId = "carriersAllPlot")
             ),
             
             box(title = "Перевозчики", status = "danger", solidHeader = TRUE,
                 collapsible = TRUE, width = 6,
                 highchartOutput(outputId = "carriersEveryDayPlot")
             ),
             
             box(title = "Перевозчики", status = "danger", solidHeader = TRUE,
                 collapsible = TRUE, width = 12,
                 reactableOutput(outputId = "carriersAllTable")
             ),
             
             
             box(title = "ТС", status = "purple", solidHeader = TRUE,
                 collapsible = TRUE, width = 6,
                 highchartOutput(outputId = "vehiclesAllPlot")
             ),
             
             box(title = "ТС", status = "purple", solidHeader = TRUE,
                 collapsible = TRUE, width = 6,
                 highchartOutput(outputId = "vehiclesEveryDayPlot")
             ),
             
             box(title = "ТС", status = "purple", solidHeader = TRUE,
                 collapsible = TRUE, width = 12,
                 reactableOutput(outputId = "vehiclesAllTable")
             )

    )
    
  )
  
)
)
  
)
)