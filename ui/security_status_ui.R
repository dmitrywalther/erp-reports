security_status_tabName <- tabItem(tabName = "security_status", fluidRow(
  column(width = 12, wellPanel(
    column(width = 6,
           dateRangeInput(inputId = "dateSB_status",
                          label = "Период",
                          start = Sys.Date() - 1,
                          end = Sys.Date(),
                          language = "ru",
                          weekstart = 1,
                          format = "dd.mm.yyyy"),
           #                           actionButton("getSBButton", "Получить данные"),
           #                           tags$style(type='text/css', "#getSBButton {    margin-left: auto;
           # margin-right: auto; margin-bottom: 10px; display: block; width: 10em}"),
           downloadButton("downloadSB_status", "Экспорт в Excel"),
           tags$style(type='text/css', "#downloadSB_status {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
           
    ),
    
    tabsetPanel(
      tabPanel("Абсолютные итоги",
               reactableOutput(outputId = "sb_staff_rating_table_status")
      ),
      
      tabPanel("Детализация",

               
               box(title = "Водители", status = "navy", solidHeader = TRUE,
                   collapsible = TRUE, width = 12,
                   reactableOutput(outputId = "driversAllTable_status")
               ),
               

               
               box(title = "Перевозчики", status = "danger", solidHeader = TRUE,
                   collapsible = TRUE, width = 12,
                   reactableOutput(outputId = "carriersAllTable_status")
               ),
               
               box(title = "ТС", status = "purple", solidHeader = TRUE,
                   collapsible = TRUE, width = 12,
                   reactableOutput(outputId = "vehiclesAllTable_status")
               )
               
      )
      
    )
    
  )
  )
  
)
)