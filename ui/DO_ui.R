DO_tabItem <- tabItem(tabName = "DO", fluidRow(
  
  column(width = 12, wellPanel(
    column(width = 6,
           dateRangeInput(inputId = "date",
                          label = "Период",
                          start = Sys.Date(),
                          end = Sys.Date(),
                          language = "ru",
                          weekstart = 1,
                          format = "dd.mm.yyyy"),
           downloadButton("downloadDO", "Экспорт в Excel"),
           tags$style(type='text/css', "#downloadDO {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
    ),
    
    tabsetPanel(
      tabPanel("Кредиторы",
               h4("Обработка документов от перевозчиков по дням", style = "text-align:center;"),
               reactableOutput(outputId = "verifyDocs1"),
               h4("Обработка документов от перевозчиков - детализация", style = "text-align:center;"),
               reactableOutput(outputId = "verifyDocs")
      ),
      
      tabPanel("Дебиторы",
               h4("Выставление счетов по дням", style = "text-align:center;"),
               reactableOutput(outputId = "billsTable1"),
               h4("Выставление счетов - детализация", style = "text-align:center;"),
               reactableOutput(outputId = "billsTable")
      ),
      tabPanel("Акты перевозчиков",
               h4("Проведение актов перевозчиков - детализация", style = "text-align:center;"),
               reactableOutput(outputId = "carriedOutActsTable")
      )

    )
  )
  )
  
)
)