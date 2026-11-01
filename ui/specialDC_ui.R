specialDC_tabName <- tabItem(tabName = "specialDC",
        fluidRow(
          
          box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
              dateRangeInput(inputId = "date_specialDC",
                             label = "Период",
                             start = Sys.Date(),
                             end = Sys.Date() + 7,
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              selectInput(inputId = "dateType_specialDC",
                          label = "Тип даты",
                          choices = c("По загрузке",
                                      "По выгрузке"),
                          selected = "По созданию"),
              selectInput(inputId = "client_specialDC",
                          label = "Клиент",
                          choices = NULL),
              selectInput(inputId = "logist_specialDC",
                          label = "Логист",
                          choices = NULL),
              downloadButton("downloadRequests_specialDC", "Экспорт в Excel"),
              tags$style(type='text/css', "#downloadRequests_specialDC {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
          ),
          
          box(title = "Рейсы с повышенным вниманием", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              reactableOutput(outputId = "tablespecialDC")
          )
          
        )
)