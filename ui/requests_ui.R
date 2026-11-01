requests_tabItem <- tabItem(tabName = "requests",
        fluidRow(
          box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
              dateRangeInput(inputId = "date_requests",
                             label = "Период",
                             start = Sys.Date(),
                             end = Sys.Date(),
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              checkboxInput("allStatusCheckbox", "Все статусы рейсов", value = FALSE),
              selectInput(inputId = "dateType",
                          label = "Тип даты",
                          choices = c("По созданию заявки",
                                      "По созданию перевозки",
                                      "По загрузке",
                                      "По выгрузке"),
                          selected = "По созданию"),
              selectInput(inputId = "client",
                          label = "Клиент",
                          choices = NULL,
                          multiple = TRUE
                          ),
              selectInput(inputId = "manager",
                          label = "Менеджер",
                          choices = NULL,
                          multiple = TRUE
                          ),
              selectInput(inputId = "logist",
                          label = "Логист",
                          choices = NULL,
                          multiple = TRUE
                          ),
              downloadButton("downloadRequests", "Экспорт в Excel"),
              tags$style(type='text/css', "#downloadRequests {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
          )
          
        ))