registersForInsurance_tabName <- tabItem(tabName = "registersForInsurance",
        fluidRow(
          
          box(title = "Инструменты", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              dateRangeInput(inputId = "registersForInsurance_dateRangeInput",
                             label = "Период",
                             start = Sys.Date() - 7,
                             end = Sys.Date(),
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              selectInput(inputId = "type_registersForInsurance",
                          label = "Тип отчёта",
                          choices = c("Стандартный",
                                      "Сборный",
                                      "Упрощённый")
              ),
              if (DEMO_MODE) helpText("Экспорт содержит синтетические рейсы выбранного периода. Файл создаётся без исходных шаблонов страховых компаний."),
              downloadButton("downloadRegistersForInsurance", "Экспорт в Excel"),
              tags$style(type='text/css', "#downloadRegistersForInsurance {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
              
              
          )
        )
)
