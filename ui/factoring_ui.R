factoring_tabName <- tabItem(tabName = "factoring", fluidRow(
  box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
      dateRangeInput(inputId = "dateFactoring",
                     label = "Период",
                     start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                     end = Sys.Date(),
                     language = "ru",
                     weekstart = 1,
                     format = "dd.mm.yyyy"),
      fileInput(
        inputId = "factoringInput",
        label = "Загрузить данные по факторингу",
        multiple = FALSE,
        accept = ".xlsx",
        width = NULL,
        buttonLabel = "Выбрать",
        placeholder = "Файл не выбран"
      ),
      
      downloadButton("downloadFactoringBills", "Скачать отчёт"),
      tags$style(type='text/css', "#downloadFactoringBills {    margin-left: auto;
    margin-right: auto; display: block; width: 23em}")
      
  )
)
)