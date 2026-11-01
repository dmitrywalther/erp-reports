logistician_plan_fact_tabItem <- tabItem(tabName = "logistician_plan_fact", fluidRow(
  
  box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
      
      dateRangeInput(inputId = "dateLogisticianPlanFact",
                     label = "Период",
                     start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                     end = Sys.Date(),
                     language = "ru",
                     weekstart = 1,
                     format = "dd.mm.yyyy"),
      
      if (ONLINE_DEMO) helpText("Загрузка Excel доступна при локальном запуске демо.") else fileInput(
          inputId = "planLogisticianInput",
          label = "Загрузить план по логистам",
          multiple = FALSE,
          accept = ".xlsx",
          width = NULL,
          buttonLabel = "Выбрать",
          placeholder = "Файл не выбран"
      ),
      
      downloadButton("downloadLogisticianPlanFact", "Скачать отчёт «План-факт по логистам»"),
      
      tags$style(type='text/css', "#downloadLogisticianPlanFact {    margin-left: auto;
    margin-right: auto; display: block; width: 23em}")
      
      
  ),
  
  box(title = "Сводная таблица", status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12, div(class = "standings",
                                          div(class = "title",
                                              h2("План-факт по логистам"),
                                              "По выгруженным рейсам"
                                          )
                                          # reactableOutput("reactableLogisticPlanFact")
      ))
  
)
)
