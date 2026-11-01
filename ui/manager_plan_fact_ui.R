manager_plan_fact_tabName <- tabItem(tabName = "manager_plan_fact", fluidRow(
  
  box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
      dateRangeInput(inputId = "dateManagerPlanFact",
                     label = "Период",
                     start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                     end = Sys.Date(),
                     language = "ru",
                     weekstart = 1,
                     format = "dd.mm.yyyy"),
      # fileInput(
      #     inputId = "planManagerInput",
      #     label = "Загрузить план по менеджерам",
      #     multiple = FALSE,
      #     accept = ".xlsx",
      #     width = NULL,
      #     buttonLabel = "Выбрать",
      #     placeholder = "Файл не выбран"
      # ),
      
      downloadButton("downloadManagerPlanFact", "Скачать отчёт «План-факт по менеджерам»"),
      tags$style(type='text/css', "#downloadManagerPlanFact {    margin-left: auto;
    margin-right: auto; display: block; width: 23em}")
      
  ),
  
  box(title = "Общие сведения", status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12,       div(class = "standings",
                                                div(class = "title",
                                                    h2("План-факт по менеджерам"),
                                                    "По выгруженным рейсам"
                                                ),
                                                reactableOutput("reactableManagerPlanFactGeneral"))
  ),
  
  box(title = "Детализация", status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12,       div(class = "standings",
                                                div(class = "title",
                                                    h2("План-факт по менеджерам"),
                                                    "По выгруженным рейсам"
                                                ),
                                                reactableOutput("reactableManagerPlanFactDetail"))
  )
  
)
)