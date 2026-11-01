tabItem_logistic_KPI <- tabItem(tabName = "productivity", fluidRow(
  
  column(width = 12,
         wellPanel(
           column(width = 6, dateRangeInput(inputId = "dateLogisticProductivity",
                                            label = "Период",
                                            start = paste0(as.POSIXlt(Sys.time())$year + 1900, '-', as.POSIXlt(Sys.time())$mon + 1, "-01"),
                                            end = Sys.Date(),
                                            language = "ru",
                                            weekstart = 1,
                                            format = "dd.mm.yyyy")),
           column(width = 6, selectInput(inputId = "dateTypeForLogisticProductivity",
                                         label = "Методика",
                                         choices = c("По созданным рейсам",
                                                     "По загруженным рейсам",
                                                     "По выгруженным рейсам"),
                                         selected = "По созданным рейсам")),
           
           column(4, selectInput(inputId = "logistJobForLogisticProductivity",
                                 label = "Должность",
                                 choices = NULL,
                                 multiple = TRUE
           )),
           
           column(4, selectInput(inputId = "logistGroupForLogisticProductivity",
                                 label = "Группа",
                                 choices = NULL,
                                 multiple = TRUE
           )),
           
           column(4, selectInput(inputId = "clientForLogisticProductivity",
                                          label = "Юридическое лицо клиента",
                                          choices = NULL,
                                          multiple = TRUE)),

           if (DEMO_MODE) {
             tags$p("Плановые показатели в демо не заданы. Доступны фактические итоги и динамика на синтетических данных.",
                    class = "text-muted")
           } else {
             column(width = 12, reactableOutput(outputId = "planVika"))
           },
          
           tabsetPanel(
             selected = if (DEMO_MODE) "Абсолютные итоги" else "Выполнение плана",
             if (!DEMO_MODE) tabPanel("Выполнение плана",
                      # column(width = 12, reactableOutput(outputId = "reactableLogisticPlanFact_general")),
                      reactableOutput(outputId = "reactableLogisticPlanFact_detail")
             ),
             tabPanel("Абсолютные итоги",
                      h3("По специалистам отдела логистики"),
                      reactableOutput("tableLogisticProductivity1"),
                      h3("По группам отдела логистики"),
                      reactableOutput("tableLogisticProductivity")
             ),
             tabPanel("Динамика",
                      selectInput(inputId = "aggregation",
                                  label = "Агрегация",
                                  choices = c("День",
                                              "Неделя",
                                              "Месяц",
                                              "Год"),
                                  selected = "День"),
                      h3("Оборот по дням"),
                      reactableOutput("tableLogisticProductivityRevenue"),
                      h3("Маржа очищенная по дням"),
                      reactableOutput("tableLogisticProductivityProfit"),
                      h3("Рейсы по дням"),
                      reactableOutput("tableLogisticProductivityRoutes")
             )
           )
         )
         #                           downloadButton("downloadLogisticProductivity", "Экспорт в Excel"),
         #                           tags$style(type='text/css', "#downloadLogisticProductivity {    margin-left: auto;
         # margin-right: auto; display: block; width: 10em}")
  ),
  
  
  
  # box(title = "Итоги по специалистам отдела логистики", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 12, reactableOutput("tableLogisticProductivity1")),
  # box(title = "Итоги по группам отдела логистики", status = "purple", solidHeader = TRUE, collapsible = TRUE, width = 12, reactableOutput("tableLogisticProductivity")),
  # box(title = "Рейсы по дням", status = "navy", solidHeader = TRUE, collapsible = TRUE, width = 12,
  #     selectInput(inputId = "aggregation",
  #                 label = "Агрегация",
  #                 choices = c("День",
  #                             "Неделя",
  #                             "Месяц",
  #                             "Год"),
  #                 selected = "День"),
  #     reactableOutput("tableLogisticProductivityRoutes")
  #     ),
  # 
  # box(title = "Выработка по должностям", status = "maroon", solidHeader = TRUE, collapsible = TRUE, width = 12, reactableOutput("tableLogisticProductivityJobTitle"))
)
)
