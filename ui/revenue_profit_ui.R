tabItem_revenue_profit <- tabItem(tabName = "revenue_profit", fluidRow(
  
  column(width = 12,
         wellPanel(
           column(width = 6, dateRangeInput(inputId = "dateRevenueProfit",
                                            label = "Период",
                                            start = paste0(as.POSIXlt(Sys.time())$year + 1900, '-', as.POSIXlt(Sys.time())$mon + 1, "-01"),
                                            end = Sys.Date(),
                                            language = "ru",
                                            weekstart = 1,
                                            format = "dd.mm.yyyy")),
           column(width = 6, selectInput(inputId = "dateTypeForRevenueProfit",
                                         label = "Методика",
                                         choices = c("По созданным рейсам",
                                                     "По выгруженным рейсам"),
                                         selected = "По выгруженным рейсам")),
           column(width = 3,
                  selectInput(inputId = "typeForRevenueProfit",
                              label = "Тип",
                              choices = c("Гарантия", "Спот"),
                              multiple = TRUE)),
           column(width = 3,
                  selectInput(inputId = "geographyForRevenueProfit",
                              label = "География",
                              choices = c("Межрегион", "Регион"),
                              multiple = TRUE)),
           column(width = 3,
                  selectInput(inputId = "managerForRevenueProfit",
                              label = "Ответственный менеджер",
                              choices = NULL,
                              multiple = TRUE)),
           
           column(3, selectInput(inputId = "logistForRevenueProfit",
                                 label = "Ответственный логист",
                                 choices = NULL,
                                 multiple = TRUE
           )),
           
           column(12,
           reactableOutput(outputId = "revenue_profit_table_general"),
           helpText('* Доля спотовых рейсов')
           ),
           
           tabsetPanel(
             tabPanel("Таблица", 
                      reactableOutput(outputId = "revenue_profit_table")
             ),
             tabPanel("Диаграммы",
                      selectInput(inputId = "aggregationRevenueProfit",
                                  label = "Агрегация",
                                  choices = c("День",
                                              "Неделя",
                                              "Месяц",
                                              "Год"),
                                  selected = "День"),
                      
                      highchartOutput(outputId = "revenueDynamicsPlot"),
                      
                      column(width = 6, highchartOutput("ratePlotCurrentMonth")),
                      
                      column(width = 6, highchartOutput("profitPlotCurrentMonth")),
                      
                      column(width = 6, highchartOutput("profitabilityPlotCurrentMonth")),
                      
                      column(width = 6, highchartOutput("routesPlotCurrentMonth")),
                      
                      highchartOutput("factPlot")
                      
             ),
             
             tabPanel("Об отчёте",
                      
                      helpText(
                        h3("Методика"),
                        "В отчёте используются следующие показатели:",
                        tags$ul(
                          tags$li(strong("«Оборот»"), " - сумма ставок клиента;"), 
                          tags$li(strong("«Выручка»"), " - сумма ставок клиента, очищенных от НДС;"), 
                          tags$li(strong("«Маржа»"), " - разность ставок клиента и перевозчика, очищенных от НДС;"), 
                          tags$li(strong("«Рентабельность»"), " - отношение очищенной маржи к очищенной ставке клиента;"),
                          tags$li(strong("«Рейсы»"), " - количество заявок.")
                        )
                      ),
                      
                      helpText("В расчёте не участвуют рейсы со статусом «Заявка активна», «Заявка отменена», «Перевозка отменена»."),
                      
                      helpText("Возможен выбор двух методик:", 
                               tags$ul(
                                 tags$li(strong("«По выгруженным рейсам»"), "(по умолчанию) - используется плановая дата последней точки выгрузки рейса;"), 
                                 tags$li(strong("«По созданным рейсам»"), " - используется дата создания перевозки (дата прикрепления к заявке перевозчика).")
                               )
                      ),
                      
                      helpText("Доступные фильтры:", 
                               tags$ul(
                                 tags$li(strong("«По дате»"), " - создания или выгрузки, в зависимости от методики;"), 
                                 tags$li(strong("«Ответственному менеджеру»"), ";"),
                                 tags$li(strong("«Типу заявки»"), " - гарантия или спот;"),
                                 tags$li(strong("«По географии»"), " - город или межгород. К городским рейсам относятся рейсы, у которых регион загрузки совпадает с регионом выгрузки. В противном случае рейс относится к межгородским перевозкам.")
                               )
                      )
             )
           )  
           
         )
  )
  
  
))
