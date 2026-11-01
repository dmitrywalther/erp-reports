carrier_prepayment_tab_item <- tabItem(tabName = "carrier_prepayment", 
                                 wellPanel(
                                   fluidRow(
                                     
                                     column(width = 4, selectInput(inputId = "carrier_prepayment_client_choose",
                                                                   label = "Клиент",
                                                                   choices = NULL,
                                                                   multiple = TRUE)),
                                     
                                     column(width = 4, selectInput(inputId = "carrier_prepayment_logist_choose",
                                                                   label = "Логист",
                                                                   choices = NULL,
                                                                   multiple = TRUE)),
                                     
                                     column(width = 4, selectInput(inputId = "carrier_prepayment_carrier_choose",
                                                                   label = "Перевозчик",
                                                                   choices = NULL,
                                                                   multiple = TRUE)),
                                     
                                     column(width = 6, dateRangeInput(inputId = "carrier_prepayment_date",
                                                                      label = "Период",
                                                                      start = paste0(as.POSIXlt(Sys.time())$year + 1900, '-', as.POSIXlt(Sys.time())$mon + 1, "-01"),
                                                                      end = Sys.Date(),
                                                                      language = "ru",
                                                                      weekstart = 1,
                                                                      format = "dd.mm.yyyy")),
                                     column(width = 6, selectInput(inputId = "carrier_prepayment_aggregation",
                                                                   label = "Агрегация",
                                                                   choices = c("День", "Неделя", "Месяц", "Год"),
                                                                   selected = "Неделя")),
                                     
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_routes")),
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_revenue")),
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_margin")),
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_profitability")),
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_advances")),
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_advances_by_client")),
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_advances_by_logist")),
                                     column(width = 12, reactableOutput(outputId = "carrier_prepayment_advances_by_carrier"))
                                     
                                     
                                   )   )
                                 
)