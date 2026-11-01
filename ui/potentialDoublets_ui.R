potentialDoublets_tabName <- tabItem(tabName = "potentialDoublets",
        fluidRow(
          
          column(width = 6, leafletOutput("mymap")),
          
          column(width = 6, wellPanel(
            h2("Кругорейс"),
            selectInput('doublets', 'Номера заявок', choices = NULL),
            tabsetPanel(
              tabPanel("Маршрут",
                       tags$div(textOutput("doublets_start_direct"), style = "text-align:center"),
                       tags$div("\U2193", style = "text-align:center; font-weight: bold"),
                       tags$div(textOutput("doublets_finish_direct"), style = "text-align:center"),
                       tags$div("\U2193", style = "text-align:center; font-weight: bold"),
                       tags$div(textOutput("doublets_start_back"), style = "text-align:center"),
                       tags$div("\U2193", style = "text-align:center; font-weight: bold"),
                       tags$div(textOutput("doublets_finish_back"), style = "text-align:center"),
              ),
              tabPanel("Оценка", 
                       tags$b("Плановое время между рейсами:"),
                       h4(textOutput("time_between_routes")),
                       tags$b("Расстояние дополнительного маршрута:"),
                       h4(textOutput("distance")),
                       tags$b("Время дополнительного маршрута:"),
                       h4(textOutput("duration"))
              ), 
              tabPanel("Общие сведения",                                            tags$b("Суммарная ставка:"),
                       h4(textOutput("sum_revenue")),
                       tags$b("Суммарное расстояние:"),
                       h4(textOutput("sum_distance")),
                       tags$b("Цена за км:"),
                       h4(textOutput("price_per_km"))
              ), 
              tabPanel("Экономика рейса",
                       numericInput("profitability", "Рентабельность, %:", 15, min = 1, max = 100),
                       tags$b("Ставка перевозчика, c НДС (без):"),
                       h4(textOutput("carrier_rate")),
                       tags$b("Цена за км для перевозчика, c НДС (без):"),
                       h4(textOutput("price_per_km_for_carrier"))
              )
            )
            
            
            
          )),
          
          column(width = 12,
                 
                 wellPanel(
                   reactableOutput(outputId = "potentialDoubletsTable"),
                   helpText("В таблице представлены потенциальные кругорейсы из активных заявок ERP и тендерных площадок.")
                 )
          ),
          
          column(width = 12,
                 wellPanel(
                   
                   helpText("В таблице представлена география маршрутов клиентов с дополнительной информацией о типе ТС"),
                   
                   
                   dateRangeInput(inputId = "clientDirectionsDateInput",
                                  label = "Период выгрузки",
                                  start = Sys.Date() - 15,
                                  end = Sys.Date(),
                                  language = "ru",
                                  weekstart = 1,
                                  format = "dd.mm.yyyy"),
                   
                   reactableOutput(outputId = "clientDirectionsTable"))
          )
          
          
        )
)
