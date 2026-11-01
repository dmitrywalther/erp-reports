priceIndicative_tabItem <- tabItem(tabName = "priceIndicative", fluidRow(
  
  column(width = 6, wellPanel(leafletOutput("indicative_map"))),
  
  column(width = 6, wellPanel(
    helpText("Для выбора маршрута используйте таблицу медианных значений цен по направлениям"),
    h4(textOutput("indicative_selected_route_name")),
    h4(textOutput("indicative_selected_route_distance")),
    tabsetPanel(
      tabPanel("Фильтры",
               dateRangeInput(inputId = "dateIndicative",
                              label = "Период",
                              start = Sys.Date() - 30,
                              end = Sys.Date(),
                              language = "ru",
                              weekstart = 1,
                              format = "dd.mm.yyyy"),
               selectInput('indicativeTypeOfBody', 'Тип ТС', choices = NULL, multiple = TRUE),
               selectInput('indicativeCapacityAndVolume', 'Объём/Тоннаж', choices = NULL, multiple = TRUE),
               selectInput('indicativeClient', 'Клиент', choices = NULL, multiple = TRUE)
      ),
      
      tabPanel("Статистика",
               checkboxInput("priceForKmCheckbox", "Цена за км", value = TRUE),
               reactableOutput("indicativeTableDetailStatistics")      
      ), 
      tabPanel("Динамика",
               highchartOutput(outputId = "priceSplineERP")
      ),
      tabPanel("Экономика рейса",
               numericInput("profitability_ind", "Рентабельность, %:", 15, min = 1, max = 100),
               tags$b("Ставка перевозчика, c НДС (без):"),
               h4(textOutput("carrier_rate_ind")),
               tags$b("Цена за км для перевозчика, с НДС (без):"),
               h4(textOutput("price_per_km_for_carrier_ind"))
      )
    )
    
    
    
  )),
  
  column(width = 12,
          h3("Медианные цены по направлениям на основе данных ERP"),
         helpText("Медиана, в отличие от среднего, менее чувствительна к выбросам и аномалиям наблюдаемой выборки"),
         wellPanel(reactableOutput(outputId = "indicativeTable")),
         h3("Таблица исходных заявок для расчёта цен"),
         wellPanel(reactableOutput(outputId = "indicativeTable_source"))
  )
  
  
  # box(title = "Динамика по данным ERP", status = "navy", solidHeader = TRUE,
  #     collapsible = TRUE, width = 12,
  #     selectInput('routeERP', 'Маршрут', choices = NULL),
  #     highchartOutput(outputId = "priceSplineERP")
  # )
  
  # box(title = "Медианные значения по данным площадки 1", status = "navy", solidHeader = TRUE,
  #     collapsible = TRUE, width = 12, reactableOutput(outputId = "indicativeTable2")),
  # 
  # box(title = "Динамика по данным площадки 1", status = "navy", solidHeader = TRUE,
  #     collapsible = TRUE, width = 12,
  #     selectInput('routeInbev', 'Маршрут', choices = NULL),
  #     highchartOutput(outputId = "priceSplineInbev")
  # ),
  # 
  # box(title = "Медианные значения по данным площадки 2", status = "navy", solidHeader = TRUE,
  #     collapsible = TRUE, width = 12, reactableOutput(outputId = "indicativeTable1")),
  # 
  # box(title = "Динамика по данным площадки 2", status = "navy", solidHeader = TRUE,
  #     collapsible = TRUE, width = 12,
  #     selectInput('routeTransporeon', 'Маршрут', choices = NULL),
  #     highchartOutput(outputId = "priceSplineTransporeon")
  # )
  
)
)
