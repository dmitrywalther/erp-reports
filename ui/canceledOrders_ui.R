canceledOrders_tabItem <- tabItem(tabName = "canceledOrders",
        fluidRow(
          useShinyjs(),
          box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
              dateRangeInput(inputId = "date_canceledOrders",
                             label = "Период",
                             start = Sys.Date() - 90,
                             end = Sys.Date(),
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              checkboxInput("allDataCheckbox", "За всю историю компании", value = FALSE),
              box(width = 12, downloadButton("downloadCanceledOrders", "Общий"),
                  tags$style(type='text/css', "#downloadCanceledOrders {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}"),
                  downloadButton("downloadCanceledOrdersCommerce", "Коммерция"),
                  tags$style(type='text/css', "#downloadCanceledOrdersCommerce {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}"),
                  downloadButton("downloadCanceledOrdersLogistics", "Логистика"),
                  tags$style(type='text/css', "#downloadCanceledOrdersLogistics {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}"),
                  downloadButton("downloadCanceledOrdersLawyers", "Юристы"),
                  tags$style(type='text/css', "#downloadCanceledOrdersLawyers {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}"))
          ),
          
          box(title = "Клиенты", status = "info", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              highchartOutput(outputId = "canceledOrdersClientPlot")
          ),
          
          box(title = "Логисты", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              highchartOutput(outputId = "canceledOrdersLogistPlot")
          ),
          
          box(title = "Перевозчики", status = "danger", solidHeader = TRUE,
              collapsible = TRUE, width = 4,
              highchartOutput(outputId = "canceledOrdersCarrierPlot")
          )
          
        ))