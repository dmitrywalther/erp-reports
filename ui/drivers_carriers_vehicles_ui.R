drivers_carriers_vehicles_tabName <- tabItem(tabName = "drivers_carriers_vehicles",
        fluidRow(
          
          box(title = "Водители", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 6,
              highchartOutput(outputId = "countDriversPlot")
          ),
          
          
          
          box(title = "Перевозчики", status = "purple", solidHeader = TRUE,
              collapsible = TRUE, width = 6,
              highchartOutput(outputId = "countCarriersPlot")
          ),
          
          box(title = "Водители", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 6,
              reactableOutput(outputId = "countDriversTable")
          ),
          
          box(title = "Перевозчики", status = "purple", solidHeader = TRUE,
              collapsible = TRUE, width = 6,
              reactableOutput(outputId = "countCarriersTable")
          )
          
          
        )
)