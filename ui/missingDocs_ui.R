missingDocs_tabItem <- tabItem(tabName = "missingDocs",
        fluidRow(
          box(title = "Инструменты", status = "success", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              actionButton("getMissingDocsButton", "Получить данные"),
              tags$style(type='text/css', "#getMissingDocsButton {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
          ),
          
          box(title = "Отсутствующие заявки и договора, всего", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              highchartOutput(outputId = "missingDocsPlotCurrentDay")
          ),
          
          box(title = "Отсутствующие заявки, по дням", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              highchartOutput(outputId = "missingRequestsPlot")
          ),
          
          box(title = "Отсутствующие договора, по дням", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              highchartOutput(outputId = "missingContractsPlot")
          )
          
        ))