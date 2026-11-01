problemDocs_tabItem <- tabItem(tabName = "problemDocs", fluidRow(
  
  box(title = "Инструменты", status = "success", solidHeader = TRUE,
      collapsible = TRUE, width = 12,
      actionButton("getProblemDocsButton", "Получить данные"),
      tags$style(type='text/css', "#getProblemDocsButton {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
  ),
  
  box(title = "Сумма по ставкам клиентам, всего", status = "warning", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      highchartOutput(outputId = "problemDocsPlotStatic")
  ),
  
  box(title = "Сумма по ставкам клиентам, всего", status = "warning", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      dataTableOutput(outputId = "docsProblemDataTable")
  ),
  
  box(title = "Сумма по ставкам клиентам, по дням", status = "warning", solidHeader = TRUE,
      collapsible = TRUE, width = 12,
      highchartOutput(outputId = "problemDocsPlotDynamics")
  )
  
)
)