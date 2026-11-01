tabItem_active_requests <- tabItem(tabName = "active_requests", fluidRow(
  
  box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
      selectInput(inputId = "client_active_requests",
                  label = "Клиент",
                  choices = NULL)
  ),
  uiOutput("boxToday"),
  uiOutput("boxTommorow"),
  uiOutput("boxAfterTommorow")
)
)