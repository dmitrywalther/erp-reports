count_requests_tabName <- tabItem(tabName = "count_requests",
        fluidRow(
          # highchartOutput(outputId = "countRequestsPlot"),
          box(title = "Создание заявок по 30-минутным интервалам", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              dateRangeInput(inputId = "dateCountRequests",
                             label = "Период",
                             start = Sys.Date() - 1,
                             end = Sys.Date(),
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              highchartOutput(outputId = "countRequestsPlot1")
          ),
          box(title = "Время наибольшей загруженности", status = "danger", solidHeader = TRUE,
              collapsible = TRUE, width = 6,
              h3("Время час пика (среднее по дням недели): ", textOutput(outputId = "totalPeak")),
              h4(textOutput(outputId = "periodCountRequests")),
              reactableOutput(outputId = "countRequestsTable1")
          ),
          box(title = "Детализация", status = "purple", solidHeader = TRUE,
              collapsible = TRUE, width = 6,
              reactableOutput(outputId = "countRequestsTable")
          )
          
        )
)