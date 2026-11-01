cash_tabItem <- tabItem(tabName = "cash",
        fluidRow(
          
          box(title = "По клиентам", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              reactableOutput(outputId = "cash")
          ),
          
          box(title = "По сотрудникам ДО", status = "purple", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              reactableOutput(outputId = "cash_do")
          )
          
        )
)