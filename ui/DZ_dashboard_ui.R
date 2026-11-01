DZ_dashboard_tabItem <- tabItem(tabName = "DZ_dashboard", fluidRow(
          
          
            
          column(width = 6,
                 wellPanel(h2("Воронка ДЗ", align = "center"),
                 reactableOutput(outputId = "funnel_ndz_table"),
                 reactableOutput(outputId = "average_payment_rate")
                 ),
                 ),
          
          column(width = 6, 
                 wellPanel(h2("Кому не выставили счета", align = "center"),
                 reactableOutput(outputId = "unformed_bills_table")),
                 wellPanel(h2("У кого просрочки?", align = "center"),
                 reactableOutput(outputId = "overdue_table"))
                 ),
          
          
          column(width = 12, 
                 wellPanel(
                 h2("Здоровье ДЗ", align = "center"),
                 numericInput("factoring_rate_numeric_input", "Ставка факторинга, %:", 32, min = 0, max = 100),
                 dateInput("turnover_date", "Сравнить с периодом:",
                           value = if (DEMO_MODE) Sys.Date() - 30 else as.Date("2022-01-01"), language = "ru",
                           weekstart = 1,
                           format = "dd.mm.yyyy"),
                 reactableOutput(outputId = "health_DZ_table_general"),
                 reactableOutput(outputId = "health_DZ_table_by_client")
                 )
          )
  
)
)
