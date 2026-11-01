DZ_new_tabItem <- tabItem(tabName = "DZ_new", fluidRow(
  
  column(width = 12,
         wellPanel(
           
           column(width = 12,
           valueBoxOutput(width = 4, "total_unconfirmed_receivables"),
           valueBoxOutput(width = 4, "total_confirmed_receivables"),
           valueBoxOutput(width = 4, "total_overdue_receivables"),
           ),
           
           column(width = 12, dateRangeInput(inputId = "date_newDZ",
                                             label = "Период",
                                             start = "2020-01-01",
                                             end = Sys.Date(),
                                             language = "ru",
                                             weekstart = 1,
                                             format = "dd.mm.yyyy")),
           
           column(width = 4,
                  downloadButton("download_newDZ", "Экспорт в Excel"),
                  tags$style(type='text/css', "#download_newDZ {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}"),
           ),

           tabsetPanel(
             
             tabPanel("Неподтверждённая", 
                      
              reactableOutput(outputId = "dz_unconfirmed_receivables")

             ),
             
             tabPanel("Подтверждённая",
                      
    #                   column(
    #                     width = 12,
    #                     downloadButton("download1_newDZ", "Экспорт в Excel"),
    #                     tags$style(type='text/css', "#download1_newDZ {    margin-left: auto;
    # margin-right: auto; display: block; width: 10em}")
    #                   ),

    selectInput(inputId = "type_dz_filter",
                label = "Категория ДЗ",
                choices = c("ПДЗ", "ПрДЗ"),
                multiple = TRUE),

             reactableOutput(outputId = "dz_confirmed_receivables")
                      
             ),
             
             tabPanel("Поступление ДС", 
                      reactableOutput(outputId = "dz_PaymentSchedule"),
                      
             ),
             
             tabPanel("Воронка ДЗ",
                      helpText("Все значения представлены в календарных днях"),
                      # column(width = 12, dateRangeInput(inputId = "date1_newDZ",
                      #                                   label = "Период",
                      #                                   start = "2020-01-01",
                      #                                   end = Sys.Date(),
                      #                                   language = "ru",
                      #                                   weekstart = 1,
                      #                                   format = "dd.mm.yyyy")),
                      reactableOutput(outputId = "dz_receivable_turnover")
             )
             
             )
           
           )
         )
  
)
)