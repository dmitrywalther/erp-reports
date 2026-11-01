client_health_tabItem <- tabItem(tabName = "client_health", 
                                 wellPanel(
                                fluidRow(
                                  
                                    column(width = 4, selectInput(inputId = "client_health_choose",
                                                                  label = "Юридическое лицо клиента",
                                                                  choices = NULL,
                                                                  multiple = TRUE)),
                                    # column(width = 4, dateRangeInput(inputId = "client_health_date",
                                    #                                  label = "Период",
                                    #                                  start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                                    #                                  end = Sys.Date(),
                                    #                                  language = "ru",
                                    #                                  weekstart = 1,
                                    #                                  format = "dd.mm.yyyy")),
                                    column(width = 2, airDatepickerInput(inputId = 'client_health_choose1', 
                                                                         label = 'Начало', 
                                                                         value = paste0(as.POSIXlt(Sys.time())$year + 1900, '-', '01', "-01"),
                                                                         multiple = FALSE,
                                                                         range = FALSE,
                                                                         separator = " - ",
                                                                         dateFormat = "mm.yyyy",
                                                                         minView = "months",
                                                                         view = "months",
                                                                         language = "ru"                                                                         )),
                                           
                                    column(width = 2, airDatepickerInput(inputId = 'client_health_choose2', 
                                                              label = 'Конец', 
                                                              value = Sys.Date(),
                                                              multiple = FALSE,
                                                              range = FALSE,
                                                              separator = " - ",
                                                              dateFormat = "mm.yyyy",
                                                              minView = "months",
                                                              view = "months",
                                                              language = "ru"                                           )
                                           ),
                                    column(width = 4, selectInput(inputId = "client_health_aggregation",
                                                                  label = "Агрегация",
                                                                  choices = c("Месяц", "Год"),
                                                                  selected = "Месяц")),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "fot_input", 
                                    #   label = "ФОТ, руб:", 
                                    #   value = 10608000, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "fines_input", 
                                    #   label = "Штрафы, руб:", 
                                    #   value = 1000000, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "administrative_expenses_input", 
                                    #   label = "Административные расходы, руб:", 
                                    #   value = 7000000, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "money_value_input", 
                                    #   label = "Стоимость денег, руб:", 
                                    #   value = 5056021, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "development_input", 
                                    #   label = "Разработка, руб:", 
                                    #   value = 2000000, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "usa_input", 
                                    #   label = "США, руб:", 
                                    #   value = 4588000, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "advance_payment_input", 
                                    #   label = "Авансирование, руб:", 
                                    #   value = 0, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    # column(width = 3, shinyWidgets::autonumericInput(
                                    #   inputId = "motivation_input", 
                                    #   label = "Мотивация, руб:", 
                                    #   value = 3200000, 
                                    #   currencySymbolPlacement = "p",
                                    #   decimalPlaces = 0,
                                    #   digitGroupSeparator = " ",
                                    #   decimalCharacter = "."
                                    # )),
                                    column(width = 12, fileInput(
                                      inputId = "cost_data_from_file",
                                      label = "Загрузить данные по издержкам",
                                      multiple = FALSE,
                                      accept = ".xlsx",
                                      width = NULL,
                                      buttonLabel = "Выбрать",
                                      placeholder = "Файл не выбран"
                                    )), 
                                    # column(width = 12, reactableOutput(outputId = "cost_table")
                                    # ),
                                    # column(width = 12, reactableOutput(outputId = "client_health_income")),
                                    # column(width = 12, reactableOutput(outputId = "client_health_levels")),
                                    column(width = 12, reactableOutput(outputId = "client_health_logists")),
                                    column(width = 12, reactableOutput(outputId = "client_health_routes")),
                                    column(width = 12, reactableOutput(outputId = "client_health_revenue")),
                                    column(width = 12, reactableOutput(outputId = "client_health_margin")),
                                    column(width = 12, reactableOutput(outputId = "client_health_profitability")),
                                    column(width = 12, reactableOutput(outputId = "client_health_vc")),
                                    column(width = 12, reactableOutput(outputId = "client_health_fc")),
                                    column(width = 12, reactableOutput(outputId = "client_health_profit")),
                                    column(width = 12, reactableOutput(outputId = "client_health_vc_payoff")),
                                    column(width = 12, reactableOutput(outputId = "client_health_fc_payoff")),
                                    column(width = 12, reactableOutput(outputId = "client_health_level3"))
                                    
                                )   )
                                
                                )