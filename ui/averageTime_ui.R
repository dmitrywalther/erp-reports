averageTime_tabName <- tabItem(tabName = "averageTimeTabName",
        fluidRow(
          
          box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
              dateRangeInput(inputId = "averageTimeDate",
                             label = "Период",
                             start = paste0("2022-", as.POSIXlt(Sys.time())$mon + 1, "-01"),
                             end = Sys.Date(),
                             language = "ru",
                             weekstart = 1,
                             format = "dd.mm.yyyy"),
              selectInput(inputId = "aggregationAverageTime",
                          label = "Агрегация на графиках динамики",
                          choices = c("День",
                                      "Неделя",
                                      "Месяц",
                                      "Год"),
                          selected = "День")
          ),
          
          
          box(title = "Расчет по событиям в ЕРП создание заявки \U2192 создание перевозки", status = "purple", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              reactableOutput(outputId = "averageTime"),
              highchartOutput(outputId = "averageTimeByDayPlot"),
              div(class = "standings",
                  div(class = "title",
                      h2("Спотовая команда"),
                      "По заявкам, созданным спот-командой"
                  ),
                  reactableOutput("averageTime_ego"))
              
          ),
          
          
          box(title = "Количество рейсов к рабочему времени логистов", status = "navy", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              numericInput("workingHours", "Рабочих часов:", 8, min = 1, max = 100),
              reactableOutput(outputId = "averageTime1"),
              highchartOutput(outputId = "averageTimeByDayPlot1")
              
          ),
          
          
          box(title = "Исходные данные", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              reactableOutput(outputId = "averageTime_initial")
              
          )
          
        )
)