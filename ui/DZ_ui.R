DZ_tabName <- tabItem(tabName = "DZ", fluidRow(
  
  box(title = "Инструменты", status = "success", solidHeader = TRUE, width = 12,
      dateRangeInput(inputId = "date1",
                     label = "Период",
                     start = "2020-01-01",
                     end = Sys.Date(),
                     language = "ru",
                     weekstart = 1,
                     format = "dd.mm.yyyy"),
      downloadButton("downloadDZ", "Экспорт в Excel"),
      tags$style(type='text/css', "#downloadDZ {    margin-left: auto;
    margin-right: auto; display: block; width: 10em}")
  ),
  
  box(title = "Итог", status = "warning", solidHeader = TRUE,
      collapsible = TRUE, width= 6,
      h4("Сумма неоплаченных счетов: ", textOutput(outputId = "totalDZ")),
      h5("Дебиторская задолженность: ", textOutput(outputId = "totalPDZ")),
      h5("Просроченная дебиторская задолженность:", textOutput(outputId = "totalPDZ7")),
      h6("До 7 дней: ", textOutput(outputId = "totalPDZ1")),
      h6("От 8 до 15 дней: ", textOutput(outputId = "totalPDZ2")),
      h6("От 16 до 30 дней: ", textOutput(outputId = "totalPDZ3")),
      h6("От 31 до 60 дней: ", textOutput(outputId = "totalPDZ4")),
      h6("От 61 до 90 дней: ", textOutput(outputId = "totalPDZ5")),
      h6("Свыше 90 дней: ", textOutput(outputId = "totalPDZ6")),
      
      h5(textOutput(outputId = "periodDZ"))
  ),
  
  box(title = "Итог", status = "purple", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      highchartOutput(outputId = "dzPlot")),
  
  box(title = "Сводная таблица", status = "info", solidHeader = TRUE,
      collapsible = TRUE, width = 12, gt_output(outputId = "gtTable")),
  
  box(title = "Исходная таблица", status = "navy", solidHeader = TRUE,
      collapsible = TRUE, width = 12,
      reactableOutput(outputId = "dz"))
  
)
)