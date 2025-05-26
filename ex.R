library(shiny)
library(DT)
library(data.table)
library(jstable)
library(ggplot2)
library(timeROC)
library(survIDINRI)
ui <- fluidPage(sidebarLayout(
  sidebarPanel(timerocUI("timeroc")),
  mainPanel(
    plotOutput("plot_timeroc"),
    ggplotdownUI("timeroc"),
    DTOutput("table_timeroc")
  )
))


server <- function(input, output, session) {
  data <- reactive({
    # mtcars 데이터를 data.table로 변환
    dt_data <- as.data.table(pbc)

    factor_vars <- names(dt_data)[sapply(dt_data, function(x){length(table(x))}) <= 6]
    dt_data[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]

    return(dt_data)
  })

  # 변경된 data() reactive를 기반으로 data.label 생성
  data.label <- reactive({
    jstable::mk.lev(data())
  })

  out_timeroc <- callModule(
    timerocModule,
    "timeroc",
    data = data,
    data_label = data.label,
    data_varStruct = NULL
  )

  observe({
    tb <- tryCatch(out_timeroc()$tb, error = function(e) NULL)
    print(tb)
  })

  output$plot_timeroc <- renderPlot({
   {
      print(out_timeroc()$plot)
    }
  })


  output$table_timeroc <- renderDT({
    datatable(
      out_timeroc()$tb,
      rownames = F,
      editable = F,
      extensions = "Buttons",
      caption = "ROC results",
      options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
    )
  })


}

shinyApp(ui, server)
