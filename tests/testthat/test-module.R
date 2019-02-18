context("Test module")

app <- shinyApp(
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        csvFileInput("datafile")
      ),
      mainPanel(
        tabsetPanel(type = "pills",
                    tabPanel("Data", DTOutput("data")),
                    tabPanel("Label", DTOutput("data_label", width = "100%"))
        )
      )
    )
  ),

  server <- function(input, output, session) {
    data <- callModule(csvFile, "datafile")

    output$data <- renderDT({
      data()$data
    })
    output$label <- renderDT({
      data()$label
    })
  }
)

test_that("Run Modules", {

  expect_is(coxUI(1), "list")
  expect_warning(coxModule(1, 1, 1, 1, 1))
  expect_is(app, "shiny.appobj")

})




