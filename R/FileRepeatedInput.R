# /R/FileRepeatedInput.R

#' @title FileRepeatedInput: UI for repeated measures analysis.
#' @description File upload UI for repeated measure analysis.
#' @param id A string, the module id.
#' @param label A string, the label for the file input.
#' @return A Shiny UI object.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(DT)
#'   library(jstable)
#'
#'   ui <- fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(FileRepeatedInput("datafile")),
#'       mainPanel(
#'         tabsetPanel(
#'           type = "pills",
#'           tabPanel("Data", DTOutput("data")),
#'           tabPanel("Label", DTOutput("data_label"))
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     data_info <- callModule(FileRepeated, "datafile")
#'     output$data <- renderDT({
#'       data_info()$data
#'     })
#'     output$label <- renderDT({
#'       data_info()$label
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#' @rdname FileRepeatedInput
#' @export
#' @import shiny
FileRepeatedInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat/dta)") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label),
    uiOutput(ns("factor")),
    uiOutput(ns("repeated")),
    uiOutput(ns("binary_check")),
    uiOutput(ns("binary_var")),
    uiOutput(ns("binary_val")),
    uiOutput(ns("ref_check")),
    uiOutput(ns("ref_var")),
    uiOutput(ns("ref_val")),
    uiOutput(ns("subset_check")),
    uiOutput(ns("subset_var")),
    uiOutput(ns("subset_val"))
  )
}

#' @title FileRepeated: Server for repeated measures analysis.
#' @description Server module for repeated measures analysis. It uses
#' `DataManager` and adds a control for selecting the repeated measures variable.
#' @param input,output,session Standard Shiny server parameters.
#' @param nfactor.limit An integer, the threshold for unique values.
#' @return A reactive list with the processed `data`, `label`, and `id.gee`.
#' @rdname FileRepeated
#' @export
FileRepeated <- function(input, output, session, nfactor.limit = 20) {
  # 1. Instantiate the common data manager
  data_manager <- DataManager$new(input, output, session, nfactor.limit = nfactor.limit)

  # 2. Get the processed data from the manager
  base_data_reactive <- data_manager$get_reactive_data()

  # 3. Render the specific UI for repeated measures
  output$repeated <- renderUI({
    processed_info <- base_data_reactive()
    req(processed_info)
    selectInput(session$ns("repeated_vname"),
      "Repeated measure variable (ID)",
      choices = names(processed_info$data),
      selected = names(processed_info$data)[1]
    )
  })

  # 4. Create the final reactive data object
  outdata <- reactive({
    processed_info <- base_data_reactive()
    req(processed_info, input$repeated_vname)

    out <- data.table::copy(processed_info$data)

    # Filter out NA values in the repeated measure ID and order by it
    out <- out[!is.na(get(input$repeated_vname))][order(get(input$repeated_vname))]

    return(list(
      data = out,
      label = processed_info$label,
      naomit = processed_info$naomit,
      id.gee = input$repeated_vname
    ))
  })

  return(outdata)
}
