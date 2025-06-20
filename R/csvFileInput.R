# /R/csvFileInput.R

#' @title csvFileInput: Shiny module UI for file upload.
#' @description Shiny module UI for file upload supporting csv, xlsx, sav,
#' sas7bdat, and dta formats. It provides UI outputs for various data
#' manipulation options.
#' @param id A string, the module id.
#' @param label A string, the label for the file input,
#' Default: 'Upload data (csv/xlsx/sav/sas7bdat/dta)'
#' @return A Shiny UI object.
#' @details This function only defines the UI. The corresponding server
#' function, `csvFile`, handles the logic.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(DT)
#'   library(jstable)
#'
#'   ui <- fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         csvFileInput("datafile")
#'       ),
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
#'     data_info <- callModule(csvFile, "datafile")
#'
#'     output$data <- renderDT({
#'       data_info()$data
#'     })
#'
#'     output$label <- renderDT({
#'       data_info()$label
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#' @rdname csvFileInput
#' @export
#' @import shiny
csvFileInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat/dta)") {
  ns <- NS(id)
  tagList(
    fileInput(
      inputId = ns("file"),
      label = label,
      accept = c(".csv", ".xlsx", ".sav", ".sas7bdat", ".dta")
    ),
    uiOutput(ns("factor")),
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

#' @title csvFile: Shiny module Server for file upload.
#' @description The server-side logic for the `csvFileInput` module. It uses
#' the `DataManager` R6 class to handle all data processing.
#' @param input,output,session Standard Shiny server parameters.
#' @param nfactor.limit An integer, the threshold for unique values to suggest
#' a numeric variable as categorical, Default: 20
#' @return A reactive expression that returns a list with two elements:
#' `data` (the processed data.table) and `label` (a data.table with variable
#' label information).
#' @rdname csvFileInput
#' @export
csvFile <- function(input, output, session, nfactor.limit = 20) {
  # Instantiate the data manager, which handles all the logic.
  data_manager <- DataManager$new(input, output, session, nfactor.limit = nfactor.limit)

  # Return the reactive data list from the manager.
  return(data_manager$get_reactive_data())
}
