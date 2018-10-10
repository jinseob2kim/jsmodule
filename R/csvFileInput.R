
#' @title csvFileInput: Shiny module UI for file upload.
#' @description Shiny module UI for file(csv or xlsx) upload.
#' @param id id
#' @param label label, Default: 'csv/xlsx file'
#' @return Shiny UI
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(shiny)
#'  ui <- fluidPage(
#'    sidebarLayout(
#'      sidebarPanel(
#'        csvFileInput("datafile", "Upload data (csv/xlsx format)")
#'      ),
#'      mainPanel(
#'        dataTableOutput("table")
#'      )
#'    )
#'  )

#'  server <- function(input, output, session) {
#'    datafile <- callModule(csvFile, "datafile")
#'
#'    output$table <- renderDataTable({
#'      datafile()
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'  }
#' }
#' @rdname csvFileInput
#' @export
#' @import shiny

csvFileInput <- function(id, label = "csv/xlsx file") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label)
  )
}



#' @title csvFile: Shiny module Server for file upload.
#' @description Shiny module Server for file(csv or xlsx) upload.
#' @param input input
#' @param output output
#' @param session session
#' @return server
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(shiny)
#'  ui <- fluidPage(
#'    sidebarLayout(
#'      sidebarPanel(
#'        csvFileInput("datafile", "Upload data (csv/xlsx format)")
#'      ),
#'      mainPanel(
#'        dataTableOutput("table")
#'      )
#'    )
#'  )

#'  server <- function(input, output, session) {
#'    datafile <- callModule(csvFile, "datafile")
#'
#'    output$table <- renderDataTable({
#'      datafile()
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'  }
#' }
#' @rdname csvFile
#' @export
#' @import shiny
#' @importFrom data.table fread data.table
#' @importFrom readxl read_excel

csvFile <- function(input, output, session) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  dataframe <- reactive({
    validate(need((grepl("csv", userFile()$name) == T) | (grepl("xlsx", userFile()$name) == T), message = "Please upload csv/xlsx file"))
    if (grepl("csv", userFile()$name) == T){
      fread(userFile()$datapath)
    } else{
      data.table(read_excel(userFile()$datapath))
    }
  })


  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}
