# /R/FileSurveyInput.R

#' @title FileSurveyInput: UI for survey data analysis.
#' @description File upload UI for survey data analysis, with controls for
#' survey design elements.
#' @param id A string, the module id.
#' @param label A string, the label for the file input.
#' @return A Shiny UI object.
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(DT)
#'   library(jstable)
#'   library(survey)
#'
#'   ui <- fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(FileSurveyInput("datafile")),
#'       mainPanel(
#'         h4("Survey object details:"),
#'         verbatimTextOutput("survey_summary"),
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
#'     data_info <- callModule(FileSurvey, "datafile")
#'     output$data <- renderDT({
#'       data_info()$data
#'     })
#'     output$label <- renderDT({
#'       data_info()$label
#'     })
#'     output$survey_summary <- renderPrint({
#'       print(data_info()$survey)
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#' @rdname FileSurveyInput
#' @export
#' @import shiny
FileSurveyInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat/dta)") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label),
    uiOutput(ns("factor")),
    uiOutput(ns("survey")),
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

#' @title FileSurvey: Server for survey data analysis.
#' @description Server module for survey data analysis. It uses `DataManager`
#' and adds controls and logic for creating a `survey.design` object.
#' @param input,output,session Standard Shiny server parameters.
#' @param nfactor.limit An integer, the threshold for unique values.
#' @return A reactive list with `data`, `label`, `naomit`, and the `survey` object.
#' @rdname FileSurveyInput
#' @export
#' @import shiny
#' @importFrom survey svydesign
#' @importFrom purrr map
FileSurvey <- function(input, output, session, nfactor.limit = 20) {
  # 1. Instantiate the common data manager
  data_manager <- DataManager$new(input, output, session, nfactor.limit = nfactor.limit)

  # 2. Get the processed data from the manager
  base_data_reactive <- data_manager$get_reactive_data()

  # 3. Render specific UI for survey design
  output$survey <- renderUI({
    processed_info <- base_data_reactive()
    req(processed_info)

    all_vars <- names(processed_info$data)
    conti_new <- setdiff(all_vars, names(processed_info$data)[sapply(processed_info$data, is.factor)])

    # Heuristics to find default survey variables
    selected_weight <- all_vars[unlist(purrr::map(c("wt", "weight"), ~ grep(.x, all_vars, ignore.case = TRUE)))][1]
    selected_cluster <- all_vars[unlist(purrr::map(c("psu", "id"), ~ grep(.x, all_vars, ignore.case = TRUE)))][1]
    selected_strata <- all_vars[unlist(purrr::map(c("strata"), ~ grep(.x, all_vars, ignore.case = TRUE)))][1]

    tagList(
      h4(tags$strong("Survey Design")),
      selectInput(session$ns("cluster_vname"), "Cluster ID",
        choices = c("None", all_vars), selected = ifelse(is.na(selected_cluster), "None", selected_cluster)
      ),
      selectInput(session$ns("strata_vname"), "Strata",
        choices = c("None", all_vars), selected = ifelse(is.na(selected_strata), "None", selected_strata)
      ),
      selectInput(session$ns("weights_vname"), "Weights",
        choices = c("None", conti_new), selected = ifelse(is.na(selected_weight), "None", selected_weight)
      )
    )
  })

  # 4. Create final reactive data object including the survey design
  outdata <- reactive({
    processed_info <- base_data_reactive()
    req(processed_info, input$cluster_vname, input$strata_vname, input$weights_vname)

    out <- data.table::copy(processed_info$data)

    cluster_formula <- if (input$cluster_vname == "None") ~1 else as.formula(paste("~", input$cluster_vname))
    strata_formula <- if (input$strata_vname == "None") NULL else as.formula(paste("~", input$strata_vname))
    weights_formula <- if (input$weights_vname == "None") NULL else as.formula(paste("~", input$weights_vname))

    # Remove rows with NAs in survey design variables
    if (input$cluster_vname != "None") out <- out[!is.na(get(input$cluster_vname))]
    if (!is.null(input$strata_vname) && input$strata_vname != "None") out <- out[!is.na(get(input$strata_vname))]
    if (!is.null(input$weights_vname) && input$weights_vname != "None") out <- out[!is.na(get(input$weights_vname))]

    validate(need(nrow(out) > 0, "No data remains after removing NAs in survey design variables."))

    survey_design <- tryCatch(
      {
        survey::svydesign(
          id = cluster_formula,
          strata = strata_formula,
          weights = weights_formula,
          data = out
        )
      },
      error = function(e) {
        # Fallback for nested designs if single PSU in strata error occurs
        survey::svydesign(
          id = cluster_formula,
          strata = strata_formula,
          weights = weights_formula,
          data = out,
          nest = TRUE
        )
      }
    )

    return(list(
      data = out,
      label = processed_info$label,
      naomit = processed_info$naomit,
      survey = survey_design
    ))
  })

  return(outdata)
}
