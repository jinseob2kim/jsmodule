# /R/FilePsInput.R

#' @title FilePsInput: Shiny module UI for propensity score analysis.
#' @description Provides a file input and UI outputs for options related to
#' propensity score matching.
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
#'       sidebarPanel(
#'         FilePsInput("datafile")
#'       ),
#'       mainPanel(
#'         tabsetPanel(
#'           type = "pills",
#'           tabPanel("Data", DTOutput("data")),
#'           tabPanel("Matching data", DTOutput("matdata")),
#'           tabPanel("Label", DTOutput("data_label"))
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     mat_info <- callModule(FilePs, "datafile")
#'
#'     output$data <- renderDT({ mat_info()$data })
#'     output$matdata <- renderDT({ mat_info()$matdata })
#'     output$data_label <- renderDT({ mat_info()$label })
#'   }
#'   shinyApp(ui, server)
#' }
#' @rdname FilePsInput
#' @export
#' @import shiny
FilePsInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat/dta)") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label, accept = c(".csv", ".xlsx", ".sav", ".sas7bdat", ".dta")),
    uiOutput(ns("factor")),
    uiOutput(ns("binary_check")),
    uiOutput(ns("binary_var")),
    uiOutput(ns("binary_val")),
    uiOutput(ns("ref_check")),
    uiOutput(ns("ref_var")),
    uiOutput(ns("ref_val")),
    uiOutput(ns("subset_check")),
    uiOutput(ns("subset_var")),
    uiOutput(ns("subset_val")),
    uiOutput(ns("group_ps")),
    uiOutput(ns("indep_ps")),
    uiOutput(ns("pcut")),
    uiOutput(ns("caliperps")),
    uiOutput(ns("ratio"))
  )
}


#' @title FilePs: Shiny module Server for propensity score analysis.
#' @description Server-side logic for propensity score analysis. It uses
#' `DataManager` for common data tasks and adds specific controls and
#' calculations for propensity score matching.
#' @param input,output,session Standard Shiny server parameters.
#' @param nfactor.limit An integer, the threshold for unique values.
#' @return A reactive expression returning a list with matched data and other
#' information.
#' @rdname FilePs
#' @export
#' @import shiny
#' @importFrom MatchIt matchit match.data
#' @import data.table
FilePs <- function(input, output, session, nfactor.limit = 20) {
  ## To remove NOTE.
  ID.pscal2828 <- pscore <- iptw <- subclass <- NULL

  ## Helper function for select input choices
  mklist <- function(varlist, vars) {
    lapply(
      varlist,
      function(x) {
        inter <- intersect(x, vars)
        if (length(inter) == 1) {
          inter <- c(inter, "")
        }
        return(inter)
      }
    )
  }

  # 1. Instantiate the common data manager
  data_manager <- DataManager$new(input, output, session, nfactor.limit = nfactor.limit)

  # 2. Get the processed data from the manager
  base_data_reactive <- data_manager$get_reactive_data()

  # 3. Render PS-specific UI elements
  observeEvent(base_data_reactive(), {
    processed_data <- base_data_reactive()
    req(processed_data)

    data_table <- processed_data$data
    data_var_struct <- list(variable = names(data_table))

    # Render Group variable UI
    output$group_ps <- renderUI({
      factor_vars <- names(data_table)[sapply(data_table, is.factor)]
      validate(need(length(factor_vars) > 0, "No categorical variables in data."))

      class01_factor <- sapply(data_table[, .SD, .SDcols = factor_vars], function(x) identical(levels(x), c("0", "1")))
      validate(need(any(class01_factor, na.rm=T), "No categorical variables coded as 0, 1 in data."))

      factor_01vars <- factor_vars[class01_factor]

      selected_var <- factor_01vars[grep("case", factor_01vars, ignore.case = TRUE)][1]
      if (is.na(selected_var)) {
        selected_var <- factor_01vars[1]
      }

      selectInput(session$ns("group_pscal"),
                  "Group variable for PS (must be 0/1)",
                  choices = mklist(data_var_struct, factor_01vars),
                  selected = selected_var)
    })

    # Render Independent variables UI
    # output$indep_ps <- renderUI({
    #   req(input$group_pscal)
    #
    #   # vars <- setdiff(names(data_table), c(processed_data$except_vars, input$var_subset, input$group_pscal))
    #   # varsIni <- vars
    #   #
    #   # if (!is.null(input$pcut_ps) && input$pcut_ps != "No") {
    #   #   pcut_val <- as.numeric(input$pcut_ps)
    #   #   sig_vars <- sapply(vars, function(v) {
    #   #     forms <- as.formula(paste0("`",input$group_pscal, "` ~ `", v, "`"))
    #   #     model <- tryCatch(glm(forms, data = data_table, family = binomial), error = function(e) NULL)
    #   #     if (is.null(model) || nrow(summary(model)$coefficients) < 2) return(FALSE)
    #   #     any(summary(model)$coefficients[-1, 4] <= pcut_val, na.rm=TRUE)
    #   #   })
    #   #   varsIni <- vars[sig_vars]
    #   # }
    #
    #   vars <- setdiff(names(data_table), c(processed_data$except_vars, input$var_subset, input$group_pscal))
    #   varsIni <- vars
    #
    #   if (!is.null(input$pcut_ps) && input$pcut_ps != "No") {
    #     pcut_val <- as.numeric(input$pcut_ps)
    #     sig_vars <- sapply(vars, function(v) {
    #       forms <- as.formula(paste0("`",input$group_pscal, "` ~ `", v, "`"))
    #       model <- tryCatch(glm(forms, data = data_table, family = binomial), error = function(e) NULL)
    #       if (is.null(model) || nrow(summary(model)$coefficients) < 2) return(FALSE)
    #       any(summary(model)$coefficients[-1, 4] <= pcut_val, na.rm=TRUE)
    #     })
    #     varsIni <- vars[sig_vars]
    #   }
    #
    #   selectInput(session$ns("indep_pscal"),
    #               "Independent variables for PS",
    #               choices = mklist(data_var_struct, vars),
    #               multiple = TRUE,
    #               selected = varsIni)
    # })
    # Render Independent variables UI
    output$indep_ps <- renderUI({
      req(input$group_pscal)

      vars <- setdiff(names(data_table), c(processed_data$except_vars, input$var_subset, input$group_pscal))

      # p-value cut 옵션 값에 따라 초기 선택 변수를 결정하는 로직
      if (!is.null(input$pcut_ps) && input$pcut_ps != "No") {
        # p-value cut을 사용하는 경우: 유의한 변수만 선택
        pcut_val <- as.numeric(input$pcut_ps)
        sig_vars <- sapply(vars, function(v) {
          forms <- as.formula(paste0("`", input$group_pscal, "` ~ `", v, "`"))
          model <- tryCatch(glm(forms, data = data_table, family = binomial), error = function(e) NULL)
          if (is.null(model) || nrow(summary(model)$coefficients) < 2) return(FALSE)
          any(summary(model)$coefficients[-1, 4] <= pcut_val, na.rm = TRUE)
        })
        varsIni <- vars[sig_vars]
      } else {
        # p-value cut을 사용하지 않는 경우 (기본값 "No" 또는 NULL):
        # 옛날 앱처럼 목록의 첫 번째 변수만 선택
        varsIni <- vars[1]
      }

      selectInput(session$ns("indep_pscal"),
                  "Independent variables for PS",
                  choices = mklist(data_var_struct, vars),
                  multiple = TRUE,
                  selected = varsIni)
    })
  })

  # Static UI elements that only need to be rendered once after file upload
  observeEvent(base_data_reactive(), {
    req(base_data_reactive())
    output$pcut <- renderUI({
      radioButtons(session$ns("pcut_ps"), "P-value cut for var selection",
                   choices = c("No", 0.05, 0.1, 0.2), selected = "No", inline = TRUE)
    })
    output$ratio <- renderUI({
      radioButtons(session$ns("ratio_ps"), "Case:control ratio",
                   choices = c("1:1" = 1, "1:2" = 2, "1:3" = 3, "1:4" = 4), selected = 1, inline = TRUE)
    })
    output$caliperps <- renderUI({
      sliderInput(session$ns("caliper"), "Caliper for matching (0=none)", value = 0, min = 0, max = 1, step = 0.01)
    })
  })


  # 4. Final reactive expression for matching
  mat.info <- eventReactive(c(input$indep_pscal, input$group_pscal, input$caliper, input$ratio_ps, base_data_reactive()), {
    processed_info <- base_data_reactive()
    req(processed_info, input$indep_pscal, input$group_pscal, input$ratio_ps, input$caliper)

    data <- data.table(processed_info$data)
    data[, ID.pscal2828 := .I]

    cols_to_check <- c(input$group_pscal, input$indep_pscal)
    complete_cases_idx <- which(complete.cases(data[, .SD, .SDcols = cols_to_check]))

    validate(need(length(complete_cases_idx) > 0, "No complete cases for matching analysis."))

    data.naomit <- data[complete_cases_idx]
    data.na <- data[-complete_cases_idx]
    if (nrow(data.na) > 0) {
      data.na[, `:=`(pscore = NA_real_, iptw = NA_real_)]
    }

    caliper_val <- if (input$caliper > 0) input$caliper else NULL

    forms <- as.formula(paste0("`", input$group_pscal, "` ~ ", paste0("`", input$indep_pscal, "`", collapse = "+")))

    m.out <- MatchIt::matchit(
      forms,
      data = as.data.frame(data.naomit),
      caliper = caliper_val,
      ratio = as.numeric(input$ratio_ps)
    )

    data.naomit[, `:=`(pscore = m.out$distance,
                       iptw = ifelse(get(input$group_pscal) == 1, 1 / m.out$distance, 1 / (1 - m.out$distance)))]


    # Add subclass info back to data.naomit for filtering
    data.naomit$subclass <- m.out$subclass

    wdata <- rbind(data.na, data.naomit, fill = TRUE)
    setorder(wdata, ID.pscal2828)

    # The final matched dataset should be based on valid subclasses from the matching result
    valid_subclasses <- m.out$subclass[!is.na(m.out$subclass)]
    matdata <- wdata[subclass %in% valid_subclasses]

    # Clean up columns
    wdata[, c("ID.pscal2828", "subclass") := NULL]
    matdata[, c("ID.pscal2828", "subclass") := NULL]


    updated_label <- rbind(processed_info$label,
                           data.table(variable = "pscore", class = "numeric", level = NA, var_label = "Propensity Score", val_label = NA),
                           data.table(variable = "iptw", class = "numeric", level = NA, var_label = "Inverse Probability of Treatment Weighting", val_label = NA),
                           fill=TRUE)

    return(list(
      data = wdata,
      matdata = matdata,
      data.label = updated_label,
      naomit = processed_info$naomit,
      group_var = input$group_pscal
    ))
  })

  return(mat.info)
}
