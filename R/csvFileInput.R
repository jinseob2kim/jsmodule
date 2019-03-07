
#' @title csvFileInput: Shiny module UI for file upload.
#' @description Shiny module UI for file(csv or xlsx) upload.
#' @param id id
#' @param label label, Default: 'csv/xlsx/sav/sas7bdat/dta file'
#' @return Shiny module UI for file(csv or xlsx) upload.
#' @details Shiny module UI for file(csv or xlsx) upload.
#' @examples
#' library(shiny);library(DT);library(data.table);library(readxl);library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       csvFileInput("datafile")
#'     ),
#'     mainPanel(
#'       tabsetPanel(type = "pills",
#'                   tabPanel("Data", DTOutput("data")),
#'                   tabPanel("Label", DTOutput("data_label", width = "100%"))
#'                  )
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'   data <- callModule(csvFile, "datafile")
#'
#'   output$data <- renderDT({
#'     data()$data
#'   })
#'
#'   output$label <- renderDT({
#'     data()$label
#'   })
#'}
#' @rdname csvFileInput
#' @export
#' @import shiny

csvFileInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat/dta)") {

  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label),
    uiOutput(ns("factor")),
    uiOutput(ns("subset_check")),
    uiOutput(ns("subset_var")),
    uiOutput(ns("subset_val"))
  )
}



#' @title csvFile: Shiny module Server for file upload.
#' @description Shiny module Server for file(csv or xlsx) upload.
#' @param input input
#' @param output output
#' @param session session
#' @param nfactor.limit nfactor limit to include, Default: 20
#' @return Shiny module Server for file(csv or xlsx) upload.
#' @details Shiny module Server for file(csv or xlsx) upload.
#' @examples
#' library(shiny);library(DT);library(data.table);library(readxl);library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       csvFileInput("datafile")
#'     ),
#'     mainPanel(
#'       tabsetPanel(type = "pills",
#'                   tabPanel("Data", DTOutput("data")),
#'                   tabPanel("Label", DTOutput("data_label", width = "100%"))
#'                  )
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'   data <- callModule(csvFile, "datafile")
#'
#'   output$data <- renderDT({
#'     data()$data
#'   })
#'
#'   output$label <- renderDT({
#'     data()$label
#'   })
#'}
#' @rdname csvFile
#' @export
#' @import shiny
#' @importFrom data.table fread data.table .SD :=
#' @importFrom readxl read_excel
#' @importFrom jstable mk.lev
#' @importFrom haven read_sav read_sas read_dta

csvFile <- function(input, output, session, nfactor.limit = 20) {

  ## To remove NOTE.
  variable <- NULL

  # The selected file, if any
  userFile <- eventReactive(input$file, {
    # If no file is selected, don't do anything
    #validate(need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  #change.vnlist = list(c(" ", "_"), c("=<", "_le_"), c("=>", "_ge_"), c("=", "_eq_"), c("\\(", "_open_"), c("\\)", "_close_"), c("%", "_percent_"), c("-", "_"),  c("/", "_"),
  #                     c("\r\n", "_"), c(",", "_comma_")
  #)

  data <- eventReactive(input$file, {
    validate(need((grepl("csv", userFile()$name) == T) | (grepl("xlsx", userFile()$name) == T) | (grepl("sav", userFile()$name) == T) | (grepl("sas7bdat", userFile()$name) == T), message = "Please upload csv/xlsx/sav/sas7bdat file"))
    if (grepl("csv", userFile()$name) == T){
      out = data.table::fread(userFile()$datapath, check.names = F, integer64 = "double")
    } else if (grepl("xlsx", userFile()$name) == T){
      out = data.table::data.table(readxl::read_excel(userFile()$datapath), check.names = F, integer64 = "double")
    } else if (grepl("sav", userFile()$name) == T){
      out = data.table::data.table(haven::read_sav(userFile()$datapath), check.names = F, integer64 = "double")
    } else if (grepl("sas7bdat", userFile()$name) == T){
      out = data.table::data.table(haven::read_sas(userFile()$datapath), check.names = F, integer64 = "double")
    } else if (grepl("dta", userFile()$name) == T){
      out = data.table::data.table(haven::read_dta(userFile()$datapath), check.names = F, integer64 = "double")
    } else {
      stop("Not supported format.")
    }
    #for (x in change.vnlist){
    #  names(out) <- gsub(x[1], x[2], names(out))
    #}

    naCol <- names(out)[unlist(out[, lapply(.SD, function(x){all(is.na(x))})])]
    if (length(naCol) ==0){
      naomit = NULL
    } else{
      out <- out[, .SD, .SDcols = -naCol]
      naomit = paste("Column <B>", paste(naCol, collapse = ", "), "</B> are(is) excluded because it is empty.", sep = "")
    }

    out.old <- out
    name.old <- names(out.old)
    out <- data.table::data.table(out, check.names = T)
    name.new <- names(out)
    ref <- list(name.old = name.old, name.new = name.new)


    numstart.vnum <- suppressWarnings(sapply(names(out),function(x){!is.na(as.numeric(substr(x, 1,1)))}))
    names(out)[numstart.vnum] <- paste("n_", names(out)[numstart.vnum], sep = "")

    factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
    if (length(factor_vars) > 0){
      out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
    }
    conti_vars <- setdiff(names(out), factor_vars)
    nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}), .SDcols = conti_vars])
    #except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
    add_vars <- names(nclass)[nclass >= 2 &  nclass <= 5]
    #factor_vars_ini <- union(factor_vars, add_vars)
    return(list(data = out, factor_original = factor_vars, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= nfactor.limit], factor_adds = add_vars, ref = ref, naomit = naomit))
  })




  output$factor <- renderUI({
    selectInput(session$ns("factor_vname"), label = "Additional categorical variables",
                choices = data()$factor_adds_list, multiple = T,
                selected = data()$factor_adds)
  })

  observeEvent(c(data()$factor_original, input$factor_vname), {
    output$subset_check <- renderUI({
      checkboxInput(session$ns("check_subset"), "Subset data")
    })
  })

  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      #factor_subset <- c(data()$factor_original, input$factor_vname)

      #validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      #)

      tagList(
        selectInput(session$ns("var_subset"), "Subset variable",
                    choices = names(data()$data), multiple = F,
                    selected = names(data()$data)[1])
      )
    })

    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(input$var_subset)
      var.factor <- c(data()$factor_original, input$factor_vname)
      #var.conti <- setdiff(data()$conti_original, input$factor_vname)

      if (input$var_subset %in% var.factor){
        varlevel <- levels(as.factor(data()$data[[input$var_subset]]))
        selectInput(session$ns("val_subset"), "Subset value",
                    choices = varlevel, multiple = T,
                    selected = varlevel[1])
      } else{
        val <- stats::quantile(data()$data[[input$var_subset]], na.rm = T)
        sliderInput(session$ns("val_subset"), "Subset range",
                    min = val[1], max = val[5],
                    value = c(val[2], val[4]))
      }

    })
  })




  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  observeEvent(input$factor_vname, {

  })

  outdata <- reactive({
    out <- data()$data
    out[, (data()$conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = data()$conti_original]
    if (length(input$factor_vname) > 0){
      out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
    }

    ref <- data()$ref
    out.label <- mk.lev(out)
    for (vn in ref[["name.new"]]){
      w <- which(ref[["name.new"]] == vn)
      out.label[variable == vn, var_label := ref[["name.old"]][w]]
    }

    if (!is.null(input$check_subset)){
      if (input$check_subset){
        validate(
          need(length(input$var_subset) > 0 , "No variables for subsetting")
        )
        var.factor <- c(data()$factor_original, input$factor_vname)
        #var.conti <- setdiff(data()$conti_original, input$factor_vname)

        if (input$var_subset %in% var.factor){
          out <- out[get(input$var_subset) %in% input$val_subset]
          #var.factor <- c(data()$factor_original, input$factor_vname)
          out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
          out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
          data.table::setkey(out.label, "variable", "class", "level")
          data.table::setkey(out.label2, "variable", "class", "level")
          out.label <- out.label[out.label2]
        } else{
          out <- out[get(input$var_subset) >= input$val_subset[1] & get(input$var_subset) <= input$val_subset[2]]
          #var.factor <- c(data()$factor_original, input$factor_vname)
          out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
          out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
          data.table::setkey(out.label, "variable", "class", "level")
          data.table::setkey(out.label2, "variable", "class", "level")
          out.label <- out.label[out.label2]
        }

      }
    }
    return(list(data = out, label = out.label, naomit = data()$naomit))
  })



  # Return the reactive that yields the data frame
  return(outdata)
}

