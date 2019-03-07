
#' @title FilePsInput: Shiny module UI for file upload for propensity score matching.
#' @description Shiny module UI for file upload for propensity score matching.
#' @param id id
#' @param label label, Default: 'csv/xlsx/sav/sas7bdat file'
#' @return Shiny module UI for file upload for propensity score matching.
#' @details Shiny module UI for file upload for propensity score matching.
#' @examples
#' library(shiny);library(DT);library(data.table);library(readxl);library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       FilePsInput("datafile")
#'     ),
#'     mainPanel(
#'       tabsetPanel(type = "pills",
#'                   tabPanel("Data", DTOutput("data")),
#'                   tabPanel("Matching data", DTOutput("matdata")),
#'                   tabPanel("Label", DTOutput("data_label", width = "100%"))
#'                  )
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'   mat.info <- callModule(FilePs, "datafile")
#'
#'   output$data <- renderDT({
#'     mat.info()$data
#'   })
#'
#'   output$matdata <- renderDT({
#'     mat.info()$matdata
#'   })
#'
#'   output$label <- renderDT({
#'     mat.info()$label
#'   })
#'}
#' @rdname FilePsInput
#' @export
#' @import shiny

FilePsInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat/dta)") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label),
    uiOutput(ns("factor")),
    uiOutput(ns("subset_check")),
    uiOutput(ns("subset_var")),
    uiOutput(ns("subset_val")),
    uiOutput(ns("group_ps")),
    uiOutput(ns("indep_ps")),
    uiOutput(ns("pcut"))
  )
}





#' @title FilePs: Shiny module Server for file upload for propensity score matching.
#' @description Shiny module Server for file upload for propensity score matching.
#' @param input input
#' @param output output
#' @param session session
#' @param nfactor.limit nfactor limit to include, Default: 20
#' @return Shiny module Server for file upload for propensity score matching.
#' @details Shiny module Server for file upload for propensity score matching.
#' @examples
#' library(shiny);library(DT);library(data.table);library(readxl);library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       FilePsInput("datafile")
#'     ),
#'     mainPanel(
#'       tabsetPanel(type = "pills",
#'                   tabPanel("Data", DTOutput("data")),
#'                   tabPanel("Matching data", DTOutput("matdata")),
#'                   tabPanel("Label", DTOutput("data_label", width = "100%"))
#'                  )
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'   mat.info <- callModule(FilePs, "datafile")
#'
#'   output$data <- renderDT({
#'     mat.info()$data
#'   })
#'
#'   output$matdata <- renderDT({
#'     mat.info()$matdata
#'   })
#'
#'   output$label <- renderDT({
#'     mat.info()$label
#'   })
#'}
#' @rdname FilePs
#' @export
#' @import shiny
#' @importFrom data.table fread data.table .SD :=
#' @importFrom readxl read_excel
#' @importFrom jstable mk.lev
#' @importFrom haven read_sav read_sas
#' @importFrom MatchIt matchit match.data

FilePs <- function(input, output, session, nfactor.limit = 20) {

  ## To remove NOTE.
  variable <- NULL

  # The selected file, if any
  userFile <- eventReactive(input$file, {
    # If no file is selected, don't do anything
    #validate(need(input$file, message = FALSE))
    input$file
  })

  mklist <- function(varlist, vars){
    lapply(varlist,
           function(x){
             inter <- intersect(x, vars)
             if (length(inter) == 1){
               inter <- c(inter, "")
             }
             return(inter)
           })
  }





  data.info <- eventReactive(input$file, {
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
    } else{
      stop("Not supported format.")
    }


    out.old <- out
    name.old <- names(out.old)
    out <- data.table::data.table(out, check.names = T)
    name.new <- names(out)
    ref <- list(name.old = name.old, name.new = name.new)



    naCol <- names(out)[colSums(is.na(out)) > 0]
    out <- out[, .SD, .SDcols = -naCol]

    data_varStruct = list(variable = names(out))

    factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
    if (!is.null(factor_vars) & length(factor_vars) > 0){
      out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
    }

    conti_vars <- setdiff(names(out), factor_vars)
    nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_vars])
    factor_adds_list = mklist(data_varStruct, names(nclass)[(nclass <= 20) & (nclass < nrow(out))])

    except_vars <- names(nclass)[ nclass== 1 | nclass >= nfactor.limit]
    add_vars <- names(nclass)[nclass >= 1 &  nclass <= 5]
    #factor_vars_ini <- union(factor_vars, add_vars)
    return(list(data = out, data_varStruct = data_varStruct, factor_original = factor_vars,
                conti_original = conti_vars, factor_adds_list = factor_adds_list,
                factor_adds = add_vars, naCol = naCol, except_vars = except_vars, ref = ref)
           )
  })

  output$pcut <- renderUI({
    if (is.null(input$file)){return(NULL)}
    radioButtons(session$ns("pcut_ps"), label = "Default p-value cut for ps calculation",
                 choices = c(0.05, 0.1, 0.2),
                 selected = 0.1, inline =T)
  })



  output$factor <- renderUI({
    selectInput(session$ns("factor_vname"), label = "Additional categorical variables",
                choices = data.info()$factor_adds_list, multiple = T,
                selected = data.info()$factor_adds)
  })

  observeEvent(c(data.info()$factor_original, input$factor_vname), {
    output$subset_check <- renderUI({
      checkboxInput(session$ns("check_subset"), "Subset data")
    })
  })

  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      #factor_subset <- c(data.info()$factor_original, input$factor_vname)

      #validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      #)

      tagList(
        selectInput(session$ns("var_subset"), "Subset variable",
                    choices = names(data.info()$data), multiple = F,
                    selected = names(data.info()$data)[1])
      )
    })

    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(input$var_subset)
      var.factor <- c(data.info()$factor_original, input$factor_vname)

      if (input$var_subset %in% var.factor){
        varlevel <- levels(as.factor(data.info()$data[[input$var_subset]]))
        selectInput(session$ns("val_subset"), "Subset value",
                    choices = varlevel, multiple = T,
                    selected = varlevel[1])
      } else{
        val <- stats::quantile(data.info()$data[[input$var_subset]], na.rm = T)
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



  data <- reactive({
    out <- data.info()$data
    out[, (data.info()$conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = data.info()$conti_original]
    if (!is.null(input$factor_vname)){
      out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
    }
    if (!is.null(input$check_subset)){
      if (input$check_subset){
        validate(
          need(length(input$var_subset) > 0 , "No variables for subsetting")
        )

        var.factor <- c(data.info()$factor_original, input$factor_vname)
        #var.conti <- setdiff(data()$conti_original, input$factor_vname)

        if (input$var_subset %in% var.factor){
          out <- out[get(input$var_subset) %in% input$val_subset]
          #var.factor <- c(data()$factor_original, input$factor_vname)
          out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]

        } else{
          out <- out[get(input$var_subset) >= input$val_subset[1] & get(input$var_subset) <= input$val_subset[2]]
          #var.factor <- c(data()$factor_original, input$factor_vname)
          out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
        }

      }
    }
    return(out)
    })

  data.label <- eventReactive(data(), {
    labeldata <- mk.lev(data())
    for (vn in data.info()$ref[["name.new"]]){
      w <- which(data.info()$ref[["name.new"]] == vn)
      labeldata[variable ==vn, var_label := data.info()$ref[["name.old"]][w]]
    }

    return(labeldata)
    })

  output$group_ps <- renderUI({
    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    validate(
      need(!is.null(factor_vars) & length(factor_vars) > 0, "No categorical variables in data")
    )

    class01_factor <- unlist(data()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}), .SDcols = factor_vars])
    #nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])
    #factor_2vars <- names(nclass_factor)[nclass_factor == 2]


    validate(
      need(!is.null(class01_factor), "No categorical variables coded as 0, 1 in data")
    )

    factor_01vars <- factor_vars[class01_factor]
    factor_01vars_case_small <- factor_01vars[unlist(sapply(factor_01vars, function(x){diff(table(data()[[x]])) <= 0}))]

    selectInput(session$ns("group_pscal"), label = "Group variable for PS calculation (0, 1 coding)",
                choices = mklist(data.info()$data_varStruct, factor_01vars_case_small), multiple = F,
                selected = factor_01vars_case_small[1])
  })

  observeEvent(input$group_pscal , {
    output$indep_ps <- renderUI({
      if (is.null(input$group_pscal)){
        return(NULL)
      }
      validate(
        need(length(input$group_pscal) != 0, "No group variables in data")
      )

      vars <- setdiff(setdiff(names(data()), data.info()$except_vars),  c(input$var_subset, input$group_pscal))
      varsIni <- sapply(vars,
                        function(v){
                          forms <- as.formula(paste(input$group_pscal, "~", v))
                          coef <- summary(glm(forms, data = data(), family = binomial))$coefficients
                          sigOK <- !all(coef[-1, 4] > as.numeric(input$pcut_ps))
                          return(sigOK)
                        })
      tagList(
        selectInput(session$ns("indep_pscal"), label = "Independent variables for PS calculation",
                    choices = mklist(data.info()$data_varStruct, vars), multiple = T,
                    selected = vars[varsIni])
      )
    })
  })



  mat.info <- eventReactive(input$indep_pscal, {
    if (is.null(input$group_pscal) | is.null(input$indep_pscal)){
      return(NULL)
    }

    forms <- as.formula(paste(input$group_pscal, " ~ ", paste(input$indep_pscal, collapse = "+"), sep=""))
    m.out <- MatchIt::matchit(forms, data = data())
    pscore <- m.out$distance
    iptw <- ifelse(m.out$treat == levels(m.out$treat)[2], 1/pscore,  1/(1-pscore))
    wdata <- cbind(data(), pscore, iptw)

    mdata <- MatchIt::match.data(m.out, distance = "pscore")
    return(list(data = wdata, matdata = mdata[, -grep("weights", names(mdata))]))
  })

  naomit <- eventReactive(data.info(), {
    if (length(data.info()$naCol) == 0) {
      return("Data has <B>no</B> missing values.")
    } else{
      txt_miss <- paste(data.info()$naCol, collapse = ", ")
      return(paste("Column <B>", txt_miss, "</B> are(is) excluded due to missing value.", sep = ""))
    }
  })

  outdata <- reactive({
    list(data = mat.info()$data, matdata = mat.info()$matdata, data.label = data.label(), naomit = naomit(), group_var = input$group_pscal)
  })




  # Return the reactive that yields the data frame
  return(outdata)
}

