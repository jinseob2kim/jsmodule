
#' @title FilePsInput: Shiny module UI for file upload for propensity score matching.
#' @description Shiny module UI for file upload for propensity score matching.
#' @param id id
#' @param label label, Default: 'csv/xlsx/sav/sas7bdat file'
#' @return Shiny UI
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(shiny);library(DT);library(data.table);library(readxl);library(jstable);library(haven)
#'  ui <- fluidPage(
#'    sidebarLayout(
#'      sidebarPanel(
#'        FilePsInput("datafile")
#'      ),
#'      mainPanel(
#'        tabsetPanel(type = "pills",
#'            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
#'            tabPanel("Matching data", withLoader(DTOutput("matdata"), type="html", loader="loader6")),
#'            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
#'        ),
#'        htmlOutput("naomit")
#'      )
#'    )
#'  )
#'
#'  server <- function(input, output, session) {
#'    mat.info <- callModule(FilePs, "datafile")
#'
#'    output$data <- renderDT({
#'      mat.info()$data
#'    })
#'
#'    output$matdata <- renderDT({
#'      mat.info()$matdata
#'    })
#'
#'    output$label <- renderDT({
#'      mat.info()$data.label
#'    })
#'    output$naomit <- renderText({
#'      mat.info()$naomit
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'  }
#' }
#' @rdname FilePsInput
#' @export
#' @import shiny

FilePsInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat)") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label),
    uiOutput(ns("factor")),
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
#' @return server
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(shiny);library(DT);library(data.table);library(readxl);library(jstable);library(haven)
#'  ui <- fluidPage(
#'    sidebarLayout(
#'      sidebarPanel(
#'        FilePsInput("datafile")
#'      ),
#'      mainPanel(
#'        tabsetPanel(type = "pills",
#'            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
#'            tabPanel("Matching data", withLoader(DTOutput("matdata"), type="html", loader="loader6")),
#'            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
#'        ),
#'        htmlOutput("naomit")
#'      )
#'    )
#'  )
#'
#'  server <- function(input, output, session) {
#'    mat.info <- callModule(FilePs, "datafile")
#'
#'    output$data <- renderDT({
#'      mat.info()$data
#'    })
#'
#'    output$matdata <- renderDT({
#'      mat.info()$matdata
#'    })
#'
#'    output$label <- renderDT({
#'      mat.info()$data.label
#'    })
#'
#'    output$naomit <- renderText({
#'      mat.info()$naomit
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'  }
#' }
#' @rdname FilePs
#' @export
#' @import shiny
#' @importFrom data.table fread data.table .SD :=
#' @importFrom readxl read_excel
#' @importFrom jstable mk.lev
#' @importFrom haven read_sav read_sas
#' @importFrom MatchIt matchit match.data

FilePs <- function(input, output, session) {
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
      out = data.table::fread(userFile()$datapath, check.names = T)
    } else if (grepl("xlsx", userFile()$name) == T){
      out = data.table::data.table(readxl::read_excel(userFile()$datapath), check.names = T)
    } else if (grepl("sav", userFile()$name) == T){
      out = data.table::data.table(haven::read_sav(userFile()$datapath), check.names = T)
    } else if (grepl("sas7bdat", userFile()$name) == T){
      out = data.table::data.table(haven::read_sas(userFile()$datapath), check.names = T)
    }



    naCol <- names(out)[colSums(is.na(out)) > 0]
    out <- out[, .SD, .SDcols = -naCol]

    data_varStruct = list(variable = names(out))

    factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
    if (!is.null(factor_vars)){
      out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
    }

    conti_vars <- setdiff(names(out), factor_vars)
    nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_vars])
    factor_adds_list = mklist(data_varStruct, names(nclass)[(nclass <= 20) & (nclass < nrow(out))])

    except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
    add_vars <- names(nclass)[nclass >= 1 &  nclass <= 5]
    #factor_vars_ini <- union(factor_vars, add_vars)
    return(list(data = out, data_varStruct = data_varStruct,
                conti_original = conti_vars, factor_adds_list = factor_adds_list,
                factor_adds = add_vars, naCol = naCol, except_vars = except_vars)
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
    return(out)
    })

  data.label <- eventReactive(data(), {
    return(mk.lev(data()))
    })

  output$group_ps <- renderUI({
    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    validate(
      need(!is.null(factor_vars), "No categorical variables in data")
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

      vars <- setdiff(setdiff(names(data()), data.info()$except_vars),  input$group_pscal)
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
    list(data = mat.info()$data, matdata = mat.info()$matdata, data.label = data.label(), naomit = naomit())
  })




  # Return the reactive that yields the data frame
  return(outdata)
}

