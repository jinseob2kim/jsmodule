jsPropensityGadget <- function(data) {


  out <- data.table(data, check.names = T)
  naCol <- names(out)[colSums(is.na(out)) > 0]
  out <- out[, .SD, .SDcols = -naCol]


  ## factor variable
  factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
  if (!is.null(factor_vars)){
    out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  }
  conti_vars <- setdiff(names(out), factor_vars)
  nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_vars])
  except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
  add_vars <- names(nclass)[nclass >= 1 &  nclass <= 5]

  data.list <- list(data = out, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= 20], factor_adds = add_vars, except_vars = except_vars)




  ui <- navbarPage("Propensity score",
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("factor"),
                                uiOutput("group_ps"),
                                uiOutput("indep_ps")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                            tabPanel("Matching data", withLoader(DTOutput("matdata"), type="html", loader="loader6")),
                                            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                ),
                                htmlOutput("naomit")
                              )
                            )
                   ),
                   tabPanel("Table 1",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("conti_vars")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Original", withLoader(DTOutput("tb1_original"), type="html", loader="loader6")),
                                            tabPanel("PS matching", withLoader(DTOutput("tb1_psmat"), type="html", loader="loader6")),
                                            tabPanel("IPTW", withLoader(DTOutput("tb1_iptw", width = "100%"), type="html", loader="loader6"))
                                ),
                                htmlOutput("naomit")
                              )
                            )
                   ),
                   tabPanel("Setting",
                            fluidPage(
                              wellPanel(
                                radioButtons("pcut_ps", label = "Default p-value cut for ps calculation",
                                             choices = c(0.05, 0.1, 0.2),
                                             selected = 0.1, inline =T)
                                )
                              )
                            )
  )

  server <- function(input, output, session) {

    output$factor <- renderUI({
      selectInput("factor_vname", label = "Additional categorical variables",
                  choices = data.list$factor_adds_list, multiple = T,
                  selected = data.list$factor_adds)
    })


    data.info <- reactive({
      out <- data.list$data
      out[, (data.list$conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = data.list$conti_original]
      if (!is.null(input$factor_vname)){
        out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
      }
      out.label <- mk.lev(out)
      return(list(data = out, label = out.label))
    })



    data <- reactive(data.info()$data)
    data.label <- reactive(data.info()$label)

    output$group_ps <- renderUI({
      factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
      nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])
      factor_2vars <- names(nclass_factor)[nclass_factor == 2]
      selectInput("group_pscal", label = "Group variable for PS calculation",
                  choices = factor_2vars, multiple = F,
                  selected = factor_2vars[1])
    })



    output$indep_ps <- renderUI({
      req(!is.null(input$group_pscal))
      vars <- setdiff(setdiff(names(data()), data.list$except_vars),  input$group_pscal)
      varsIni <- sapply(vars,
                        function(v){
                          forms <- as.formula(paste(input$group_pscal, "~", v))
                          coef <- summary(glm(forms, data = data(), family = binomial))$coefficients
                          sigOK <- !all(coef[-1, 4] > as.numeric(input$pcut_ps))
                          return(sigOK)
                        })
      tagList(
        selectInput("indep_pscal", label = "Group variable for PS calculation",
                    choices = vars, multiple = T,
                    selected = vars[varsIni])
      )
    })

    mat.info <- reactive({
      req(!is.null(input$group_pscal))
      forms <- as.formula(paste(input$group_pscal, " ~ ", paste(input$indep_pscal, collapse = "+"), sep=""))
      m.out <- matchit(forms, data = data())
      pscore <- m.out$distance
      iptw <- ifelse(m.out$treat == levels(m.out$treat)[2], 1/pscore,  1/(1-pscore))
      wdata <- cbind(data(), pscore, iptw)

      mdata <- match.data(m.out, distance = "pscore")
      return(list(data = wdata <- cbind(data(), pscore, iptw), matdata = mdata[, -grep("weights", names(mdata))]))
    })




    output$data <- renderDT({
      datatable(mat.info()$data, rownames=F, editable = F, extensions= "Buttons", caption = "Data",
                options = jstable::opt.data("data")
      )
    })

    output$matdata <- renderDT({
      datatable(mat.info()$matdata, rownames=F, editable = F, extensions= "Buttons", caption = "Matching data",
                options = jstable::opt.data("data")
      )
    })


    output$data_label <- renderDT({
      datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = jstable::opt.data("label")
      )
    })

    output$naomit <- renderText({
      if (length(naCol) == 0) {
        return("Data has <B>no</B> missing values.")
      } else{
        txt_miss <- paste(naCol, collapse = ", ")
        return(paste("Column <B>", txt_miss, "</B> are(is) excluded due to missing value.", sep = ""))
      }
    })

  }


  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}



#' @title jsPropensityAddin: Rstudio addin of jsPropensityGadget
#' @description Rstudio addin of jsPropensityGadget
#' @return Rstudio addin of jsPropensityGadget
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rstudioapi]{rstudio-editors}}
#' @rdname jsPropensityAddin
#' @export
#' @importFrom rstudioapi getActiveDocumentContext


jsPropensityAddin <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # Set the default data to use based on the selection.
  dataString <- context$selection[[1]]$text
  data <- get(dataString, envir = .GlobalEnv)
  #viewer <- dialogViewer("Subset", width = 1000, height = 800)
  jsPropensityGadget(data)
}


