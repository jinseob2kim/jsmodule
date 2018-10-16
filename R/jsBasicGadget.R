#' @title jsBasicGadget: Shiny Gadget of Basic Statistics in Medical Research.
#' @description Shiny Gadget including Data, Label info, Table 1, Regression(liear, logistic), Basic plot
#' @param data data
#' @return Shiny Gadget including Data, Label info, Table 1, Regression(liear, logistic), Basic plot
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(survival)
#'  jsBasicGadget(lung)
#'  }
#' }
#' @rdname jsBasicGadget
#' @export
#' @importFrom GGally ggpairs
#' @importFrom epiDisplay regress.display logistic.display
#' @importFrom stats as.formula binomial glm
#' @importFrom data.table data.table := .SD
#' @importFrom DT datatable %>% formatStyle styleEqual renderDT DTOutput
#' @importFrom shinycustomloader withLoader
#' @import ggplot2
#' @import shiny

jsBasicGadget <- function(data) {

  change.vnlist = list(c(" ", "_"), c("=<", "_le_"), c("=>", "_ge_"), c("=", "_eq_"), c("\\(", "_open_"), c("\\)", "_close_"), c("%", "_percent_"), c("-", "_"), c("/", "_"))

  out <- data.table(data)


  ## Initial variable name
  for (x in change.vnlist){
    names(out) <- gsub(x[1], x[2], names(out))
  }
  numstart.vnum <- suppressWarnings(sapply(names(out),function(x){!is.na(as.numeric(substr(x, 1,1)))}))
  names(out)[numstart.vnum] <- paste("n_", names(out)[numstart.vnum], sep = "")

  ## factor variable
  factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
  out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  conti_vars <- setdiff(names(out), factor_vars)
  nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_vars])
  #except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
  add_vars <- names(nclass)[nclass >= 1 &  nclass <= 5]

  data.list <- list(data = out, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= 20], factor_adds = add_vars)



  ui <- navbarPage("Basic statistics",
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("factor")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                )
                              )
                            )
                   ),
                   tabPanel("Table 1",
                            sidebarLayout(
                              sidebarPanel(
                                tb1moduleUI("tb1")
                              ),
                              mainPanel(
                                withLoader(DTOutput("table1"), type="html", loader="loader6"),
                                wellPanel(
                                  h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                  h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                  h5("Categorical variables  are summarized with table")
                                )
                              )
                            )

                   ),
                   navbarMenu("Regression",
                              tabPanel("Linear regression",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("linear")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Logistic regression",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("logistic")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                         )
                                       )
                              )

                   ),
                   navbarMenu("Plot",
                              tabPanel("Scatter plot",
                                       sidebarLayout(
                                         sidebarPanel(
                                           ggpairsModuleUI1("ggpairs")
                                         ),
                                         mainPanel(
                                           withLoader(plotOutput("ggpairs_plot"), type="html", loader="loader6"),
                                           ggpairsModuleUI2("ggpairs")
                                         )
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

    output$data <- renderDT({
      datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
                options = opt.data("data")
      )
    })


    output$data_label <- renderDT({
      datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = opt.data("label")
      )
    })




    out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL)

    output$table1 <- renderDT({
      tb = out_tb1()$table
      cap = out_tb1()$caption
      out.tb1 = datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                          options = c(opt.tb1("tb1"),
                                      list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                      ),
                                      list(scrollX = TRUE)
                          )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })

    out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL)

    output$lineartable <- renderDT({
      hide = which(colnames(out_linear()$table) == c("P(F-test)",  "sig"))
      datatable(out_linear()$table, rownames=T, extensions = "Buttons", caption = out_linear()$caption,
                options = c(opt.tbreg(out_linear()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            )
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL)

    output$logistictable <- renderDT({
      hide = which(colnames(out_logistic()$table) == c("P(F-test)",  "sig"))
      datatable(out_logistic()$table, rownames=T, extensions = "Buttons", caption = out_logistic()$caption,
                options = c(opt.tbreg(out_logistic()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            )
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })


    out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL)

    output$ggpairs_plot <- renderPlot({
      print(out_ggpairs())
    })
  }



  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}



#' @title jsBasicAddin: Rstudio addin of jsBasicGadget
#' @description Rstudio addin of jsBasicGadget
#' @return Rstudio addin of jsBasicGadget
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rstudioapi]{rstudio-editors}}
#' @rdname jsBasicAddin
#' @export
#' @importFrom rstudioapi getActiveDocumentContext


jsBasicAddin <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # Set the default data to use based on the selection.
  dataString <- context$selection[[1]]$text
  data <- get(dataString, envir = .GlobalEnv)
  #viewer <- dialogViewer("Subset", width = 1000, height = 800)
  jsBasicGadget(data)
}


