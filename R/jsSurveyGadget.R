#' @title jsSurveyGadget: Shiny Gadget of survey data analysis.
#' @description Shiny Gadget including Data, Label info, Table 1, svyglm, Basic plot
#' @param data data
#' @param nfactor.limit nlevels limit for categorical variables
#' @return Shiny Gadget including Data, Label info, Table 1, svyglm, Basic plot
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(survival)
#'  jsSurveyGadget(lung)
#'  }
#' }
#' @rdname jsSurveyGadget
#' @export
#' @importFrom GGally ggpairs
#' @importFrom stats as.formula binomial
#' @importFrom data.table data.table := .SD
#' @importFrom DT datatable %>% formatStyle styleEqual renderDT DTOutput
#' @importFrom shinycustomloader withLoader
#' @importFrom jstable opt.data opt.tb1 opt.tbreg
#' @importFrom survey svyglm
#' @import ggplot2
#' @import shiny

jsSurveyGadget <- function(data, nfactor.limit = 20) {

  change.vnlist = list(c(" ", "_"), c("=<", "_le_"), c("=>", "_ge_"), c("=", "_eq_"), c("\\(", "_open_"), c("\\)", "_close_"), c("%", "_percent_"), c("-", "_"), c("/", "_"),
                       c("\r\n", "_"), c(",", "_comma_"))

  out <- data.table(data, check.names = T)


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

  data.list <- list(data = out, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= nfactor.limit], factor_adds = add_vars)



  ui <- navbarPage("Survey data analysis",
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("factor"),
                                uiOutput("survey")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                )
                              )
                            )
                   ),
                   navbarMenu("Table 1",
                              tabPanel("Unweighted",
                                       sidebarLayout(
                                         sidebarPanel(
                                           tb1moduleUI("untb1")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("untable1"), type="html", loader="loader6"),
                                           wellPanel(
                                             h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                             h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                             h5("Categorical variables  are summarized with table")
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Weighted",
                                       sidebarLayout(
                                         sidebarPanel(
                                           tb1moduleUI("tb1")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("table1"), type="html", loader="loader6"),
                                           wellPanel(
                                             h5("Normal continuous variables  are summarized with Mean (SD) and complex survey regression"),
                                             h5("Non-normal continuous variables are summarized with median [IQR] and complex sampling rank test"),
                                             h5("Categorical variables  are summarized with table and svychisq test")
                                           )
                                         )
                                       )
                              )
                   ),
                   navbarMenu("Survey regression",
                              tabPanel("Linear",
                                       sidebarLayout(
                                         sidebarPanel(
                                           GEEModuleUI("linear")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Binomial",
                                       sidebarLayout(
                                         sidebarPanel(
                                           GEEModuleUI("logistic")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Cox model",
                                       sidebarLayout(
                                         sidebarPanel(
                                           coxUI("cox")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("coxtable"), type="html", loader="loader6")
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
                              ),
                              tabPanel("Kaplan-meier plot",
                                       sidebarLayout(
                                         sidebarPanel(
                                           kaplanUI("kaplan")
                                         ),
                                         mainPanel(
                                           withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                                           ggplotdownUI("kaplan")
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

    observeEvent(input$factor_vname, {
      output$survey <-  renderUI({
        conti_new <- setdiff(data.list$conti_original, input$factor_vname)
        #validate(
        #  need(length(conti_new) > 0, "No candidate variables to be weight.")
        #)

        candidate.weight <- c("wt", "weight", "Weight", "WEIGHT", "WEIGHTS", "Weights", "weights")
        selected.weight <- unlist(purrr::map(candidate.weight, ~grep(.x, conti_new)))
        selected.weight.final <- ifelse(length(selected.weight) > 0, conti_new[selected.weight[1]], "None")

        candidate.cluster <- c("psu", "id")
        selected.cluster <- unlist(purrr::map(candidate.cluster, ~grep(.x, names(data.list$data))))
        selected.cluster.final <- ifelse(length(selected.cluster) > 0, conti_new[selected.cluster[1]], "None")

        candidate.strata <- c("strata")
        selected.strata <- unlist(purrr::map(candidate.strata, ~grep(.x, names(data.list$data))))
        selected.strata.final <- ifelse(length(selected.strata) > 0, conti_new[selected.strata[1]], "None")


        tagList(
          h4(tags$strong("Survey design")),
          selectInput(session$ns("cluster_vname"), label = "Cluster ID",
                      choices = c("None", names(data.list$data)), multiple = F,
                      selected = selected.cluster.final),

          selectInput(session$ns("strata_vname"), label = "Strata",
                      choices = c("None", names(data.list$data)), multiple = F,
                      selected = selected.strata.final),

          selectInput(session$ns("weights_vname"), label = "Weights",
                      choices = c("None", conti_new), multiple = F,
                      selected = selected.weight.final)
        )
      })
    })


    data.info <- reactive({
      out <- data.list$data
      out[, (data.list$conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = data.list$conti_original]
      if (!is.null(input$factor_vname)){
        out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
      }

      if (input$cluster_vname == "None"){
        cluster.survey <- as.formula("~ 1")
      } else{
        cluster.survey <- as.formula(paste("~", input$cluster_vname))
        out <- out[!is.na(get(input$cluster_vname))]
      }

      if (input$strata_vname == "None"){
        strata.survey <- NULL
      } else{
        strata.survey <- as.formula(paste("~", input$strata_vname))
        out <- out[!is.na(get(input$strata_vname))]
      }

      if (input$weights_vname == "None"){
        weights.survey <- NULL
      } else{
        weights.survey <- as.formula(paste("~", input$weights_vname))
        out <- out[!is.na(get(input$weights_vname))]
      }

      surveydata <- survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out)

      out.label <- mk.lev(out)
      return(list(data = out, label = out.label, survey = surveydata))
    })

    data <- reactive(data.info()$data)
    data.label <- reactive(data.info()$label)
    design.survey <- reactive(data.info()$survey)

    output$data <- renderDT({
      datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
                options = c(jstable::opt.data("data"), list(scrollX = TRUE))
      )
    })


    output$data_label <- renderDT({
      datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = c(jstable::opt.data("label"), list(scrollX = TRUE))
      )
    })

    out_untb1 <- callModule(tb1module2, "untb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    output$untable1 <- renderDT({
      tb = out_untb1()$table
      cap = out_untb1()$caption
      out.tb1 = datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                          options = c(jstable::opt.tb1("tb1"),
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




    out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, design.survey = design.survey)

    output$table1 <- renderDT({
      tb = out_tb1()$table
      cap = out_tb1()$caption
      out.tb1 = datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                          options = c(jstable::opt.tb1("tb1"),
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

    out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, design.survey = design.survey, default.unires = F)

    output$lineartable <- renderDT({
      hide = which(colnames(out_linear()$table) == "sig")
      datatable(out_linear()$table, rownames=T, extensions = "Buttons", caption = out_linear()$caption,
                options = c(jstable::opt.tbreg(out_linear()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, design.survey = design.survey, default.unires = F)

    output$logistictable <- renderDT({
      hide = which(colnames(out_logistic()$table) == "sig")
      datatable(out_logistic()$table, rownames=T, extensions = "Buttons", caption = out_logistic()$caption,
                options = c(jstable::opt.tbreg(out_logistic()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, design.survey = design.survey, default.unires = F)

    output$coxtable <- renderDT({
      hide = which(colnames(out_cox()$table) == c("sig"))
      datatable(out_cox()$table, rownames=T, extensions= "Buttons", caption = out_cox()$caption,
                options = c(opt.tbreg(out_cox()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })


    out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL)

    output$ggpairs_plot <- renderPlot({
      print(out_ggpairs())
    })

    out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, design.survey = design.survey)

    output$kaplan_plot <- renderPlot({
      print(out_kaplan())
    })

  }



  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}



#' @title jsSurveydAddin: Rstudio addin of jsSurveyGadget
#' @description Rstudio addin of jsSurveyGadget
#' @return Rstudio addin of jsSurveyGadget
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rstudioapi]{rstudio-editors}}
#' @rdname jsSurveydAddin
#' @export
#' @importFrom rstudioapi getActiveDocumentContext


jsSurveydAddin <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # Set the default data to use based on the selection.
  dataString <- context$selection[[1]]$text
  data <- get(dataString, envir = .GlobalEnv)
  #viewer <- dialogViewer("Subset", width = 1000, height = 800)
  jsSurveyGadget(data, nfactor.limit = 20)
}
