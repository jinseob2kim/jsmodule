#' @title jsSurveyGadget: Shiny Gadget of survey data analysis.
#' @description Shiny Gadget including Data, Label info, Table 1, svyglm, Basic plot
#' @param data data
#' @param nfactor.limit nlevels limit for categorical variables
#' @return Shiny Gadget including Data, Label info, Table 1, svyglm, Basic plot
#' @details Shiny Gadget including Data, Label info, Table 1, svyglm, Basic plot
#' @examples
#' if(interactive()){
#'  jsSurveyGadget(mtcars)
#'  }
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
  requireNamespace("survival")
  requireNamespace("survC1")
  options(survey.lonely.psu = "certainty")

  ## To remove NOTE.
  val_label <- BinaryGroupRandom <- variable <- NULL

  out <- data.table(data, check.names = F)
  name.old <- names(out)
  out <- data.table(data, check.names = T)
  name.new <- names(out)
  #ref <- data.table(name.old = name.old, name.new = name.new);setkey(ref, name.new)
  ref <- list(name.old = name.old, name.new = name.new)

  ## factor variable
  factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
  out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  conti_vars <- setdiff(names(out), factor_vars)
  nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_vars])
  #except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
  add_vars <- names(nclass)[nclass >= 1 &  nclass <= 5]

  data.list <- list(data = out, factor_original = factor_vars, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= nfactor.limit], factor_adds = add_vars)



  ui <- navbarPage("Survey data analysis",
                   tabPanel("Data", icon = icon("table"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("factor"),
                                uiOutput("survey"),
                                uiOutput("binary_check"),
                                uiOutput("binary_var"),
                                uiOutput("binary_val"),
                                uiOutput("subset_check"),
                                uiOutput("subset_var"),
                                uiOutput("subset_val")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                )
                              )
                            )
                   ),
                   tabPanel("Table 1", icon = icon("percentage"),
                            sidebarLayout(
                              sidebarPanel(
                                tb1moduleUI("tb1")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Unweighted",
                                                     withLoader(DTOutput("untable1"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("Weighted",
                                                     withLoader(DTOutput("table1"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and complex survey regression"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR or min,max] and complex sampling rank test"),
                                                       h5("Categorical variables  are summarized with table and svychisq test")
                                                     )
                                            )
                                )

                              )
                            )
                   ),
                   navbarMenu("Survey regression", icon = icon("list-alt"),
                              tabPanel("Linear",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("linear")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Binomial",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("logistic")
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
                   navbarMenu("Plot", icon = icon("bar-chart-o"),
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
                                           optionUI("kaplan"),
                                           withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                                           ggplotdownUI("kaplan")
                                         )
                                       )
                              )

                   ),
                   navbarMenu("ROC analysis", icon = icon("check"),
                              tabPanel("ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           rocUI("roc")
                                         ),
                                         mainPanel(
                                           withLoader(plotOutput("plot_roc"), type="html", loader="loader6"),
                                           ggplotdownUI("roc"),
                                           withLoader(DTOutput("table_roc"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Time-dependent ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           timerocUI("timeroc")
                                         ),
                                         mainPanel(
                                           withLoader(plotOutput("plot_timeroc"), type="html", loader="loader6"),
                                           ggplotdownUI("timeroc"),
                                           withLoader(DTOutput("table_timeroc"), type="html", loader="loader6")
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

    observeEvent(c(data.list$factor_original, input$factor_vname, input$repeated_vname, input$cluster_vname, input$strata_vname, input$weights_vname), {
      output$binary_check <- renderUI({
        checkboxInput(session$ns("check_binary"), "Make binary variables")
      })

      output$subset_check <- renderUI({
        checkboxInput("check_subset", "Subset data")
      })

      var.conti <- setdiff(names(data.list$data), c(data.list$factor_original, input$factor_vname, input$repeated_vname, input$cluster_vname, input$strata_vname, input$weights_vname))
      output$binary_var <- renderUI({
        req(input$check_binary == T)
        selectInput(session$ns("var_binary"), "Variables to dichotomize",
                    choices = var.conti, multiple = T,
                    selected = var.conti[1])
      })

      output$binary_val <- renderUI({
        req(input$check_binary == T)
        req(length(input$var_binary) > 0)
        outUI <- tagList()
        for (v in seq_along(input$var_binary)){
          med <- stats::quantile(data.list$data[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = T)
          outUI[[v]] <- splitLayout(cellWidths = c("25%", "75%"),
                                    selectInput(session$ns(paste0("con_binary", v)), paste0("Define reference:"),
                                                choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"
                                    ),
                                    numericInput(session$ns(paste0("cut_binary", v)), input$var_binary[[v]],
                                                 value = med[2], min = med[1], max = med[3]
                                    )
          )

        }
        outUI

      })
    })




    observeEvent(input$check_subset, {
      output$subset_var <- renderUI({
        req(input$check_subset == T)
        #factor_subset <- setdiff(c(data.list$factor_original, input$factor_vname), c(input$repeated_vname, input$strata_vname, input$weights_vname))

        #validate(
        #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
        #)

        tagList(
          selectInput("var_subset", "Subset variables",
                      choices = names(data.list$data), multiple = T,
                      selected = names(data.list$data)[1])
        )
      })

      output$subset_val <- renderUI({
        req(input$check_subset == T)
        req(input$var_subset)
        var.factor <- c(data.list$factor_original, input$factor_vname)

        outUI <- tagList()

        for (v in seq_along(input$var_subset)){
          if (input$var_subset[[v]] %in% var.factor){
            varlevel <- levels(as.factor(data.list$data[[input$var_subset[[v]]]]))
            outUI[[v]] <- selectInput(session$ns(paste0("val_subset", v)), paste0("Subset value: ", input$var_subset[[v]]),
                                      choices = varlevel, multiple = T,
                                      selected = varlevel[1])
          } else{
            val <- stats::quantile(data.list$data[[input$var_subset[[v]]]], na.rm = T)
            outUI[[v]] <- sliderInput(session$ns(paste0("val_subset", v)), paste0("Subset range: ", input$var_subset[[v]]),
                                      min = val[1], max = val[5],
                                      value = c(val[2], val[4]))
          }

        }
        outUI
      })
    })


    data.info <- reactive({
      req(!is.null(input$check_binary))
      out <- data.table::data.table(data.list$data)
      out[, (data.list$conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = data.list$conti_original]
      if (!is.null(input$factor_vname)){
        out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
      }

      req(input$cluster_vname)
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

      #surveydata <- survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out)
      #surveydata <- tryCatch(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out),
      #                       error = function(e){return(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out, nest = T))})

      out.label <- mk.lev(out)

      if (!is.null(input$check_binary) & input$check_binary){
        validate(
          need(length(input$var_binary) > 0 , "No variables to dichotomize")
        )
        sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
        names(sym.ineq) <- sym.ineq[4:1]
        sym.ineq2 <- c("le", "ge", "l", "g")
        names(sym.ineq2) <- sym.ineq
        for (v in seq_along(input$var_binary)){
          req(input[[paste0("con_binary", v)]])
          req(input[[paste0("cut_binary", v)]])
          if (input[[paste0("con_binary", v)]] == "\u2264"){
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) <= input[[paste0("cut_binary", v)]]))]

          } else if (input[[paste0("con_binary", v)]] == "\u2265"){
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) >= input[[paste0("cut_binary", v)]]))]
          } else if (input[[paste0("con_binary", v)]] == "\u003c"){
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) < input[[paste0("cut_binary", v)]]))]
          } else{
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) > input[[paste0("cut_binary", v)]]))]
          }
          cn.new <- paste0(input$var_binary[[v]], "_group_", sym.ineq2[input[[paste0("con_binary", v)]]], input[[paste0("cut_binary", v)]])
          data.table::setnames(out, "BinaryGroupRandom", cn.new)

          label.binary <- mk.lev(out[, .SD, .SDcols = cn.new])
          label.binary[, var_label := paste0(input$var_binary[[v]], "_group")]
          label.binary[, val_label := paste0(c(input[[paste0("con_binary", v)]], sym.ineq[input[[paste0("con_binary", v)]]]), " ", input[[paste0("cut_binary", v)]])]

          out.label <- rbind(out.label, label.binary)
        }
        #surveydata <- tryCatch(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out),
        #                       error = function(e){return(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out, nest = T))})

      }


      if (!is.null(input$check_subset)){
        if (input$check_subset){
          validate(
            need(length(input$var_subset) > 0 , "No variables for subsetting"),
            need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting")
          )
          var.factor <- c(data.list$factor_original, input$factor_vname)

          for (v in seq_along(input$var_subset)){
            if (input$var_subset[[v]] %in% var.factor){
              out <- out[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
              #surveydata <- survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out)
              #surveydata <- tryCatch(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out),
              #                       error = function(e){return(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out, nest = T))})
              #var.factor <- c(data()$factor_original, input$factor_vname)
              out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
              out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
              data.table::setkey(out.label, "variable", "class", "level")
              data.table::setkey(out.label2, "variable", "class", "level")
              out.label <- out.label[out.label2]
            } else{
              out <- out[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
              #surveydata <- survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out)
              #surveydata <- tryCatch(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out),
              #                       error = function(e){return(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out, nest = T))})

              #var.factor <- c(data()$factor_original, input$factor_vname)
              out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
              out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
              data.table::setkey(out.label, "variable", "class", "level")
              data.table::setkey(out.label2, "variable", "class", "level")
              out.label <- out.label[out.label2]
            }
          }

        }
      }

      #out.label[, var_label := ref[out.label$variable, name.old]]
      for (vn in ref[["name.new"]]){
        w <- which(ref[["name.new"]] == vn)
        out.label[variable == vn, var_label := ref[["name.old"]][w]]
      }

      surveydata <- tryCatch(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out),
                             error = function(e){return(survey::svydesign(id = cluster.survey, strata = strata.survey, weights = weights.survey, data = out, nest = T))})


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

    out_untb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, showAllLevels = T)
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




    out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, design.survey = design.survey, showAllLevels = T)

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


    out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$ggpairs_plot <- renderPlot({
      print(out_ggpairs())
    })

    out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, design.survey = design.survey)

    output$kaplan_plot <- renderPlot({
      print(out_kaplan())
    })


    out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$plot_roc <- renderPlot({
      print(out_roc()$plot)
    })

    output$table_roc <- renderDT({
      datatable(out_roc()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })



    out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$plot_timeroc <- renderPlot({
      print(out_timeroc()$plot)
    })

    output$table_timeroc <- renderDT({
      datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("timeroctable"), list(scrollX = TRUE)))
    })

  }



  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}



#' @title jsSurveyAddin: Rstudio addin of jsSurveyGadget
#' @description Rstudio addin of jsSurveyGadget
#' @return Rstudio addin of jsSurveyGadget
#' @details Rstudio addin of jsSurveyGadget
#' @examples
#' if(interactive()){
#'  jsSurveydAddin()
#'  }
#' @seealso
#'  \code{\link[rstudioapi]{rstudio-editors}}
#' @rdname jsSurveyAddin
#' @export
#' @importFrom rstudioapi getActiveDocumentContext


jsSurveyAddin <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # Set the default data to use based on the selection.
  dataString <- context$selection[[1]]$text
  data <- get(dataString, envir = .GlobalEnv)
  #viewer <- dialogViewer("Subset", width = 1000, height = 800)
  jsSurveyGadget(data, nfactor.limit = 20)
}




#' @title jsSurveyExtAddin: RStudio Addin for survey data analysis with external data.
#' @description RStudio Addin for survey data analysis with external csv/xlsx/sas7bdat/sav/dta file.
#' @param nfactor.limit nlevels limit for categorical variables, Default: 20
#' @param max.filesize Maximum file size to upload (MB), Default: 2048 (2 GB)
#' @return RStudio Addin for survey data analysis with external data.
#' @details RStudio Addin for survey data analysis with external csv/xlsx/sas7bdat/sav/dta file.
#' @examples
#' if(interactive()){
#'  jsSurveyExtAddin()
#'  }
#' @seealso
#'  \code{\link[data.table]{fwrite}}
#'  \code{\link[jstable]{opt.tb1}},\code{\link[jstable]{opt.tbreg}}
#' @rdname jsSurveyExtAddin
#' @export
#' @importFrom data.table fwrite
#' @importFrom jstable opt.tb1 opt.tbreg
#' @importFrom DT datatable %>% formatStyle styleEqual renderDT DTOutput
#' @importFrom shinycustomloader withLoader
#' @importFrom jstable opt.data opt.tb1 opt.tbreg
#' @importFrom utils data
#' @import shiny


jsSurveyExtAddin <- function(nfactor.limit = 20, max.filesize = 2048){

  data.example <- utils::data("nhanes", package = "survey")
  options(shiny.maxRequestSize = max.filesize * 1024^2, survey.lonely.psu = "certainty")

  ui <- navbarPage("Survey data analysis",
                   tabPanel("Data", icon = icon("table"),
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("import"),
                                downloadButton("downloadData", "Example data")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                ),
                                htmlOutput("naomit")

                              )
                            )
                   ),
                   tabPanel("Table 1", icon = icon("percentage"),
                            sidebarLayout(
                              sidebarPanel(
                                tb1moduleUI("tb1")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Unweighted",
                                                     withLoader(DTOutput("untable1"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("Weighted",
                                                     withLoader(DTOutput("table1"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and complex survey regression"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and complex sampling rank test"),
                                                       h5("Categorical variables  are summarized with table and svychisq test")
                                                     )
                                            )
                                )

                              )
                            )
                   ),
                   navbarMenu("Survey regression", icon = icon("list-alt"),
                              tabPanel("Linear",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("linear")
                                         ),
                                         mainPanel(
                                           withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Binomial",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("logistic")
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
                   navbarMenu("Plot", icon = icon("bar-chart-o"),
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
                                           optionUI("kaplan"),
                                           withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                                           ggplotdownUI("kaplan")
                                         )
                                       )
                              )

                   ),
                   navbarMenu("ROC analysis", icon = icon("check"),
                              tabPanel("ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           rocUI("roc")
                                         ),
                                         mainPanel(
                                           withLoader(plotOutput("plot_roc"), type="html", loader="loader6"),
                                           ggplotdownUI("roc"),
                                           withLoader(DTOutput("table_roc"), type="html", loader="loader6")
                                         )
                                       )
                              ),
                              tabPanel("Time-dependent ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           timerocUI("timeroc")
                                         ),
                                         mainPanel(
                                           withLoader(plotOutput("plot_timeroc"), type="html", loader="loader6"),
                                           ggplotdownUI("timeroc"),
                                           withLoader(DTOutput("table_timeroc"), type="html", loader="loader6")
                                         )
                                       )
                              )
                   )
  )




  server <- function(input, output, session) {


    output$downloadData <- downloadHandler(
      filename = function() {
        paste("example_survey", ".csv", sep = "")
      },
      content = function(file) {
        data.table::fwrite(get(data.example), file)
      }
    )

    output$import <- renderUI({
      FileSurveyInput("datafile")

    })

    data.info <- callModule(FileSurvey, "datafile", nfactor.limit = nfactor.limit)
    data <- reactive(data.info()$data)
    data.label <- reactive(data.info()$label)
    id.weight.survey <- reactive(data.info()$id.weight.survey)
    design.survey <- reactive(data.info()$survey)

    output$data <- renderDT({
      datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
                options = c(opt.data("data"), list(scrollX = TRUE))
      )
    })


    output$data_label <- renderDT({
      datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = c(opt.data("label"), list(scrollX = TRUE))
      )
    })

    output$naomit <- renderText({
      data.info()$naomit
    })


    out_untb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
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

    out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$table1 <- renderDT({
      tb = out_tb1()$table
      cap = out_tb1()$caption
      out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
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

    out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, default.unires = F, nfactor.limit = nfactor.limit)

    output$lineartable <- renderDT({
      hide = which(colnames(out_linear()$table) == "sig")
      datatable(out_linear()$table, rownames=T, extensions= "Buttons", caption = out_linear()$caption,
                options = c(opt.tbreg(out_linear()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, default.unires = F, nfactor.limit = nfactor.limit)

    output$logistictable <- renderDT({
      hide = which(colnames(out_logistic()$table) == "sig")
      datatable(out_logistic()$table, rownames=T, extensions= "Buttons", caption = out_logistic()$caption,
                options = c(opt.tbreg(out_logistic()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, default.unires = F, nfactor.limit = nfactor.limit)

    output$coxtable <- renderDT({
      hide = which(colnames(out_cox()$table) == c("sig"))
      datatable(out_cox()$table, rownames=T, extensions= "Buttons", caption = out_cox()$caption,
                options = c(opt.tbreg(out_cox()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })


    out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$ggpairs_plot <- renderPlot({
      print(out_ggpairs())
    })

    out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$kaplan_plot <- renderPlot({
      print(out_kaplan())
    })

    out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$plot_roc <- renderPlot({
      print(out_roc()$plot)
    })

    output$table_roc <- renderDT({
      datatable(out_roc()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$plot_timeroc <- renderPlot({
      print(out_timeroc()$plot)
    })

    output$table_timeroc <- renderDT({
      datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

  }

  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)


  }
