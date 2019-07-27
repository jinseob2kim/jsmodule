
#' @title jsPropensityGadget: Shiny Gadget for propensity score analysis.
#' @description Shiny Gadget including original/matching/IPTW data, Label info, Table 1, Cox model, Basic/kaplan-meier plot.
#' @param data data
#' @param nfactor.limit nlevels limit for categorical variables, Default: 20
#' @return Shiny Gadget including original/matching/IPTW data, Label info, Table 1, Cox model, Basic/kaplan-meier plot.
#' @details Shiny Gadget including original/matching/IPTW data, Label info, Table 1, Cox model, Basic/kaplan-meier plot.
#' @examples
#' if(interactive()){
#'  jsPropensityGadget(mtcars)
#'  }
#' @seealso
#'  \code{\link[data.table]{data.table}}
#'  \code{\link[MatchIt]{matchit}},\code{\link[MatchIt]{match.data}}
#'  \code{\link[jstable]{cox2.display}},\code{\link[jstable]{svycox.display}}
#'  \code{\link[survival]{survfit}},\code{\link[survival]{coxph}},\code{\link[survival]{Surv}}
#'  \code{\link[jskm]{jskm}},\code{\link[jskm]{svyjskm}}
#'  \code{\link[ggplot2]{ggsave}}
#'  \code{\link[survey]{svykm}}
#' @rdname jsPropensityGadget
#' @export
#' @importFrom data.table data.table
#' @importFrom MatchIt matchit match.data
#' @importFrom jstable cox2.display svycox.display
#' @importFrom survival survfit
#' @importFrom jskm jskm svyjskm
#' @importFrom ggplot2 ggsave
#' @importFrom survey svykm
#' @importFrom purrr map_lgl
#' @importFrom stats model.frame

jsPropensityGadget <- function(data, nfactor.limit = 20){

  requireNamespace("survival")
  requireNamespace("survC1")

  ## To remove NOTE.
  level <- val_label <- variable <- NULL

  ## Data label
  out.old <- data.table::data.table(data)
  name.old <- names(out.old)
  out <- data.table::data.table(data, check.names = T)
  name.new <- names(out)
  ref <- list(name.old = name.old, name.new = name.new)

  data_varStruct1 = list(variable = names(out))


  ## Vars
  naCol <- names(out)[colSums(is.na(out)) > 0]
  out <- out[, .SD, .SDcols = -naCol]

  factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
  if (!is.null(factor_vars) & length(factor_vars) > 0){
    out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  }

  factor_original <- factor_vars
  conti_original <- setdiff(names(out), factor_vars)
  nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_original])
  factor_adds_list = mklist(data_varStruct1, names(nclass)[nclass <= nfactor.limit])

  except_vars <- names(nclass)[ nclass== 1 | nclass >= nfactor.limit]
  factor_adds <- names(nclass)[nclass >= 1 &  nclass <= 5]





  ui <- navbarPage("Propensity score analysis",
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("factor"),
                                uiOutput("subset_check"),
                                uiOutput("subset_var"),
                                uiOutput("subset_val"),
                                uiOutput("group_ps"),
                                uiOutput("indep_ps"),
                                uiOutput("pcut")

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
                                tb1moduleUI("tb1")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Original",
                                                     withLoader(DTOutput("table1_original"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("Matching",
                                                     withLoader(DTOutput("table1_ps"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("IPTW",
                                                     withLoader(DTOutput("table1_iptw"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and complex survey regression"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and complex sampling rank test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            )
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(DTOutput("linear_original"), type="html", loader="loader6"),
                                                                br(),
                                                                uiOutput("warning_linear_original")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(DTOutput("linear_ps"), type="html", loader="loader6"),
                                                                br(),
                                                                uiOutput("warning_linear_ps")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(DTOutput("linear_iptw"), type="html", loader="loader6")
                                                       )
                                           )

                                         )
                                       )
                              ),
                              tabPanel("Logistic regression",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("logistic")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(DTOutput("logistic_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(DTOutput("logistic_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(DTOutput("logistic_iptw"), type="html", loader="loader6")
                                                       )
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Cox model",
                                       sidebarLayout(
                                         sidebarPanel(
                                           coxUI("cox")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(DTOutput("cox_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(DTOutput("cox_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(DTOutput("cox_iptw"), type="html", loader="loader6")
                                                       )
                                           )
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("ggpairs_plot_original"), type="html", loader="loader6")

                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("ggpairs_plot_ps"), type="html", loader="loader6")
                                                       )

                                           ),
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("kaplan_plot_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("kaplan_plot_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(plotOutput("kaplan_plot_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("kaplan")
                                         )
                                       )
                              )
                   ),
                   navbarMenu("ROC analysis",
                              tabPanel("ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           rocUI("roc")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("plot_roc_original"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("plot_roc_ps"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(plotOutput("plot_roc_iptw"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("roc")
                                         )
                                       )
                              ),
                              tabPanel("Time-dependent ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           timerocUI("timeroc")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("plot_timeroc_original"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("plot_timeroc_ps"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(plotOutput("plot_timeroc_iptw"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("timeroc")
                                         )
                                       )
                              )
                   )


  )

  server <- function(input, output, session) {

    output$pcut <- renderUI({
      radioButtons("pcut_ps", label = "Default p-value cut for ps calculation",
                   choices = c(0.05, 0.1, 0.2),
                   selected = 0.1, inline =T)
    })

    output$factor <- renderUI({
      selectInput("factor_vname", label = "Additional categorical variables",
                  choices = factor_adds_list, multiple = T,
                  selected = factor_adds)
    })

    observeEvent(c(factor_original, input$factor_vname), {
      output$subset_check <- renderUI({
        checkboxInput("check_subset", "Subset data")
      })
    })

    observeEvent(input$check_subset, {
      output$subset_var <- renderUI({
        req(input$check_subset == T)
        #factor_subset <- c(factor_original, input$factor_vname)

        #validate(
        #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
        #)

        tagList(
          selectInput("var_subset", "Subset variable",
                      choices = names(out), multiple = F,
                      selected = names(out)[1])
        )
      })

      output$subset_val <- renderUI({
        req(input$check_subset == T)
        req(input$var_subset)
        var.factor <- c(factor_original, input$factor_vname)

        if (input$var_subset %in% var.factor){
          varlevel <- levels(as.factor(out[[input$var_subset]]))
          selectInput("val_subset", "Subset value",
                      choices = varlevel, multiple = T,
                      selected = varlevel[1])
        } else{
          val <- stats::quantile(out[[input$var_subset]], na.rm = T)
          sliderInput("val_subset", "Subset range",
                      min = val[1], max = val[5],
                      value = c(val[2], val[4]))
        }
      })
    })



    data.info <- reactive({
      out1 <- out
      out1[, (conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = conti_original]
      if (!is.null(input$factor_vname) & length(input$factor_vname) > 0){
        out1[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
      }

      out.label <- mk.lev(out1)
      for (vn in ref[["name.new"]]){
        w <- which(ref[["name.new"]] == vn)
        out.label[variable == vn, var_label := ref[["name.old"]][w]]
      }


      if (!is.null(input$check_subset)){
        if (input$check_subset){
          validate(
            need(length(input$var_subset) > 0 , "No variable for subsetting")
          )
          var.factor <- c(factor_original, input$factor_vname)

          if (input$var_subset %in% var.factor){
            out1 <- out1[get(input$var_subset) %in% input$val_subset]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
            out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
            data.table::setkey(out.label, "variable", "class", "level")
            data.table::setkey(out.label2, "variable", "class", "level")
            out.label <- out.label[out.label2]

          } else{
            out1 <- out1[get(input$var_subset) >= input$val_subset[1] & get(input$var_subset) <= input$val_subset[2]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
            out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
            data.table::setkey(out.label, "variable", "class", "level")
            data.table::setkey(out.label2, "variable", "class", "level")
            out.label <- out.label[out.label2]
          }

        }
      }

      return(list(data = out1, label = out.label))
    })



    output$group_ps <- renderUI({
      factor_vars <- names(data.info()$data)[data.info()$data[, lapply(.SD, class) %in% c("factor", "character")]]
      validate(
        need(!is.null(factor_vars) & length(factor_vars) > 0, "No categorical variables in data")
      )

      class01_factor <- unlist(data.info()$data[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}), .SDcols = factor_vars])
      #nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])
      #factor_2vars <- names(nclass_factor)[nclass_factor == 2]


      validate(
        need(!is.null(class01_factor), "No categorical variables coded as 0, 1 in data")
      )

      factor_01vars <- factor_vars[class01_factor]
      factor_01vars_case_small <- factor_01vars[unlist(sapply(factor_01vars, function(x){diff(table(data.info()$data[[x]])) <= 0}))]

      validate(
        need(length(factor_01vars_case_small) > 0, "No candidate group variable for PS calculation")
      )


      selectInput("group_pscal", label = "Group variable for PS calculation (0, 1 coding)",
                  choices = mklist(data_varStruct1, factor_01vars_case_small), multiple = F,
                  selected = factor_01vars_case_small[1])
    })

    observeEvent(input$group_pscal , {
      output$indep_ps <- renderUI({
        if (is.null(input$group_pscal)){
          return(NULL)
        }
        validate(
          need(length(input$group_pscal) > 0, "No group variables in data")
        )

        vars <- setdiff(setdiff(names(data.info()$data), except_vars),  c(input$var_subset, input$group_pscal))
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$group_pscal, "~", v))
                            coef <- tryCatch(summary(glm(forms, data = data.info()$data, family = binomial))$coefficients, error = function(e){return(NULL)})
                            sigOK <- !all(coef[-1, 4] > as.numeric(input$pcut_ps))
                            return(sigOK)
                          })
        tagList(
          selectInput("indep_pscal", label = "Independent variables for PS calculation",
                      choices = mklist(data_varStruct1, vars), multiple = T,
                      selected = vars[varsIni])
        )
      })
    })

    mat.info <- eventReactive(input$indep_pscal, {
      if (is.null(input$group_pscal) | is.null(input$indep_pscal)){
        return(NULL)
      }

      forms <- as.formula(paste(input$group_pscal, " ~ ", paste(input$indep_pscal, collapse = "+"), sep=""))
      m.out <- MatchIt::matchit(forms, data = data.info()$data)
      pscore <- m.out$distance
      iptw <- ifelse(m.out$treat == levels(m.out$treat)[2], 1/pscore,  1/(1-pscore))
      wdata <- cbind(data.info()$data, pscore, iptw)

      mdata <- MatchIt::match.data(m.out, distance = "pscore")
      return(list(data = wdata, matdata = mdata[, -grep("weights", names(mdata))]))
    })



    output$data <- renderDT({
      datatable(mat.info()$data, rownames=F, editable = F, extensions= "Buttons", caption = "Data with ps, iptw",
                options = c(opt.data("data"), list(scrollX = TRUE))
      )
    })

    output$matdata <- renderDT({
      datatable(mat.info()$matdata, rownames=F, editable = F, extensions= "Buttons", caption = "Matching data",
                options = c(opt.data("matching_data"), list(scrollX = TRUE))
      )
    })

    output$data_label <- renderDT({
      datatable(data.info()$label, rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = c(opt.data("data_label"), list(scrollX = TRUE))
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


    ## tb1
    data <- reactive({
      mat.info()$data[, .SD, .SDcols = -c("iptw")]
    })
    matdata <- reactive(data.table::data.table(mat.info()$matdata))
    data.label <- reactive(data.info()$label)
    #data_varStruct <- reactive(list(variable = names(mat.info()$matdata)))
    design.survey <- reactive(survey::svydesign(ids = ~ 1, data = mat.info()$data, weights = ~ iptw))


    tb1_original <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, design.survey = NULL, nfactor.limit = nfactor.limit)
    tb1_ps <- callModule(tb1module2, "tb1", data = matdata, data_label = data.label, data_varStruct = NULL, design.survey = NULL, nfactor.limit = nfactor.limit)
    tb1_iptw <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$table1_original <- renderDT({
      tb <- tb1_original()$table
      cap <- tb1_original()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })

    output$table1_ps <- renderDT({
      tb <- tb1_ps()$table
      cap <- tb1_ps()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })

    output$table1_iptw <- renderDT({
      tb <- tb1_iptw()$table
      cap <- tb1_iptw()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })


    ## Regression

    out_linear_original <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_linear_ps <- callModule(regressModule2, "linear", data = matdata, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_linear_iptw <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$linear_original <- renderDT({
      hide <- which(colnames(out_linear_original()$table) == "sig")
      datatable(out_linear_original()$table, rownames=T, extensions= "Buttons", caption = out_linear_original()$caption,
                options = c(opt.tbreg(out_linear_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$warning_linear_original <- renderText({
      paste("<b>", out_linear_original()$warning, "</b>")
    })

    output$linear_ps <- renderDT({
      hide <- which(colnames(out_linear_ps()$table) == "sig")
      datatable(out_linear_ps()$table, rownames=T, extensions= "Buttons", caption = out_linear_ps()$caption,
                options = c(opt.tbreg(out_linear_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$warning_linear_ps <- renderText({
      paste("<b>", out_linear_ps()$warning, "</b>")
    })

    output$linear_iptw <- renderDT({
      hide <- which(colnames(out_linear_iptw()$table) == "sig")
      datatable(out_linear_iptw()$table, rownames=T, extensions= "Buttons", caption = out_linear_iptw()$caption,
                options = c(opt.tbreg(out_linear_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })


    ## Logistic

    out_logistic_original <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_logistic_ps <- callModule(logisticModule2, "logistic", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_logistic_iptw <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$logistic_original <- renderDT({
      hide = which(colnames(out_logistic_original()$table) == "sig")
      datatable(out_logistic_original()$table, rownames=T, extensions= "Buttons", caption = out_logistic_original()$caption,
                options = c(opt.tbreg(out_logistic_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$logistic_ps <- renderDT({
      hide = which(colnames(out_logistic_ps()$table) == "sig")
      datatable(out_logistic_ps()$table, rownames=T, extensions= "Buttons", caption = out_logistic_ps()$caption,
                options = c(opt.tbreg(out_logistic_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$logistic_iptw <- renderDT({
      hide = which(colnames(out_logistic_iptw()$table) == "sig")
      datatable(out_logistic_iptw()$table, rownames=T, extensions= "Buttons", caption = out_logistic_iptw()$caption,
                options = c(opt.tbreg(out_logistic_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })


    ## Cox

    out_cox_original <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_cox_ps <- callModule(coxModule, "cox", data = matdata, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_cox_iptw <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$cox_original <- renderDT({
      hide = which(colnames(out_cox_original()$table) == c("sig"))
      datatable(out_cox_original()$table, rownames=T, extensions= "Buttons", caption = out_cox_original()$caption,
                options = c(opt.tbreg(out_cox_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$cox_ps <- renderDT({
      hide = which(colnames(out_cox_ps()$table) == c("sig"))
      datatable(out_cox_ps()$table, rownames=T, extensions= "Buttons", caption = out_cox_ps()$caption,
                options = c(opt.tbreg(out_cox_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$cox_iptw <- renderDT({
      hide = which(colnames(out_cox_iptw()$table) == c("sig"))
      datatable(out_cox_iptw()$table, rownames=T, extensions= "Buttons", caption = out_cox_iptw()$caption,
                options = c(opt.tbreg(out_cox_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    ## ggpairs

    out_ggpairs_original <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_ggpairs_ps <- callModule(ggpairsModule2, "ggpairs", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$ggpairs_plot_original <- renderPlot({
      print(out_ggpairs_original())
    })

    output$ggpairs_plot_ps <- renderPlot({
      print(out_ggpairs_ps())
    })


    ## Kaplan

    out_kaplan_original <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_kaplan_ps <- callModule(kaplanModule, "kaplan", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_kaplan_iptw <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$kaplan_plot_original <- renderPlot({
      print(out_kaplan_original())
    })

    output$kaplan_plot_ps <- renderPlot({
      print(out_kaplan_ps())
    })

    output$kaplan_plot_iptw <- renderPlot({
      print(out_kaplan_iptw())
    })


    ## ROC

    out_roc_original <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_roc_ps <- callModule(rocModule, "roc", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_roc_iptw <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$plot_roc_original <- renderPlot({
      print(out_roc_original()$plot)
    })

    output$table_roc_original <- renderDT({
      datatable(out_roc_original()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_roc_ps <- renderPlot({
      print(out_roc_ps()$plot)
    })

    output$table_roc_ps <- renderDT({
      datatable(out_roc_ps()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_roc_iptw <- renderPlot({
      print(out_roc_iptw()$plot)
    })

    output$table_roc_iptw <- renderDT({
      datatable(out_roc_iptw()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    ## Time-ROC

    out_timeroc_original <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_timeroc_ps <- callModule(timerocModule, "timeroc", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_timeroc_iptw <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$plot_timeroc_original <- renderPlot({
      print(out_timeroc_original()$plot)
    })

    output$table_timeroc_original <- renderDT({
      datatable(out_timeroc_original()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_timeroc_ps <- renderPlot({
      print(out_timeroc_ps()$plot)
    })

    output$table_timeroc_ps <- renderDT({
      datatable(out_timeroc_ps()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_timeroc_iptw <- renderPlot({
      print(out_timeroc_iptw()$plot)
    })

    output$table_timeroc_iptw <- renderDT({
      datatable(out_timeroc_iptw()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

  }




  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}



#' @title jsPropensityAddin: Rstudio addin of jsPropensityGadget
#' @description Rstudio addin of jsPropensityGadget
#' @return Rstudio addin of jsPropensityGadget
#' @details Rstudio addin of jsPropensityGadget
#' @examples
#' if(interactive()){
#'  jsPropensityAddin()
#'  }
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





#' @title jsPropensityExtAddin: RStudio Addin for propensity score analysis with external data.
#' @description RStudio Addin for propensity score analysis with external csv/xlsx/sas7bdat/sav/dta file.
#' @param nfactor.limit nlevels limit for categorical variables, Default: 20
#' @param max.filesize Maximum file size to upload (MB), Default: 2048 (2 GB)
#' @return RStudio Addin for propensity score analysis with external data.
#' @details RStudio Addin for propensity score analysis with external csv/xlsx/sas7bdat/sav/dta file.
#' @examples
#' if(interactive()){
#'  jsPropensityExtAddin()
#'  }
#' @seealso
#'  \code{\link[survival]{pbc}}
#'  \code{\link[data.table]{fwrite}},\code{\link[data.table]{data.table-package}}
#'  \code{\link[survey]{svydesign}}
#'  \code{\link[jstable]{opt.tbreg}}
#' @rdname jsPropensityExtAddin
#' @export
#' @importFrom data.table fwrite data.table
#' @importFrom survey svydesign
#' @importFrom jstable opt.tbreg
#' @importFrom DT datatable %>% formatStyle styleEqual renderDT DTOutput
#' @importFrom shinycustomloader withLoader
#' @import shiny

jsPropensityExtAddin <- function(nfactor.limit = 20, max.filesize = 2048){

  options(shiny.maxRequestSize = max.filesize * 1024^2)
  requireNamespace("survival")
  requireNamespace("survC1")

  ui <- navbarPage("Propensity score analysis",
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("import"),
                                downloadButton("downloadData", "Example data")
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
                                tb1moduleUI("tb1")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Original",
                                                     withLoader(DTOutput("table1_original"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("Matching",
                                                     withLoader(DTOutput("table1_ps"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("IPTW",
                                                     withLoader(DTOutput("table1_iptw"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and complex survey regression"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and complex sampling rank test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            )
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(DTOutput("linear_original"), type="html", loader="loader6"),
                                                                br(),
                                                                uiOutput("warning_linear_original")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(DTOutput("linear_ps"), type="html", loader="loader6"),
                                                                br(),
                                                                uiOutput("warning_linear_ps")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(DTOutput("linear_iptw"), type="html", loader="loader6")
                                                       )
                                           )

                                         )
                                       )
                              ),
                              tabPanel("Logistic regression",
                                       sidebarLayout(
                                         sidebarPanel(
                                           regressModuleUI("logistic")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(DTOutput("logistic_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(DTOutput("logistic_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(DTOutput("logistic_iptw"), type="html", loader="loader6")
                                                       )
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Cox model",
                                       sidebarLayout(
                                         sidebarPanel(
                                           coxUI("cox")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(DTOutput("cox_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(DTOutput("cox_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(DTOutput("cox_iptw"), type="html", loader="loader6")
                                                       )
                                           )
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("ggpairs_plot_original"), type="html", loader="loader6")

                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("ggpairs_plot_ps"), type="html", loader="loader6")
                                                       )

                                           ),
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
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("kaplan_plot_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("kaplan_plot_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(plotOutput("kaplan_plot_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("kaplan")
                                         )
                                       )
                              )
                   ),
                   navbarMenu("ROC analysis",
                              tabPanel("ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           rocUI("roc")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("plot_roc_original"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("plot_roc_ps"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(plotOutput("plot_roc_iptw"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_roc_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("roc")
                                         )
                                       )
                              ),
                              tabPanel("Time-dependent ROC",
                                       sidebarLayout(
                                         sidebarPanel(
                                           timerocUI("timeroc")
                                         ),
                                         mainPanel(
                                           tabsetPanel(type = "pills",
                                                       tabPanel("Original",
                                                                withLoader(plotOutput("plot_timeroc_original"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_original"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("Matching",
                                                                withLoader(plotOutput("plot_timeroc_ps"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_ps"), type="html", loader="loader6")
                                                       ),
                                                       tabPanel("IPTW",
                                                                withLoader(plotOutput("plot_timeroc_iptw"), type="html", loader="loader6"),
                                                                withLoader(DTOutput("table_timeroc_iptw"), type="html", loader="loader6")
                                                       )
                                           ),
                                           ggplotdownUI("timeroc")
                                         )
                                       )
                              )
                   )


  )




  server <- function(input, output, session) {

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("example_ps", ".csv", sep = "")
      },
      content = function(file) {
        out <- survival::pbc
        out$status <- as.integer(out$status == 2)
        data.table::fwrite(na.omit(out)[, -1], file)
      }
    )


    output$import <- renderUI({
      FilePsInput("datafile")
    })

    mat.info <- callModule(FilePs, "datafile", nfactor.limit = nfactor.limit)

    output$data <- renderDT({
      datatable(mat.info()$data, rownames=F, editable = F, extensions= "Buttons", caption = "Data with ps, iptw",
                options = c(opt.data("data"), list(scrollX = TRUE))
      )
    })

    output$matdata <- renderDT({
      datatable(mat.info()$matdata, rownames=F, editable = F, extensions= "Buttons", caption = "Matching data",
                options = c(opt.data("matching_data"), list(scrollX = TRUE))
      )
    })

    output$data_label <- renderDT({
      datatable(mat.info()$data.label, rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = c(opt.data("data_label"), list(scrollX = TRUE))
      )
    })

    output$naomit <- renderText({
      mat.info()$naomit
    })


    ## tb1
    data <- reactive({
      mat.info()$data[, .SD, .SDcols = -c("iptw")]
    })
    matdata <- reactive(data.table::data.table(mat.info()$matdata))
    data.label <- reactive(mat.info()$data.label)
    #data_varStruct <- reactive(list(variable = names(mat.info()$matdata)))
    design.survey <- reactive(survey::svydesign(ids = ~ 1, data = mat.info()$data, weights = ~ iptw))


    tb1_original <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, design.survey = NULL, nfactor.limit = nfactor.limit)
    tb1_ps <- callModule(tb1module2, "tb1", data = matdata, data_label = data.label, data_varStruct = NULL, design.survey = NULL, nfactor.limit = nfactor.limit)
    tb1_iptw <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$table1_original <- renderDT({
      tb <- tb1_original()$table
      cap <- tb1_original()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })

    output$table1_ps <- renderDT({
      tb <- tb1_ps()$table
      cap <- tb1_ps()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })

    output$table1_iptw <- renderDT({
      tb <- tb1_iptw()$table
      cap <- tb1_iptw()$caption
      out.tb1 <- datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                           options = c(opt.tb1("tb1"),
                                       list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                       ),
                                       list(scrollX = TRUE)
                           )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })


    ## Regression

    out_linear_original <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_linear_ps <- callModule(regressModule2, "linear", data = matdata, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_linear_iptw <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$linear_original <- renderDT({
      hide <- which(colnames(out_linear_original()$table) == "sig")
      datatable(out_linear_original()$table, rownames=T, extensions= "Buttons", caption = out_linear_original()$caption,
                options = c(opt.tbreg(out_linear_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$warning_linear_original <- renderText({
      paste("<b>", out_linear_original()$warning, "</b>")
    })

    output$linear_ps <- renderDT({
      hide <- which(colnames(out_linear_ps()$table) == "sig")
      datatable(out_linear_ps()$table, rownames=T, extensions= "Buttons", caption = out_linear_ps()$caption,
                options = c(opt.tbreg(out_linear_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$warning_linear_ps <- renderText({
      paste("<b>", out_linear_ps()$warning, "</b>")
    })

    output$linear_iptw <- renderDT({
      hide <- which(colnames(out_linear_iptw()$table) == "sig")
      datatable(out_linear_iptw()$table, rownames=T, extensions= "Buttons", caption = out_linear_iptw()$caption,
                options = c(opt.tbreg(out_linear_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })


    ## Logistic

    out_logistic_original <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_logistic_ps <- callModule(logisticModule2, "logistic", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_logistic_iptw <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$logistic_original <- renderDT({
      hide = which(colnames(out_logistic_original()$table) == "sig")
      datatable(out_logistic_original()$table, rownames=T, extensions= "Buttons", caption = out_logistic_original()$caption,
                options = c(opt.tbreg(out_logistic_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$logistic_ps <- renderDT({
      hide = which(colnames(out_logistic_ps()$table) == "sig")
      datatable(out_logistic_ps()$table, rownames=T, extensions= "Buttons", caption = out_logistic_ps()$caption,
                options = c(opt.tbreg(out_logistic_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$logistic_iptw <- renderDT({
      hide = which(colnames(out_logistic_iptw()$table) == "sig")
      datatable(out_logistic_iptw()$table, rownames=T, extensions= "Buttons", caption = out_logistic_iptw()$caption,
                options = c(opt.tbreg(out_logistic_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets =hide))
                            ),
                            list(scrollX = TRUE)
                )
      ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })


    ## Cox

    out_cox_original <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_cox_ps <- callModule(coxModule, "cox", data = matdata, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)
    out_cox_iptw <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$cox_original <- renderDT({
      hide = which(colnames(out_cox_original()$table) == c("sig"))
      datatable(out_cox_original()$table, rownames=T, extensions= "Buttons", caption = out_cox_original()$caption,
                options = c(opt.tbreg(out_cox_original()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$cox_ps <- renderDT({
      hide = which(colnames(out_cox_ps()$table) == c("sig"))
      datatable(out_cox_ps()$table, rownames=T, extensions= "Buttons", caption = out_cox_ps()$caption,
                options = c(opt.tbreg(out_cox_ps()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    output$cox_iptw <- renderDT({
      hide = which(colnames(out_cox_iptw()$table) == c("sig"))
      datatable(out_cox_iptw()$table, rownames=T, extensions= "Buttons", caption = out_cox_iptw()$caption,
                options = c(opt.tbreg(out_cox_iptw()$caption),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
    })

    ## ggpairs

    out_ggpairs_original <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_ggpairs_ps <- callModule(ggpairsModule2, "ggpairs", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$ggpairs_plot_original <- renderPlot({
      print(out_ggpairs_original())
    })

    output$ggpairs_plot_ps <- renderPlot({
      print(out_ggpairs_ps())
    })


    ## Kaplan

    out_kaplan_original <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_kaplan_ps <- callModule(kaplanModule, "kaplan", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_kaplan_iptw <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)

    output$kaplan_plot_original <- renderPlot({
      print(out_kaplan_original())
    })

    output$kaplan_plot_ps <- renderPlot({
      print(out_kaplan_ps())
    })

    output$kaplan_plot_iptw <- renderPlot({
      print(out_kaplan_iptw())
    })


    ## ROC

    out_roc_original <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_roc_ps <- callModule(rocModule, "roc", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_roc_iptw <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$plot_roc_original <- renderPlot({
      print(out_roc_original()$plot)
    })

    output$table_roc_original <- renderDT({
      datatable(out_roc_original()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_roc_ps <- renderPlot({
      print(out_roc_ps()$plot)
    })

    output$table_roc_ps <- renderDT({
      datatable(out_roc_ps()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_roc_iptw <- renderPlot({
      print(out_roc_iptw()$plot)
    })

    output$table_roc_iptw <- renderDT({
      datatable(out_roc_iptw()$tb, rownames=F, editable = F, extensions= "Buttons",
                caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    ## Time-ROC

    out_timeroc_original <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_timeroc_ps <- callModule(timerocModule, "timeroc", data = matdata, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)
    out_timeroc_iptw <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, design.survey = design.survey, nfactor.limit = nfactor.limit)


    output$plot_timeroc_original <- renderPlot({
      print(out_timeroc_original()$plot)
    })

    output$table_timeroc_original <- renderDT({
      datatable(out_timeroc_original()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_timeroc_ps <- renderPlot({
      print(out_timeroc_ps()$plot)
    })

    output$table_timeroc_ps <- renderDT({
      datatable(out_timeroc_ps()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })

    output$plot_timeroc_iptw <- renderPlot({
      print(out_timeroc_iptw()$plot)
    })

    output$table_timeroc_iptw <- renderDT({
      datatable(out_timeroc_iptw()$tb, rownames=F, editable = F, extensions= "Buttons", caption = "ROC results",
                options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
    })
  }



  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)

}
