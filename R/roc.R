#' @title rocUI: shiny module UI for roc analysis
#' @description Shiny module UI for roc analysis
#' @param id id
#' @return Shiny module UI for roc analysis
#' @details Shiny module UI for roc analysis
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' library(ggplot2)
#' library(pROC)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       rocUI("roc")
#'     ),
#'     mainPanel(
#'       plotOutput("plot_roc"),
#'       tableOutput("cut_roc"),
#'       ggplotdownUI("roc"),
#'       DTOutput("table_roc")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(data1))
#'
#'   out_roc <- callModule(rocModule, "roc",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$plot_roc <- renderPlot({
#'     print(out_roc()$plot)
#'   })
#'
#'   output$cut_roc <- renderTable({
#'     print(out_roc()$cut)
#'   })
#'
#'   output$table_roc <- renderDT({
#'     datatable(out_roc()$tb,
#'       rownames = F, editable = F, extensions = "Buttons",
#'       caption = "ROC results",
#'       options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
#'     )
#'   })
#' }
#' @rdname rocUI
#' @export

rocUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("event")),
    uiOutput(ns("indep")),
    uiOutput(ns("addmodel")),
    checkboxInput(ns("subcheck"), "Sub-group analysis"),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval")),
    checkboxInput(ns("spetype"), "Show 1-specificity", T)
  )
}




#' @title reclassificationJS: Function for reclassification table and statistics
#' @description Modified function of PredictABEL::reclassification: return output table
#' @param data Data frame or matrix that includes the outcome and predictors variables.
#' @param cOutcome Column number of the outcome variable.
#' @param predrisk1 Vector of predicted risks of all individuals using initial model.
#' @param predrisk2 Vector of predicted risks of all individuals using updated model.
#' @param cutoff Cutoff values for risk categories. Define the cut-off values. Ex: c(0,.20,.30,1)
#' @param dec.value digits of value, Default: 4
#' @param dec.p digits of p, Default: 3
#' @return Table including NRI(categorical), NRI(continuous), IDI with 95% CI and p-values.
#' @details Modified function of PredictABEL::reclassification
#' @examples
#' m1 <- glm(vs ~ am + gear, data = mtcars, family = binomial)
#' m2 <- glm(vs ~ am + gear + wt, data = mtcars, family = binomial)
#' reclassificationJS(
#'   data = mtcars, cOutcome = 8,
#'   predrisk1 = predict(m1, type = "response"),
#'   predrisk2 = predict(m2, type = "response"), cutoff = c(0, .20, .40, 1)
#' )
#' @seealso
#'  \code{\link[Hmisc]{rcorrp.cens}}
#' @rdname reclassificationJS
#' @export
#' @importFrom Hmisc improveProb
#' @importFrom stats pnorm

reclassificationJS <- function(data, cOutcome, predrisk1, predrisk2, cutoff, dec.value = 3, dec.p = 3) {
  c1 <- cut(predrisk1,
    breaks = cutoff, include.lowest = TRUE,
    right = FALSE
  )
  c2 <- cut(predrisk2,
    breaks = cutoff, include.lowest = TRUE,
    right = FALSE
  )
  tabReclas <- table(`Initial Model` = c1, `Updated Model` = c2)
  # cat(" _________________________________________\n")
  # cat(" \n     Reclassification table    \n")
  # cat(" _________________________________________\n")
  ta <- table(c1, c2, data[, cOutcome])
  # cat("\n Outcome: absent \n  \n")
  TabAbs <- ta[, , 1]
  tab1 <- cbind(TabAbs, ` % reclassified` = round((rowSums(TabAbs) -
    diag(TabAbs)) / rowSums(TabAbs), 2) * 100)
  names(dimnames(tab1)) <- c("Initial Model", "Updated Model")
  # print(tab1)
  # cat("\n \n Outcome: present \n  \n")
  TabPre <- ta[, , 2]
  tab2 <- cbind(TabPre, ` % reclassified` = round((rowSums(TabPre) -
    diag(TabPre)) / rowSums(TabPre), 2) * 100)
  names(dimnames(tab2)) <- c("Initial Model", "Updated Model")
  # print(tab2)
  # cat("\n \n Combined Data \n  \n")
  Tab <- tabReclas
  tab <- cbind(Tab, ` % reclassified` = round((rowSums(Tab) -
    diag(Tab)) / rowSums(Tab), 2) * 100)
  names(dimnames(tab)) <- c("Initial Model", "Updated Model")
  # print(tab)
  # cat(" _________________________________________\n")
  c11 <- factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
  c22 <- factor(c2, levels = levels(c2), labels = c(1:length(levels(c2))))
  x <- Hmisc::improveProb(
    x1 = as.numeric(c11) * (1 / (length(levels(c11)))),
    x2 = as.numeric(c22) * (1 / (length(levels(c22)))), y = data[
      ,
      cOutcome
    ]
  )
  y <- Hmisc::improveProb(x1 = predrisk1, x2 = predrisk2, y = data[
    ,
    cOutcome
  ])
  # cat("\n NRI(Categorical) [95% CI]:", round(x$nri, 4), "[",
  #    round(x$nri - 1.96 * x$se.nri, 4), "-", round(x$nri +
  #                                                    1.96 * x$se.nri, 4), "]", "; p-value:", round(2 *
  #                                                                                                    pnorm(-abs(x$z.nri)), 5), "\n")
  # cat(" NRI(Continuous) [95% CI]:", round(y$nri, 4), "[", round(y$nri -
  #                                                                1.96 * y$se.nri, 4), "-", round(y$nri + 1.96 * y$se.nri,
  #                                                                                                4), "]", "; p-value:", round(2 * pnorm(-abs(y$z.nri)),
  #                                                                                                                             5), "\n")
  # cat(" IDI [95% CI]:", round(y$idi, 4), "[", round(y$idi -
  #                                                    1.96 * y$se.idi, 4), "-", round(y$idi + 1.96 * y$se.idi,
  #                                                                                    4), "]", "; p-value:", round(2 * pnorm(-abs(y$z.idi)),
  #                                                                                                                 5), "\n")

  value <- round(c(x$nri, y$nri, y$idi), dec.value)
  lowerCI <- round(c(x$nri, y$nri, y$idi) - qnorm(0.975) * c(x$se.nri, y$se.nri, y$se.idi), dec.value)
  upperCI <- round(c(x$nri, y$nri, y$idi) + qnorm(0.975) * c(x$se.nri, y$se.nri, y$se.idi), dec.value)
  p <- round(2 * pnorm(-abs(c(x$z.nri, y$z.nri, y$z.idi))), dec.p)
  p <- ifelse(p < 0.001, "< 0.001", p)
  out <- data.frame(value = value, CI = paste0(lowerCI, "-", upperCI), p)
  names(out)[2] <- "95% CI"
  rownames(out) <- c("NRI(Categorical)", "NRI(Continuous)", "IDI")
  return(out)
}




#' @title ROC_table: extract AUC, NRI and IDI information from list of roc object in pROC packages.
#' @description extract AUC, NRI and IDI information from list of roc in pROC packages
#' @param ListModel list of roc object
#' @param dec.auc digits for AUC, Default: 3
#' @param dec.p digits for p value, Default: 3
#' @return table of AUC, NRI and IDI information
#' @details extract AUC, NRI and IDI information from list of roc object in pROC packages.
#' @examples
#' library(pROC)
#' m1 <- glm(vs ~ am + gear, data = mtcars, family = binomial)
#' m2 <- glm(vs ~ am + gear + wt, data = mtcars, family = binomial)
#' m3 <- glm(vs ~ am + gear + wt + mpg, data = mtcars, family = binomial)
#' roc1 <- roc(m1$y, predict(m1, type = "response"))
#' roc2 <- roc(m2$y, predict(m2, type = "response"))
#' roc3 <- roc(m3$y, predict(m3, type = "response"))
#' list.roc <- list(roc1, roc2, roc3)
#' ROC_table(list.roc)
#' @seealso
#'  \code{\link[pROC]{ci.auc}},\code{\link[pROC]{roc.test}}
#'  \code{\link[data.table]{data.table}}, \code{\link[data.table]{rbindlist}}
#' @rdname ROC_table
#' @export
#' @importFrom pROC ci.auc roc.test
#' @importFrom data.table data.table rbindlist

ROC_table <- function(ListModel, dec.auc = 3, dec.p = 3) {
  auc <- round(sapply(ListModel, function(x) {
    x$auc
  }), dec.auc)
  auc.ci <- sapply(ListModel, function(x) {
    paste0(round(pROC::ci.auc(x)[1], dec.auc), "-", round(pROC::ci.auc(x)[3], dec.auc))
  })
  if (length(ListModel) == 1) {
    out <- data.table::data.table(paste0("Model ", seq_along(ListModel)), auc, auc.ci)
    names(out) <- c("Prediction Model", "AUC", "95% CI")
  } else {
    info.diff <- data.table::rbindlist(
      lapply(
        seq_along(ListModel),
        function(x) {
          if (x == 1) {
            return(data.frame(t(rep(NA, 7))))
          }
          p <- pROC::roc.test(ListModel[[x]], ListModel[[x - 1]])$p.value
          p <- ifelse(p < 0.001, "< 0.001", round(p, dec.p))
          out.int <- as.integer(as.vector(factor(ListModel[[x]]$response, labels = c(0, 1))))
          reclass <- reclassificationJS(
            data = data.frame(out.int), cOutcome = 1,
            ListModel[[x - 1]]$predictor, ListModel[[x]]$predictor,
            cutoff = c(0, 0.5, 1)
          )
          res <- cbind(p, reclass[3, ], reclass[2, ])
          return(res)
        }
      ),
      use.names = FALSE
    )


    out <- data.table::data.table(paste0("Model ", seq_along(ListModel)), auc, auc.ci, info.diff)
    names(out) <- c(
      "Prediction Model", "AUC", "95% CI", "P-value for AUC Difference", "IDI", "95% CI", "P-value for IDI",
      "continuous NRI", "95% CI", "P-value for NRI"
    )
  }

  return(out[])
}





#' @title rocModule: shiny module server for roc analysis
#' @description shiny module server for roc analysis
#' @param input input
#' @param output output
#' @param session session
#' @param data Reactive data
#' @param data_label Reactuve data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey Reactive survey data. default: NULL
#' @param id.cluster Reactive cluster variable if marginal model, Default: NULL
#' @return shiny module server for roc analysis
#' @details shiny module server for roc analysis
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' library(ggplot2)
#' library(pROC)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       rocUI("roc")
#'     ),
#'     mainPanel(
#'       plotOutput("plot_roc"),
#'       tableOutput("cut_roc"),
#'       ggplotdownUI("roc"),
#'       DTOutput("table_roc")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(data1))
#'
#'   out_roc <- callModule(rocModule, "roc",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$plot_roc <- renderPlot({
#'     print(out_roc()$plot)
#'   })
#'
#'   output$cut_roc <- renderTable({
#'     print(out_roc()$cut)
#'   })
#'
#'   output$table_roc <- renderDT({
#'     datatable(out_roc()$tb,
#'       rownames = F, editable = F, extensions = "Buttons",
#'       caption = "ROC results",
#'       options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
#'     )
#'   })
#' }
#' @seealso
#'  \code{\link[stats]{quantile}}
#'  \code{\link[data.table]{setkey}}
#'  \code{\link[pROC]{ggroc}}
#'  \code{\link[geepack]{geeglm}}
#'  \code{\link[survey]{svyglm}}
#'  \code{\link[see]{theme_modern}}
#' @rdname rocModule
#' @export
#' @importFrom stats quantile
#' @importFrom data.table setkey
#' @importFrom pROC roc ggroc coords
#' @importFrom geepack geeglm
#' @importFrom survey svyglm
#' @importFrom see theme_modern
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location

rocModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL) {
  ## To remove NOTE.
  level <- variable <- NULL

  if (is.null(data_varStruct)) {
    data_varStruct <- reactive(list(variable = names(data())))
  }

  vlist <- reactive({
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

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    # factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
    factor_list <- mklist(data_varStruct(), factor_vars)


    conti_vars <- setdiff(names(data()), factor_vars)
    if (!is.null(design.survey)) {
      conti_vars <- setdiff(conti_vars, c(names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    }
    conti_vars_positive <- conti_vars[unlist(data()[, lapply(.SD, function(x) {
      min(x, na.rm = T) >= 0
    }), .SDcols = conti_vars])]
    conti_list <- mklist(data_varStruct(), conti_vars)

    nclass_factor <- unlist(data()[, lapply(.SD, function(x) {
      length(levels(x))
    }), .SDcols = factor_vars])
    # nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})
    class01_factor <- unlist(data()[, lapply(.SD, function(x) {
      identical(levels(x), c("0", "1"))
    }), .SDcols = factor_vars])

    validate(
      need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
    )
    factor_01vars <- factor_vars[class01_factor]

    factor_01_list <- mklist(data_varStruct(), factor_01vars)

    group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]

    return(list(
      factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list, conti_vars_positive = conti_vars_positive,
      factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_vars = group_vars, group_list = group_list, except_vars = except_vars
    ))
  })

  output$event <- renderUI({
    validate(
      need(length(vlist()$factor_01vars) >= 1, "No candidate event variables coded as 0, 1")
    )

    tagList(
      selectInput(session$ns("event_roc"), "Event",
        choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = F,
        selected = NULL
      )
    )
  })


  nmodel <- reactiveVal(1)


  output$addmodel <- renderUI({
    if (nmodel() <= 1) {
      actionButton(session$ns("add"), label = "Add model", icon("plus"), class = "btn-primary")
    } else if (nmodel() > 1) {
      tagList(
        actionButton(session$ns("add"), label = "Add model", icon("plus"), class = "btn-primary"),
        actionButton(session$ns("rmv"), label = "Remove model", icon("minus"))
      )
    }
  })


  indeproc <- reactive({
    req(!is.null(input$event_roc))
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


    if (!is.null(design.survey)) {
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_roc, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    } else if (!is.null(id.cluster)) {
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_roc, id.cluster()))
    } else {
      indep.roc <- setdiff(names(data()), c(vlist()$except_vars, input$event_roc))
    }
    return(indep.roc)
  })


  output$indep <- renderUI({
    selectInput(session$ns(paste0("indep_roc", 1)), paste0("Independent variables for Model ", 1),
      choices = mklist(data_varStruct(), indeproc()), multiple = T,
      selected = unlist(mklist(data_varStruct(), indeproc()))[1]
    )
  })

  observeEvent(input$add, {
    insertUI(
      selector = paste0("div:has(> #", session$ns("add"), ")"),
      where = "beforeBegin",
      ui = selectInput(session$ns(paste0("indep_roc", nmodel() + 1)), paste0("Independent variables for Model ", nmodel() + 1),
        choices = mklist(data_varStruct(), indeproc()), multiple = T,
        selected = unlist(mklist(data_varStruct(), indeproc()))[1:min(length(indeproc()), nmodel() + 1)]
      )
    )
    nmodel(nmodel() + 1)
  })

  observeEvent(input$rmv, {
    removeUI(
      selector = paste0("div:has(>> #", session$ns(paste0("indep_roc", nmodel())), ")")
    )
    nmodel(nmodel() - 1)
  })




  indeps <- reactive(lapply(1:nmodel(), function(i) {
    input[[paste0("indep_roc", i)]]
  }))





  observeEvent(input$subcheck, {
    output$subvar <- renderUI({
      req(input$subcheck == T)
      indeps.unique <- unique(unlist(indeps()))

      var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$event_roc, indeps.unique))
      if (!is.null(id.cluster)) {
        var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$event_roc, indeps.unique, id.cluster()))
      } else if (!is.null(design.survey)) {
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(vlist()$except_vars, input$event_roc, indeps.unique)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0, "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_roc"), "Sub-group variables",
          choices = var_subgroup_list, multiple = T,
          selected = var_subgroup[1]
        )
      )
    })
  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(length(input$subvar_roc) > 0)

    outUI <- tagList()

    for (v in seq_along(input$subvar_roc)) {
      if (input$subvar_roc[[v]] %in% vlist()$factor_vars) {
        outUI[[v]] <- selectInput(session$ns(paste0("subval_roc", v)), paste0("Sub-group value: ", input$subvar_roc[[v]]),
          choices = data_label()[variable == input$subvar_roc[[v]], level], multiple = T,
          selected = data_label()[variable == input$subvar_roc[[v]], level][1]
        )
      } else {
        val <- stats::quantile(data()[[input$subvar_roc[[v]]]], na.rm = T)
        outUI[[v]] <- sliderInput(session$ns(paste0("subval_roc", v)), paste0("Sub-group range: ", input$subvar_roc[[v]]),
          min = val[1], max = val[5],
          value = c(val[2], val[4])
        )
      }
    }
    outUI
  })



  rocList <- reactive({
    req(!is.null(input$event_roc))

    for (i in 1:nmodel()) {
      req(!is.null(input[[paste0("indep_roc", i)]]))
    }
    req(!is.null(indeps()))
    collapse.indep <- sapply(1:nmodel(), function(i) {
      paste0(input[[paste0("indep_roc", i)]], collapse = "")
    })
    validate(
      need(anyDuplicated(collapse.indep) == 0, "Please select different models")
    )

    data.roc <- data()[complete.cases(data()[, .SD, .SDcols = unique(unlist(indeps()))])]
    label.regress <- data_label()
    data.roc[[input$event_roc]] <- as.numeric(as.vector(data.roc[[input$event_roc]]))
    if (input$subcheck == TRUE) {
      validate(
        need(length(input$subvar_roc) > 0, "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_roc), function(x) {
          length(input[[paste0("subval_roc", x)]])
        })), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_roc)) {
        if (input$subvar_roc[[v]] %in% vlist()$factor_vars) {
          data.roc <- data.roc[get(input$subvar_roc[[v]]) %in% input[[paste0("subval_roc", v)]]]
        } else {
          data.roc <- data.roc[get(input$subvar_roc[[v]]) >= input[[paste0("subval_roc", v)]][1] & get(input$subvar_roc[[v]]) <= input[[paste0("subval_roc", v)]][2]]
        }
      }


      data.roc[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.roc)[, c("variable", "level")]
      data.table::setkey(data_label(), "variable", "level")
      data.table::setkey(label.regress2, "variable", "level")
      label.regress <- data_label()[label.regress2]
      data.roc[[input$event_roc]] <- as.numeric(as.vector(data.roc[[input$event_roc]]))
    }

    if (is.null(design.survey)) {
      if (is.null(id.cluster)) {
        res.roc <- lapply(indeps(), function(x) {
          forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
          mm <- glm(as.formula(forms), data = data.roc, family = binomial, x = T)
          return(pROC::roc(mm$y, predict(mm, type = "response")))
        })

        if (nmodel() == 1 & length(indeps()) == 1) {
          res.roc1 <- lapply(indeps(), function(x) {
            forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
            mm <- glm(as.formula(forms), data = data.roc, family = binomial, x = T)
            return(pROC::roc(mm$y, mm$x[, 2]))
          })
          res.cut <- pROC::coords(res.roc1[[1]],
            x = "best", input = "threshold", best.method = "youden",
            ret = c("threshold", "sensitivity", "specificity", "accuracy", "ppv", "npv")
          )
        } else {
          res.cut <- NULL
        }
      } else {
        res.roc <- lapply(indeps(), function(x) {
          forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
          mm <- geepack::geeglm(as.formula(forms), data = data.roc, family = "binomial", id = get(id.cluster()), corstr = "exchangeable")
          pROC::roc(mm$y, predict(mm, type = "response"))
        })

        res.cut <- NULL
      }

      res.tb <- ROC_table(res.roc, dec.auc = 3, dec.p = 3)
    } else {
      data.design <- design.survey()
      label.regress <- data_label()
      data.design$variables[[input$event_roc]] <- as.numeric(as.vector(data.design$variables[[input$event_roc]]))
      if (input$subcheck == TRUE) {
        validate(
          need(length(input$subvar_roc) > 0, "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_roc), function(x) {
            length(input[[paste0("subval_roc", x)]])
          })), "No value for subsetting")
        )

        for (v in seq_along(input$subvar_roc)) {
          if (input$subvar_roc[[v]] %in% vlist()$factor_vars) {
            data.design <- subset(data.design, get(input$subvar_roc[[v]]) %in% input[[paste0("subval_roc", v)]])
          } else {
            data.design <- subset(data.design, get(input$subvar_roc[[v]]) >= input[[paste0("subval_roc", v)]][1] & get(input$subvar_roc[[v]]) <= input[[paste0("subval_roc", v)]][2])
          }
        }


        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
        data.design$variables[[input$event_roc]] <- as.numeric(as.vector(data.design$variables[[input$event_roc]]))
      }
      res.roc <- lapply(indeps(), function(x) {
        forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
        mm <- survey::svyglm(as.formula(forms), design = data.design, family = quasibinomial(), x = T)
        return(pROC::roc(mm$y, predict(mm, type = "response")))
      })

      if (nmodel() == 1 & length(indeps()) == 1) {
        res.roc1 <- lapply(indeps(), function(x) {
          forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
          mm <- survey::svyglm(as.formula(forms), design = data.design, family = quasibinomial(), x = T)
          return(pROC::roc(mm$y, mm$x[, 2]))
        })
        res.cut <- pROC::coords(res.roc1[[1]],
          x = "best", input = "threshold", best.method = "youden",
          ret = c("threshold", "sensitivity", "specificity", "accuracy", "ppv", "npv")
        )
      } else {
        res.cut <- NULL
      }

      res.tb <- ROC_table(res.roc, dec.auc = 3, dec.p = 3)
    }


    p <- pROC::ggroc(res.roc, legacy.axes = input$spetype) + see::theme_modern() + geom_abline(slope = 1, intercept = as.integer(!input$spetype), lty = 2) + scale_color_discrete("Model", labels = paste("Model", 1:nmodel()))

    return(list(plot = p, cut = res.cut, tb = res.tb))
  })





  output$downloadControls <- renderUI({
    tagList(
      column(
        4,
        selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
          choices = c("jpg", "pdf", "tiff", "svg", "pptx"), multiple = F,
          selected = "pptx"
        )
      ),
      column(
        4,
        sliderInput(session$ns("fig_width"), "Width (in):",
          min = 5, max = 15, value = 8
        )
      ),
      column(
        4,
        sliderInput(session$ns("fig_height"), "Height (in):",
          min = 5, max = 15, value = 6
        )
      )
    )
  })

  output$downloadButton <- downloadHandler(
    filename = function() {
      if (is.null(design.survey)) {
        if (is.null(id.cluster)) {
          return(paste(input$event_roc, "_ROC.", input$file_ext, sep = ""))
        } else {
          return(paste(input$event_roc, "_ROC_marginal.", input$file_ext, sep = ""))
        }
      } else {
        return(paste(input$event_roc, "_ROC_survey.", input$file_ext, sep = ""))
      }
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }

          if (input$file_ext == "pptx") {
            my_vec_graph <- rvg::dml(ggobj = rocList()$plot)
            doc <- officer::read_pptx()
            doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
            doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
            print(doc, target = file)
          } else {
            ggsave(file, rocList()$plot, dpi = 300, units = "in", width = input$fig_width, height = input$fig_height)
          }
        }
      )
    }
  )

  return(rocList)
}





#' @title rocModule2: shiny module server for roc analysis- input number of model as integer
#' @description shiny module server for roc analysis- input number of model as integer
#' @param input input
#' @param output output
#' @param session session
#' @param data Reactive data
#' @param data_label Reactuve data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey Reactive survey data. default: NULL
#' @param id.cluster Reactive cluster variable if marginal model, Default: NULL
#' @return shiny module server for roc analysis- input number of model as integer
#' @details shiny module server for roc analysis- input number of model as integer
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' library(ggplot2)
#' library(pROC)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       rocUI("roc")
#'     ),
#'     mainPanel(
#'       plotOutput("plot_roc"),
#'       tableOutput("cut_roc"),
#'       ggplotdownUI("roc"),
#'       DTOutput("table_roc")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(data1))
#'
#'   out_roc <- callModule(rocModule2, "roc",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$plot_roc <- renderPlot({
#'     print(out_roc()$plot)
#'   })
#'
#'   output$cut_roc <- renderTable({
#'     print(out_roc()$cut)
#'   })
#'
#'   output$table_roc <- renderDT({
#'     datatable(out_roc()$tb,
#'       rownames = F, editable = F, extensions = "Buttons",
#'       caption = "ROC results",
#'       options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
#'     )
#'   })
#' }
#' @seealso
#'  \code{\link[stats]{quantile}}
#'  \code{\link[data.table]{setkey}}
#'  \code{\link[pROC]{ggroc}}
#'  \code{\link[geepack]{geeglm}}
#'  \code{\link[survey]{svyglm}}
#'  \code{\link[see]{theme_modern}}
#' @rdname rocModule2
#' @export
#' @importFrom stats quantile
#' @importFrom data.table setkey
#' @importFrom pROC roc ggroc coords
#' @importFrom geepack geeglm
#' @importFrom survey svyglm
#' @importFrom see theme_modern
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location



rocModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL) {
  ## To remove NOTE.
  level <- variable <- NULL

  if (is.null(data_varStruct)) {
    data_varStruct <- reactive(list(variable = names(data())))
  }

  vlist <- reactive({
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

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    # factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
    factor_list <- mklist(data_varStruct(), factor_vars)


    conti_vars <- setdiff(names(data()), factor_vars)
    if (!is.null(design.survey)) {
      conti_vars <- setdiff(conti_vars, c(names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    }
    conti_vars_positive <- conti_vars[unlist(data()[, lapply(.SD, function(x) {
      min(x, na.rm = T) >= 0
    }), .SDcols = conti_vars])]
    conti_list <- mklist(data_varStruct(), conti_vars)

    nclass_factor <- unlist(data()[, lapply(.SD, function(x) {
      length(levels(x))
    }), .SDcols = factor_vars])
    # nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})
    class01_factor <- unlist(data()[, lapply(.SD, function(x) {
      identical(levels(x), c("0", "1"))
    }), .SDcols = factor_vars])

    validate(
      need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
    )
    factor_01vars <- factor_vars[class01_factor]

    factor_01_list <- mklist(data_varStruct(), factor_01vars)

    group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]

    return(list(
      factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list, conti_vars_positive = conti_vars_positive,
      factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_vars = group_vars, group_list = group_list, except_vars = except_vars
    ))
  })

  output$event <- renderUI({
    validate(
      need(length(vlist()$factor_01vars) >= 1, "No candidate event variables coded as 0, 1")
    )

    tagList(
      selectInput(session$ns("event_roc"), "Event",
        choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = F,
        selected = NULL
      )
    )
  })


  output$addmodel <- renderUI({
    radioButtons(session$ns("nmodel"), "Number of models", 1:5, selected = 1, inline = T)
  })

  nmodel <- reactive(as.integer(input$nmodel))



  indeproc <- reactive({
    req(!is.null(input$event_roc))
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


    if (!is.null(design.survey)) {
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_roc, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    } else if (!is.null(id.cluster)) {
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_roc, id.cluster()))
    } else {
      indep.roc <- setdiff(names(data()), c(vlist()$except_vars, input$event_roc))
    }
    return(indep.roc)
  })


  output$indep <- renderUI({
    req(nmodel())
    lapply(1:nmodel(), {
      function(x) {
        selectInput(session$ns(paste0("indep_roc", x)), paste0("Independent variables for Model ", x),
          choices = mklist(data_varStruct(), indeproc()), multiple = T,
          selected = unlist(mklist(data_varStruct(), indeproc()))[x]
        )
      }
    })
  })







  indeps <- reactive(lapply(1:nmodel(), function(i) {
    input[[paste0("indep_roc", i)]]
  }))




  observeEvent(input$subcheck, {
    output$subvar <- renderUI({
      req(input$subcheck == T)
      indeps.unique <- unique(unlist(indeps()))

      var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$event_roc, indeps.unique))
      if (!is.null(id.cluster)) {
        var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$event_roc, indeps.unique, id.cluster()))
      } else if (!is.null(design.survey)) {
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(vlist()$except_vars, input$event_roc, indeps.unique)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0, "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_roc"), "Sub-group variables",
          choices = var_subgroup_list, multiple = T,
          selected = var_subgroup[1]
        )
      )
    })
  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(length(input$subvar_roc) > 0)

    outUI <- tagList()

    for (v in seq_along(input$subvar_roc)) {
      if (input$subvar_roc[[v]] %in% vlist()$factor_vars) {
        outUI[[v]] <- selectInput(session$ns(paste0("subval_roc", v)), paste0("Sub-group value: ", input$subvar_roc[[v]]),
          choices = data_label()[variable == input$subvar_roc[[v]], level], multiple = T,
          selected = data_label()[variable == input$subvar_roc[[v]], level][1]
        )
      } else {
        val <- stats::quantile(data()[[input$subvar_roc[[v]]]], na.rm = T)
        outUI[[v]] <- sliderInput(session$ns(paste0("subval_roc", v)), paste0("Sub-group range: ", input$subvar_roc[[v]]),
          min = val[1], max = val[5],
          value = c(val[2], val[4])
        )
      }
    }
    outUI
  })



  rocList <- reactive({
    req(!is.null(input$event_roc))

    for (i in 1:nmodel()) {
      req(!is.null(input[[paste0("indep_roc", i)]]))
    }
    req(!is.null(indeps()))
    collapse.indep <- sapply(1:nmodel(), function(i) {
      paste0(input[[paste0("indep_roc", i)]], collapse = "")
    })
    validate(
      need(anyDuplicated(collapse.indep) == 0, "Please select different models")
    )

    data.roc <- data()
    label.regress <- data_label()
    data.roc[[input$event_roc]] <- as.numeric(as.vector(data.roc[[input$event_roc]]))
    if (input$subcheck == TRUE) {
      validate(
        need(length(input$subvar_roc) > 0, "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_roc), function(x) {
          length(input[[paste0("subval_roc", x)]])
        })), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_roc)) {
        if (input$subvar_roc[[v]] %in% vlist()$factor_vars) {
          data.roc <- data.roc[get(input$subvar_roc[[v]]) %in% input[[paste0("subval_roc", v)]]]
        } else {
          data.roc <- data.roc[get(input$subvar_roc[[v]]) >= input[[paste0("subval_roc", v)]][1] & get(input$subvar_roc[[v]]) <= input[[paste0("subval_roc", v)]][2]]
        }
      }


      data.roc[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.roc)[, c("variable", "class", "level")]
      data.table::setkey(data_label(), "variable", "class", "level")
      data.table::setkey(label.regress2, "variable", "class", "level")
      label.regress <- data_label()[label.regress2]
      data.roc[[input$event_roc]] <- as.numeric(as.vector(data.roc[[input$event_roc]]))
    }

    if (is.null(design.survey)) {
      if (is.null(id.cluster)) {
        res.roc <- lapply(indeps(), function(x) {
          forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
          mm <- glm(as.formula(forms), data = data.roc, family = binomial, x = T)
          return(pROC::roc(mm$y, predict(mm, type = "response")))
        })

        if (nmodel() == 1 & length(indeps()) == 1) {
          res.roc1 <- lapply(indeps(), function(x) {
            forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
            mm <- glm(as.formula(forms), data = data.roc, family = binomial, x = T)
            return(pROC::roc(mm$y, mm$x[, 2]))
          })
          res.cut <- pROC::coords(res.roc1[[1]],
            x = "best", input = "threshold", best.method = "youden",
            ret = c("threshold", "sensitivity", "specificity", "accuracy", "ppv", "npv")
          )
        } else {
          res.cut <- NULL
        }
      } else {
        res.roc <- lapply(indeps(), function(x) {
          forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
          mm <- geepack::geeglm(as.formula(forms), data = data.roc, family = "binomial", id = get(id.cluster()), corstr = "exchangeable")
          pROC::roc(mm$y, predict(mm, type = "response"))
        })

        res.cut <- NULL
      }

      res.tb <- ROC_table(res.roc, dec.auc = 3, dec.p = 3)
    } else {
      data.design <- design.survey()
      label.regress <- data_label()
      data.design$variables[[input$event_roc]] <- as.numeric(as.vector(data.design$variables[[input$event_roc]]))
      if (input$subcheck == TRUE) {
        validate(
          need(length(input$subvar_roc) > 0, "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_roc), function(x) {
            length(input[[paste0("subval_roc", x)]])
          })), "No value for subsetting")
        )

        for (v in seq_along(input$subvar_roc)) {
          if (input$subvar_roc[[v]] %in% vlist()$factor_vars) {
            data.design <- subset(data.design, get(input$subvar_roc[[v]]) %in% input[[paste0("subval_roc", v)]])
          } else {
            data.design <- subset(data.design, get(input$subvar_roc[[v]]) >= input[[paste0("subval_roc", v)]][1] & get(input$subvar_roc[[v]]) <= input[[paste0("subval_roc", v)]][2])
          }
        }


        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
        data.design$variables[[input$event_roc]] <- as.numeric(as.vector(data.design$variables[[input$event_roc]]))
      }
      res.roc <- lapply(indeps(), function(x) {
        forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
        mm <- survey::svyglm(as.formula(forms), design = data.design, family = quasibinomial(), x = T)
        return(pROC::roc(mm$y, predict(mm, type = "response")))
      })

      if (nmodel() == 1 & length(indeps()) == 1) {
        res.roc1 <- lapply(indeps(), function(x) {
          forms <- paste0(input$event_roc, "~", paste(x, collapse = "+"))
          mm <- survey::svyglm(as.formula(forms), design = data.design, family = quasibinomial(), x = T)
          return(pROC::roc(mm$y, mm$x[, 2]))
        })
        res.cut <- pROC::coords(res.roc1[[1]],
          x = "best", input = "threshold", best.method = "youden",
          ret = c("threshold", "sensitivity", "specificity", "accuracy", "ppv", "npv")
        )
      } else {
        res.cut <- NULL
      }

      res.tb <- ROC_table(res.roc, dec.auc = 3, dec.p = 3)
    }


    p <- pROC::ggroc(res.roc, legacy.axes = input$spetype) + see::theme_modern() + geom_abline(slope = 1, intercept = as.integer(!input$spetype), lty = 2) + scale_color_discrete("Model", labels = paste("Model", 1:nmodel()))

    return(list(plot = p, cut = res.cut, tb = res.tb))
  })





  output$downloadControls <- renderUI({
    tagList(
      column(
        4,
        selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
          choices = c("jpg", "pdf", "tiff", "svg", "pptx"), multiple = F,
          selected = "pptx"
        )
      ),
      column(
        4,
        sliderInput(session$ns("fig_width"), "Width (in):",
          min = 5, max = 15, value = 8
        )
      ),
      column(
        4,
        sliderInput(session$ns("fig_height"), "Height (in):",
          min = 5, max = 15, value = 6
        )
      )
    )
  })

  output$downloadButton <- downloadHandler(
    filename = function() {
      if (is.null(design.survey)) {
        if (is.null(id.cluster)) {
          return(paste(input$event_roc, "_ROC.", input$file_ext, sep = ""))
        } else {
          return(paste(input$event_roc, "_ROC_marginal.", input$file_ext, sep = ""))
        }
      } else {
        return(paste(input$event_roc, "_ROC_survey.", input$file_ext, sep = ""))
      }
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(
        message = "Download in progress",
        detail = "This may take a while...",
        value = 0,
        {
          for (i in 1:15) {
            incProgress(1 / 15)
            Sys.sleep(0.01)
          }

          if (input$file_ext == "pptx") {
            my_vec_graph <- rvg::dml(ggobj = rocList()$plot)

            doc <- officer::read_pptx()
            doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
            doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
            print(doc, target = file)
          } else {
            ggsave(file, rocList()$plot, dpi = 300, units = "in", width = input$fig_width, height = input$fig_height)
          }
        }
      )
    }
  )

  return(rocList)
}
