#' @title timerocUI: shiny module UI for time-dependent roc analysis
#' @description Shiny module UI for time-dependent roc analysis
#' @param id id
#' @return Shiny module UI for time-dependent roc analysis
#' @details Shiny module UI for time-dependent roc analysis
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable);library(ggplot2)
#' library(timeROC);library(survIDINRI)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      timerocUI("timeroc")
#'    ),
#'    mainPanel(
#'      plotOutput("plot_timeroc"),
#'      ggplotdownUI("timeroc"),
#'      DTOutput("table_timeroc")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- jstable::mk.lev(mtcars)
#'
#'   out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label,
#'                             data_varStruct = NULL)
#'
#'   output$plot_timeroc <- renderPlot({
#'     print(out_timeroc()$plot)
#'   })
#'
#'   output$table_timeroc <- renderDT({
#'     datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons",
#'               caption = "ROC results",
#'               options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
#'   })
#'}
#' @rdname timerocUI
#' @export

timerocUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("eventtime")),
    uiOutput(ns("indep")),
    uiOutput(ns("addmodel")),
    uiOutput(ns("time")),
    checkboxInput(ns("subcheck"), "Sub-group analysis"),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval"))
  )
}



#' @title timeROChelper: Helper function for timerocModule
#' @description Helper function for timerocModule
#' @param var.event event
#' @param var.time time
#' @param vars.ind independent variable
#' @param t time
#' @param data data
#' @param design.survey survey data, Default: NULL
#' @param id.cluster cluster variable if marginal model, Default: NULL
#' @return timeROC and coxph object
#' @details Helper function for timerocModule
#' @examples
#' #library(survival)
#' #timeROChelper("status", "time", c("age", "sex"), t = 365, data = lung)
#' @seealso
#'  \code{\link[survival]{coxph}}
#'  \code{\link[survey]{svycoxph}}
#'  \code{\link[stats]{predict}}
#'  \code{\link[timeROC]{timeROC}}
#' @rdname timeROChelper
#' @importFrom survival coxph Surv
#' @importFrom survey svycoxph
#' @importFrom stats predict
#' @importFrom timeROC timeROC



timeROChelper <- function(var.event, var.time, vars.ind, t, data, design.survey = NULL, id.cluster = NULL) {
  data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
  forms <- as.formula(paste0("survival::Surv(", var.time, ",", var.event, ") ~ " , paste(vars.ind, collapse = "+")))

  cmodel <- NULL
  if (is.null(design.survey)){
    if (!is.null(id.cluster)){
      cmodel <- survival::coxph(forms, data = data, y = T, cluster = get(id.cluster))
    } else{
      cmodel <- survival::coxph(forms, data = data, y = T)
    }

  } else{
    cmodel <- survey::svycoxph(forms, design = design.survey, y = T)
  }

  lp <- stats::predict(cmodel, type = "lp")
  vec.y <- sapply(cmodel$y, `[[`, 1)
  out <- timeROC::timeROC(T = vec.y[1:(length(vec.y)/2)],
                          delta = vec.y[(length(vec.y)/2 +1):length(vec.y)],
                          marker = lp,
                          cause = 1,
                          weighting="marginal",
                          times = t)

  ## Coxph object
  data[[var.event]][data[[var.event]] == 1 & data[[var.time]] > t] <- 0
  data[[var.time]][data[[var.time]] > t] <- t
  if (is.null(design.survey)){
    if (!is.null(id.cluster)){
      cmodel <- survival::coxph(forms, data = data, y = T, cluster = get(id.cluster))
    } else{
      cmodel <- survival::coxph(forms, data = data, y = T)
    }

  } else{
    cmodel <- survey::svycoxph(forms, design = design.survey, y = T)
  }

  return(list(coxph = cmodel, timeROC = out))
}




#' @title timeROC_table: extract AUC information from list of timeROChelper object.
#' @description extract AUC information from list of timeROChelper object.
#' @param ListModel list of timeROChelper object
#' @param dec.auc digits for AUC, Default: 3
#' @param dec.p digits for p value, Default: 3
#' @return table of AUC information
#' @details extract AUC information from list of timeROChelper object.
#' @examples
#' #library(survival)
#' #list.timeROC <- lapply(list("age", c("age", "sex")),
#' #                      function(x){
#' #                        timeROChelper("status", "time", x, t = 365, data = lung)
#' #                       })
#' #timeROC_table(list.timeROC)
#' @seealso
#'  \code{\link[stats]{confint}}
#'  \code{\link[data.table]{data.table}}
#' @rdname timeROC_table
#' @importFrom stats confint qnorm
#' @importFrom data.table data.table
#' @importFrom survival concordance

timeROC_table <- function(ListModel, dec.auc =3, dec.p = 3){
  res.roc <- eval(parse(text = paste0("survival::concordance(", paste(paste0("lapply(ListModel, `[[`, 'coxph')[[", seq_along(ListModel), "]]"), collapse = ", "), ")")))
  auc <- res.roc$concordance
  se1.96 <- stats::qnorm(0.975) * sqrt(ifelse(length(ListModel) == 1, res.roc$var, diag(res.roc$var)))
  auc.ci <- paste0(round(auc - se1.96, dec.auc), "-", round(auc + se1.96, dec.auc))
  auc <- round(auc, dec.auc)

  if (length(ListModel) == 1){
    out <- data.table::data.table(paste0("Model ", seq_along(ListModel)), auc, auc.ci)
    names(out) <- c("Prediction Model", "AUC", "95% CI")
  } else{
    auc.pdiff <- c(NA, sapply(seq_along(ListModel)[-1],
                              function(x){
                                contr <- c(-1, 1)
                                dtest <- contr %*% res.roc$concordance[(x-1):x]
                                dvar <- contr %*% res.roc$var[(x-1):x, (x-1):x] %*% contr
                                p <- 2 * pnorm(abs(dtest/sqrt(dvar)), lower.tail = F)
                                p <- ifelse(p < 0.001, "< 0.001", round(p, dec.p))
                                return(p)
                              }))

    out <- data.table::data.table(paste0("Model ", seq_along(ListModel)), auc, auc.ci, auc.pdiff)
    names(out) <- c("Prediction Model", "AUC", "95% CI", "P-value for AUC Difference")
  }


  return(out[])
}




#' @title survIDINRI_helper: Helper function for IDI.INF.OUT in survIDINRI packages
#' @description Helper function for IDI.INF.OUT in survIDINRI packages
#' @param var.event event
#' @param var.time time
#' @param list.vars.ind list of independent variable
#' @param t time
#' @param data data
#' @param dec.auc digits for AUC, Default: 3
#' @param dec.p digits for p value, Default: 3
#' @param id.cluster cluster variable if marginal model, Default: NULL
#' @return IDI, NRI
#' @details Helper function for IDI.INF.OUT in survIDINRI packages
#' @examples
#' #library(survival)
#' #survIDINRI_helper("status", "time", list.vars.ind = list("age", c("age", "sex")),
#' #                  t = 365, data = lung)
#' @seealso
#'  \code{\link[data.table]{data.table}}
#'  \code{\link[stats]{model.matrix}}
#'  \code{\link[survival]{coxph}}
#'  \code{\link[survival]{Surv}}
#'  \code{\link[survIDINRI]{IDI.INF.OUT}}
#'  \code{\link[survIDINRI]{IDI.INF}}
#' @rdname survIDINRI_helper
#' @importFrom data.table data.table
#' @importFrom stats model.matrix
#' @importFrom survival coxph Surv
#' @importFrom survIDINRI IDI.INF.OUT IDI.INF

survIDINRI_helper <- function(var.event, var.time, list.vars.ind, t, data, dec.auc =3, dec.p = 3, id.cluster = NULL){
  data <- data.table::data.table(data)
  data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
  vars <- c(Reduce(union, list(var.event, var.time, unlist(list.vars.ind))))

  if (!is.null(id.cluster)){
    data <- na.omit(data[, .SD, .SDcols = c(vars, id.cluster)])
  } else{
    data <- na.omit(data[, .SD, .SDcols = vars])
  }

  mm <- lapply(list.vars.ind,
               function(x){
                 if (!is.null(id.cluster)){
                   stats::model.matrix(survival::coxph(as.formula(paste0("survival::Surv(", var.time, ",", var.event, ") ~ " , paste(x, collapse = "+") , "+ cluster(", id.cluster, ")")), data = data))
               } else{
                 stats::model.matrix(survival::coxph(as.formula(paste0("survival::Surv(", var.time, ",", var.event, ") ~ " , paste(x, collapse = "+"))), data = data))
                 }
               }

  )

  res <- lapply(seq_along(list.vars.ind)[-1],
                function(x){
                  resIDINRI <- survIDINRI::IDI.INF.OUT(survIDINRI::IDI.INF(data[, .SD, .SDcols = c(var.time, var.event)], mm[[x-1]], mm[[x]], t, npert=200))
                  zz <- lapply(list(resIDINRI[1, ], resIDINRI[2, ]),
                               function(x){
                                 c(round(x[1], dec.auc), paste0(round(x[2], dec.auc), "-", round(x[3], dec.auc)), ifelse(x[4] < 0.001, "< 0.001", round(x[4], dec.p)))
                               })
                  return(unlist(zz))
                })
  out <- data.table::data.table(Reduce(rbind, c(list(rep(NA, 6)), res)))
  names(out) <- c("IDI", "95% CI", "P-value for IDI", "continuous NRI", "95% CI", "P-value for NRI")
  return(out[])

}


#' @title timerocModule: shiny module server for time-dependent roc analysis
#' @description shiny module server for time-dependent roc analysis
#' @param input input
#' @param output output
#' @param session session
#' @param data Reactive data
#' @param data_label Reactuve data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey Reactive survey data. default: NULL
#' @param id.cluster Reactive cluster variable if marginal model, Default: NULL
#' @param iid logical, get CI of AUC, Default: T
#' @param NRIIDI logical, get NRI & IDI, Default: T
#' @return shiny module server for time-dependent roc analysis
#' @details shiny module server for time-dependent roc analysis
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable);library(ggplot2)
#' library(timeROC);library(survIDINRI)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      timerocUI("timeroc")
#'    ),
#'    mainPanel(
#'      plotOutput("plot_timeroc"),
#'      ggplotdownUI("timeroc"),
#'      DTOutput("table_timeroc")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- jstable::mk.lev(mtcars)
#'
#'   out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label,
#'                             data_varStruct = NULL)
#'
#'   output$plot_timeroc <- renderPlot({
#'     print(out_timeroc()$plot)
#'   })
#'
#'   output$table_timeroc <- renderDT({
#'     datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons",
#'               caption = "ROC results",
#'               options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
#'   })
#'}
#' @seealso
#'  \code{\link[stats]{quantile}}
#'  \code{\link[data.table]{setkey}}
#'  \code{\link[data.table]{data.table}}
#'  \code{\link[data.table]{rbindlist}}
#' @rdname timerocModule
#' @export
#' @importFrom stats quantile median
#' @importFrom data.table setkey rbindlist data.table
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location

timerocModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL, iid = T, NRIIDI = T) {

  ## To remove NOTE.
  ListModel <- compare <- level <- variable <- FP <- TP <- model <- NULL

  if (is.null(data_varStruct)){
    data_varStruct <- reactive(list(variable = names(data())))
  }

  vlist <- reactive({

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

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    #factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
    factor_list <- mklist(data_varStruct(), factor_vars)


    conti_vars <- setdiff(names(data()), factor_vars)
    if (!is.null(design.survey)){
      conti_vars <- setdiff(conti_vars, c(names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    }
    conti_vars_positive <- conti_vars[unlist(data()[, lapply(.SD, function(x){min(x, na.rm = T) >= 0}), .SDcols = conti_vars])]
    conti_list <- mklist(data_varStruct(), conti_vars)

    nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars])
    #nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})
    class01_factor <- unlist(data()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}), .SDcols = factor_vars])

    validate(
      need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
    )
    factor_01vars <- factor_vars[class01_factor]

    factor_01_list <- mklist(data_varStruct(), factor_01vars)

    group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]

    return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list, conti_vars_positive = conti_vars_positive,
                factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_vars = group_vars, group_list = group_list, except_vars = except_vars)
    )

  })

  output$eventtime <- renderUI({
    validate(
      need(length(vlist()$factor_01vars) >=1 , "No candidate event variables coded as 0, 1"),
      need(length(vlist()$conti_vars_positive) >=1, "No candidate time variables")
    )

    tagList(
      selectInput(session$ns("event_km"), "Event",
                  choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = F,
                  selected = NULL
      ),
      selectInput(session$ns("time_km"), "Time",
                  choices = mklist(data_varStruct(), vlist()$conti_vars_positive), multiple = F,
                  selected = NULL
      )
    )
  })

  nmodel <- reactiveVal(1)


  output$addmodel <- renderUI({
    if (nmodel() <= 1){
      actionButton(session$ns("add"), label = "Add model", icon("plus"), class = "btn-primary")
    } else if (nmodel() > 1){
      tagList(
        actionButton(session$ns("add"), label = "Add model", icon("plus"), class = "btn-primary"),
        actionButton(session$ns("rmv"), label = "Remove model", icon("minus"))
      )
    }
  })

  indeproc <- reactive({
    req(!is.null(input$event_km))
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


    if (!is.null(design.survey)){
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    } else if (!is.null(id.cluster)){
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, id.cluster()))
    } else{
      indep.roc <- setdiff(names(data()), c(vlist()$except_vars, input$event_km ))
    }
    return(indep.roc)
  })



  output$indep <- renderUI({
    selectInput(session$ns(paste0("indep_km", 1)), paste0("Independent variables for Model ", 1),
                choices = mklist(data_varStruct(), indeproc()), multiple = T,
                selected = unlist(mklist(data_varStruct(), indeproc()))[1])
  })

  observeEvent(input$add, {
    insertUI(
      selector = paste0("div:has(> #", session$ns("add"), ")"),
      where = "beforeBegin",
      ui = selectInput(session$ns(paste0("indep_km", nmodel() + 1)), paste0("Independent variables for Model ", nmodel() + 1),
                       choices = mklist(data_varStruct(), indeproc()), multiple = T,
                       selected = unlist(mklist(data_varStruct(), indeproc()))[1:min(length(indeproc()), nmodel() + 1)])
    )
    nmodel(nmodel() + 1)
  })

  observeEvent(input$rmv, {
    removeUI(
      selector = paste0("div:has(>> #", session$ns(paste0("indep_km", nmodel())), ")")
    )
    nmodel(nmodel() - 1)
  })


  indeps <-  reactive(lapply(1:nmodel(), function(i){input[[paste0("indep_km", i)]]}))

  output$time <- renderUI({
    req(input$time_km)
    tvar <- data()[[input$time_km]]
    if (min(tvar, na.rm = T) >= 365){
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = median(tvar, na.rm= T))
    } else if (max(tvar, na.rm = T) >= 365){
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = 365, step = 5)
    } else if (max(tvar, na.rm = T) >= 12){
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = 12)
    } else{
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = median(tvar, na.rm= T))
    }
  })


  observeEvent(input$subcheck, {
    output$subvar <- renderUI({
      req(input$subcheck == T)
      indeps.unique <- unique(unlist(indeps()))

      var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$time_km, input$event_km,  indeps.unique))
      if (!is.null(id.cluster)){
        var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$time_km, input$event_km, indeps.unique, id.cluster()))
      } else if (!is.null(design.survey)){
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(vlist()$except_vars, input$time_km, input$event_km, indeps.unique)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0 , "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_km"), "Sub-group variables",
                    choices = var_subgroup_list, multiple = T,
                    selected = var_subgroup[1])
      )


    })

  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(length(input$subvar_km) > 0)

    outUI <- tagList()

    for (v in seq_along(input$subvar_km)){
      if (input$subvar_km[[v]] %in% vlist()$factor_vars){
        outUI[[v]] <- selectInput(session$ns(paste0("subval_km", v)), paste0("Sub-group value: ", input$subvar_km[[v]]),
                                  choices = data_label()[variable == input$subvar_km[[v]], level], multiple = T,
                                  selected = data_label()[variable == input$subvar_km[[v]], level][1])
      } else{
        val <- stats::quantile(data()[[input$subvar_km[[v]]]], na.rm = T)
        outUI[[v]] <- sliderInput(session$ns(paste0("subval_km", v)), paste0("Sub-group range: ", input$subvar_km[[v]]),
                                  min = val[1], max = val[5],
                                  value = c(val[2], val[4]))
      }

    }
    outUI

  })





  timerocList <- reactive({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
    #req(!is.null(input$indep_km1))
    #req(!is.null(input$indep_km2))
    for (i in 1:nmodel()){req(!is.null(input[[paste0("indep_km", i)]]))}
    req(!is.null(indeps()))
    collapse.indep <- sapply(1:nmodel(), function(i){paste0(input[[paste0("indep_km", i)]], collapse = "")})
    validate(
      need(anyDuplicated(collapse.indep) == 0, "Please select different models")
    )


    data.km <- data()[complete.cases(data()[, .SD, .SDcols = unique(unlist(indeps()))])]
    label.regress <- data_label()
    data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
    if(input$subcheck == TRUE){
      validate(
        need(length(input$subvar_km) > 0 , "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_km), function(x){length(input[[paste0("subval_km", x)]])})), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_km)){
        if (input$subvar_km[[v]] %in% vlist()$factor_vars){
          data.km <- data.km[get(input$subvar_km[[v]]) %in% input[[paste0("subval_km", v)]]]
        } else{
          data.km <- data.km[get(input$subvar_km[[v]]) >= input[[paste0("subval_km", v)]][1] & get(input$subvar_km[[v]]) <= input[[paste0("subval_km", v)]][2]]
        }
      }
      data.km[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.km)[, c("variable", "level")]
      data.table::setkey(data_label(), "variable", "level")
      data.table::setkey(label.regress2, "variable", "level")
      label.regress <- data_label()[label.regress2]
      data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
    }

    if (is.null(design.survey)){
      if (is.null(id.cluster)){
        res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x, t = input$time_to_roc, data = data.km)})

        if ((nmodel() == 1 | NRIIDI == F)){
          res.tb <- timeROC_table(res.roc)

        } else{
          res.tb <- cbind(timeROC_table(res.roc),
                          survIDINRI_helper(input$event_km, input$time_km,
                                            list.vars.ind = indeps(),
                                            t = input$time_to_roc,
                                            data = data.km))
        }


      } else{
        res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x, t = input$time_to_roc, data = data.km, id.cluster = id.cluster())})

        if (nmodel() == 1 | NRIIDI == F){
          res.tb <- timeROC_table(res.roc)

        } else{
          res.tb <- cbind(timeROC_table(res.roc),
                          survIDINRI_helper(input$event_km, input$time_km,
                                            list.vars.ind = indeps(),
                                            t = input$time_to_roc,
                                            data = data.km, id.cluster = id.cluster())
                          )
        }




      }
      #res.tb <- timeROC_table(res.roc)



    } else{
      data.design <- design.survey()
      label.regress <- data_label()
      data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))
      if(input$subcheck == TRUE){
        validate(
          need(length(input$subvar_km) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_km), function(x){length(input[[paste0("subval_km", x)]])})), "No value for subsetting")
        )

        for (v in seq_along(input$subvar_km)){
          if (input$subvar_km[[v]] %in% vlist()$factor_vars){
            data.design <- subset(data.design, get(input$subvar_km[[v]]) %in% input[[paste0("subval_km", v)]])
          } else{
            data.design <- subset(data.design, get(input$subvar_km[[v]]) >= input[[paste0("subval_km", v)]][1] & get(input$subvar_km[[v]]) <= input[[paste0("subval_km", v)]][2])
          }
        }
        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
        data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))

      }
      res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x,
                                                            t = input$time_to_roc, data = data.km, design.survey = data.design)})

      if (nmodel() == 1 | NRIIDI == F){
        res.tb <- timeROC_table(res.roc)

      } else{
        res.tb <- cbind(timeROC_table(res.roc),
                        survIDINRI_helper(input$event_km, input$time_km,
                                          list.vars.ind = indeps(),
                                          t = input$time_to_roc,
                                          data = data.km))
      }
    }

    res.timeROC <- lapply(res.roc, `[[`, "timeROC")
    data.rocplot <- data.table::rbindlist(
      lapply(1:length(res.timeROC),
             function(x){
               data.table::data.table(FP = res.timeROC[[x]]$FP[, which(res.timeROC[[x]]$times == input$time_to_roc)],
                                      TP = res.timeROC[[x]]$TP[, which(res.timeROC[[x]]$times == input$time_to_roc)],
                                      model = paste0("model ", x))
               }))

    p <- ggplot(data.rocplot, aes(FP, TP, colour = model)) + geom_line() + geom_abline(slope = 1, lty = 2) + xlab("1-Specificity") + ylab("Sensitivity")

    return(list(plot = p, tb = res.tb))
  })





  output$downloadControls <- renderUI({
    tagList(
      column(4,
             selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
                            choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F,
                            selected = "pptx"
             )
      ),
      column(4,
             sliderInput(session$ns("fig_width"), "Width (in):",
                         min = 5, max = 15, value = 8
             )
      ),
      column(4,
             sliderInput(session$ns("fig_height"), "Height (in):",
                         min = 5, max = 15, value = 6
             )
      )
    )
  })

  output$downloadButton <- downloadHandler(
    filename =  function() {
      if (is.null(design.survey)){
        if (is.null(id.cluster)){
          return(paste(input$event_km, "_", input$time_km,"_timeROC.",input$file_ext ,sep=""))
        } else{
          return(paste(input$event_km, "_", input$time_km,"_timeROC_marginal.",input$file_ext ,sep=""))
        }
      } else{
        return(paste(input$event_km, "_", input$time_km,"__timeROC_survey.",input$file_ext ,sep=""))
      }

    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }

                     if (input$file_ext == "pptx"){
                       my_vec_graph <- rvg::dml(ggobj  = timerocList()$plot)
                       doc <- officer::read_pptx()
                       doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                       doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
                       print(doc, target = file)

                     } else{
                       ggsave(file, timerocList()$plot, dpi = 300, units = "in", width = input$fig_width, height =input$fig_height)
                     }

                   })

    }
  )

  return(timerocList)



}


#' @title timerocModule2: shiny module server for time dependent roc analysis- input number of model as integer
#' @description shiny module server for time-dependent roc analysis- input number of model as integer
#' @param input input
#' @param output output
#' @param session session
#' @param data Reactive data
#' @param data_label Reactuve data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey Reactive survey data. default: NULL
#' @param id.cluster Reactive cluster variable if marginal model, Default: NULL
#' @param iid logical, get CI of AUC, Default: T
#' @param NRIIDI logical, get NRI & IDI, Default: T
#' @return shiny module server for time dependent roc analysis- input number of model as integer
#' @details shiny module server for time dependent roc analysis- input number of model as integer
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable);library(ggplot2)
#' library(timeROC);library(survIDINRI)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      timerocUI("timeroc")
#'    ),
#'    mainPanel(
#'      plotOutput("plot_timeroc"),
#'      ggplotdownUI("timeroc"),
#'      DTOutput("table_timeroc")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- jstable::mk.lev(mtcars)
#'
#'   out_timeroc <- callModule(timerocModule2, "timeroc", data = data, data_label = data.label,
#'                             data_varStruct = NULL)
#'
#'   output$plot_timeroc <- renderPlot({
#'     print(out_timeroc()$plot)
#'   })
#'
#'   output$table_timeroc <- renderDT({
#'     datatable(out_timeroc()$tb, rownames=F, editable = F, extensions= "Buttons",
#'               caption = "ROC results",
#'               options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE)))
#'   })
#'}
#' @seealso
#'  \code{\link[stats]{quantile}}
#'  \code{\link[data.table]{setkey}}
#'  \code{\link[data.table]{data.table}}
#'  \code{\link[data.table]{rbindlist}}
#' @rdname timerocModule
#' @export
#' @importFrom stats quantile median
#' @importFrom data.table setkey rbindlist data.table
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location
timerocModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL, iid = T, NRIIDI = T) {

  ## To remove NOTE.
  ListModel <- compare <- level <- variable <- FP <- TP <- model <- NULL

  if (is.null(data_varStruct)){
    data_varStruct <- reactive(list(variable = names(data())))
  }

  vlist <- reactive({

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

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    #factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
    factor_list <- mklist(data_varStruct(), factor_vars)


    conti_vars <- setdiff(names(data()), factor_vars)
    if (!is.null(design.survey)){
      conti_vars <- setdiff(conti_vars, c(names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    }
    conti_vars_positive <- conti_vars[unlist(data()[, lapply(.SD, function(x){min(x, na.rm = T) >= 0}), .SDcols = conti_vars])]
    conti_list <- mklist(data_varStruct(), conti_vars)

    nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars])
    #nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})
    class01_factor <- unlist(data()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}), .SDcols = factor_vars])

    validate(
      need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
    )
    factor_01vars <- factor_vars[class01_factor]

    factor_01_list <- mklist(data_varStruct(), factor_01vars)

    group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]

    return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list, conti_vars_positive = conti_vars_positive,
                factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_vars = group_vars, group_list = group_list, except_vars = except_vars)
    )

  })

  output$eventtime <- renderUI({
    validate(
      need(length(vlist()$factor_01vars) >=1 , "No candidate event variables coded as 0, 1"),
      need(length(vlist()$conti_vars_positive) >=1, "No candidate time variables")
    )

    tagList(
      selectInput(session$ns("event_km"), "Event",
                  choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = F,
                  selected = NULL
      ),
      selectInput(session$ns("time_km"), "Time",
                  choices = mklist(data_varStruct(), vlist()$conti_vars_positive), multiple = F,
                  selected = NULL
      )
    )
  })

  output$addmodel <- renderUI({
    radioButtons(session$ns("nmodel"), "Number of models", 1:5, selected = 1, inline = T)
  })

  nmodel <- reactive(as.integer(input$nmodel))


  indeproc <- reactive({
    req(!is.null(input$event_km))
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


    if (!is.null(design.survey)){
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    } else if (!is.null(id.cluster)){
      indep.roc <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, id.cluster()))
    } else{
      indep.roc <- setdiff(names(data()), c(vlist()$except_vars, input$event_km ))
    }
    return(indep.roc)
  })



  output$indep <- renderUI({
    req(nmodel())
    lapply(1:nmodel(), function(x){
      selectInput(session$ns(paste0("indep_km", x)), paste0("Independent variables for Model ", x),
                  choices = mklist(data_varStruct(), indeproc()), multiple = T,
                  selected = unlist(mklist(data_varStruct(), indeproc()))[x])
    })

  })



  indeps <-  reactive(lapply(1:nmodel(), function(i){input[[paste0("indep_km", i)]]}))

  output$time <- renderUI({
    req(input$time_km)
    tvar <- data()[[input$time_km]]
    if (min(tvar, na.rm = T) >= 365){
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = median(tvar, na.rm= T))
    } else if (max(tvar, na.rm = T) >= 365){
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = 365, step = 5)
    } else if (max(tvar, na.rm = T) >= 12){
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = 12)
    } else{
      sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = median(tvar, na.rm= T))
    }
  })


  observeEvent(input$subcheck, {
    output$subvar <- renderUI({
      req(input$subcheck == T)
      indeps.unique <- unique(unlist(indeps()))

      var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$time_km, input$event_km,  indeps.unique))
      if (!is.null(id.cluster)){
        var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$time_km, input$event_km, indeps.unique, id.cluster()))
      } else if (!is.null(design.survey)){
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(vlist()$except_vars, input$time_km, input$event_km, indeps.unique)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0 , "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_km"), "Sub-group variables",
                    choices = var_subgroup_list, multiple = T,
                    selected = var_subgroup[1])
      )


    })

  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(length(input$subvar_km) > 0)

    outUI <- tagList()

    for (v in seq_along(input$subvar_km)){
      if (input$subvar_km[[v]] %in% vlist()$factor_vars){
        outUI[[v]] <- selectInput(session$ns(paste0("subval_km", v)), paste0("Sub-group value: ", input$subvar_km[[v]]),
                                  choices = data_label()[variable == input$subvar_km[[v]], level], multiple = T,
                                  selected = data_label()[variable == input$subvar_km[[v]], level][1])
      } else{
        val <- stats::quantile(data()[[input$subvar_km[[v]]]], na.rm = T)
        outUI[[v]] <- sliderInput(session$ns(paste0("subval_km", v)), paste0("Sub-group range: ", input$subvar_km[[v]]),
                                  min = val[1], max = val[5],
                                  value = c(val[2], val[4]))
      }

    }
    outUI

  })





  timerocList <- reactive({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
    #req(!is.null(input$indep_km1))
    #req(!is.null(input$indep_km2))
    for (i in 1:nmodel()){req(!is.null(input[[paste0("indep_km", i)]]))}
    req(!is.null(indeps()))
    collapse.indep <- sapply(1:nmodel(), function(i){paste0(input[[paste0("indep_km", i)]], collapse = "")})
    validate(
      need(anyDuplicated(collapse.indep) == 0, "Please select different models")
    )


    data.km <- data()
    label.regress <- data_label()
    data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
    if(input$subcheck == TRUE){
      validate(
        need(length(input$subvar_km) > 0 , "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_km), function(x){length(input[[paste0("subval_km", x)]])})), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_km)){
        if (input$subvar_km[[v]] %in% vlist()$factor_vars){
          data.km <- data.km[get(input$subvar_km[[v]]) %in% input[[paste0("subval_km", v)]]]
        } else{
          data.km <- data.km[get(input$subvar_km[[v]]) >= input[[paste0("subval_km", v)]][1] & get(input$subvar_km[[v]]) <= input[[paste0("subval_km", v)]][2]]
        }
      }
      data.km[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.km)[, c("variable", "class", "level")]
      data.table::setkey(data_label(), "variable", "class", "level")
      data.table::setkey(label.regress2, "variable", "class", "level")
      label.regress <- data_label()[label.regress2]
      data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
    }

    if (is.null(design.survey)){
      if (is.null(id.cluster)){
        res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x, t = input$time_to_roc, data = data.km)})

        if (nmodel() == 1 | NRIIDI == F){
          res.tb <- timeROC_table(res.roc)

        } else{
          res.tb <- cbind(timeROC_table(res.roc),
                          survIDINRI_helper(input$event_km, input$time_km,
                                            list.vars.ind = indeps(),
                                            t = input$time_to_roc,
                                            data = data.km))
        }


      } else{
        res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x, t = input$time_to_roc, data = data.km, id.cluster = id.cluster())})

        if (nmodel() == 1 | NRIIDI == F){
          res.tb <- timeROC_table(res.roc)

        } else{
          res.tb <- cbind(timeROC_table(res.roc),
                          survIDINRI_helper(input$event_km, input$time_km,
                                            list.vars.ind = indeps(),
                                            t = input$time_to_roc,
                                            data = data.km, id.cluster = id.cluster())
          )
        }




      }
      #res.tb <- timeROC_table(res.roc)



    } else{
      data.design <- design.survey()
      label.regress <- data_label()
      data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))
      if(input$subcheck == TRUE){
        validate(
          need(length(input$subvar_km) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_km), function(x){length(input[[paste0("subval_km", x)]])})), "No value for subsetting")
        )

        for (v in seq_along(input$subvar_km)){
          if (input$subvar_km[[v]] %in% vlist()$factor_vars){
            data.design <- subset(data.design, get(input$subvar_km[[v]]) %in% input[[paste0("subval_km", v)]])
          } else{
            data.design <- subset(data.design, get(input$subvar_km[[v]]) >= input[[paste0("subval_km", v)]][1] & get(input$subvar_km[[v]]) <= input[[paste0("subval_km", v)]][2])
          }
        }
        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
        data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))

      }
      res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x,
                                                            t = input$time_to_roc, data = data.km, design.survey = data.design)})

      if (nmodel() == 1 | NRIIDI == F){
        res.tb <- timeROC_table(res.roc)

      } else{
        res.tb <- cbind(timeROC_table(res.roc),
                        survIDINRI_helper(input$event_km, input$time_km,
                                          list.vars.ind = indeps(),
                                          t = input$time_to_roc,
                                          data = data.km))
      }
    }

    res.timeROC <- lapply(res.roc, `[[`, "timeROC")
    data.rocplot <- data.table::rbindlist(
      lapply(1:length(res.timeROC),
             function(x){
               data.table::data.table(FP = res.timeROC[[x]]$FP[, which(res.timeROC[[x]]$times == input$time_to_roc)],
                                      TP = res.timeROC[[x]]$TP[, which(res.timeROC[[x]]$times == input$time_to_roc)],
                                      model = paste0("model ", x))
             }))

    p <- ggplot(data.rocplot, aes(FP, TP, colour = model)) + geom_line() + geom_abline(slope = 1, lty = 2) + xlab("1-Specificity") + ylab("Sensitivity")

    return(list(plot = p, tb = res.tb))
  })





  output$downloadControls <- renderUI({
    tagList(
      column(4,
             selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
                            choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F,
                            selected = "pptx"
             )
      ),
      column(4,
             sliderInput(session$ns("fig_width"), "Width (in):",
                         min = 5, max = 15, value = 8
             )
      ),
      column(4,
             sliderInput(session$ns("fig_height"), "Height (in):",
                         min = 5, max = 15, value = 6
             )
      )
    )
  })

  output$downloadButton <- downloadHandler(
    filename =  function() {
      if (is.null(design.survey)){
        if (is.null(id.cluster)){
          return(paste(input$event_km, "_", input$time_km,"_timeROC.",input$file_ext ,sep=""))
        } else{
          return(paste(input$event_km, "_", input$time_km,"_timeROC_marginal.",input$file_ext ,sep=""))
        }
      } else{
        return(paste(input$event_km, "_", input$time_km,"__timeROC_survey.",input$file_ext ,sep=""))
      }

    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }

                     if (input$file_ext == "pptx"){
                       my_vec_graph <- rvg::dml(ggobj  = timerocList()$plot)
                       doc <- officer::read_pptx()
                       doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                       doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
                       print(doc, target = file)

                     } else{
                       ggsave(file, timerocList()$plot, dpi = 300, units = "in", width = input$fig_width, height =input$fig_height)
                     }

                   })

    }
  )

  return(timerocList)



}
