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
    uiOutput(ns("nmodel")),
    uiOutput(ns("indep")),
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
#' @return timeROC object
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
  form <- paste0("survival::Surv(", var.time, ",", var.event, ") ~ " , paste(vars.ind, collapse = "+"))

  if (!is.null(id.cluster)){
    forms <- as.formula(paste0("survival::Surv(", var.time, ",", var.event, ") ~ " , paste(vars.ind, collapse = "+"), "+ cluster(", id.cluster, ")"))
    #forms <- as.formula(paste0(form, "+ cluster(", id.cluster, ")"))
    #data <- na.omit(data[, .SD, .SDcols = c(var.event, var.time, vars.ind, id.cluster)])
  } else{
    forms <- as.formula(form)
    #data <- na.omit(data[, .SD, .SDcols = c(var.event, var.time, vars.ind)])
  }

  cmodel <- NULL
  if (is.null(design.survey)){
    cmodel <- survival::coxph(forms, data = data, y = T)
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
                          times = t,
                          iid = TRUE)

  return(out)
}




#' @title timeROC_table: extract AUC information from list of timeROC object.
#' @description extract AUC information from list of timeROC object.
#' @param ListModel list of timeROC object
#' @param dec.auc digits for AUC, Default: 3
#' @param dec.p digits for p value, Default: 3
#' @return table of AUC information
#' @details extract AUC information from list of timeROC object.
#' @examples
#' #library(survival)
#' #list.timeROC <- lapply(list("age", c("age", "sex")),
#' #                      function(x){
#' #                        timeROChelper("status", "time", x, t = 365, data = lung)
#' #                       })
#' #timeROC_table(list.timeROC)
#' @seealso
#'  \code{\link[stats]{confint}}
#'  \code{\link[data.table]{data.table-package}}
#' @rdname timeROC_table
#' @importFrom stats confint
#' @importFrom data.table data.table

timeROC_table <- function(ListModel, dec.auc =3, dec.p = 3){
  auc <- round(sapply(ListModel, function(x){x$AUC[[2]]}), dec.auc)
  auc.ci <- sapply(ListModel, function(x){paste(round(stats::confint(x)$CI_AUC/100, dec.auc), collapse = "-")})
  auc.pdiff <- c(NA, sapply(seq_along(ListModel)[-1],
                            function(x){
                              p <- timeROC::compare(ListModel[[x]], ListModel[[x-1]])$p_values_AUC[2]
                              p <- ifelse(p < 0.001, "< 0.001", round(p, dec.p))
                              return(p)
                            }))

  out <- data.table::data.table(paste0("Model ", seq_along(ListModel)), auc, auc.ci, auc.pdiff)
  names(out) <- c("Prediction Model", "AUC", "95% CI", "P-value for AUC Difference")
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
#'  \code{\link[data.table]{data.table-package}}
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
timerocModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL) {

  ## To remove NOTE.
  compare <- level <- variable <- FP <- TP <- model <- NULL

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

    return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list,
                factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_vars = group_vars, group_list = group_list, except_vars = except_vars)
    )

  })

  output$eventtime <- renderUI({
    validate(
      need(length(vlist()$factor_01vars) >=1 , "No candidate event variables coded as 0, 1"),
      need(length(vlist()$conti_list) >=1, "No candidate time variables")
    )

    tagList(
      selectInput(session$ns("event_km"), "Event",
                  choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = F,
                  selected = NULL
      ),
      selectInput(session$ns("time_km"), "Time",
                  choices = vlist()$conti_list, multiple = F,
                  selected = NULL
      )
    )
  })

  output$nmodel <- renderUI({
    sliderInput(session$ns("n_model"), "Number of models", value = 2, min = 1, max = 5)

  })

  output$indep <- renderUI({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
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
      indep.km <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, input$time_km, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    } else if (!is.null(id.cluster)){
      indep.km <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, input$time_km, id.cluster()))
    } else{
      indep.km <- setdiff(names(data()), c(vlist()$except_vars, input$event_km, input$time_km ))
    }

    dynamic_selection_list <- lapply(1:input$n_model, function(i) {
      selectInput(session$ns(paste0("indep_km", i)), paste0("Independent variables for Model ", i),
                  choices = mklist(data_varStruct(), indep.km), multiple = T,
                  selected = unlist(mklist(data_varStruct(), indep.km))[1]
                  )
    })

  })

  indeps <-  reactive(lapply(1:input$n_model, function(i){input[[paste0("indep_km", i)]]}))

  output$time <- renderUI({
    req(input$time_km)
    tvar <- data()[[input$time_km]]
    sliderInput(session$ns("time_to_roc"), "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = median(tvar, na.rm= T))
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
        selectInput(session$ns("subvar_km"), "Sub-group variable",
                    choices = var_subgroup_list, multiple = F,
                    selected = var_subgroup[1])
      )


    })

  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(input$subvar_km)

    if (input$subvar_km %in% vlist()$factor_vars){
      selectInput(session$ns("subval_km"), "Sub-group value",
                  choices = data_label()[variable == input$subvar_km, level], multiple = T,
                  selected = data_label()[variable == input$subvar_km, level][1])
    } else{
      val <- stats::quantile(data()[[input$subvar_km]], na.rm = T)
      sliderInput(session$ns("subval_km"), "Sub-group range",
                  min = val[1], max = val[5],
                  value = c(val[2], val[4]))
    }

  })





  timerocList <- reactive({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
    #req(!is.null(input$indep_km1))
    #req(!is.null(input$indep_km2))
    for (i in 1:input$n_model){req(!is.null(input[[paste0("indep_km", i)]]))}
    req(!is.null(indeps()))

    data.km <- data()
    label.regress <- data_label()
    data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
    if(input$subcheck == TRUE){
      req(input$subvar_km)
      req(input$subval_km)
      if (input$subvar_km %in% vlist()$factor_vars){
        data.km <- data.km[get(input$subvar_km) %in% input$subval_km]
      } else{
        data.km <- data.km[get(input$subvar_km) >= input$subval_km[1] & get(input$subvar_km) <= input$subval_km[2]]
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
        res.tb <- cbind(timeROC_table(res.roc),
                        survIDINRI_helper(input$event_km, input$time_km,
                                          list.vars.ind = indeps(),
                                          t = input$time_to_roc,
                                          data = data.km))
      } else{
        res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x, t = input$time_to_roc, data = data.km, id.cluster = id.cluster())})
        res.tb <- cbind(timeROC_table(res.roc),
                        survIDINRI_helper(input$event_km, input$time_km,
                                          list.vars.ind = indeps(),
                                          t = input$time_to_roc,
                                          data = data.km, id.cluster = id.cluster()))

      }
      #res.tb <- timeROC_table(res.roc)



    } else{
      data.design <- design.survey()
      label.regress <- data_label()
      data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))
      if(input$subcheck == TRUE){
        req(input$subvar_km)
        req(input$subval_km)

        if (input$subvar_km %in% vlist()$factor_vars){
          data.design <- subset(data.design, get(input$subvar_km) %in% input$subval_km)
        } else{
          data.design <- subset(data.design, get(input$subvar_km) >= input$subval_km[1] & get(input$subvar_km) <= input$subval_km[2])
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
      res.tb <- cbind(timeROC_table(res.roc),
                      survIDINRI_helper(input$event_km, input$time_km,
                                        list.vars.ind = indeps(),
                                        t = input$time_to_roc, data = data.km))
    }

    data.rocplot <- data.table::rbindlist(
      lapply(1:length(res.roc),
             function(x){
               data.table::data.table(FP = res.roc[[x]]$FP[, which(res.roc[[x]]$times == input$time_to_roc)],
                                      TP = res.roc[[x]]$TP[, which(res.roc[[x]]$times == input$time_to_roc)],
                                      model = paste0("model ", x))
               }))

    p <- ggplot(data.rocplot, aes(FP, TP, colour = model)) + geom_line() + geom_abline(slope = 1, lty = 2) + xlab("1-Specificity") + ylab("Sensitivity")

    return(list(plot = p, tb = res.tb))
  })





  output$downloadControls <- renderUI({
    tagList(
      column(4,
             selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
                            choices = c("jpg","pdf", "tiff", "svg"), multiple = F,
                            selected = "jpg"
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

                     ggsave(file, timerocList()$plot, dpi = 300, units = "in", width = input$fig_width, height =input$fig_height)
                   })

    }
  )

  return(timerocList)



}



