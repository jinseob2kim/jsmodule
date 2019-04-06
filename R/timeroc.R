#' @title timerocUI: shiny module UI for time-dependent roc analysis
#' @description Shiny module UI for time-dependent roc analysis
#' @param id id
#' @return Shiny module UI for time-dependent roc analysis
#' @details Shiny module UI for time-dependent roc analysis
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable);library(ggplot2)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      timerocUI("timeroc")
#'    ),
#'    mainPanel(
#'      plotOutput("plot_timeroc"),
#'      ggplotdownUI("downloadButton_timeroc"),
#'      DTOutput("table_timeroc")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_kaplan <- callModule(timerocModule, "kaplan", data = data, data_label = data.label,
#'                            data_varStruct = NULL)
#'
#'   output$plot_timeroc <- renderPlot({
#'     print(out_timeroc())
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
#' @param event event
#' @param time time
#' @param vars.ind independent variable
#' @param t time
#' @param data data
#' @param design.survey survey data, Default: NULL
#' @param id.cluster cluster variable if marginal model, Default: NULL
#' @return timeROC object
#' @details Helper function for timerocModule
#' @examples
#' library(survival)
#' timeROChelper(status, time, c("age", "sex"), t = 365, data = lung)
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



timeROChelper <- function(event, time, vars.ind, t, data, design.survey = NULL, id.cluster = NULL) {
  data[[event]] <- as.numeric(as.vector(data[[event]]))
  form <- paste0("Surv(", time, ",", event, ") ~ " , paste(vars.ind, collapse = "+"))

  if (!is.null(id.cluster)){
    forms <- as.formula(paste0(form, "+ cluster(", id.cluster, ")"))
    data <- na.omit(data[, .SD, .SDcols = c(event, time, vars.ind, id.cluster)])
  } else{
    forms <- as.formula(form)
    data <- na.omit(data[, .SD, .SDcols = c(event, time, vars.ind)])
  }

  cmodel <- NULL
  if (is.null(design.survey)){
    cmodel <- survival::coxph(forms, data = data)
  } else{
    cmodel <- survey::svycoxph(as.formula(form), design = design.survey)
  }
  data$lp <- stats::predict(cmodel, type = "lp")
  out <- timeROC::timeROC(T = data[[time]],
                          delta = data[[event]],
                          marker = data$lp,
                          cause = 1,
                          weighting="marginal",
                          time = t,
                          iid = TRUE)

  return(out)
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
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{quantile}}
#'  \code{\link[data.table]{setkey}}
#' @rdname timerocModule
#' @export
#' @importFrom stats quantile
#' @importFrom data.table setkey
timerocModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL) {

  ## To remove NOTE.
  level <- val_label <- variable <- NULL

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


  output$time <- renderUI({
    req(input$time_km)
    tvar <- data()[[input$time_km]]
    sliderInput("time_to_roc", "Time to analyze", min = min(tvar, na.rm= T), max = max(tvar, na.rm= T), value = median(tvar, na.rm= T))
  })



  timerocList <- reactive({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
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
      res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x, t = input$time_to_roc, data = data.km)})
      #res.roc <- timeROC_helper(input$event_km, input$time_km, vars.ind =  "Group.Gy.", t = input$time_to_roc, data = data.km)

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
      res.roc <- lapply(indeps(), function(x){timeROChelper(input$event_km, input$time_km, vars.ind =  x, t = input$time_to_roc, data = data.km, design.survey = data.design)})
    }
    return(res.roc)
  })


  rocplot <- reactive({
    plot(timerocList()[[1]], col = 1, title = F, time =  input$time_to_roc, lty = 1)
    if (input$n_model > 1){
      for (i in 2:input$n_model){
        plot(timerocList()[[i]], col = i, add =T, time = input$time_to_roc, lty = i)
      }
    }
    legend("bottomright", paste0("model ", 1:input$n_model), col=1:input$n_model,lty=1:input$n_model)
  })

  return(rocplot)




}



