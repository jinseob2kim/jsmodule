#' @title mklist: function to make variable list lncluding specific variables.
#' @description Function to make variable list lncluding specific variables.
#' @param varlist Original variable list.
#' @param vars variable to include.
#' @return variable list lncluding specific variables.
#' @details Internal function
#' @examples
#'  data_varStruct <- list(variable = names(mtcars))
#'  mklist(data_varStruct, names(mtcars))
#' @rdname mklist
#' @export


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

#' @title mksetdiff: function to make variable list excluding specific variables.
#' @description Function to make variable list excluding specific variables.
#' @param varlist Original variable list
#' @param vars variable to exclude.
#' @return variable list excluding specific variables.
#' @details Internal function
#' @examples
#'  data_varStruct <- list(variable = names(mtcars))
#'  mksetdiff(data_varStruct, "mpg")
#' @rdname mksetdiff
#' @export


mksetdiff <- function(varlist, vars){
  lapply(varlist,
         function(x){
           inter <- setdiff(x, vars)
           if (length(inter) == 1){
             inter <- c(inter, "")
           }
           return(inter)
         })
}




#' @title regressModuleUI: shiny modulde UI for linear regression.
#' @description Shiny modulde UI for linear regression.
#' @param id id
#' @return Shiny modulde UI for linear regression.
#' @details Shiny modulde UI for linear regression.
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      regressModuleUI("linear")
#'    ),
#'    mainPanel(
#'      DTOutput("lineartable")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label,
#'                            data_varStruct = NULL)
#'
#'   output$lineartable <- renderDT({
#'     datatable(out_linear()$table, rownames=T, caption = out_linear()$caption)
#'   })
#'}
#' @rdname regressModuleUI
#' @export

regressModuleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("dep")),
    uiOutput(ns("indep")),
    sliderInput(ns("decimal"), "Digits",
                min = 1, max = 4, value = 2
    ),
    checkboxInput(ns("regressUI_subcheck"), "Sub-group analysis"),
    uiOutput(ns("regressUI_subvar")),
    uiOutput(ns("regressUI_subval")),
    checkboxInput(ns("step_check"), "Stepwise variable selection"),
    uiOutput(ns("step_direction")),
    uiOutput(ns("step_scope"))
  )
}




#' @title regressModule2: Shiny modulde server for linear regression for reactive data.
#' @description Shiny modulde server for linear regression for reactive data.
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactive data label
#' @param data_varStruct List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey reactive survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis, Default: T
#' @param limit.unires Change to default.unires = F if number of independent variables > limit.unires, Default: 20
#' @return Shiny modulde server for linear regression.
#' @details Shiny modulde server for linear regression.
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      regressModuleUI("linear")
#'    ),
#'    mainPanel(
#'      DTOutput("lineartable")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label,
#'                            data_varStruct = NULL)
#'
#'   output$lineartable <- renderDT({
#'     datatable(out_linear()$table, rownames=T, caption = out_linear()$caption)
#'   })
#'}
#' @rdname regressModule2
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame model.matrix step
#' @importFrom epiDisplay regress.display
#' @importFrom purrr map_lgl

regressModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T, limit.unires = 20) {

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

    group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]

    return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list,
                group_vars = group_vars, group_list = group_list, except_vars = except_vars)
    )

  })



  output$dep <- renderUI({
    tagList(
      selectInput(session$ns("dep_vars"), "Dependent variable",
                  choices = vlist()$conti_list, multiple = F,
                  selected = vlist()$conti_vars[1]
      )
    )
  })


  output$indep <- renderUI({
    req(!is.null(input$dep_vars))
    if (is.null(design.survey)){
      vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  input$dep_vars)
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            coef <- tryCatch(summary(glm(forms, data = data()))$coefficients, error = function(e){return(NULL)})
                            #coef <- summary(glm(forms, data = data()))$coefficients
                            sigOK <- ifelse(is.null(coef), F, !all(coef[-1, "Pr(>|t|)"] > 0.05))
                            return(sigOK)
                          })
        if (length(varsIni[varsIni == T]) > limit.unires) {varsIni <- c(T, rep(F, length(vars) -1))}
      } else{
        varsIni <- c(T, rep(F, length(vars) -1))
      }

    } else{
      vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  c(input$dep_vars, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            coef <- tryCatch(summary(survey::svyglm(forms, design = design.survey()))$coefficients, error = function(e){return(NULL)})
                            #coef <- summary(survey::svyglm(forms, design = design.survey()))$coefficients
                            sigOK <- ifelse(is.null(coef), F, !all(coef[-1, "Pr(>|t|)"] > 0.05))
                            return(sigOK)
                          })
        if (length(varsIni[varsIni == T]) > limit.unires) {varsIni <- c(T, rep(F, length(vars) -1))}
      } else{
        varsIni <- c(T, rep(F, length(vars) -1))
      }

    }



    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = mklist(data_varStruct(), vars), multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  observeEvent(input$regressUI_subcheck, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)
      var_subgroup <- setdiff(names(data()), c(input$dep_vars, input$indep_vars))
      if (!is.null(design.survey)){
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(input$dep_vars, input$indep_vars)))
      }
      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0 , "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_regress"), "Sub-group variables",
                    choices = var_subgroup_list, multiple = T,
                    selected = var_subgroup[1])
      )
    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(length(input$subvar_regress) > 0)

      outUI <- tagList()

      for (v in seq_along(input$subvar_regress)){
        if (input$subvar_regress[[v]] %in% vlist()$factor_vars){
          outUI[[v]] <- selectInput(session$ns(paste0("subval_regress", v)), paste0("Sub-group value: ", input$subvar_regress[[v]]),
                                    choices = data_label()[variable == input$subvar_regress[[v]], level], multiple = T,
                                    selected = data_label()[variable == input$subvar_regress[[v]], level][1])
        } else{
          val <- stats::quantile(data()[[input$subvar_regress[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(session$ns(paste0("subval_regress", v)), paste0("Sub-group range: ", input$subvar_regress[[v]]),
                                    min = val[1], max = val[5],
                                    value = c(val[2], val[4]))
        }

      }
      outUI

    })

  })

  observeEvent(input$step_check, {
    output$step_direction <- renderUI({
      req(input$step_check == T)
      radioButtons(session$ns("step_direction"), "Step direction", choices = c("backward", "forward", "both"), selected = "backward", inline = T)
    })

    output$step_scope <- renderUI({
      req(input$step_check == T)
      req(input$indep_vars)
      tagList(
        fluidRow(
          column(6, selectInput(session$ns("step_lower"), "Lower limit", choices = input$indep_vars, selected = NULL, multiple = T)),
          column(6, selectInput(session$ns("step_upper"), "Upper limit", choices = input$indep_vars, selected = input$indep_vars, multiple = T))
        )

      )
    })
  })





  out <- reactive({
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    y <- input$dep_vars
    xs <- input$indep_vars
    form <- as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))

    if (is.null(design.survey)){
      data.regress <- data()
      label.regress <- data_label()
      if(input$regressUI_subcheck == T){
        validate(
          need(length(input$subvar_regress) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_regress), function(x){length(input[[paste0("subval_regress", x)]])})), "No value for subsetting")
        )

        for (v in seq_along(input$subvar_regress)){
          if (input$subvar_regress[[v]] %in% vlist()$factor_vars){
            data.regress <- data.regress[get(input$subvar_regress[[v]]) %in% input[[paste0("subval_regress", v)]]]
          } else{
            data.regress <- data.regress[get(input$subvar_regress[[v]]) >= input[[paste0("subval_regress", v)]][1] & get(input$subvar_regress[[v]]) <= input[[paste0("subval_regress", v)]][2]]
          }
        }

        data.regress[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.regress)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
      }
      mf <- model.frame(form, data.regress)
      validate(
        need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
      )
      lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
      validate(
        need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
      )


      ## Warning when multicollinearity
      mm <- model.matrix(form, data.regress)
      zz <- tryCatch(solve(t(as.matrix(mm)) %*% as.matrix(mm)), error = function(e) e)
      if ("error" %in% class(zz)) {
        warn <- "There are strongly correlated independent values. Please see correlation in `Plot- Scatter plot` tab"
      } else{ warn <- NULL}

      res.linear <- glm(form, data = data.regress)
      if (input$step_check == T){
        validate(
          need(!is.null(input$step_upper), "Upper limits can't be NULL, please select at least 1 variable."),
          need(length((setdiff(input$step_lower, input$step_upper))) == 0, "Upper limits must include lower limits. Please add the variables to upper limits")
        )
        scope <- lapply(list(input$step_upper, input$step_lower), function(x){
          as.formula(ifelse(is.null(x), "~1", paste0("~", paste(x, collapse = "+"))))
        })

        res.linear <- stats::step(glm(form, data = data.regress[complete.cases(data.regress[, .SD, .SDcols = c(y, xs)])]), direction = input$step_direction, scope = list(upper = scope[[1]], lower = scope[[2]]))
      }
      tb.linear <- jstable::glmshow.display(res.linear, decimal = input$decimal)
      cap.linear <- paste("Linear regression predicting ", label.regress[variable == y, var_label][1], sep="")
      if(input$regressUI_subcheck == T){
        for (v in seq_along(input$subvar_regress)){
          if (input$subvar_regress[[v]] %in% vlist()$factor_vars){
            cap.linear <- paste(cap.linear, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_regress[[v]] & level %in% input[[paste0("subval_regress", v)]], val_label], collapse = ", "), sep = "")
          } else{
            cap.linear <- paste(cap.linear, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(input[[paste0("subval_regress", v)]], collapse = "~"), sep = "")
          }
        }
      }
      if (input$step_check == T){
        cap.linear <- paste0(cap.linear, "- stepwise selection")
      }
      out.linear <- jstable::LabelepiDisplay(tb.linear, label = T, ref = label.regress)
    } else{
      data.design <- design.survey()
      label.regress <- data_label()
      if(input$regressUI_subcheck == T){
        validate(
          need(length(input$subvar_regress) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_regress), function(x){length(input[[paste0("subval_regress", x)]])})), "No value for subsetting")
        )
        for (v in seq_along(input$subvar_regress)){
          if (input$subvar_regress[[v]] %in% vlist()$factor_vars){
            data.design <- subset(data.design, get(input$subvar_regress[[v]]) %in% input[[paste0("subval_regress", v)]])
          } else{
            data.design <- subset(data.design, get(input$subvar_regress[[v]]) >= input[[paste0("subval_regress", v)]][1] & get(input$subvar_regress[[v]]) <= input[[paste0("subval_regress", v)]][2])
          }
        }

        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
      }

      mf <- model.frame(form, data.design$variables)
      validate(
        need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
      )
      lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
      validate(
        need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
      )


      res.svyglm <- survey::svyglm(form, design = data.design)
      if (input$step_check == T){
        validate(
          need(!is.null(input$step_upper), "Upper limits can't be NULL, please select at least 1 variable."),
          need(length((setdiff(input$step_lower, input$step_upper))) == 0, "Upper limits must include lower limits. Please add the variables to upper limits"),
          need(input$step_direction != "both", "Only forward/backward directions are supported"),
          need(is.null(design.survey), "Survey glm can't support stepwise selection")
        )
        scope <- lapply(list(input$step_upper, input$step_lower), function(x){
          as.formula(ifelse(is.null(x), "~1", paste0("~", paste(x, collapse = "+"))))
        })

        d2 <- na.omit(data.design[, c(y, xs)])

        res.svyglm <- stats::step(survey::svyglm(form, design = d2),
                                                 direction = input$step_direction, scope = list(upper = scope[[1]], lower = scope[[2]]))
      }

      tb.svyglm <- jstable::svyregress.display(res.svyglm, decimal = input$decimal)
      cap.linear <- paste("Linear regression predicting ", label.regress[variable == y, var_label][1], "- survey data", sep="")
      if (input$regressUI_subcheck == T){
        for (v in seq_along(input$subvar_regress)){
          if (input$subvar_regress[[v]] %in% vlist()$factor_vars){
            cap.linear <- paste(cap.linear, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_regress[[v]] & level %in% input[[paste0("subval_regress", v)]], val_label], collapse = ", "), sep = "")
          } else{
            cap.linear <- paste(cap.linear, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(input[[paste0("subval_regress", v)]], collapse = "~"), sep = "")
          }
        }
      }
      if (input$step_check == T){
        cap.linear <- paste0(cap.linear, "- stepwise selection")
      }
      out.linear <- jstable::LabelepiDisplay(tb.svyglm, label = T, ref = label.regress)
      warn <- NULL
    }

    #out.linear = summary(res.linear)$coefficients
    #sig = ifelse(out.linear[, 4] <= 0.05, "**", "NA")
    return(list(table = out.linear, caption = cap.linear, warning = warn))
  })

  return(out)


}



#' @title logisticModule2: Shiny modulde server for logistic regression for reactive data.
#' @description Shiny modulde server for logistic regression for reactive data.
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactive data label
#' @param data_varStruct List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey reactive survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis, Default: T
#' @param limit.unires Change to default.unires = F if number of independent variables > limit.unires, Default: 20
#' @return Shiny modulde server for logistic regression.
#' @details Shiny modulde server for logistic regression.
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      regressModuleUI("logistic")
#'    ),
#'    mainPanel(
#'      DTOutput("logistictable")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label,
#'                            data_varStruct = NULL)
#'
#'   output$logistictable <- renderDT({
#'     datatable(out_logistic()$table, rownames=T, caption = out_logistic()$caption)
#'   })
#'}
#' @rdname logisticModule2
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula binomial model.frame quasibinomial step
#' @importFrom epiDisplay logistic.display
#' @importFrom purrr map_lgl


logisticModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T, limit.unires = 20) {

  ## To remove NOTE.
  level <- val_label <- variable <- NULL

  if (is.null(data_varStruct)){
    data_varStruct = reactive(list(variable = names(data())))
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
    #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
    factor_list <- mklist(data_varStruct(), factor_vars)


    nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars])

    factor2_vars <- factor_vars[nclass_factor == 2]
    if (!is.null(design.survey)){
      factor2_vars <- setdiff(factor2_vars, c(names(design.survey()$strata), names(design.survey()$cluster)))
    }
    factor2_list <- mklist(data_varStruct(), factor2_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]
    return(list(factor_vars = factor_vars, factor_list = factor_list, nclass_factor = nclass_factor, factor2_vars = factor2_vars,
                factor2_list = factor2_list, except_vars = except_vars)
           )

    })




  output$dep <- renderUI({
    validate(
      need(length(vlist()$factor2_vars) >=1 , "No candidate dependent variable")
    )
    tagList(
      selectInput(session$ns("dep_vars"), "Dependent variable",
                  choices = vlist()$factor2_list, multiple = F,
                  selected = vlist()$factor2_vars[1]
      )
    )
  })


  output$indep <- renderUI({
    req(!is.null(input$dep_vars))
    if (is.null(design.survey)){
      vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  input$dep_vars)
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            coef <- tryCatch(summary(glm(forms, data = data(), family = binomial))$coefficients, error = function(e){return(NULL)})
                            sigOK <- ifelse(is.null(coef), F, !all(coef[-1, 4] > 0.05))
                            return(sigOK)
                          })
        if (length(varsIni[varsIni == T]) > limit.unires) {varsIni <- c(T, rep(F, length(vars) -1))}
      } else{
        varsIni <- c(T, rep(F, length(vars) - 1))
      }

    } else{
      vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  c(input$dep_vars, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            coef <- tryCatch(summary(survey::svyglm(forms, design = design.survey(), family = binomial))$coefficients, error = function(e){return(NULL)})
                            sigOK <- ifelse(is.null(coef), F, !all(coef[-1, 4] > 0.05))

                            return(sigOK)
                          })
        if (length(varsIni[varsIni == T]) > limit.unires) {varsIni <- c(T, rep(F, length(vars) -1))}
      } else{
        varsIni <- c(T, rep(F, length(vars) - 1))
      }


    }

    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = mklist(data_varStruct(), vars), multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  observeEvent(input$regressUI_subcheck, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)
      var_subgroup <- setdiff(names(data()), c(input$dep_vars, input$indep_vars))
      if (!is.null(design.survey)){

        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(input$dep_vars, input$indep_vars)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0 , "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_logistic"), "Sub-group variables",
                    choices = var_subgroup_list, multiple = T,
                    selected = var_subgroup[1])
      )

    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(length(input$subvar_logistic) > 0)

      outUI <- tagList()

      for (v in seq_along(input$subvar_logistic)){
        if (input$subvar_logistic[[v]] %in% vlist()$factor_vars){
          outUI[[v]] <- selectInput(session$ns(paste0("subval_logistic", v)), paste0("Sub-group value: ", input$subvar_logistic[[v]]),
                                    choices = data_label()[variable == input$subvar_logistic[[v]], level], multiple = T,
                                    selected = data_label()[variable == input$subvar_logistic[[v]], level][1])
        } else{
          val <- stats::quantile(data()[[input$subvar_logistic[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(session$ns(paste0("subval_logistic", v)), paste0("Sub-group range: ", input$subvar_logistic[[v]]),
                                    min = val[1], max = val[5],
                                    value = c(val[2], val[4]))
        }

      }
      outUI

    })

  })

  observeEvent(input$step_check, {
    output$step_direction <- renderUI({
      req(input$step_check == T)
      radioButtons(session$ns("step_direction"), "Step direction", choices = c("backward", "forward", "both"), selected = "backward", inline = T)
    })

    output$step_scope <- renderUI({
      req(input$step_check == T)
      req(input$indep_vars)
      tagList(
        fluidRow(
          column(6, selectInput(session$ns("step_lower"), "Lower limit", choices = input$indep_vars, selected = NULL, multiple = T)),
          column(6, selectInput(session$ns("step_upper"), "Upper limit", choices = input$indep_vars, selected = input$indep_vars, multiple = T))
        )

      )
    })
  })




  out <- reactive({
    data.logistic <- data()
    label.regress <- data_label()
    if(input$regressUI_subcheck == T){
      validate(
        need(length(input$subvar_logistic) > 0 , "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_logistic), function(x){length(input[[paste0("subval_logistic", x)]])})), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_logistic)){
        if (input$subvar_logistic[[v]] %in% vlist()$factor_vars){
          data.logistic <- data.logistic[get(input$subvar_logistic[[v]]) %in% input[[paste0("subval_logistic", v)]]]
        } else{
          data.logistic <- data.logistic[get(input$subvar_logistic[[v]]) >= input[[paste0("subval_logistic", v)]][1] & get(input$subvar_logistic[[v]]) <= input[[paste0("subval_logistic", v)]][2]]
        }
      }

      data.logistic[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.logistic)[, c("variable", "class", "level")]
      data.table::setkey(data_label(), "variable", "class", "level")
      data.table::setkey(label.regress2, "variable", "class", "level")
      label.regress <- data_label()[label.regress2]

    }
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form = as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))
    mf <- model.frame(form, data.logistic)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
    )


    if (is.null(design.survey)){
      res.logistic <- glm(form, data = data.logistic, family = binomial)
      if (input$step_check == T){
        validate(
          need(!is.null(input$step_upper), "Upper limits can't be NULL, please select at least 1 variable."),
          need(length((setdiff(input$step_lower, input$step_upper))) == 0, "Upper limits must include lower limits. Please add the variables to upper limits")
        )
        scope <- lapply(list(input$step_upper, input$step_lower), function(x){
          as.formula(ifelse(is.null(x), "~1", paste0("~", paste(x, collapse = "+"))))
        })

        res.logistic <- stats::step(glm(form, data = data.logistic[complete.cases(data.logistic[, .SD, .SDcols = c(y, xs)])],  family = binomial), direction = input$step_direction, scope = list(upper = scope[[1]], lower = scope[[2]]))
      }

      tb.logistic <- jstable::glmshow.display(res.logistic, decimal = input$decimal)
      cap.logistic <- paste("Logistic regression predicting ", label.regress[variable == y, var_label][1], sep="")
      if (input$regressUI_subcheck == T){
        for (v in seq_along(input$subvar_logistic)){
          if (input$subvar_logistic[[v]] %in% vlist()$factor_vars){
            cap.logistic <- paste(cap.logistic, ", ", label.regress[variable == input$subvar_logistic[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_logistic[[v]] & level %in% input[[paste0("subval_logistic", v)]], val_label], collapse = ", "), sep = "")
          } else{
            cap.logistic <- paste(cap.logistic, ", ", label.regress[variable == input$subvar_logistic[[v]], var_label][1], ": ", paste(input[[paste0("subval_logistic", v)]], collapse = "~"), sep = "")
          }
        }
      }
      if (input$step_check == T){
        cap.logistic <- paste0(cap.logistic, "- stepwise selection")
      }
      out.logistic <- jstable::LabelepiDisplay(tb.logistic, label = T, ref = label.regress)
      #out.logistic = summary(res.logistic)$coefficients
      #sig = ifelse(out.logistic[, 4] <= 0.05, "**", "NA")
    } else{
      #validate(
      #  need(var.weights.survey() %in% names(data.logistic) , "Weight variable isn't in data-, Please select appropriate weight variable.")
      #)
      #data.design <- survey::svydesign(ids = ~ 1, data = data.logistic, weights = ~ get(var.weights.survey()))
      data.design <- design.survey()
      if(input$regressUI_subcheck == T){
        validate(
          need(length(input$subvar_logistic) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_logistic), function(x){length(input[[paste0("subval_logistic", x)]])})), "No value for subsetting")
        )
        for (v in seq_along(input$subvar_logistic)){
          if (input$subvar_logistic[[v]] %in% vlist()$factor_vars){
            data.design <- subset(data.design, get(input$subvar_logistic[[v]]) %in% input[[paste0("subval_logistic", v)]])
          } else{
            data.design <- subset(data.design, get(input$subvar_logistic[[v]]) >= input[[paste0("subval_logistic", v)]][1] & get(input$subvar_logistic[[v]]) <= input[[paste0("subval_logistic", v)]][2])
          }
        }

        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
      }

      res.svyglm <- survey::svyglm(form, design = data.design, family = quasibinomial())
      if (input$step_check == T){
        validate(
          need(!is.null(input$step_upper), "Upper limits can't be NULL, please select at least 1 variable."),
          need(length((setdiff(input$step_lower, input$step_upper))) == 0, "Upper limits must include lower limits. Please add the variables to upper limits"),
          need(input$step_direction != "both", "Only forward/backward directions are supported"),
          need(is.null(design.survey), "Survey glm can't support stepwise selection")
        )
        scope <- lapply(list(input$step_upper, input$step_lower), function(x){
          as.formula(ifelse(is.null(x), "~1", paste0("~", paste(x, collapse = "+"))))
        })

        d2 <- na.omit(data.design[, c(y, xs)])

        res.svyglm <- stats::step(res.svyglm,
                                  direction = input$step_direction, scope = list(upper = scope[[1]], lower = scope[[2]]))
      }
      tb.svyglm <- jstable::svyregress.display(res.svyglm, decimal = input$decimal)
      cap.logistic <- paste("Logistic regression predicting ", label.regress[variable == y, var_label][1], "- survey data", sep="")
      if (input$regressUI_subcheck == T){
        for (v in seq_along(input$subvar_logistic)){
          if (input$subvar_logistic[[v]] %in% vlist()$factor_vars){
            cap.logistic <- paste(cap.logistic, ", ", label.regress[variable == input$subvar_logistic[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_logistic[[v]] & level %in% input[[paste0("subval_logistic", v)]], val_label], collapse = ", "), sep = "")
          } else{
            cap.logistic <- paste(cap.logistic, ", ", label.regress[variable == input$subvar_logistic[[v]], var_label][1], ": ", paste(input[[paste0("subval_logistic", v)]], collapse = "~"), sep = "")
          }
        }
      }
      if (input$step_check == T){
        cap.logistic <- paste0(cap.logistic, "- stepwise selection")
      }
      out.logistic <- jstable::LabelepiDisplay(tb.svyglm, label = T, ref = label.regress)

    }


    return(list(table = out.logistic, caption = cap.logistic))
  })

  return(out)


}

