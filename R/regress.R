#' @title mklist: make intersect varlist
#' @description make intersect varlist
#' @param varlist varlist
#' @param vars vars
#' @return Intersect varlist
#' @details Internal function
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data_varStruct <- list(variable = names(data))
#'  conti_list <- mklist(data_varStruct, conti_vars)
#'  }
#' }
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

#' @title mksetdiff: make setdiff varlist
#' @description make setdiff varlist
#' @param varlist varlist
#' @param vars vars
#' @return Setdiff varlist
#' @details Internal function
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # exam
#'  }
#' }
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




#' @title regressModuleUI: ModuleUI for regression
#' @description ModuleUI for regression
#' @param id id
#' @return regressModuleUI
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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
    uiOutput(ns("regressUI_subval"))
  )
}






#' @title regressModule: Module for linear regression
#' @description Module for linear regression
#' @param input input
#' @param output output
#' @param session session
#' @param data data
#' @param data_label data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis.
#' @return regressModule
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname regressModule
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame model.matrix
#' @importFrom epiDisplay regress.display
#' @importFrom jstable LabelepiDisplay
#' @importFrom purrr map_lgl

regressModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T) {

  if (is.null(data_varStruct)){
    data_varStruct = list(variable = names(data))
  }

  if (!("data.table" %in% class(data))) {data = data.table(data)}
  if (!("data.table" %in% class(data_label))) {data_label = data.table(data_label)}

  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  factor_list <- mklist(data_varStruct, factor_vars)

  conti_vars <- setdiff(names(data), factor_vars)
  if (!is.null(design.survey)){
    conti_vars <- setdiff(conti_vars, c(names(design.survey$allprob), names(design.survey$strata), names(design.survey$cluster)))
  }
  conti_list <- mklist(data_varStruct, conti_vars)

  nclass_factor <- unlist(data[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars])

  group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data)]
  group_list <- mklist(data_varStruct, group_vars)

  except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data)]


  output$dep <- renderUI({
    tagList(
      selectInput(session$ns("dep_vars"), "Dependent variable",
                  choices = conti_list, multiple = F,
                  selected = conti_vars[1]
      )
    )
  })


  output$indep <- renderUI({
    req(!is.null(input$dep_vars))
    if (is.null(design.survey)){
      vars <- setdiff(setdiff(names(data), except_vars),  input$dep_vars)
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            coef <- summary(glm(forms, data =data))$coefficients
                            sigOK <- !all(coef[-1, "Pr(>|t|)"] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(vars) -1))
      }

    } else{
      vars <- setdiff(setdiff(names(data), except_vars),  c(input$dep_vars, names(design.survey$allprob), names(design.survey$strata), names(design.survey$cluster)))
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            #data.design <- survey::svydesign(ids = ~ 1, data = data(), weights = ~ get(var.weights.survey()))
                            coef <- summary(survey::svyglm(forms, design = design.survey))$coefficients
                            sigOK <- !all(coef[-1, "Pr(>|t|)"] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(vars) -1))
      }

    }


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = mklist(data_varStruct, vars), multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  observeEvent(input$regressUI_subcheck, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)
      factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
      factor_subgroup <- setdiff(factor_vars, c(input$dep_vars, input$indep_vars))
      factor_subgroup_list <- mklist(data_varStruct, factor_subgroup)
      validate(
        need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_regress"), "Sub-group variable",
                    choices = factor_subgroup_list, multiple = F,
                    selected = factor_subgroup[1])
      )
    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(input$subvar_regress)
      selectInput(session$ns("subval_regress"), "Sub-group value",
                  choices = data_label[variable == input$subvar_regress, level], multiple = F,
                  selected = data_label[variable == input$subvar_regress, level][1])
    })
  })






  out <- reactive({
    data.regress <- data
    if(input$regressUI_subcheck == T){
      req(input$subvar_regress)
      data.regress <- data.regress[get(input$subvar_regress) == input$subval_regress, ]
    }
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form = as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))
    mf <- model.frame(form, data.regress)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )

    lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
    )

    ## Warning when multicollinearity
    warn <- NULL
    mm <- model.matrix(form, data.regress)
    zz <- tryCatch(solve(t(as.matrix(mm)) %*% as.matrix(mm)), error = function(e) e)
    if ("error" %in% class(zz)) {
      warn <- "There are strongly correlated independent values. Please see correlation in `Plot- Scatter plot` tab"
    }


    if (is.null(design.survey)){
      res.linear <- glm(form, data = data.regress)
      tb.linear <- jstable::glmshow.display(res.linear, decimal = input$decimal)
      cap.linear <- paste("Linear regression predicting ", data_label()[variable == y, var_label][1], sep="")
      if(input$regressUI_subcheck == T){
        cap.linear <- paste(cap.linear, " - ", data_label()[variable == input$subvar_regress, var_label][1], ": ", data_label()[variable == input$subvar_regress & level == input$subval_regress, val_label], sep = "")
      }
      out.linear <- jstable::LabelepiDisplay(tb.linear, label = T, ref = data_label())
    } else{
      #validate(
      #  need(var.weights.survey %in% names(data.regress) , "Weight variable isn't in data-, Please select appropriate weight variable.")
      #)
      #data.design <- survey::svydesign(ids = ~ 1, data = data.regress, weights = ~ get(var.weights.survey))
      data.design <- design.survey
      if(input$regressUI_subcheck == T){
        req(input$subvar_regress)
        data.design <- subset(data.design, get(input$subvar_regress) == input$subval_regress)
      }

      res.svyglm <- survey::svyglm(form, design = data.design)
      tb.svyglm <- jstable::svyregress.display(res.svyglm, decimal = input$decimal)
      cap.linear <- paste("Linear regression predicting ", data_label()[variable == y, var_label][1], "- survey data", sep="")
      if(input$regressUI_subcheck == T){
        cap.linear <- paste(cap.linear, " - ", data_label()[variable == input$subvar_regress, var_label][1], ": ", data_label()[variable == input$subvar_regress & level == input$subval_regress, val_label], sep = "")
      }
      out.linear <- jstable::LabelepiDisplay(tb.svyglm, label = T, ref = data_label())

    }
    return(list(table = out.linear, caption = cap.linear, warning = warn))
  })

  return(out)


}



#' @title regressModule2: Module for linear regression using reactive data
#' @description Module for linear regression using reactive data
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactive data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis.
#' @return regressModule2
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname regressModule2
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame model.matrix
#' @importFrom epiDisplay regress.display
#' @importFrom purrr map_lgl

regressModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T) {

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
                            coef <- summary(glm(forms, data = data()))$coefficients
                            sigOK <- !all(coef[-1, "Pr(>|t|)"] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(vars) -1))
      }

    } else{
      vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  c(input$dep_vars, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            #data.design <- survey::svydesign(ids = ~ 1, data = data(), weights = ~ get(var.weights.survey()))
                            coef <- summary(survey::svyglm(forms, design = design.survey()))$coefficients
                            sigOK <- !all(coef[-1, "Pr(>|t|)"] > 0.05)
                            return(sigOK)
                          })
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
      factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
      factor_subgroup <- setdiff(factor_vars, c(input$dep_vars, input$indep_vars))
      if (!is.null(design.survey)){
        factor_subgroup <- setdiff(factor_subgroup, c(names(design.survey()$strata), names(design.survey()$cluster)))
      }
      factor_subgroup_list <- mklist(data_varStruct(), factor_subgroup)
      validate(
        need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_regress"), "Sub-group variable",
                    choices = factor_subgroup_list, multiple = F,
                    selected = factor_subgroup[1])
      )
    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(input$subvar_regress)
      selectInput(session$ns("subval_regress"), "Sub-group value",
                  choices = data_label()[variable == input$subvar_regress, level], multiple = F,
                  selected = data_label()[variable == input$subvar_regress, level][1])
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
      if(input$regressUI_subcheck == T){
        req(input$subvar_regress)
        data.regress <- data.regress[get(input$subvar_regress) == input$subval_regress, ]
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
      tb.linear <- jstable::glmshow.display(res.linear, decimal = input$decimal)
      cap.linear <- paste("Linear regression predicting ", data_label()[variable == y, var_label][1], sep="")
      if(input$regressUI_subcheck == T){
        cap.linear <- paste(cap.linear, " - ", data_label()[variable == input$subvar_regress, var_label][1], ": ", data_label()[variable == input$subvar_regress & level == input$subval_regress, val_label], sep = "")
      }
      out.linear <- jstable::LabelepiDisplay(tb.linear, label = T, ref = data_label())
    } else{
      data.design <- design.survey()
      if(input$regressUI_subcheck == T){
        req(input$subvar_regress)
        data.design <- subset(data.design, get(input$subvar_regress) == input$subval_regress)
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
      tb.svyglm <- jstable::svyregress.display(res.svyglm, decimal = input$decimal)
      cap.linear <- paste("Linear regression predicting ", data_label()[variable == y, var_label][1], "- survey data", sep="")
      if(input$regressUI_subcheck == T){
        cap.linear <- paste(cap.linear, " - ", data_label()[variable == input$subvar_regress, var_label][1], ": ", data_label()[variable == input$subvar_regress & level == input$subval_regress, val_label], sep = "")
      }
      out.linear <- jstable::LabelepiDisplay(tb.svyglm, label = T, ref = data_label())
    }

    #out.linear = summary(res.linear)$coefficients
    #sig = ifelse(out.linear[, 4] <= 0.05, "**", "NA")
    return(list(table = out.linear, caption = cap.linear, warning = warn))
  })

  return(out)


}




#' @title logisticModule: Module for logistic regression
#' @description Module for logistic regression
#' @param input input
#' @param output output
#' @param session session
#' @param data data
#' @param data_label data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis.
#' @return logisticModule
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname logisticModule
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula binomial model.frame quasibinomial
#' @importFrom epiDisplay logistic.display
#' @importFrom jstable LabelepiDisplay
#' @importFrom purrr map_lgl

logisticModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T) {

  if (is.null(data_varStruct)){
    data_varStruct = list(variable = names(data))
  }

  if (!("data.table" %in% class(data))) {data = data.table(data)}
  if (!("data.table" %in% class(data_label))) {data_label = data.table(data_label)}

  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  factor_list <- mklist(data_varStruct, factor_vars)


  nclass_factor <- unlist(data[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars])

  factor2_vars <- factor_vars[nclass_factor == 2]
  if (!is.null(design.survey)){
    factor2_vars <- setdiff(factor2_vars, c(names(design.survey$strata), names(design.survey$cluster)))
  }
  factor2_list <- mklist(data_varStruct, factor2_vars)

  except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data)]


  output$dep <- renderUI({
    validate(
      need(length(factor2_vars) >=1 , "No candidate dependent variable")
    )
    tagList(
      selectInput(session$ns("dep_vars"), "Dependent variable",
                  choices = factor2_list, multiple = F,
                  selected = factor2_vars[1]
      )
    )
  })


  output$indep <- renderUI({
    req(!is.null(input$dep_vars))

    if (is.null(design.survey)){
      vars <- setdiff(setdiff(names(data), except_vars),  input$dep_vars)
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            coef <- summary(glm(forms, data = data(), family = binomial))$coefficients
                            sigOK <- !all(coef[-1, 4] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(vars)-1))
      }

    } else{
      vars <- setdiff(setdiff(names(data), except_vars),  c(input$dep_vars, names(design.survey$allprob), names(design.survey$strata), names(design.survey$cluster)))
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            #data.design <- survey::svydesign(ids = ~ 1, data = data(), weights = ~ get(var.weights.survey))
                            coef <- summary(survey::svyglm(forms, design = design.survey, family = binomial))$coefficients
                            sigOK <- !all(coef[-1, 4] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(vars)-1))
      }

    }


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = mklist(data_varStruct, vars), multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  observeEvent(input$regressUI_subcheck, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)
      factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
      factor_subgroup <- setdiff(factor_vars, c(input$dep_vars, input$indep_vars))
      if (!is.null(design.survey)){
        factor_subgroup <- setdiff(factor_subgroup, c(names(design.survey$strata), names(design.survey$cluster)))
      }
      factor_subgroup_list <- mklist(data_varStruct, factor_subgroup)
      validate(
        need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_logistic"), "Sub-group variable",
                    choices = factor_subgroup_list, multiple = F,
                    selected = factor_subgroup[1])
      )
    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(input$subvar_logistic)
      selectInput(session$ns("subval_logistic"), "Sub-group value",
                  choices = data_label[variable == input$subvar_logistic, level], multiple = F,
                  selected = data_label[variable == input$subvar_logistic, level][1])
    })
  })





  out <- reactive({
    data.logistic <- data
    if(input$regressUI_subcheck == T){
      req(input$subvar_logistic)
      data.logistic <- data.logistic[get(input$subvar_logistic) == input$subval_logistic, ]
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
      res.logistic = glm(form, data = data.logistic, family = binomial)
      tb.logistic = jstable::glmshow.display(res.logistic, decimal = input$decimal)
      cap.logistic = paste("Logistic regression predicting ", data_label()[variable == y, var_label][1], sep="")
      if(input$regressUI_subcheck == T){
        cap.logistic <- paste(cap.logistic, " - ", data_label()[variable == input$subvar_logistic, var_label][1], ": ", data_label()[variable == input$subvar_logistic & level == input$subval_logistic, val_label], sep = "")
      }
      out.logistic = jstable::LabelepiDisplay(tb.logistic, label = T, ref = data_label())
      #out.logistic = summary(res.logistic)$coefficients
      #sig = ifelse(out.logistic[, 4] <= 0.05, "**", "NA")
    } else{
      #validate(
      #  need(var.weights.survey %in% names(data.logistic) , "Weight variable isn't in data-, Please select appropriate weight variable.")
      #)
      #data.design <- survey::svydesign(ids = ~ 1, data = data.logistic, weights = ~ get(var.weights.survey))
      data.design <- design.survey
      if(input$regressUI_subcheck == T){
        req(input$subvar_logistic)
        data.design <- subset(data.design, get(input$subvar_logistic) == input$subval_logistic)
      }

      res.svyglm <- survey::svyglm(form, design = data.design, family = quasibinomial())
      tb.svyglm <- jstable::svyregress.display(res.svyglm, decimal = input$decimal)
      cap.logistic <- paste("Logistic regression predicting ", data_label()[variable == y, var_label][1], "- survey data", sep="")
      if(input$regressUI_subcheck == T){
        cap.logistic <- paste(cap.logistic, " - ", data_label()[variable == input$subvar_logistic, var_label][1], ": ", data_label()[variable == input$subvar_logistic & level == input$subval_regress, val_label], sep = "")
      }
      out.logistic <- jstable::LabelepiDisplay(tb.svyglm, label = T, ref = data_label())

    }
    return(list(table = out.logistic, caption = cap.logistic, warning = warn))
  })

  return(out)


}





#' @title logisticModule2: Module for logistic regression using reactive data
#' @description Module for logistic regression using reactive data
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactive data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis.
#' @return logisticModule2
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname logisticModule2
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula binomial model.frame quasibinomial
#' @importFrom epiDisplay logistic.display
#' @importFrom purrr map_lgl


logisticModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T) {

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
                            coef <- summary(glm(forms, data = data(), family = binomial))$coefficients
                            sigOK <- !all(coef[-1, 4] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(vars) - 1))
      }

    } else{
      vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  c(input$dep_vars, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
      if (default.unires){
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$dep_vars, "~", v))
                            #data.design <- survey::svydesign(ids = ~ 1, data = data(), weights = ~ get(var.weights.survey()))
                            coef <- summary(survey::svyglm(forms, design = design.survey(), family = binomial))$coefficients
                            sigOK <- !all(coef[-1, 4] > 0.05)
                            return(sigOK)
                          })
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
      factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
      factor_subgroup <- setdiff(factor_vars, c(input$dep_vars, input$indep_vars))
      if (!is.null(design.survey)){
        factor_subgroup <- setdiff(factor_subgroup, c(names(design.survey()$strata), names(design.survey()$cluster)))
      }
      factor_subgroup_list <- mklist(data_varStruct(), factor_subgroup)
      validate(
        need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_logistic"), "Sub-group variable",
                    choices = factor_subgroup_list, multiple = F,
                    selected = factor_subgroup[1])
      )
    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(input$subvar_logistic)
      selectInput(session$ns("subval_logistic"), "Sub-group value",
                  choices = data_label()[variable == input$subvar_logistic, level], multiple = F,
                  selected = data_label()[variable == input$subvar_logistic, level][1])
    })

  })




  out <- reactive({
    data.logistic <- data()
    if(input$regressUI_subcheck == T){
      req(input$subvar_logistic)
      data.logistic <- data.logistic[get(input$subvar_logistic) == input$subval_logistic, ]
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
      res.logistic = glm(form, data = data.logistic, family = binomial)
      tb.logistic = jstable::glmshow.display(res.logistic, decimal = input$decimal)
      cap.logistic = paste("Logistic regression predicting ", data_label()[variable == y, var_label][1], sep="")
      if(input$regressUI_subcheck == T){
        cap.logistic <- paste(cap.logistic, " - ", data_label()[variable == input$subvar_logistic, var_label][1], ": ", data_label()[variable == input$subvar_logistic & level == input$subval_logistic, val_label], sep = "")
      }
      out.logistic = jstable::LabelepiDisplay(tb.logistic, label = T, ref = data_label())
      #out.logistic = summary(res.logistic)$coefficients
      #sig = ifelse(out.logistic[, 4] <= 0.05, "**", "NA")
    } else{
      #validate(
      #  need(var.weights.survey() %in% names(data.logistic) , "Weight variable isn't in data-, Please select appropriate weight variable.")
      #)
      #data.design <- survey::svydesign(ids = ~ 1, data = data.logistic, weights = ~ get(var.weights.survey()))
      data.design <- design.survey()
      if(input$regressUI_subcheck == T){
        req(input$subvar_logistic)
        data.design <- subset(data.design, get(input$subvar_logistic) == input$subval_logistic)
      }

      res.svyglm <- survey::svyglm(form, design = data.design, family = quasibinomial())
      tb.svyglm <- jstable::svyregress.display(res.svyglm, decimal = input$decimal)
      cap.logistic <- paste("Logistic regression predicting ", data_label()[variable == y, var_label][1], "- survey data", sep="")
      if(input$regressUI_subcheck == T){
        cap.logistic <- paste(cap.logistic, " - ", data_label()[variable == input$subvar_logistic, var_label][1], ": ", data_label()[variable == input$subvar_logistic & level == input$subval_logistic, val_label], sep = "")
      }
      out.logistic <- jstable::LabelepiDisplay(tb.svyglm, label = T, ref = data_label())

    }


    return(list(table = out.logistic, caption = cap.logistic, warning = warn))
  })

  return(out)


}

