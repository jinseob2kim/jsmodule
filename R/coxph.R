#' @title coxUI: ModuleUI for cox model
#' @description ModuleUI for cox model
#' @param id id
#' @return coxUI
#' @details ModuleUI for cox model
#' @examples
#'  coxUI(1)
#' @rdname coxUI
#' @export

coxUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("eventtime")),
    uiOutput(ns("indep")),
    sliderInput(ns("decimal"), "Digits",
                min = 1, max = 4, value = 2
    ),
    checkboxInput(ns("subcheck"), "Sub-group analysis"),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval"))
  )
}



#' @title coxModule: Module for cox model
#' @description Module for cox model
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactuve data_label
#' @param data_varStruct reactive data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey reactive survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis.
#' @param id.cluster reactive cluster variable if marginal cox model, Default: NULL
#' @return regressModule
#' @details Module for cox model
#' @examples
#'  #EXAMPLE1
#' @rdname coxModule
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame
#' @importFrom epiDisplay regress.display
#' @importFrom jstable LabelepiDisplay
#' @importFrom purrr map_lgl
#' @importFrom survival cluster coxph Surv

coxModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T, id.cluster = NULL) {

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
      need(length(class01_factor) >=1, "No categorical variables coded as 0, 1 in data")
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
      selectInput(session$ns("event_cox"), "Event",
                  choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = F,
                  selected = NULL
      ),
      selectInput(session$ns("time_cox"), "Time",
                  choices = vlist()$conti_list, multiple = F,
                  selected = NULL
      )
    )
  })

  output$indep <- renderUI({
    req(!is.null(input$event_cox))
    req(!is.null(input$time_cox))
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


    if (is.null(design.survey)){
      indep.cox <- setdiff(names(data()), c(vlist()$except_vars, input$event_cox, input$time_cox))
      if (!is.null(id.cluster)){
        indep.cox <- setdiff(names(data()), c(vlist()$except_vars, input$event_cox, input$time_cox, id.cluster()))
      }

      if (default.unires){
        data.cox <- data()
        data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))

        varsIni <- sapply(indep.cox,
                          function(v){
                            if (is.null(id.cluster)){
                              forms <- as.formula(paste("survival::Surv(", input$time_cox, ",", input$event_cox, ") ~ ", v, sep = ""))
                            } else{
                              forms <- as.formula(paste("survival::Surv(", input$time_cox, ",", input$event_cox, ") ~ ", v, " + cluster(", id.cluster(), ")", sep = ""))
                            }
                            coef <- summary(survival::coxph(forms, data = data.cox))$coefficients
                            sigOK <- !all(coef[, "Pr(>|z|)"] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(indep.cox) -1))
      }
    } else{
      indep.cox <- setdiff(names(data()), c(vlist()$except_vars, input$event_cox, input$time_cox, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
      if (default.unires){
        data.design <- design.survey()
        data.design$variables[[input$event_cox]] <- as.numeric(as.vector(data.design$variables[[input$event_cox]]))

        varsIni <- sapply(indep.cox,
                          function(v){
                            forms <- as.formula(paste("survival::Surv(",input$time_cox,",", input$event_cox,") ~ ", v, sep=""))
                            coef <- summary(survey::svycoxph(forms, design =data.design))$coefficients
                            sigOK <- !all(coef[, "Pr(>|z|)"] > 0.05)
                            return(sigOK)
                          })
      } else{
        varsIni <- c(T, rep(F, length(indep.cox) -1))
      }
    }


    tagList(
      selectInput(session$ns("indep_cox"), "Independent variables",
                  choices = mklist(data_varStruct(), indep.cox), multiple = T,
                  selected = indep.cox[varsIni]
      )
    )
  })

  observeEvent(input$subcheck, {
    output$subvar <- renderUI({
      req(input$subcheck == T)

      var_subgroup <- setdiff(names(data()), c(input$time_cox, input$event_cox, input$indep_cox))
      if (!is.null(id.cluster)){
        var_subgroup <- setdiff(names(data()), c(input$time_cox, input$event_cox, input$indep_cox, id.cluster()))
      } else if (!is.null(design.survey)){
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(input$time_cox, input$event_cox, input$indep_cox)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0 , "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_cox"), "Sub-group variable",
                    choices = var_subgroup_list, multiple = F,
                    selected = var_subgroup[1])
      )

    })

  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(input$subvar_cox)

    if (input$subvar_cox %in% vlist()$factor_vars){
      selectInput(session$ns("subval_cox"), "Sub-group value",
                  choices = data_label()[variable == input$subvar_cox, level], multiple = T,
                  selected = data_label()[variable == input$subvar_cox, level][1])
    } else{
      val <- stats::quantile(data()[[input$subvar_cox]], na.rm = T)
      sliderInput(session$ns("subval_cox"), "Sub-group range",
                  min = val[1], max = val[5],
                  value = c(val[2], val[4]))
    }

  })



  form.cox <- reactive({
    validate(
      need(!is.null(input$indep_cox), "Please select at least 1 independent variable.")
    )
    if (is.null(id.cluster)){
      return(as.formula(paste("survival::Surv(",input$time_cox,",", input$event_cox,") ~ ", paste(input$indep_cox, collapse="+"), sep="")))
    } else{
      return(as.formula(paste("survival::Surv(",input$time_cox,",", input$event_cox,") ~ ", paste(input$indep_cox, collapse="+"), " + cluster(", id.cluster(), ")", sep="")))
    }
  })


  out <- reactive({
    req(!is.null(input$event_cox))
    req(!is.null(input$time_cox))
    data.cox <- data()
    data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))
    label.regress <- data_label()
    if(input$subcheck == T){
      req(input$subvar_cox)
      req(input$subval_cox)

      if (input$subvar_cox %in% vlist()$factor_vars){
        data.cox <- data.cox[get(input$subvar_cox) %in% input$subval_cox]
      } else{
        data.cox <- data.cox[get(input$subvar_cox) >= input$subval_cox[1] & get(input$subvar_cox) <= input$subval_cox[2]]
      }
      data.cox[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.cox)[, c("variable", "class", "level")]
      data.table::setkey(data_label(), "variable", "class", "level")
      data.table::setkey(label.regress2, "variable", "class", "level")
      label.regress <- data_label()[label.regress2]
      data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))

    }
    mf <- model.frame(form.cox(), data.cox)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
    )

    if (is.null(design.survey)){
      cc <- substitute(survival::coxph(.form, data= data.cox, model = T), list(.form= form.cox()))
      res.cox <- eval(cc)
      tb.cox <- jstable::cox2.display(res.cox, dec = input$decimal)
      tb.cox <- jstable::LabeljsCox(tb.cox, ref = label.regress)
      out.cox <- rbind(tb.cox$table, tb.cox$metric)
      sig <- out.cox[, ncol(out.cox)]
      sig <- gsub("< ", "", sig)
      sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
      out.cox <- cbind(out.cox, sig)
      if (is.null(id.cluster)){
        cap.cox <- paste("Cox's proportional hazard model on time ('", label.regress[variable == input$time_cox, var_label][1] , "') to event ('", label.regress[variable == input$event_cox, var_label][1], "')", sep="")
      } else{
        cap.cox <- paste("Marginal cox model on time ('", label.regress[variable == input$time_cox, var_label][1] , "') to event ('", label.regress[variable == input$event_cox, var_label][1], "')", sep="")
      }

      if(input$subcheck == T){
        if (input$subvar_cox %in% vlist()$factor_vars){
          cap.cox <- paste(cap.cox, " - ", label.regress[variable == input$subvar_cox, var_label][1], ": ", paste(label.regress[variable == input$subvar_cox & level %in% input$subval_cox, val_label], collapse = ", "), sep = "")
        } else{
          cap.cox <- paste(cap.cox, " - ", label.regress[variable == input$subvar_cox, var_label][1], ": ", paste(input$subval_cox[1], "~", input$subval_cox[2], sep = ""), sep = "")
        }
      }
    } else{
      data.design <- design.survey()
      data.design$variables[[input$event_cox]] <- as.numeric(as.vector(data.design$variables[[input$event_cox]]))
      if(input$subcheck == T){
        req(input$subvar_cox)
        req(input$subval_cox)
        if (input$subvar_cox %in% vlist()$factor_vars){
          data.design <- subset(data.design, get(input$subvar_cox) %in% input$subval_cox)
        } else{
          data.design <- subset(data.design, get(input$subvar_cox) >= input$subval_cox[1] & get(input$subvar_cox) <= input$subval_cox[2])
        }
        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
        data.design$variables[[input$event_cox]] <- as.numeric(as.vector(data.design$variables[[input$event_cox]]))
      }

      cc <- substitute(survey::svycoxph(.form, design= data.design), list(.form= form.cox()))
      res.cox <- eval(cc)
      tb.cox <- jstable::svycox.display(res.cox, decimal = input$decimal)
      tb.cox <- jstable::LabeljsCox(tb.cox, label.regress)
      out.cox <- rbind(tb.cox$table, tb.cox$metric)
      sig <- out.cox[, ncol(out.cox)]
      sig <- gsub("< ", "", sig)
      sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
      out.cox <- cbind(out.cox, sig)
      cap.cox <- paste("Weighted cox's proportional hazard model on time ('", label.regress[variable == input$time_cox, var_label][1] , "') to event ('", label.regress[variable == input$event_cox, var_label][1], "') ", sep="")
      if(input$subcheck == T){
        if (input$subvar_cox %in% vlist()$factor_vars){
          cap.cox <- paste(cap.cox, " - ", label.regress[variable == input$subvar_cox, var_label][1], ": ", paste(label.regress[variable == input$subvar_cox & level %in% input$subval_cox, val_label], collapse = ", "), sep = "")
        } else{
          cap.cox <- paste(cap.cox, " - ", label.regress[variable == input$subvar_cox, var_label][1], ": ", paste(input$subval_cox[1], "~", input$subval_cox[2], sep = ""), sep = "")
        }
      }
    }

    return(list(table = out.cox, caption = cap.cox))

  })

  return(out)


  }



