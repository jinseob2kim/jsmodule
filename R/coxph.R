#' @title coxUI: shiny modulde UI for Cox's model.
#' @description Shiny modulde UI for Cox's model.
#' @param id id
#' @return coxUI
#' @details Shiny modulde UI for Cox's model.
#' @examples
#' coxUI(1)
#' @rdname coxUI
#' @export

coxUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("eventtime")),
    checkboxInput(ns("check_rangetime"), "Choose time ranges"),
    checkboxInput(ns("cmp_risk_check"), "Competing Risk Analysis(Fine-Gray)"),
    uiOutput(ns("cmp_eventtime")),
    uiOutput(ns("rangetime")),
    uiOutput(ns("indep")),
    sliderInput(ns("decimal"), "Digits",
      min = 1, max = 4, value = 2
    ),
    checkboxInput(ns("subcheck"), "Sub-group analysis"),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval")),
    checkboxInput(ns("step_check"), "Stepwise variable selection"),
    uiOutput(ns("step_direction")),
    uiOutput(ns("step_scope"))
  )
}



#' @title coxModule: shiny modulde server for Cox's model.
#' @description Shiny modulde server for Cox's model.
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactuve data label
#' @param data_varStruct reactive list of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey reactive survey data. default: NULL
#' @param default.unires Set default independent variables using univariate analysis.
#' @param limit.unires Change to default.unires = F if number of independent variables > limit.unires, Default: 20
#' @param id.cluster reactive cluster variable if marginal cox model, Default: NULL
#' @param ties.coxph 'coxph' ties option, one of 'efron', 'breslow', 'exact', default: 'erfon'
#' @param vec.event event variables as vector for survival analysis,  Default: NULL
#' @param vec.time time variables as vector for survival analysis,  Default: NULL
#' @return Shiny modulde server for Cox's model.
#' @details Shiny modulde server for Cox's model.
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       coxUI("cox")
#'     ),
#'     mainPanel(
#'       DTOutput("coxtable")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_cox <- callModule(coxModule, "cox",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$coxtable <- renderDT({
#'     datatable(out_cox()$table, rownames = T, caption = out_cox()$caption)
#'   })
#' }
#' @rdname coxModule
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame step
#' @importFrom purrr map_lgl
#' @importFrom survival cluster coxph Surv finegray
#' @importFrom stats setNames


coxModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, default.unires = T, limit.unires = 20, id.cluster = NULL, ties.coxph = "efron", vec.event = NULL, vec.time = NULL) {
  ## To remove NOTE.
  data.cox.step <- level <- val_label <- variable <- NULL
  fix_et <- !is.null(vec.event) && !is.null(vec.time) && (length(vec.event) == length(vec.time))
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




  output$eventtime <- renderUI({
    if (!fix_et) {
      validate(
        need(length(vlist()$factor_01vars) >= 1, "No candidate event variables coded as 0, 1"),
        need(length(vlist()$conti_vars_positive) >= 1, "No candidate time variables")
      )
      tagList(
        selectInput(session$ns("event_cox"), "Event",
          choices = mklist(data_varStruct(), vlist()$factor_01vars),
          multiple = FALSE,
          selected = NULL
        ),
        selectInput(session$ns("time_cox"), "Time",
          choices = mklist(data_varStruct(), vlist()$conti_vars_positive),
          multiple = FALSE,
          selected = NULL
        )
      )
    } else {
      tagList(
        selectInput(session$ns("event_cox"), "Event", choices = vec.event, selected = NULL),
        selectInput(session$ns("time_cox"), "Time", choices = vec.time, selected = NULL)
      )
    }
  })

  if (fix_et) {
    paired <- setNames(vec.time, vec.event)
    paired_rev <- setNames(vec.event, vec.time)

    observeEvent(input$event_cox, {
      req(input$event_cox)
      new_time <- paired[[input$event_cox]]
      if (!is.null(new_time) && new_time != input$time_cox) {
        updateSelectInput(session, "time_cox", selected = new_time)
      }
    })

    observeEvent(input$time_cox, {
      req(input$time_cox)
      new_event <- paired_rev[[input$time_cox]]
      if (!is.null(new_event) && new_event != input$event_cox) {
        updateSelectInput(session, "event_cox", selected = new_event)
      }
    })
  }

  observeEvent(input$cmp_risk_check, {
    output$cmp_eventtime <- renderUI({
      req(input$cmp_risk_check == TRUE)
      validate(
        need(length(vlist()$factor_01vars) >= 1, "No candidate event variables coded as 0, 1"),
        need(length(vlist()$conti_vars_positive) >= 1, "No candidate time variables")
      )
      tagList(
        selectInput(session$ns("cmp_event_cox"), "Competing Event",
          choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = FALSE,
          selected = NULL
        ),
        selectInput(session$ns("cmp_time_cox"), "Competing Time",
          choices = mklist(data_varStruct(), vlist()$conti_vars_positive), multiple = FALSE,
          selected = NULL
        )
      )
    })
  })

  observeEvent(input$check_rangetime, {
    output$rangetime <- renderUI({
      req(input$check_rangetime == T)
      sliderInput(session$ns("range_time"), "Time ranges",
        min = min(data()[[input$time_cox]], na.rm = T), max = max(data()[[input$time_cox]], na.rm = T),
        value = c(min(data()[[input$time_cox]], na.rm = T), median(data()[[input$time_cox]], na.rm = T))
      )
    })
  })



  output$indep <- renderUI({
    req(!is.null(input$event_cox))
    req(!is.null(input$time_cox))

    # Helper function (as in your original code)
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

    # Determine the list of independent variables based on fix_et:
    if (fix_et) {
      # Fixed list: use fixed vectors (vec.event & vec.time) rather than the current inputs.
      if (is.null(design.survey)) {
        indep.cox <- setdiff(names(data()), c(vlist()$except_vars, vec.event, vec.time))
        if (!is.null(id.cluster)) {
          indep.cox <- setdiff(names(data()), c(vlist()$except_vars, vec.event, vec.time, id.cluster()))
        }
      } else {
        indep.cox <- setdiff(
          names(data()),
          c(
            vlist()$except_vars, vec.event, vec.time,
            names(design.survey()$allprob),
            names(design.survey()$strata),
            names(design.survey()$cluster)
          )
        )
      }
      # When fixed, initially nothing is selected.
      selected.indep <- if (!is.null(input$indep_cox)) intersect(input$indep_cox, indep.cox) else character(0)
    } else {
      # Dynamic list: exclude the current event and time (as in your original code)
      if (is.null(design.survey)) {
        indep.cox <- setdiff(names(data()), c(vlist()$except_vars, input$event_cox, input$time_cox))
        if (!is.null(id.cluster)) {
          indep.cox <- setdiff(names(data()), c(vlist()$except_vars, input$event_cox, input$time_cox, id.cluster()))
        }

        if (default.unires) {
          data.cox <- data()
          if (input$check_rangetime == T) {
            data.cox <- data.cox[!(get(input$time_cox) < input$range_time[1])]
            data.cox[[input$event_cox]] <- ifelse(
              data.cox[[input$time_cox]] >= input$range_time[2] & data.cox[[input$event_cox]] == "1",
              0, as.numeric(as.vector(data.cox[[input$event_cox]]))
            )
            data.cox[[input$time_cox]] <- ifelse(
              data.cox[[input$time_cox]] >= input$range_time[2],
              input$range_time[2],
              data.cox[[input$time_cox]]
            )
          }
          data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))
          varsIni <- sapply(
            indep.cox,
            function(v) {
              if (is.null(id.cluster)) {
                forms <- as.formula(paste("survival::Surv(", input$time_cox, ",", input$event_cox, ") ~ ", v, sep = ""))
                coef <- tryCatch(summary(survival::coxph(forms, data = data.cox, ties = ties.coxph))$coefficients,
                  error = function(e) {
                    return(NULL)
                  }
                )
              } else {
                forms <- as.formula(paste("survival::Surv(", input$time_cox, ",", input$event_cox, ") ~ ", v, " + cluster(", id.cluster(), ")", sep = ""))
                coef <- tryCatch(summary(survival::coxph(forms, data = data.cox, robust = TRUE, ties = ties.coxph))$coefficients,
                  error = function(e) {
                    return(NULL)
                  }
                )
              }
              sigOK <- ifelse(is.null(coef), FALSE, !all(coef[, "Pr(>|z|)"] > 0.05))
              return(sigOK)
            }
          )
          if (length(varsIni[varsIni == TRUE]) > limit.unires) {
            varsIni <- c(TRUE, rep(FALSE, length(indep.cox) - 1))
          }
          selected.indep <- indep.cox[varsIni]
        } else {
          selected.indep <- indep.cox[1]
        }
      } else {
        indep.cox <- setdiff(
          names(data()),
          c(
            vlist()$except_vars, input$event_cox, input$time_cox,
            names(design.survey()$allprob),
            names(design.survey()$strata),
            names(design.survey()$cluster)
          )
        )
        if (default.unires) {
          data.design <- design.survey()
          if (input$check_rangetime == T) {
            data.design <- subset(data.design, !(get(input$time_cox) < input$range_time[1]))
            data.design$variables[[input$event_cox]] <- ifelse(
              data.design$variables[[input$time_cox]] >= input$range_time[2] &
                data.design$variables[[input$event_cox]] == "1",
              0, as.numeric(as.vector(data.design$variables[[input$event_cox]]))
            )
            data.design$variables[[input$time_cox]] <- ifelse(
              data.design$variables[[input$time_cox]] >= input$range_time[2],
              input$range_time[2],
              data.design$variables[[input$time_cox]]
            )
          }
          data.design$variables[[input$event_cox]] <- as.numeric(as.vector(data.design$variables[[input$event_cox]]))
          varsIni <- sapply(
            indep.cox,
            function(v) {
              forms <- as.formula(paste("survival::Surv(", input$time_cox, ",", input$event_cox, ") ~ ", v, sep = ""))
              coef <- tryCatch(summary(survey::svycoxph(forms, design = data.design))$coefficients,
                error = function(e) {
                  return(NULL)
                }
              )
              sigOK <- ifelse(is.null(coef), FALSE, !all(coef[, "Pr(>|z|)"] > 0.05))
              return(sigOK)
            }
          )
          if (length(varsIni[varsIni == TRUE]) > limit.unires) {
            varsIni <- c(TRUE, rep(FALSE, length(indep.cox) - 1))
          }
          selected.indep <- indep.cox[varsIni]
        } else {
          selected.indep <- indep.cox
        }
      }
    }

    tagList(
      selectInput(session$ns("indep_cox"), "Independent variables",
        choices = mklist(data_varStruct(), indep.cox),
        multiple = TRUE,
        selected = selected.indep
      )
    )
  })



  observeEvent(input$subcheck, {
    output$subvar <- renderUI({
      req(input$subcheck == T)

      var_subgroup <- setdiff(names(data()), c(input$time_cox, input$event_cox, input$indep_cox))
      if (!is.null(id.cluster)) {
        var_subgroup <- setdiff(names(data()), c(input$time_cox, input$event_cox, input$indep_cox, id.cluster()))
      } else if (!is.null(design.survey)) {
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(input$time_cox, input$event_cox, input$indep_cox)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0, "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_cox"), "Sub-group variables",
          choices = var_subgroup_list, multiple = T,
          selected = var_subgroup[1]
        )
      )
    })
  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(length(input$subvar_cox) > 0)

    outUI <- tagList()

    for (v in seq_along(input$subvar_cox)) {
      if (input$subvar_cox[[v]] %in% vlist()$factor_vars) {
        outUI[[v]] <- selectInput(session$ns(paste0("subval_cox", v)), paste0("Sub-group value: ", input$subvar_cox[[v]]),
          choices = data_label()[variable == input$subvar_cox[[v]], level], multiple = T,
          selected = data_label()[variable == input$subvar_cox[[v]], level][1]
        )
      } else {
        val <- stats::quantile(data()[[input$subvar_cox[[v]]]], na.rm = T)
        outUI[[v]] <- sliderInput(session$ns(paste0("subval_cox", v)), paste0("Sub-group range: ", input$subvar_cox[[v]]),
          min = val[1], max = val[5],
          value = c(val[2], val[4])
        )
      }
    }
    outUI
  })


  observeEvent(input$step_check, {
    output$step_direction <- renderUI({
      req(input$step_check == T)
      radioButtons(session$ns("step_direction"), "Step direction", choices = c("backward", "forward", "both"), selected = "backward", inline = T)
    })

    output$step_scope <- renderUI({
      req(input$step_check == T)
      req(input$indep_cox)
      tagList(
        fluidRow(
          column(6, selectInput(session$ns("step_lower"), "Lower limit", choices = input$indep_cox, selected = NULL, multiple = T)),
          column(6, selectInput(session$ns("step_upper"), "Upper limit", choices = input$indep_cox, selected = input$indep_cox, multiple = T))
        )
      )
    })
  })



  form.cox <- reactive({
    validate(
      need(!is.null(input$indep_cox), "Please select at least 1 independent variable.")
    )
    if (input$cmp_risk_check) {
      req(input$cmp_event_cox)
      req(input$cmp_time_cox)
      as.formula(paste(
        "survival::Surv(fgstart, fgstop, fgstatus) ~ ",
        paste(input$indep_cox, collapse = "+")
      ))
    } else {
      if (is.null(id.cluster)) {
        return(as.formula(paste("survival::Surv(", input$time_cox, ",", input$event_cox, ") ~ ", paste(input$indep_cox, collapse = "+"), sep = "")))
      } else {
        return(as.formula(paste("survival::Surv(", input$time_cox, ",", input$event_cox, ") ~ ", paste(input$indep_cox, collapse = "+"), " + cluster(", id.cluster(), ")", sep = "")))
      }
    }
  })


  out <- reactive({
    req(!is.null(input$event_cox))
    req(!is.null(input$time_cox))
    data.cox <- data()
    if (input$check_rangetime == T) {
      req(input$time_cox)
      data.cox <- data.cox[!(get(input$time_cox) < input$range_time[1])]
      data.cox[[input$event_cox]] <- ifelse(data.cox[[input$time_cox]] >= input$range_time[2] & data.cox[[input$event_cox]] == "1", 0, as.numeric(as.vector(data.cox[[input$event_cox]])))
      data.cox[[input$time_cox]] <- ifelse(data.cox[[input$time_cox]] >= input$range_time[2], input$range_time[2], data.cox[[input$time_cox]])
    }

    data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))
    label.regress <- data_label()
    if (input$subcheck == T) {
      validate(
        need(length(input$subvar_cox) > 0, "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_cox), function(x) {
          length(input[[paste0("subval_cox", x)]])
        })), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_cox)) {
        if (input$subvar_cox[[v]] %in% vlist()$factor_vars) {
          data.cox <- data.cox[get(input$subvar_cox[[v]]) %in% input[[paste0("subval_cox", v)]]]
        } else {
          data.cox <- data.cox[get(input$subvar_cox[[v]]) >= input[[paste0("subval_cox", v)]][1] & get(input$subvar_cox[[v]]) <= input[[paste0("subval_cox", v)]][2]]
        }
      }

      data.cox[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.cox)[, c("variable", "level")]
      data.table::setkey(data_label(), "variable", "level")
      data.table::setkey(label.regress2, "variable", "level")
      label.regress <- data_label()[label.regress2]
      data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))
    }
    if (input$cmp_risk_check == T) {
      req(input$cmp_event_cox)
      req(input$cmp_time_cox)
      req(input$event_cox)
      req(input$time_cox)
      data.cox[[input$cmp_event_cox]] <- as.numeric(as.vector(data.cox[[input$cmp_event_cox]]))
      data.cox$cmpp_time <- with(data.cox, ifelse(data.cox[[input$event_cox]] == 0, data.cox[[input$cmp_time_cox]], data.cox[[input$time_cox]]))
      data.cox$cmpp_event <- with(data.cox, ifelse(data.cox[[input$event_cox]] == 0, 2 * data.cox[[input$cmp_event_cox]], 1))
      data.cox$cmpp_event <- factor(data.cox$cmpp_event)
      fg_data <- survival::finegray(formula = survival::Surv(cmpp_time, cmpp_event) ~ ., data = data.cox)
      data.cox <- data.table::data.table(fg_data)
      cc <- substitute(survival::coxph(.form, data = data.cox, weight = fgwt, model = T, ties = .ties), list(.form = form.cox(), .ties = ties.coxph))
    }
    mf <- model.frame(form.cox(), data.cox)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~ length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse = " ,"), "has(have) a unique value. Please remove that from independent variables"))
    )
    if (is.null(design.survey)) {
      if (is.null(id.cluster) & !input$cmp_risk_check) {
        cc <- substitute(survival::coxph(.form, data = data.cox, model = T, ties = .ties), list(.form = form.cox(), .ties = ties.coxph))
      } else if (!is.null(id.cluster) & !input$cmp_risk_check) {
        cc <- substitute(survival::coxph(.form, data = data.cox, model = T, robust = T, ties = .ties), list(.form = form.cox(), .ties = ties.coxph))
      }
      res.cox <- eval(cc)
      if (input$step_check == T) {
        validate(
          need(!is.null(input$step_upper), "Upper limits can't be NULL, please select at least 1 variable."),
          need(length((setdiff(input$step_lower, input$step_upper))) == 0, "Upper limits must include lower limits. Please add the variables to upper limits")
        )
        scope <- lapply(list(input$step_upper, input$step_lower), function(x) {
          as.formula(ifelse(is.null(x), "~1", paste0("~", paste(x, collapse = "+"))))
        })
        data.cox.step <<- data.cox[complete.cases(data.cox[, .SD, .SDcols = c(input$time_cox, input$event_cox, input$indep_cox)])]

        if (is.null(id.cluster)) {
          cc.step <- substitute(survival::coxph(.form, data = data.cox.step, model = T, ties = .ties), list(.form = form.cox(), .ties = ties.coxph))
        } else {
          cc.step <- substitute(survival::coxph(.form, data = data.cox.step, model = T, robust = T, ties = .ties), list(.form = form.cox(), .ties = ties.coxph))
        }

        res.cox <- stats::step(eval(cc.step), direction = input$step_direction, scope = list(upper = scope[[1]], lower = scope[[2]]))
      }
      tb.cox <- jstable::cox2.display(res.cox, dec = input$decimal)
      tb.cox <- jstable::LabeljsCox(tb.cox, ref = label.regress)
      out.cox <- rbind(tb.cox$table, tb.cox$metric)
      sig <- out.cox[, ncol(out.cox)]
      sig <- gsub("< ", "", sig)
      sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
      out.cox <- cbind(out.cox, sig)
      if (is.null(id.cluster)) {
        cap.cox <- paste("Cox's proportional hazard model on time ('", label.regress[variable == input$time_cox, var_label][1], "') to event ('", label.regress[variable == input$event_cox, var_label][1], "')", sep = "")
      } else {
        cap.cox <- paste("Marginal cox model on time ('", label.regress[variable == input$time_cox, var_label][1], "') to event ('", label.regress[variable == input$event_cox, var_label][1], "')", sep = "")
      }

      if (input$subcheck == T) {
        for (v in seq_along(input$subvar_cox)) {
          if (input$subvar_cox[[v]] %in% vlist()$factor_vars) {
            cap.cox <- paste(cap.cox, ", ", label.regress[variable == input$subvar_cox[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_cox[[v]] & level %in% input[[paste0("subval_cox", v)]], val_label], collapse = ", "), sep = "")
          } else {
            cap.cox <- paste(cap.cox, ", ", label.regress[variable == input$subvar_cox[[v]], var_label][1], ": ", paste(input[[paste0("subval_cox", v)]], collapse = "~"), sep = "")
          }
        }
      }
      if (input$step_check == T) {
        cap.cox <- paste0(cap.cox, "- stepwise selection")
      }
    } else {
      data.design <- design.survey()
      if (input$check_rangetime == T) {
        data.design <- subset(data.design, !(get(input$time_cox) < input$range_time[1]))
        data.design$variables[[input$event_cox]] <- ifelse(data.design$variables[[input$time_cox]] >= input$range_time[2] & data.design$variables[[input$event_cox]] == "1", 0, as.numeric(as.vector(data.design$variables[[input$event_cox]])))
        data.design$variables[[input$time_cox]] <- ifelse(data.design$variables[[input$time_cox]] >= input$range_time[2], input$range_time[2], data.design$variables[[input$time_cox]])
      }

      data.design$variables[[input$event_cox]] <- as.numeric(as.vector(data.design$variables[[input$event_cox]]))
      if (input$subcheck == T) {
        validate(
          need(length(input$subvar_cox) > 0, "No variables for subsetting"),
          need(all(sapply(1:length(input$subvar_cox), function(x) {
            length(input[[paste0("subval_cox", x)]])
          })), "No value for subsetting")
        )

        for (v in seq_along(input$subvar_cox)) {
          if (input$subvar_cox[[v]] %in% vlist()$factor_vars) {
            data.design <- subset(data.design, get(input$subvar_cox[[v]]) %in% input[[paste0("subval_cox", v)]])
          } else {
            data.design <- subset(data.design, get(input$subvar_cox[[v]]) >= input[[paste0("subval_cox", v)]][1] & get(input$subvar_cox[[v]]) <= input[[paste0("subval_cox", v)]][2])
          }
        }

        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
        data.design$variables[[input$event_cox]] <- as.numeric(as.vector(data.design$variables[[input$event_cox]]))
      }

      cc <- substitute(survey::svycoxph(.form, design = data.design), list(.form = form.cox()))
      res.cox <- eval(cc)
      if (input$step_check == T) {
        validate(
          need(is.null(design.survey), "Survey cox model can't support stepwise selection")
        )
      }
      tb.cox <- jstable::svycox.display(res.cox, decimal = input$decimal)
      tb.cox <- jstable::LabeljsCox(tb.cox, label.regress)
      out.cox <- rbind(tb.cox$table, tb.cox$metric)
      sig <- out.cox[, ncol(out.cox)]
      sig <- gsub("< ", "", sig)
      sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
      out.cox <- cbind(out.cox, sig)
      cap.cox <- paste("Weighted cox's proportional hazard model on time ('", label.regress[variable == input$time_cox, var_label][1], "') to event ('", label.regress[variable == input$event_cox, var_label][1], "') ", sep = "")
      if (input$subcheck == T) {
        for (v in seq_along(input$subvar_cox)) {
          if (input$subvar_cox[[v]] %in% vlist()$factor_vars) {
            cap.cox <- paste(cap.cox, ", ", label.regress[variable == input$subvar_cox[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_cox[[v]] & level %in% input[[paste0("subval_cox", v)]], val_label], collapse = ", "), sep = "")
          } else {
            cap.cox <- paste(cap.cox, ", ", label.regress[variable == input$subvar_cox[[v]], var_label][1], ": ", paste(input[[paste0("subval_cox", v)]], collapse = "~"), sep = "")
          }
        }
      }
    }

    return(list(table = out.cox, caption = cap.cox))
  })

  return(out)
}
