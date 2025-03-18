#' @title GEEModuleUI: shiny modulde UI for generalized estimating equation(GEE).
#' @description Shiny modulde UI for generalized estimating equation(GEE).
#' @param id id
#' @return Shiny modulde UI for generalized estimating equation(GEE).
#' @details Shiny modulde UI for generalized estimating equation(GEE).
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       GEEModuleUI("linear")
#'     ),
#'     mainPanel(
#'       DTOutput("lineartable")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'   id.gee <- reactive("mpg")
#'
#'   out_linear <- callModule(GEEModuleLinear, "linear",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL, id.gee = id.gee
#'   )
#'
#'   output$lineartable <- renderDT({
#'     hide <- which(colnames(out_linear()$table) == "sig")
#'     datatable(out_linear()$table,
#'       rownames = T, extension = "Buttons", caption = out_linear()$caption,
#'       options = c(
#'         opt.tbreg(out_linear()$caption),
#'         list(columnDefs = list(list(visible = FALSE, targets = hide))),
#'         list(scrollX = TRUE)
#'       )
#'     ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
#'   })
#' }
#' @rdname GEEModuleUI
#' @export

GEEModuleUI <- function(id) {
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
    checkboxInput(ns("pcut_univariate"),"filter_pvalue", value = FALSE),
    uiOutput(ns("pcut_slider"))
  )
}




#' @title GEEModuleLinear: shiny modulde server for gaussian generalized estimating equation(GEE) using reactive data.
#' @description Shiny modulde server for gaussian generalized estimating equation(GEE) using reactive data.
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data, ordered by id.
#' @param data_label reactive data label
#' @param data_varStruct List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param id.gee reactive repeated measure variable
#' @return Shiny modulde server for gaussian generalized estimating equation(GEE).
#' @details Shiny modulde server for gaussian generalized estimating equation(GEE) using reactive data.
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       GEEModuleUI("linear")
#'     ),
#'     mainPanel(
#'       DTOutput("lineartable")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'   id.gee <- reactive("mpg")
#'
#'   out_linear <- callModule(GEEModuleLinear, "linear",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL, id.gee = id.gee
#'   )
#'
#'   output$lineartable <- renderDT({
#'     hide <- which(colnames(out_linear()$table) == "sig")
#'     datatable(out_linear()$table,
#'       rownames = T, extension = "Buttons", caption = out_linear()$caption,
#'       options = c(
#'         opt.tbreg(out_linear()$caption),
#'         list(columnDefs = list(list(visible = FALSE, targets = hide))),
#'         list(scrollX = TRUE)
#'       )
#'     ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
#'   })
#' }
#' @rdname GEEModuleLinear
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame complete.cases
#' @importFrom purrr map_lgl
#' @importFrom geepack geeglm

GEEModuleLinear <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, id.gee) {
  ## To remove NOTE.
  level <- val_label <- variable <- NULL

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

    id <- id.gee()

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    factor_vars <- setdiff(factor_vars, id)
    # factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
    factor_list <- mklist(data_varStruct(), factor_vars)


    conti_vars <- setdiff(names(data()), factor_vars)
    conti_vars <- setdiff(conti_vars, id)
    conti_list <- mklist(data_varStruct(), conti_vars)

    nclass_factor <- unlist(data()[, lapply(.SD, function(x) {
      length(levels(x))
    }), .SDcols = factor_vars])
    # nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})

    group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]

    return(list(
      factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list,
      group_vars = group_vars, group_list = group_list, except_vars = except_vars
    ))
  })

  output$pcut_slider <- renderUI({
    req(input$pcut_univariate)
    print("Rendering sliderInput!")
    sliderInput(session$ns("pcut"), "Choose a p-value", min = 0, max = 0.1, value = 0.05, step = 0.001)
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
    id <- id.gee()
    req(!is.null(input$dep_vars))
    vars <- setdiff(setdiff(names(data()), vlist()$except_vars), c(input$dep_vars, id))
    # varsIni <- sapply(vars,
    #                  function(v){
    #                    forms <- as.formula(paste(input$dep_vars, "~", v))
    #                    coef <- summary(geepack::geeglm(forms, data = data()[!is.na(get(v))], family = "gaussian", id = get(id), corstr = "exchangeable"))$coef
    #                    sigOK <- !all(coef[-1, "Pr(>|W|)"] > 0.05)
    #                    return(sigOK)
    #                  })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
        choices = mklist(data_varStruct(), vars), multiple = T,
        selected = vars[1]
      )
    )
  })

  observeEvent(input$regressUI_subcheck, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)

      var_subgroup <- setdiff(names(data()), c(input$dep_vars, input$indep_vars, id.gee()))
      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0, "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_regress"), "Sub-group variables",
          choices = var_subgroup_list, multiple = T,
          selected = var_subgroup[1]
        )
      )
    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(length(input$subvar_regress) > 0)

      outUI <- tagList()

      for (v in seq_along(input$subvar_regress)) {
        if (input$subvar_regress[[v]] %in% vlist()$factor_vars) {
          outUI[[v]] <- selectInput(session$ns(paste0("subval_regress", v)), paste0("Sub-group value: ", input$subvar_regress[[v]]),
            choices = data_label()[variable == input$subvar_regress[[v]], level], multiple = T,
            selected = data_label()[variable == input$subvar_regress[[v]], level][1]
          )
        } else {
          val <- stats::quantile(data()[[input$subvar_regress[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(session$ns(paste0("subval_regress", v)), paste0("Sub-group range: ", input$subvar_regress[[v]]),
            min = val[1], max = val[5],
            value = c(val[2], val[4])
          )
        }
      }
      outUI
    })
  })






  out <- reactive({
    data.regress <- data()
    label.regress <- data_label()
    idgee_Plz_Noduplicate <- id.gee()
    if (input$regressUI_subcheck == T) {
      validate(
        need(length(input$subvar_regress) > 0, "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_regress), function(x) {
          length(input[[paste0("subval_regress", x)]])
        })), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_regress)) {
        if (input$subvar_regress[[v]] %in% vlist()$factor_vars) {
          data.regress <- data.regress[get(input$subvar_regress[[v]]) %in% input[[paste0("subval_regress", v)]]]
        } else {
          data.regress <- data.regress[get(input$subvar_regress[[v]]) >= input[[paste0("subval_regress", v)]][1] & get(input$subvar_regress[[v]]) <= input[[paste0("subval_regress", v)]][2]]
        }
      }
      data.regress[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.regress)[, c("variable", "level")]
      data.table::setkey(data_label(), "variable", "level")
      data.table::setkey(label.regress2, "variable", "level")
      label.regress <- data_label()[label.regress2]
    }
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars), "Please select at least 1 variable")
    )
    form <- as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))

    mf <- model.frame(form, data.regress)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~ length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse = " ,"), "has(have) a unique value. Please remove that from independent variables"))
    )

    nomiss <- stats::complete.cases(data.regress[, c(y, xs), with = F])
    res.gee <- geepack::geeglm(form, data = data.regress[nomiss, ], family = "gaussian", id = get(idgee_Plz_Noduplicate), corstr = "exchangeable")
    if(input$pcut_univariate==FALSE){
      info.gee <- jstable::geeglm.display(res.gee, decimal = input$decimal)
    }else{
      info.gee <- jstable::geeglm.display(res.gee, decimal = input$decimal, pcut.univariate = input$pcut)
    }

    info.gee$caption <- gsub("idgee_Plz_Noduplicate", idgee_Plz_Noduplicate, info.gee$caption)
    ltb.gee <- jstable::LabeljsGeeglm(info.gee, ref = label.regress)
    out.tb <- rbind(ltb.gee$table, ltb.gee$metric)
    cap.gee <- ltb.gee$caption

    if (input$regressUI_subcheck == T) {
      for (v in seq_along(input$subvar_regress)) {
        if (input$subvar_regress[[v]] %in% vlist()$factor_vars) {
          cap.gee <- paste(cap.gee, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_regress[[v]] & level %in% input[[paste0("subval_regress", v)]], val_label], collapse = ", "), sep = "")
        } else {
          cap.gee <- paste(cap.gee, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(input[[paste0("subval_regress", v)]], collapse = "~"), sep = "")
        }
      }
    }
    sig <- ifelse(out.tb[, ncol(out.tb)] == "< 0.001", "**", ifelse(as.numeric(out.tb[, ncol(out.tb)]) <= 0.05, "**", NA))
    out.gee <- cbind(out.tb, sig)
    return(list(table = out.gee, caption = cap.gee))
  })

  return(out)
}



#' @title GEEModuleLogistic: shiny modulde server for binomial gaussian generalized estimating equation(GEE) using reactive data.
#' @description Shiny modulde server for binomial gaussian generalized estimating equation(GEE) using reactive data.
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data, ordered by id.
#' @param data_label reactive data label
#' @param data_varStruct List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param id.gee reactive repeated measure variable
#' @return Shiny modulde server for binomial gaussian generalized estimating equation(GEE).
#' @details Shiny modulde server for binomial gaussian generalized estimating equation(GEE) using reactive data.
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       GEEModuleUI("logistic")
#'     ),
#'     mainPanel(
#'       DTOutput("logistictable")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'   id.gee <- reactive("mpg")
#'
#'   out_logistic <- callModule(GEEModuleLogistic, "logistic",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL, id.gee = id.gee
#'   )
#'
#'   output$logistictable <- renderDT({
#'     hide <- which(colnames(out_logistic()$table) == "sig")
#'     datatable(out_logistic()$table,
#'       rownames = T, extension = "Buttons",
#'       caption = out_logistic()$caption,
#'       options = c(
#'         opt.tbreg(out_logistic()$caption),
#'         list(columnDefs = list(list(visible = FALSE, targets = hide))),
#'         list(scrollX = TRUE)
#'       )
#'     ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
#'   })
#' }
#' @rdname GEEModuleLogistic
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame complete.cases
#' @importFrom purrr map_lgl
#' @importFrom geepack geeglm

GEEModuleLogistic <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, id.gee) {
  ## To remove NOTE.
  level <- val_label <- variable <- NULL

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

    id <- id.gee()

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    factor_vars <- setdiff(factor_vars, id)
    # data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
    factor_list <- mklist(data_varStruct(), factor_vars)


    nclass_factor <- unlist(data()[, lapply(.SD, function(x) {
      length(levels(x))
    }), .SDcols = factor_vars])
    class01_factor <- unlist(data()[, lapply(.SD, function(x) {
      identical(levels(x), c("0", "1"))
    }), .SDcols = factor_vars])

    validate(
      need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
    )
    factor_01vars <- factor_vars[class01_factor]
    factor_01_list <- mklist(data_varStruct(), factor_01vars)


    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]
    return(list(
      factor_vars = factor_vars, factor_list = factor_list, nclass_factor = nclass_factor, factor_01vars = factor_01vars,
      factor_01_list = factor_01_list, except_vars = except_vars
    ))
  })

  output$pcut_slider <- renderUI({
    req(input$pcut_univariate)
    print("Rendering sliderInput!")
    sliderInput(session$ns("pcut"), "Choose a p-value", min = 0, max = 0.1, value = 0.05, step = 0.001)
  })


  output$dep <- renderUI({
    validate(
      need(length(vlist()$factor_01vars) >= 1, "No candidate dependent variable coded as 0, 1")
    )
    tagList(
      selectInput(session$ns("dep_vars"), "Dependent variable",
        choices = vlist()$factor_01_list, multiple = F,
        selected = vlist()$factor_01vars[1]
      )
    )
  })


  output$indep <- renderUI({
    id <- id.gee()
    req(!is.null(input$dep_vars))
    vars <- setdiff(setdiff(names(data()), vlist()$except_vars), c(input$dep_vars, id))
    # varsIni <- sapply(vars,
    #                  function(v){
    #                    forms <- as.formula(paste(input$dep_vars, "~", v))
    #                    coef <- summary(geepack::geeglm(forms, data = data()[!is.na(get(v))], family = "gaussian", id = get(id), corstr = "exchangeable"))$coef
    #                    sigOK <- !all(coef[-1, "Pr(>|W|)"] > 0.05)
    #                    return(sigOK)
    #                  })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
        choices = mklist(data_varStruct(), vars), multiple = T,
        selected = vars[1]
      )
    )
  })

  observeEvent(input$regressUI_subcheck, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)
      var_subgroup <- setdiff(names(data()), c(input$dep_vars, input$indep_vars, id.gee()))
      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0, "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_regress"), "Sub-group variables",
          choices = var_subgroup_list, multiple = T,
          selected = var_subgroup[1]
        )
      )
    })

    output$regressUI_subval <- renderUI({
      req(input$regressUI_subcheck == T)
      req(length(input$subvar_regress) > 0)

      outUI <- tagList()

      for (v in seq_along(input$subvar_regress)) {
        if (input$subvar_regress[[v]] %in% vlist()$factor_vars) {
          outUI[[v]] <- selectInput(session$ns(paste0("subval_regress", v)), paste0("Sub-group value: ", input$subvar_regress[[v]]),
            choices = data_label()[variable == input$subvar_regress[[v]], level], multiple = T,
            selected = data_label()[variable == input$subvar_regress[[v]], level][1]
          )
        } else {
          val <- stats::quantile(data()[[input$subvar_regress[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(session$ns(paste0("subval_regress", v)), paste0("Sub-group range: ", input$subvar_regress[[v]]),
            min = val[1], max = val[5],
            value = c(val[2], val[4])
          )
        }
      }
      outUI
    })
  })






  out <- reactive({
    req(input$dep_vars)
    req(input$indep_vars)
    data.logistic <- data()
    label.regress <- data_label()
    idgee_Plz_Noduplicate <- id.gee()
    if (input$regressUI_subcheck == T) {
      validate(
        need(length(input$subvar_regress) > 0, "No variables for subsetting"),
        need(all(sapply(1:length(input$subvar_regress), function(x) {
          length(input[[paste0("subval_regress", x)]])
        })), "No value for subsetting")
      )

      for (v in seq_along(input$subvar_regress)) {
        if (input$subvar_regress[[v]] %in% vlist()$factor_vars) {
          data.logistic <- data.logistic[get(input$subvar_regress[[v]]) %in% input[[paste0("subval_regress", v)]]]
        } else {
          data.logistic <- data.logistic[get(input$subvar_regress[[v]]) >= input[[paste0("subval_regress", v)]][1] & get(input$subvar_regress[[v]]) <= input[[paste0("subval_regress", v)]][2]]
        }
      }
      data.logistic[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.logistic)[, c("variable", "level")]
      data.table::setkey(data_label(), "variable", "level")
      data.table::setkey(label.regress2, "variable", "level")
      label.regress <- data_label()[label.regress2]
    }
    y <- input$dep_vars
    xs <- input$indep_vars
    data.logistic[[y]] <- as.numeric(as.vector(data.logistic[[y]]))

    validate(
      need(!is.null(input$indep_vars), "Please select at least 1 variable")
    )
    form <- as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))

    mf <- model.frame(form, data.logistic)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~ length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse = " ,"), "has(have) a unique value. Please remove that from independent variables"))
    )

    nomiss <- stats::complete.cases(data.logistic[, c(y, xs), with = F])
    res.gee <- geepack::geeglm(form, data = data.logistic[nomiss, ], family = "binomial", id = get(idgee_Plz_Noduplicate), corstr = "exchangeable")
    if(input$pcut_univariate==FALSE){
      info.gee <- jstable::geeglm.display(res.gee, decimal = input$decimal)
    }else{
      info.gee <- jstable::geeglm.display(res.gee, decimal = input$decimal, input$pcut)
    }
    info.gee$caption <- gsub("idgee_Plz_Noduplicate", idgee_Plz_Noduplicate, info.gee$caption)
    ltb.gee <- jstable::LabeljsGeeglm(info.gee, ref = label.regress)
    out.tb <- rbind(ltb.gee$table, ltb.gee$metric)
    cap.gee <- ltb.gee$caption

    if (input$regressUI_subcheck == T) {
      for (v in seq_along(input$subvar_regress)) {
        if (input$subvar_regress[[v]] %in% vlist()$factor_vars) {
          cap.gee <- paste(cap.gee, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(label.regress[variable == input$subvar_regress[[v]] & level %in% input[[paste0("subval_regress", v)]], val_label], collapse = ", "), sep = "")
        } else {
          cap.gee <- paste(cap.gee, ", ", label.regress[variable == input$subvar_regress[[v]], var_label][1], ": ", paste(input[[paste0("subval_regress", v)]], collapse = "~"), sep = "")
        }
      }
    }
    sig <- ifelse(out.tb[, ncol(out.tb)] == "< 0.001", "**", ifelse(as.numeric(out.tb[, ncol(out.tb)]) <= 0.05, "**", NA))
    out.gee <- cbind(out.tb, sig)
    return(list(table = out.gee, caption = cap.gee))
  })

  return(out)
}
