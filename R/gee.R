
#' @title GEEModuleUI: ModuleUI for GEE
#' @description ModuleUI for GEE
#' @param id id
#' @return GEEModuleUI
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
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
    uiOutput(ns("regressUI_subval"))
  )
}




#' @title GEEModuleLinear: Module for linear GEE using reactive data
#' @description Module for linear GEE using reactive data
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data, ordered by id.
#' @param data_label reactive data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param id.gee reactive repeated measure variable
#' @return GEEModuleLinear
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname GEEModuleLinear
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame
#' @importFrom epiDisplay regress.display
#' @importFrom purrr map_lgl
#' @importFrom geepack geeglm

GEEModuleLinear <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, id.gee) {

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

    id <- id.gee()

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    factor_vars <- setdiff(factor_vars, id)
    #factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
    factor_list <- mklist(data_varStruct(), factor_vars)


    conti_vars <- setdiff(names(data()), factor_vars)
    conti_vars <- setdiff(conti_vars, id)
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
    id <- id.gee()
    req(!is.null(input$dep_vars))
    vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  c(input$dep_vars, id))
    #varsIni <- sapply(vars,
    #                  function(v){
    #                    forms <- as.formula(paste(input$dep_vars, "~", v))
    #                    coef <- summary(geepack::geeglm(forms, data = data()[!is.na(get(v))], family = "gaussian", id = get(id), corstr = "exchangeable"))$coef
    #                    sigOK <- !all(coef[-1, "Pr(>|W|)"] > 0.05)
    #                    return(sigOK)
    #                  })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = vars, multiple = T,
                  selected = vars[1]
      )
    )
  })

  observeEvent(input$indep_vars, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)
      factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
      factor_subgroup <- setdiff(factor_vars, c(input$dep_vars, input$indep_vars, id.gee()))
      factor_subgroup_list <- mklist(data_varStruct(), factor_subgroup)
      validate(
        need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_regress"), "Sub-group variable",
                    choice = factor_subgroup_list, multiple = F,
                    selected = factor_subgroup[1])
      )
    })

  })


  output$regressUI_subval <- renderUI({
    req(input$regressUI_subcheck == T)
    req(input$subvar_regress)
    selectInput(session$ns("subval_regress"), "Sub-group value",
                choice = data_label()[variable == input$subvar_regress, val_label], multiple = F,
                selected = data_label()[variable == input$subvar_regress, val_label][1])
  })



  out <- reactive({
    data.regress <- data()
    id <- id.gee()
    if(input$regressUI_subcheck == T){
      req(input$subvar_regress)
      data.regress <- data.regress[get(input$subvar_regress) == input$subval_regress, ]
    }
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form <- as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))

    mf <- model.frame(form, data.regress)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
    )

    res.gee <- geepack::geeglm(form, data = data.regress, family = "gaussian", id = get(id), corstr = "exchangeable")
    info.gee <- jstable::geeglm.display(res.gee, decimal = input$decimal)
    info.gee$caption = gsub("id", id, info.gee$caption)
    ltb.gee <- jstable::LabeljsGeeglm(info.gee, ref = data_label())
    out.tb <- rbind(ltb.gee$table, ltb.gee$metric)
    cap.gee <- ltb.gee$caption

    if(input$regressUI_subcheck == T){
      cap.gee <- paste(cap.gee, " : ", data_label()[variable == input$subvar_regress, var_label][1], ": ", data_label()[variable == input$subvar_regress & level == input$subval_regress, val_label], sep = "")
    }
    sig <- ifelse(out.tb[, ncol(out.tb)] == "< 0.001", "**", ifelse(as.numeric(out.tb[, ncol(out.tb)]) <= 0.05, "**", NA))
    out.gee <- cbind(out.tb, sig)
    return(list(table = out.gee, caption = cap.gee))
  })

  return(out)


}



#' @title GEEModuleLogistic: Module for logistic GEE using reactive data
#' @description Module for logistic GEE using reactive data
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data, ordered by id.
#' @param data_label reactive data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param id.gee reactive repeated measure variable
#' @return GEEModuleLogistic
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname GEEModuleLogistic
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame
#' @importFrom epiDisplay regress.display
#' @importFrom purrr map_lgl
#' @importFrom geepack geeglm

GEEModuleLogistic <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, id.gee) {

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

    id <- id.gee()

    factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
    factor_vars <- setdiff(factor_vars, id)
    #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
    factor_list <- mklist(data_varStruct(), factor_vars)


    nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars])

    factor2_vars <- factor_vars[nclass_factor == 2]
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
    id <- id.gee()
    req(!is.null(input$dep_vars))
    vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  c(input$dep_vars, id))
    #varsIni <- sapply(vars,
    #                  function(v){
    #                    forms <- as.formula(paste(input$dep_vars, "~", v))
    #                    coef <- summary(geepack::geeglm(forms, data = data()[!is.na(get(v))], family = "gaussian", id = get(id), corstr = "exchangeable"))$coef
    #                    sigOK <- !all(coef[-1, "Pr(>|W|)"] > 0.05)
    #                    return(sigOK)
    #                  })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = vars, multiple = T,
                  selected = vars[1]
      )
    )
  })

  observeEvent(input$indep_vars, {
    output$regressUI_subvar <- renderUI({
      req(input$regressUI_subcheck == T)
      factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
      factor_subgroup <- setdiff(factor_vars, c(input$dep_vars, input$indep_vars, id.gee()))
      factor_subgroup_list <- mklist(data_varStruct(), factor_subgroup)
      validate(
        need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_regress"), "Sub-group variable",
                    choice = factor_subgroup_list, multiple = F,
                    selected = factor_subgroup[1])
      )
    })

  })


  output$regressUI_subval <- renderUI({
    req(input$regressUI_subcheck == T)
    req(input$subvar_regress)
    selectInput(session$ns("subval_regress"), "Sub-group value",
                choice = data_label()[variable == input$subvar_regress, val_label], multiple = F,
                selected = data_label()[variable == input$subvar_regress, val_label][1])
  })



  out <- reactive({
    data.logistic <- data()
    id <- id.gee()
    if(input$regressUI_subcheck == T){
      req(input$subvar_regress)
      data.logistic <- data.logistic[get(input$subvar_regress) == input$subval_regress, ]
    }
    y <- input$dep_vars
    xs <- input$indep_vars
    data.logistic[[y]] <- as.numeric(as.vector(data.logistic[[y]]))

    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form <- as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))

    mf <- model.frame(form, data.logistic)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
    )
    lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
    validate(
      need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
    )

    res.gee <- geepack::geeglm(form, data = data.logistic, family = "binomial", id = get(id), corstr = "exchangeable")
    info.gee <- jstable::geeglm.display(res.gee, decimal = input$decimal)
    info.gee$caption = gsub("id", id, info.gee$caption)
    ltb.gee <- jstable::LabeljsGeeglm(info.gee, ref = data_label())
    out.tb <- rbind(ltb.gee$table, ltb.gee$metric)
    cap.gee <- ltb.gee$caption

    if(input$regressUI_subcheck == T){
      cap.gee <- paste(cap.gee, " : ", data_label()[variable == input$subvar_regress, var_label][1], ": ", data_label()[variable == input$subvar_regress & level == input$subval_regress, val_label], sep = "")
    }
    sig <- ifelse(out.tb[, ncol(out.tb)] == "< 0.001", "**", ifelse(as.numeric(out.tb[, ncol(out.tb)]) <= 0.05, "**", NA))
    out.gee <- cbind(out.tb, sig)
    return(list(table = out.gee, caption = cap.gee))
  })

  return(out)


}
