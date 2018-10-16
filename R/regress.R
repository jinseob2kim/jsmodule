#' @title mklist: make intersect varlist
#' @description make intersect varlist
#' @param varlist varlist
#' @param vars vars
#' @return Intersect varlist
#' @details Internal function
#' @examples
#' \dontrun{
#' if(interactive()){
#'  data_varStruct = list(variable = names(data))
#'  conti_list <- mklist(data_varStruct, conti_vars)
#'  }
#' }
#' @rdname mklist



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
    )
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
#' @importFrom stats glm as.formula
#' @importFrom epiDisplay regress.display
#' @importFrom jstable LabelepiDisplay
#'

regressModule <- function(input, output, session, data, data_label, data_varStruct = NULL) {

  if (is.null(data_varStruct)){
    data_varStruct = list(variable = names(data))
  }

  if (!("data.table" %in% class(data))) {data = data.table(data)}
  if (!("data.table" %in% class(data_label))) {data_label = data.table(data_label)}

  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  factor_list <- mklist(data_varStruct, factor_vars)

  conti_vars <- setdiff(names(data), factor_vars)
  conti_list <- mklist(data_varStruct, conti_vars)

  nclass_factor <- unlist(data[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])

  group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10 & nclass_factor < nrow(data)]
  group_list <- mklist(data_varStruct, group_vars)

  except_vars <- factor_vars[nclass_factor > 10 | nclass_factor == 1 | nclass_factor == nrow(data)]


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
    vars <- setdiff(setdiff(names(data), except_vars),  input$dep_vars)
    varsIni <- sapply(vars,
                      function(v){
                        forms <- as.formula(paste(input$dep_vars, "~", v))
                        coef <- summary(glm(forms, data = data))$coefficients
                        sigOK <- !all(coef[-1, "Pr(>|t|)"] > 0.05)
                        return(sigOK)
                      })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = vars, multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  out <- reactive({
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form = as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))
    res.linear = glm(form, data = data)
    tb.linear = epiDisplay::regress.display(res.linear, crude = T, crude.p.value = T, decimal = input$decimal)
    cap.linear = paste("Linear regression predicting ", data_label[variable == y, var_label][1], sep="")
    out.linear = jstable::LabelepiDisplay(tb.linear, xs, ref = data_label)
    return(list(table = out.linear, caption = cap.linear))
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
#' @importFrom stats glm as.formula
#' @importFrom epiDisplay regress.display

regressModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL) {

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
    #factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
    factor_list <- mklist(data_varStruct(), factor_vars)


    conti_vars <- setdiff(names(data()), factor_vars)
    conti_list <- mklist(data_varStruct(), conti_vars)

    nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])
    #nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})

    group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10 & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > 10 | nclass_factor == 1 | nclass_factor == nrow(data())]

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
    vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  input$dep_vars)
    varsIni <- sapply(vars,
                      function(v){
                        forms <- as.formula(paste(input$dep_vars, "~", v))
                        coef <- summary(glm(forms, data = data()))$coefficients
                        sigOK <- !all(coef[-1, "Pr(>|t|)"] > 0.05)
                        return(sigOK)
                      })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = vars, multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  out <- reactive({
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form = as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))
    res.linear = glm(form, data = data())
    tb.linear = regress.display2(res.linear, crude = T, crude.p.value = T, decimal = input$decimal, simplified = F)
    cap.linear = paste("Linear regression predicting ", data_label()[variable == y, var_label][1], sep="")
    out.linear = jstable::LabelepiDisplay(tb.linear, xs, ref = data_label())
    #out.linear = summary(res.linear)$coefficients
    #sig = ifelse(out.linear[, 4] <= 0.05, "**", "NA")
    return(list(table = out.linear, caption = cap.linear))
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
#' @importFrom stats glm as.formula binomial
#' @importFrom epiDisplay logistic.display
#' @importFrom jstable LabelepiDisplay
#'

logisticModule <- function(input, output, session, data, data_label, data_varStruct = NULL) {

  if (is.null(data_varStruct)){
    data_varStruct = list(variable = names(data))
  }

  if (!("data.table" %in% class(data))) {data = data.table(data)}
  if (!("data.table" %in% class(data_label))) {data_label = data.table(data_label)}

  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  factor_list <- mklist(data_varStruct, factor_vars)


  nclass_factor <- unlist(data[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])

  factor2_vars <- factor_vars[nclass_factor == 2]
  factor2_list <- mklist(data_varStruct, factor2_vars)

  except_vars <- factor_vars[nclass_factor > 10 | nclass_factor == 1 | nclass_factor == nrow(data)]


  output$dep <- renderUI({
    tagList(
      selectInput(session$ns("dep_vars"), "Dependent variable",
                  choices = factor2_list, multiple = F,
                  selected = factor2_vars[1]
      )
    )
  })


  output$indep <- renderUI({
    req(!is.null(input$dep_vars))
    vars <- setdiff(setdiff(names(data), except_vars),  input$dep_vars)
    varsIni <- sapply(vars,
                      function(v){
                        forms <- as.formula(paste(input$dep_vars, "~", v))
                        coef <- summary(glm(forms, data = data, family = binomial))$coefficients
                        sigOK <- !all(coef[-1, 4] > 0.05)
                        return(sigOK)
                      })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = vars, multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  out <- reactive({
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form = as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))
    res.logistic = glm(form, data = data, family = "binomial")
    tb.logistic = epiDisplay::logistic.display(res.logistic, crude = T, crude.p.value = T, decimal = input$decimal)
    cap.logistic = paste("Logistic regression predicting ", data_label[variable == y, var_label][1], sep="")
    out.logistic = jstable::LabelepiDisplay(tb.logistic, xs, ref = data_label)
    return(list(table = out.logistic, caption = cap.logistic))
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
#' @importFrom stats glm as.formula binomial
#' @importFrom epiDisplay logistic.display


logisticModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL) {

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


    nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])

    factor2_vars <- factor_vars[nclass_factor == 2]
    factor2_list <- mklist(data_varStruct(), factor2_vars)

    except_vars <- factor_vars[nclass_factor > 10 | nclass_factor == 1 | nclass_factor == nrow(data())]
    return(list(factor_vars = factor_vars, factor_list = factor_list, nclass_factor = nclass_factor, factor2_vars = factor2_vars,
                factor2_list = factor2_list, except_vars = except_vars)
           )

    })




  output$dep <- renderUI({
    tagList(
      selectInput(session$ns("dep_vars"), "Dependent variable",
                  choices = vlist()$factor2_list, multiple = F,
                  selected = vlist()$factor2_vars[1]
      )
    )
  })


  output$indep <- renderUI({
    req(!is.null(input$dep_vars))
    vars <- setdiff(setdiff(names(data()), vlist()$except_vars),  input$dep_vars)
    varsIni <- sapply(vars,
                      function(v){
                        forms <- as.formula(paste(input$dep_vars, "~", v))
                        coef <- summary(glm(forms, data = data(), family = binomial))$coefficients
                        sigOK <- !all(coef[-1, 4] > 0.05)
                        return(sigOK)
                      })


    tagList(
      selectInput(session$ns("indep_vars"), "Independent variables",
                  choices = vars, multiple = T,
                  selected = vars[varsIni]
      )
    )
  })

  out <- reactive({
    y <- input$dep_vars
    xs <- input$indep_vars
    validate(
      need(!is.null(input$indep_vars) , "Please select at least 1 variable")
    )
    form = as.formula(paste(y, "~", paste(xs, collapse = " + "), sep = " "))
    res.logistic = glm(form, data = data(), family = binomial)
    tb.logistic = logistic.display2(res.logistic, crude = T, crude.p.value = T, decimal = input$decimal)
    cap.logistic = paste("Logistic regression predicting ", data_label()[variable == y, var_label][1], sep="")
    out.logistic = jstable::LabelepiDisplay(tb.logistic, xs, ref = data_label)
    #out.logistic = summary(res.logistic)$coefficients
    #sig = ifelse(out.logistic[, 4] <= 0.05, "**", "NA")
    return(list(table = out.logistic, caption = cap.logistic))
  })

  return(out)


}
