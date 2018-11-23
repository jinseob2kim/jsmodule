
#' @title tb1simpleUI : tb1 module UI for propensity score analysis
#' @description tb1 module UI for propensity score analysis
#' @param id id
#' @return UI
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname tb1simpleUI
#' @export

tb1simpleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("base")),
    uiOutput(ns("sub2"))
  )
}







#' @title tb1simple: tb1 module for propensity score analysis
#' @description tb1 module for propensity score analysis
#' @param input input
#' @param output output
#' @param session session
#' @param data data
#' @param matdata matdata
#' @param data_label data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param group_var group_var
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[labelled]{var_label}}
#'  \code{\link[jstable]{CreateTableOneJS}}
#'  \code{\link[survey]{svydesign}}
#'  \code{\link[tableone]{svyCreateTableOne}}
#' @rdname tb1simple
#' @export
#' @importFrom labelled var_label
#' @importFrom jstable CreateTableOneJS
#' @importFrom survey svydesign
#' @importFrom tableone svyCreateTableOne


tb1simple <- function(input, output, session, data, matdata, data_label, data_varStruct = NULL, group_var){

  if (is.null(data_varStruct)){
    data_varStruct = list(variable = names(data))
    }

  if (!("data.table" %in% class(data))) {data = data.table(data)}
  if (!("data.table" %in% class(data_label))) {data_label = data.table(data_label)}

  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  factor_list <- mklist(data_varStruct, factor_vars)

  conti_vars <- setdiff(names(data), c(factor_vars, "pscore", "iptw"))
  conti_list <- mklist(data_varStruct, conti_vars)

  nclass_factor <- unlist(data[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}), .SDcols = factor_vars])

  group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10 & nclass_factor < nrow(data)]
  group_list <- mklist(data_varStruct, group_vars)

  except_vars <- factor_vars[nclass_factor > 10 | nclass_factor == 1 | nclass_factor == nrow(data)]


  ## non-normal: shapiro test
  f <- function(x) {
      if (diff(range(x, na.rm = T)) == 0) return(F) else return(shapiro.test(x)$p.value <= 0.05)
    }

  non_normal <- ifelse(nrow(data) <=3 | nrow(data) >= 5000,
                         rep(F, length(conti_vars)),
                         sapply(conti_vars, function(x){f(data[[x]])})
                       )



  output$base <- renderUI({
    tagList(
      selectInput(session$ns("nonnormal_vars"), "Non-normal variable (continuous)",
                  choices = conti_list, multiple = T,
                  selected = conti_vars[non_normal]
      ),
      sliderInput(session$ns("decimal_tb1_con"), "Digits (continuous)",
                  min = 1, max = 3, value = 1
      ),
      sliderInput(session$ns("decimal_tb1_cat"), "Digits (categorical, %)",
                  min = 1, max = 3, value = 1
      ),
      sliderInput(session$ns("decimal_tb1_p"), "Digits (p)",
                  min = 3, max = 5, value = 3
      ),
      checkboxInput(session$ns("smd"), "Show SMD", T)
    )

  })




  output$sub2 <- renderUI({
    tagList(
      selectInput(session$ns("group2_vars"), "2nd group (optional)",
                  choices = c("None", mksetdiff(group_list, group_var)), multiple = F,
                  selected = "None"),
      checkboxInput(session$ns("psub"), "Subgroup p-values", F)
    )

  })



  labelled::var_label(data) = sapply(names(data), function(v){data_label[variable == v, var_label][1]}, simplify = F)

  out <- reactive({
    vars = setdiff(setdiff(names(data),except_vars),  group_var)
    if (input$group2_vars == "None"){
      vars.tb1 = setdiff(vars, c(group_var, "pscore", "iptw"))

      vars.fisher = sapply(setdiff(factor_vars, group_var), function(x){is(tryCatch(chisq.test(table(data[[group_var]], data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      vars.fisher = setdiff(factor_vars, group_var)[unlist(vars.fisher)]

      res = jstable::CreateTableOneJS(data = data,
                                      vars = vars.tb1, strata = group_var, includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)

      res.ps = jstable::CreateTableOneJS(data = matdata,
                                         vars = vars.tb1, strata = group_var, includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)

    } else{
      vars.tb1 = setdiff(vars, c(group_var, input$group2_vars, "pscore", "iptw"))

      vars.fisher = sapply(setdiff(factor_vars, c(group_var, input$group2_vars)), function(x){is(tryCatch(chisq.test(table(data[[group_var]], data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      vars.fisher = setdiff(factor_vars, c(group_var, input$group2_vars))[unlist(vars.fisher)]

      res = jstable::CreateTableOneJS(data = data,
                                      vars = vars.tb1, strata = group_var, strata2 = input$group2_vars, includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)

      res.ps = jstable::CreateTableOneJS(data = matdata,
                                         vars = vars.tb1, strata = group_var, strata2 = input$group2_vars, includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)

    }




    Svydesign <- survey::svydesign(ids = ~ 1, data = data, weights = ~ iptw)

    res.iptw1 <- tableone::svyCreateTableOne(vars = vars.tb1, strata = group_var, data = Svydesign, smd = input$smd)
    ptb1 <- print(res.iptw1, nonnormal = input$nonnormal_vars, catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p,
                  showAllLevels = T, printToggle = F, quote = F, smd = input$smd)

    rownames(ptb1) = gsub("(mean (sd))", "", rownames(ptb1), fixed=T)
    colnames(ptb1)[1] = data_label()[get("variable") == group_var(), "var_label"][1]

    #colname.group_var = unlist(data_label()[get("variable") == strata, "val_label"])
    #colnames(ptb1)[1:(length(colname.group_var)+1)] = unlist(c(data_label()[get("variable") == strata, "var_label"][1], colname.group_var))
    #ptb1[,1] = vals.tb1
    sig = ifelse(ptb1[,"p"] == "<0.001", "0", ptb1[,"p"])
    sig = as.numeric(as.vector(sig))
    sig = ifelse(sig <= 0.05, "**", "")
    res.iptw = list(table = cbind(ptb1, sig), caption = paste(res$caption, "- IPTW"))

    return(list(original = res, ps = res.ps, iptw = res.iptw))

    })
  return(out)

  }






#' @title tb1simple2: tb1 module for propensity score analysis for reactive data
#' @description tb1 module for propensity score analysis
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param matdata reactive matdata
#' @param data_label reactive data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param vlist variable lists
#' @param group_var reactive group_var
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[jstable]{CreateTableOneJS}}
#'  \code{\link[survey]{svydesign}}
#'  \code{\link[tableone]{svyCreateTableOne}}
#' @rdname tb1simple2
#' @export
#' @importFrom jstable CreateTableOneJS
#' @importFrom survey svydesign
#' @importFrom tableone svyCreateTableOne
#'
tb1simple2 <- function(input, output, session, data, matdata, data_label, data_varStruct = NULL, vlist, group_var ){

  if (is.null(data_varStruct)){
    data_varStruct = reactive(list(variable = names(data())))
  }


  output$base <- renderUI({
    tagList(
      selectInput(session$ns("nonnormal_vars"), "Non-normal variable (continuous)",
                  choices = vlist()$conti_list, multiple = T,
                  selected = vlist()$conti_vars[vlist()$non_normal]
      ),
      sliderInput(session$ns("decimal_tb1_con"), "Digits (continuous)",
                  min = 1, max = 3, value = 1
      ),
      sliderInput(session$ns("decimal_tb1_cat"), "Digits (categorical, %)",
                  min = 1, max = 3, value = 1
      ),
      sliderInput(session$ns("decimal_tb1_p"), "Digits (p)",
                  min = 3, max = 5, value = 3
      ),
      checkboxInput(session$ns("smd"), "Show SMD", T)
      )

  })

  output$sub2 <- renderUI({
    tagList(
      selectInput(session$ns("group2_vars"), "2nd group (optional)",
                  choices = c("None", mksetdiff(vlist()$group_list, group_var())), multiple = F,
                  selected = "None"),
      checkboxInput(session$ns("psub"), "Subgroup p-values", F)
    )

  })


  out <- reactive({
    req(!is.null(group_var()))
    vars = setdiff(setdiff(names(data()),vlist()$except_vars),  group_var())
    if (input$group2_vars == "None"){
      vars.tb1 = setdiff(vars, c(group_var(), "pscore", "iptw"))

      vars.fisher = sapply(setdiff(vlist()$factor_vars, group_var()), function(x){is(tryCatch(chisq.test(table(data()[[group_var()]], data()[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      vars.fisher = setdiff(vlist()$factor_vars, group_var())[unlist(vars.fisher)]


      res = jstable::CreateTableOneJS(data = data(),
                                      vars = vars.tb1, strata = group_var(), includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label())

      res.ps = jstable::CreateTableOneJS(data = matdata(),
                                         vars = vars.tb1, strata = group_var(), includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label())
    } else{
      vars.tb1 = setdiff(vars, c(group_var(), input$group2_vars, "pscore", "iptw"))

      vars.fisher = sapply(setdiff(factor_vars, c(group_var(), input$group2_vars)), function(x){is(tryCatch(chisq.test(table(data[[group_var()]], data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      vars.fisher = setdiff(factor_vars, c(group_var(), input$group2_vars))[unlist(vars.fisher)]

      res = jstable::CreateTableOneJS(data = data(),
                                      vars = vars.tb1, strata = group_var(), strata2 = input$group2_vars, includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label(), psub = input$psub)

      res.ps = jstable::CreateTableOneJS(data = matdata(),
                                         vars = vars.tb1, strata = group_var(), strata2 = input$group2_vars, includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = vars.fisher, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label(), psub = input$psub)

    }





    ## iptw
    #labelled::var_label(data) = sapply(names(data()), function(v){as.character(data_label()[get("variable") == v, "var_label"][1])}, simplify = F)
    #vals.tb1 = c(NA, unlist(sapply(vars.tb1, function(v){labeldata[get("variable") == v, "val_label"]})))

    Svydesign <- survey::svydesign(ids = ~ 1, data = data(), weights = ~ iptw)

    res.iptw1 <- tableone::svyCreateTableOne(vars = vars.tb1, strata = group_var(), data = Svydesign, smd = input$smd)
    ptb1 <- print(res.iptw1, nonnormal = input$nonnormal_vars, catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p,
                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd)

    rownames(ptb1) = gsub("(mean (sd))", "", rownames(ptb1), fixed=T)
    colnames(ptb1)[1] = data_label()[get("variable") == group_var(), "var_label"][1]

    #colname.group_var = unlist(data_label()[get("variable") == strata, "val_label"])
    #colnames(ptb1)[1:(length(colname.group_var)+1)] = unlist(c(data_label()[get("variable") == strata, "var_label"][1], colname.group_var))
    #ptb1[,1] = vals.tb1
    sig = ifelse(ptb1[,"p"] == "<0.001", "0", ptb1[,"p"])
    sig = as.numeric(as.vector(sig))
    sig = ifelse(sig <= 0.05, "**", "")
    res.iptw = list(table = cbind(ptb1, sig), caption = paste(res$caption, "- IPTW"))



    return(list(original = res, ps = res.ps, iptw = res.iptw))
    })



  return(out)
}

