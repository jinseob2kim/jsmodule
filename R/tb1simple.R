
#' @title tb1simpleUI : tb1 module UI for propensity score analysis
#' @description tb1 module UI for propensity score analysis
#' @param id id
#' @return UI
#' @details tb1 module UI for propensity score analysis
#' @examples
#' library(shiny);library(DT);library(data.table);library(readxl);library(jstable)
#' library(haven);library(survey)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      FilePsInput("datafile"),
#'      tb1simpleUI("tb1")
#'    ),
#'    mainPanel(
#'      DTOutput("table1_original"),
#'      DTOutput("table1_ps"),
#'      DTOutput("table1_iptw")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   mat.info <- callModule(FilePs, "datafile")
#'
#'   data <- reactive(mat.info()$data)
#'   matdata <- reactive(mat.info()$matdata)
#'   data.label <- reactive(mat.info()$data.label)
#'
#'
#'   vlist <- eventReactive(mat.info(), {
#'     mklist <- function(varlist, vars){
#'       lapply(varlist,
#'              function(x){
#'                inter <- intersect(x, vars)
#'                if (length(inter) == 1){
#'                  inter <- c(inter, "")
#'                }
#'                return(inter)
#'              })
#'     }
#'     factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
#'     factor_list <- mklist(data_varStruct(), factor_vars)
#'     conti_vars <- setdiff(names(data()), c(factor_vars, "pscore", "iptw"))
#'     conti_list <- mklist(data_varStruct(), conti_vars)
#'     nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}),
#'                                    .SDcols = factor_vars])
#'     class01_factor <- unlist(data()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}),
#'                                     .SDcols = factor_vars])
#'     validate(
#'       need(!is.null(class01_factor), "No categorical variables coded as 0, 1 in data")
#'    )
#'     factor_01vars <- factor_vars[class01_factor]
#'     factor_01_list <- mklist(data_varStruct(), factor_01vars)
#'     group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10 & nclass_factor < nrow(data())]
#'     group_list <- mklist(data_varStruct(), group_vars)
#'     except_vars <- factor_vars[nclass_factor>10 | nclass_factor==1 | nclass_factor==nrow(data())]
#'
#'     ## non-normal: shapiro test
#'       f <- function(x) {
#'         if (diff(range(x, na.rm = T)) == 0) return(F) else return(shapiro.test(x)$p.value <= 0.05)
#'       }
#'
#'       non_normal <- ifelse(nrow(data()) <=3 | nrow(data()) >= 5000,
#'                            rep(F, length(conti_vars)),
#'                            sapply(conti_vars, function(x){f(data()[[x]])})
#'       )
#'       return(list(factor_vars = factor_vars, factor_list = factor_list,
#'                   conti_vars = conti_vars, conti_list = conti_list, factor_01vars = factor_01vars,
#'                   factor_01_list = factor_01_list, group_list = group_list,
#'                   except_vars = except_vars, non_normal = non_normal)
#'       )
#'
#'     })
#'
#'   out.tb1 <- callModule(tb1simple2, "tb1", data = data, matdata = matdata, data_label = data.label,
#'                         data_varStruct = NULL, vlist = vlist,
#'                         group_var = reactive(mat.info()$group_var))
#'
#'   output$table1_original <- renderDT({
#'     tb <- out.tb1()$original$table
#'     cap <- out.tb1()$original$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'
#'   output$table1_ps <- renderDT({
#'     tb <- out.tb1()$ps$table
#'     cap <- out.tb1()$ps$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'
#'   output$table1_iptw <- renderDT({
#'     tb <- out.tb1()$iptw$table
#'     cap <- out.tb1()$iptw$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'}
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
#' @return tb1 module for propensity score analysis
#' @details tb1 module for propensity score analysis
#' @examples
#' library(shiny);library(DT);library(data.table);library(readxl);library(jstable)
#' library(haven);library(survey)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      FilePsInput("datafile"),
#'      tb1simpleUI("tb1")
#'    ),
#'    mainPanel(
#'      DTOutput("table1_original"),
#'      DTOutput("table1_ps"),
#'      DTOutput("table1_iptw")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   mat.info <- callModule(FilePs, "datafile")
#'
#'   data <- reactive(mat.info()$data)
#'   matdata <- reactive(mat.info()$matdata)
#'   data.label <- reactive(mat.info()$data.label)
#'
#'
#'   vlist <- eventReactive(mat.info(), {
#'     mklist <- function(varlist, vars){
#'       lapply(varlist,
#'              function(x){
#'                inter <- intersect(x, vars)
#'                if (length(inter) == 1){
#'                  inter <- c(inter, "")
#'                }
#'                return(inter)
#'              })
#'     }
#'     factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
#'     factor_list <- mklist(data_varStruct(), factor_vars)
#'     conti_vars <- setdiff(names(data()), c(factor_vars, "pscore", "iptw"))
#'     conti_list <- mklist(data_varStruct(), conti_vars)
#'     nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}),
#'                                    .SDcols = factor_vars])
#'     class01_factor <- unlist(data()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}),
#'                                     .SDcols = factor_vars])
#'     validate(
#'       need(!is.null(class01_factor), "No categorical variables coded as 0, 1 in data")
#'    )
#'     factor_01vars <- factor_vars[class01_factor]
#'     factor_01_list <- mklist(data_varStruct(), factor_01vars)
#'     group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10 & nclass_factor < nrow(data())]
#'     group_list <- mklist(data_varStruct(), group_vars)
#'     except_vars <- factor_vars[nclass_factor>10 | nclass_factor==1 | nclass_factor==nrow(data())]
#'
#'     ## non-normal: shapiro test
#'       f <- function(x) {
#'         if (diff(range(x, na.rm = T)) == 0) return(F) else return(shapiro.test(x)$p.value <= 0.05)
#'       }
#'
#'       non_normal <- ifelse(nrow(data()) <=3 | nrow(data()) >= 5000,
#'                            rep(F, length(conti_vars)),
#'                            sapply(conti_vars, function(x){f(data()[[x]])})
#'       )
#'       return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars,
#'                   conti_list = conti_list, factor_01vars = factor_01vars,
#'                   factor_01_list = factor_01_list, group_list = group_list,
#'                   except_vars = except_vars, non_normal = non_normal)
#'       )
#'
#'     })
#'
#'   out.tb1 <- callModule(tb1simple2, "tb1", data = data, matdata = matdata, data_label = data.label,
#'                         data_varStruct = NULL, vlist = vlist,
#'                         group_var = reactive(mat.info()$group_var))
#'
#'   output$table1_original <- renderDT({
#'     tb <- out.tb1()$original$table
#'     cap <- out.tb1()$original$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'
#'   output$table1_ps <- renderDT({
#'     tb <- out.tb1()$ps$table
#'     cap <- out.tb1()$ps$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'
#'   output$table1_iptw <- renderDT({
#'     tb <- out.tb1()$iptw$table
#'     cap <- out.tb1()$iptw$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'}
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

  ## To remove NOTE.
  variable <- NULL

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
      checkboxInput(session$ns("smd"), "Show SMD", T),
      selectInput(session$ns("group2_vars"), "Stratified by (optional)",
                  choices = c("None", mksetdiff(group_list, group_var)), multiple = F,
                  selected = "None")
    )

  })




  output$sub2 <- renderUI({
    req(!is.null(input$group2_vars))
    if (input$group2_vars == 'None') return(NULL)
    tagList(
      checkboxInput(session$ns("psub"), "Subgroup p-values", T)
    )

  })



  labelled::var_label(data) = sapply(names(data), function(v){data_label[variable == v, var_label][1]}, simplify = F)

  out <- reactive({
    vars <- setdiff(setdiff(names(data),except_vars),  group_var)
    Svydesign <- survey::svydesign(ids = ~ 1, data = data, weights = ~ iptw)

    if (input$group2_vars == "None"){
      vars.tb1 = setdiff(vars, c(group_var, "pscore", "iptw"))

      #vars.fisher = sapply(setdiff(factor_vars, group_var), function(x){is(tryCatch(chisq.test(table(data[[group_var]], data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      #vars.fisher = setdiff(factor_vars, group_var)[unlist(vars.fisher)]

      res = jstable::CreateTableOneJS(data = data,
                                      vars = vars.tb1, strata = group_var, includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)

      res.ps = jstable::CreateTableOneJS(data = matdata,
                                         vars = vars.tb1, strata = group_var, includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)

      res.iptw <- jstable::svyCreateTableOneJS(data = Svydesign, vars = vars.tb1, strata = group_var, includeNA = F, test = T,
                                               showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, nonnormal = input$nonnormal_vars,
                                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)


    } else{
      vars.tb1 = setdiff(vars, c(group_var, input$group2_vars, "pscore", "iptw"))

      #vars.fisher = sapply(setdiff(factor_vars, c(group_var, input$group2_vars)), function(x){is(tryCatch(chisq.test(table(data[[group_var]], data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      #vars.fisher = setdiff(factor_vars, c(group_var, input$group2_vars))[unlist(vars.fisher)]

      res = jstable::CreateTableOneJS(data = data,
                                      vars = vars.tb1, strata = input$group2_vars, strata2 = group_var, includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label, psub = input$psub)

      res.ps = jstable::CreateTableOneJS(data = matdata,
                                         vars = vars.tb1, strata = input$group2_vars, strata2 = group_var, includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label, psub = input$psub)

      res.iptw <- jstable::svyCreateTableOneJS(data = Svydesign, vars = vars.tb1, strata = input$group2_vars, strata2 = group_var, includeNA = F, test = T,
                                               showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, nonnormal = input$nonnormal_vars,
                                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label, psub = input$psub)


    }

    #Svydesign <- survey::svydesign(ids = ~ 1, data = data, weights = ~ iptw)

    #res.iptw1 <- tableone::svyCreateTableOne(vars = vars.tb1, strata = group_var, data = Svydesign, smd = input$smd)
    #ptb1 <- print(res.iptw1, nonnormal = input$nonnormal_vars, catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p,
    #              showAllLevels = T, printToggle = F, quote = F, smd = input$smd)

    #rownames(ptb1) = gsub("(mean (sd))", "", rownames(ptb1), fixed=T)
    #colnames(ptb1)[1] = data_label()[get("variable") == group_var(), "var_label"][1]

    #colname.group_var = unlist(data_label()[get("variable") == strata, "val_label"])
    #colnames(ptb1)[1:(length(colname.group_var)+1)] = unlist(c(data_label()[get("variable") == strata, "var_label"][1], colname.group_var))
    #ptb1[,1] = vals.tb1
    #sig = ifelse(ptb1[,"p"] == "<0.001", "0", ptb1[,"p"])
    #sig = as.numeric(as.vector(sig))
    #sig = ifelse(sig <= 0.05, "**", "")
    #res.iptw = list(table = cbind(ptb1, sig), caption = paste(res$caption, "- IPTW"))

    return(list(original = res, ps = res.ps, iptw = res.iptw))

    })
  return(out)

  }






#' @title tb1simple2: tb1 module for propensity score analysis for reactive data
#' @description tb1 module for propensity score analysis for reactive data
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param matdata reactive matdata
#' @param data_label reactive data_label
#' @param data_varStruct data_varStruct, Default: NULL
#' @param vlist variable lists
#' @param group_var reactive group_var
#' @return tb1 module for propensity score analysis for reactive data
#' @details tb1 module for propensity score analysis for reactive data
#' @examples
#' library(shiny);library(DT);library(data.table);library(readxl);library(jstable)
#' library(haven);library(survey)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      FilePsInput("datafile"),
#'      tb1simpleUI("tb1")
#'    ),
#'    mainPanel(
#'      DTOutput("table1_original"),
#'      DTOutput("table1_ps"),
#'      DTOutput("table1_iptw")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   mat.info <- callModule(FilePs, "datafile")
#'
#'   data <- reactive(mat.info()$data)
#'   matdata <- reactive(mat.info()$matdata)
#'   data.label <- reactive(mat.info()$data.label)
#'
#'
#'   vlist <- eventReactive(mat.info(), {
#'     mklist <- function(varlist, vars){
#'       lapply(varlist,
#'              function(x){
#'                inter <- intersect(x, vars)
#'                if (length(inter) == 1){
#'                  inter <- c(inter, "")
#'                }
#'                return(inter)
#'              })
#'     }
#'     factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
#'     factor_list <- mklist(data_varStruct(), factor_vars)
#'     conti_vars <- setdiff(names(data()), c(factor_vars, "pscore", "iptw"))
#'     conti_list <- mklist(data_varStruct(), conti_vars)
#'     nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}),
#'                                    .SDcols = factor_vars])
#'     class01_factor <- unlist(data()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}),
#'                                     .SDcols = factor_vars])
#'     validate(
#'       need(!is.null(class01_factor), "No categorical variables coded as 0, 1 in data")
#'    )
#'     factor_01vars <- factor_vars[class01_factor]
#'     factor_01_list <- mklist(data_varStruct(), factor_01vars)
#'     group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10 & nclass_factor < nrow(data())]
#'     group_list <- mklist(data_varStruct(), group_vars)
#'     except_vars <- factor_vars[nclass_factor>10 | nclass_factor==1 | nclass_factor==nrow(data())]
#'
#'     ## non-normal: shapiro test
#'       f <- function(x) {
#'         if (diff(range(x, na.rm = T)) == 0) return(F) else return(shapiro.test(x)$p.value <= 0.05)
#'       }
#'
#'       non_normal <- ifelse(nrow(data()) <=3 | nrow(data()) >= 5000,
#'                            rep(F, length(conti_vars)),
#'                            sapply(conti_vars, function(x){f(data()[[x]])})
#'       )
#'       return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars,
#'                   conti_list = conti_list, factor_01vars = factor_01vars,
#'                   factor_01_list = factor_01_list, group_list = group_list,
#'                   except_vars = except_vars, non_normal = non_normal)
#'       )
#'
#'     })
#'
#'   out.tb1 <- callModule(tb1simple2, "tb1", data = data, matdata = matdata, data_label = data.label,
#'                         data_varStruct = NULL, vlist = vlist,
#'                         group_var = reactive(mat.info()$group_var))
#'
#'   output$table1_original <- renderDT({
#'     tb <- out.tb1()$original$table
#'     cap <- out.tb1()$original$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'
#'   output$table1_ps <- renderDT({
#'     tb <- out.tb1()$ps$table
#'     cap <- out.tb1()$ps$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'
#'   output$table1_iptw <- renderDT({
#'     tb <- out.tb1()$iptw$table
#'     cap <- out.tb1()$iptw$caption
#'     out <- datatable(tb, rownames = T, extension= "Buttons", caption = cap)
#'     return(out)
#'   })
#'}
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
      ,
      selectInput(session$ns("group2_vars"), "Stratified by (optional)",
                  choices = c("None", mksetdiff(vlist()$group_list, group_var())), multiple = F,
                  selected = "None")
      )


 })

  output$sub2 <- renderUI({
    req(!is.null(input$group2_vars))
    if (input$group2_vars == 'None') return(NULL)
    tagList(
      checkboxInput(session$ns("psub"), "Subgroup p-values", T)
    )

  })


  out <- reactive({
    req(!is.null(group_var()))
    vars = setdiff(setdiff(names(data()),vlist()$except_vars),  group_var())
    Svydesign <- survey::svydesign(ids = ~ 1, data = data(), weights = ~ iptw)
    if (input$group2_vars == "None"){
      vars.tb1 = setdiff(vars, c(group_var(), "pscore", "iptw"))

      #vars.fisher = sapply(setdiff(vlist()$factor_vars, group_var()), function(x){is(tryCatch(chisq.test(table(data()[[group_var()]], data()[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      #vars.fisher = setdiff(vlist()$factor_vars, group_var())[unlist(vars.fisher)]


      res = jstable::CreateTableOneJS(data = data(),
                                      vars = vars.tb1, strata = group_var(), includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label())

      res.ps = jstable::CreateTableOneJS(data = matdata(),
                                         vars = vars.tb1, strata = group_var(), includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label())

      res.iptw <- jstable::svyCreateTableOneJS(data = Svydesign, vars = vars.tb1, strata = group_var(), includeNA = F, test = T,
                                               showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, nonnormal = input$nonnormal_vars,
                                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label())

    } else{
      vars.tb1 = setdiff(vars, c(group_var(), input$group2_vars, "pscore", "iptw"))

      #vars.fisher = sapply(setdiff(factor_vars, c(group_var(), input$group2_vars)), function(x){is(tryCatch(chisq.test(table(data[[group_var()]], data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      #vars.fisher = setdiff(factor_vars, c(group_var(), input$group2_vars))[unlist(vars.fisher)]

      res = jstable::CreateTableOneJS(data = data(),
                                      vars = vars.tb1, strata = input$group2_vars, strata2 = group_var(), includeNA = F, test = T,
                                      testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                      testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                      testNormal = oneway.test, argsNormal = list(var.equal = F),
                                      testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                      showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                      catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label(), psub = input$psub)

      res.ps = jstable::CreateTableOneJS(data = matdata(),
                                         vars = vars.tb1, strata = input$group2_vars, strata2 = group_var(), includeNA = F, test = T,
                                         testApprox = chisq.test, argsApprox = list(correct = TRUE),
                                         testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                                         testNormal = oneway.test, argsNormal = list(var.equal = F),
                                         testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                                         showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                                         catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label(), psub = input$psub)

      res.iptw <- jstable::svyCreateTableOneJS(data = Svydesign, vars = vars.tb1, strata = input$group2_vars, strata2 = group_var(), includeNA = F, test = T,
                                               showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, nonnormal = input$nonnormal_vars,
                                               catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label(), psub = input$psub)


    }





    ## iptw
    #labelled::var_label(data) = sapply(names(data()), function(v){as.character(data_label()[get("variable") == v, "var_label"][1])}, simplify = F)
    #vals.tb1 = c(NA, unlist(sapply(vars.tb1, function(v){labeldata[get("variable") == v, "val_label"]})))

    #Svydesign <- survey::svydesign(ids = ~ 1, data = data(), weights = ~ iptw)

    #res.iptw1 <- tableone::svyCreateTableOne(vars = vars.tb1, strata = group_var(), data = Svydesign, smd = input$smd)
    #ptb1 <- print(res.iptw1, nonnormal = input$nonnormal_vars, catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p,
    #                  showAllLevels = T, printToggle = F, quote = F, smd = input$smd)

    #rownames(ptb1) = gsub("(mean (sd))", "", rownames(ptb1), fixed=T)
    #colnames(ptb1)[1] = data_label()[get("variable") == group_var(), "var_label"][1]

    #colname.group_var = unlist(data_label()[get("variable") == strata, "val_label"])
    #colnames(ptb1)[1:(length(colname.group_var)+1)] = unlist(c(data_label()[get("variable") == strata, "var_label"][1], colname.group_var))
    #ptb1[,1] = vals.tb1
    #sig = ifelse(ptb1[,"p"] == "<0.001", "0", ptb1[,"p"])
    #sig = as.numeric(as.vector(sig))
    #sig = ifelse(sig <= 0.05, "**", "")
    #res.iptw = list(table = cbind(ptb1, sig), caption = paste(res$caption, "- IPTW"))



    return(list(original = res, ps = res.ps, iptw = res.iptw))
    })



  return(out)
}

