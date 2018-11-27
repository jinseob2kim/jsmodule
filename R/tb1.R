
#' @title tb1moduleUI: Shiny module UI for Table 1.
#' @description Shiny module UI for Table 1.
#' @param id id
#' @return Shiny UI
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(shiny);library(data.table);library(DT)
#'  library(jstable);library(shinycustomloader);library(tableone);library(labelled)
#'  data = data.table(mtcars)
#'  data$vs = as.factor(data$vs)
#'  data$am = as.factor(data$am)
#'  data$cyl = as.factor(data$cyl)
#'  data.label = mk.lev(data)
#'
#'  ui <- fluidPage(
#'  sidebarLayout(
#'    sidebarPanel(
#'      tb1moduleUI("tb1")
#'    ),
#'    mainPanel(
#'      withLoader(DTOutput("table1"), type="html", loader="loader6"),
#'    )
#'  )
#'  )
#'
#'  server <- function(input, output, session) {
#'    out_tb1 <- callModule(tb1module, "tb1", data = data,
#'                          data_label = data.label, data_varStruct = NULL)
#'    output$table1 <- renderDT({
#'      tb = out_tb1()$table
#'      cap = out_tb1()$caption
#'      out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
#'                          options = c(opt.tb1("tb1"),
#'                                      list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
#'                                      ),
#'                                      list(scrollX = TRUE)
#'                          )
#'      )
#'      if ("sig" %in% colnames(tb)){
#'        out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
#'      }
#'      return(out.tb1)
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'  }
#' }
#' @rdname tb1moduleUI
#' @export
#' @import shiny

tb1moduleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("base")),
    uiOutput(ns("sub1")),
    uiOutput(ns("sub2"))
  )
}



#' @title tb1module: Shiny module Server for Table 1.
#' @description Shiny module Server for Table 1.
#' @param input input
#' @param output output
#' @param session session
#' @param data data
#' @param data_label data label
#' @param data_varStruct Variable structure list of data, Default: NULL
#' @param nfactor.limit maximum factor levels to include, Default: 10
#' @return Shiny module
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(shiny);library(data.table);library(DT)
#'  library(jstable);library(shinycustomloader);library(tableone);library(labelled)
#'  data = data.table(mtcars)
#'  data$vs = as.factor(data$vs)
#'  data$am = as.factor(data$am)
#'  data$cyl = as.factor(data$cyl)
#'  data.label = mk.lev(data)
#'
#'  ui <- fluidPage(
#'  sidebarLayout(
#'    sidebarPanel(
#'      tb1moduleUI("tb1")
#'    ),
#'    mainPanel(
#'      withLoader(DTOutput("table1"), type="html", loader="loader6"),
#'    )
#'  )
#'  )
#'
#'  server <- function(input, output, session) {
#'    out_tb1 <- callModule(tb1module, "tb1",
#'                          data = data, data_label = data.label, data_varStruct = NULL)
#'    output$table1 <- renderDT({
#'      tb = out_tb1()$table
#'      cap = out_tb1()$caption
#'      out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
#'                          options = c(opt.tb1("tb1"),
#'                                      list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
#'                                      ),
#'                                      list(scrollX = TRUE)
#'                          )
#'      )
#'      if ("sig" %in% colnames(tb)){
#'        out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
#'      }
#'      return(out.tb1)
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'  }
#' }
#' @rdname tb1module
#' @export
#' @import shiny
#' @importFrom data.table fread data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats fisher.test chisq.test shapiro.test oneway.test kruskal.test
#' @importFrom jstable CreateTableOneJS
#' @importFrom methods is


tb1module <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10){

  if (is.null(data_varStruct)){
    data_varStruct = list(variable = names(data))
  }

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

  if (!("data.table" %in% class(data))) {data = data.table(data)}
  if (!("data.table" %in% class(data_label))) {data_label = data.table(data_label)}

  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  factor_list <- mklist(data_varStruct, factor_vars)

  conti_vars <- setdiff(names(data), factor_vars)
  conti_list <- mklist(data_varStruct, conti_vars)

  nclass_factor <- unlist(data[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}), .SDcols = factor_vars])

  group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data)]
  group_list <- mklist(data_varStruct, group_vars)

  except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data)]


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
      selectInput(session$ns("group_vars"), "Stratified by",
                     choices =  c("None", group_list), multiple = F,
                     selected = "None"
      ),
      selectInput(session$ns("nonnormal_vars"), "Non-normal variable (continuous)",
                     choices = conti_list, multiple = T,
                     selected = conti_vars[non_normal]
      ),
      sliderInput(session$ns("decimal_tb1_con"), "Digits (continuous)",
                  min = 1, max = 3, value = 1
      ),
      sliderInput(session$ns("decimal_tb1_cat"), "Digits (categorical, %)",
                  min = 1, max = 3, value = 1
      )
    )
  })



  output$sub1 <- renderUI({
    req(!is.null(input$group_vars))
    if (input$group_vars == "None") return(NULL)
    tagList(
      sliderInput(session$ns("decimal_tb1_p"), "Digits (p)",
                  min = 3, max = 5, value = 3
      ),
      checkboxInput(session$ns("smd"), "Show SMD", F),
      selectInput(session$ns("group2_vars"), "2nd group (optional)",
                  choices = c("None", mksetdiff(group_list, input$group_vars)), multiple = F,
                  selected = "None"
      )
    )
  })



  output$sub2 <- renderUI({
    req(!is.null(input$group_vars), req(!is.null(input$group2_vars)))
    if ((input$group_vars == 'None') | (input$group2_vars == 'None') | (input$group2_vars == input$group_vars)) return(NULL)
    tagList(
      checkboxInput(session$ns("psub"), "Subgroup p-values", F)
    )

  })

  labelled::var_label(data) = sapply(names(data), function(v){data_label[variable == v, var_label][1]}, simplify = F)

  out <- reactive({
    req(!is.null(input$group_vars))
    vars = setdiff(setdiff(names(data),except_vars),  input$group_vars)

    if (input$group_vars == "None"){
      res = jstable::CreateTableOneJS(data = data,
                             vars = vars, includeNA = F, test = T,
                             testApprox = chisq.test, argsApprox = list(correct = TRUE),
                             testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                             testNormal = oneway.test, argsNormal = list(var.equal = F),
                             testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                             showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                             catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, labeldata = data_label)

      return(res)
    } else if(is.null(input$group2_vars)) {
      return(NULL)
    } else if ((input$group2_vars == "None") | (input$group2_vars == input$group_vars)){
      vars.tb1 = setdiff(vars, input$group_vars)

      #vars.fisher = sapply(setdiff(factor_vars, input$group_vars), function(x){is(tryCatch(chisq.test(table(data[[input$group_vars]], data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      #vars.fisher = setdiff(factor_vars, input$group_vars)[vars.fisher]

      res = jstable::CreateTableOneJS(data = data,
                             vars = vars.tb1, strata = input$group_vars, includeNA = F, test = T,
                             testApprox = chisq.test, argsApprox = list(correct = TRUE),
                             testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                             testNormal = oneway.test, argsNormal = list(var.equal = F),
                             testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                             showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                             catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label)

      return(res)
    } else {
      vars.tb1 = setdiff(vars, c(input$group2_vars, input$group_vars))
      #vars.group = paste(data[[input$group_vars]], data[[input$group2_vars]], sep= ":")

     # vars.fisher = sapply(setdiff(factor_vars, c(input$group2_vars, input$group_vars)), function(x){is(tryCatch(chisq.test(table(vars.group, data[[x]])),error=function(e) e, warning=function(w) w), "warning")})
     # vars.fisher = setdiff(factor_vars, c(input$group2_vars, input$group_vars))[unlist(vars.fisher)]

      res = jstable::CreateTableOneJS(data = data,
                             vars = vars.tb1, strata = input$group_vars, strata2 = input$group2_vars, includeNA = F, test = T,
                             testApprox = chisq.test, argsApprox = list(correct = TRUE),
                             testExact = fisher.test, argsExact = list(workspace = 2 * 10^5, hybrid = T),
                             testNormal = oneway.test, argsNormal = list(var.equal = F),
                             testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                             showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                             catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label, psub = input$psub)

      return(res)
    }

  })

  return(out)


}



#' @title tb1module2: Shiny module Server for Table 1 using reactive data.
#' @description Shiny module Server for Table 1 using reactive data.
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactive data label(reactive)
#' @param data_varStruct Variable structure list of data, Default: NULL
#' @param nfactor.limit maximum factor levels to include, Default: 10
#' @return Shiny module
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(shiny);library(data.table);library(DT)
#'  library(jstable);library(shinycustomloader);library(tableone);library(labelled)
#'
#'  ui <- navbarPage("Basic statistics",
#'  tabPanel("Data",
#'           sidebarLayout(
#'             sidebarPanel(
#'               csvFileInput("datafile")
#'             ),
#'             mainPanel(
#'               tabsetPanel(type = "pills",
#'                           tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
#'                           tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
#'               )
#'             )
#'           )
#'  ),
#'  tabPanel("Table 1",
#'           sidebarLayout(
#'             sidebarPanel(
#'               tb1moduleUI("tb1")
#'             ),
#'             mainPanel(
#'               withLoader(DTOutput("table1"), type="html", loader="loader6"),
#'               wellPanel(
#'                 h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
#'                 h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
#'                 h5("Categorical variables  are summarized with table")
#'               )
#'             )
#'           )
#'
#'  )
#'  )
#'
#'  server <- function(input, output, session) {
#'    out_tb1 <- callModule(tb1module2, "tb1",
#'                          data = data, data_label = data.label, data_varStruct = NULL)
#'    output$table1 <- renderDT({
#'      tb = out_tb1()$table
#'      cap = out_tb1()$caption
#'      out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
#'                          options = c(opt.tb1("tb1"),
#'                                      list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
#'                                      ),
#'                                      list(scrollX = TRUE)
#'                          )
#'      )
#'      if ("sig" %in% colnames(tb)){
#'        out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
#'      }
#'      return(out.tb1)
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'  }
#' }
#' @rdname tb1module2
#' @export
#' @import shiny
#' @importFrom data.table fread data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats fisher.test chisq.test shapiro.test oneway.test kruskal.test
#' @importFrom jstable CreateTableOneJS
#' @importFrom methods is


tb1module2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10){


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

    nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}), .SDcols = factor_vars])
    #nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})

    group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
    group_list <- mklist(data_varStruct(), group_vars)

    except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]

    ## non-normal: shapiro test
    f <- function(x) {
      if (diff(range(x, na.rm = T)) == 0) return(F) else return(shapiro.test(x)$p.value <= 0.05)
    }

    non_normal <- ifelse(nrow(data) <=3 | nrow(data) >= 5000,
                         rep(F, length(conti_vars)),
                         sapply(conti_vars, function(x){f(data()[[x]])})
    )

    return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list,
                group_vars = group_vars, group_list = group_list, except_vars = except_vars, non_normal = non_normal
    ))
  })


  output$base <- renderUI({
    tagList(
      selectInput(session$ns("group_vars"), "Stratified by",
                  choices =  c("None", vlist()$group_list), multiple = F,
                  selected = "None"
      ),
      selectInput(session$ns("nonnormal_vars"), "Non-normal variable (continuous)",
                  choices = vlist()$conti_list, multiple = T,
                  selected = vlist()$conti_vars[vlist()$non_normal]
      ),
      sliderInput(session$ns("decimal_tb1_con"), "Digits (continuous)",
                  min = 1, max = 3, value = 1
      ),
      sliderInput(session$ns("decimal_tb1_cat"), "Digits (categorical, %)",
                  min = 1, max = 3, value = 1
      )
    )
  })



  output$sub1 <- renderUI({
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

    req(!is.null(input$group_vars))
    if (input$group_vars == "None") return(NULL)


    tagList(
      sliderInput(session$ns("decimal_tb1_p"), "Digits (p)",
                  min = 3, max = 5, value = 3
      ),
      checkboxInput(session$ns("smd"), "Show SMD", F),
      selectInput(session$ns("group2_vars"), "2nd group (optional)",
                  choices = c("None", mksetdiff(vlist()$group_list, input$group_vars)), multiple = F,
                  selected = "None"
      )
    )
  })



  output$sub2 <- renderUI({
    req(!is.null(input$group_vars), req(!is.null(input$group2_vars)))
    if ((input$group_vars == 'None') | (input$group2_vars == 'None') | (input$group2_vars == input$group_vars)) return(NULL)
    tagList(
      checkboxInput(session$ns("psub"), "Subgroup p-values", F)
    )

  })

  #observeEvent(data_label(), {
  #  var_label(data()) <- sapply(names(data()), function(v){data_label()[variable == v, var_label][1]}, simplify = F)
  #})




  out <- reactive({
    req(!is.null(input$group_vars))
    vars = setdiff(setdiff(names(data()),vlist()$except_vars),  input$group_vars)

    if (input$group_vars == "None"){
      res = jstable::CreateTableOneJS(data = data(),
                             vars = vars, includeNA = F, test = T,
                             testApprox = chisq.test, argsApprox = list(correct = TRUE),
                             testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                             testNormal = oneway.test, argsNormal = list(var.equal = F),
                             testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                             showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                             catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, labeldata = data_label())

      return(res)
    } else if(is.null(input$group2_vars)) {
      return(NULL)
      } else if ((input$group2_vars == "None") | (input$group2_vars == input$group_vars)){
      vars.tb1 = setdiff(vars, input$group_vars)

      #vars.fisher = sapply(setdiff(vlist()$factor_vars, input$group_vars), function(x){is(tryCatch(chisq.test(table(data()[[input$group_vars]], data()[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      #vars.fisher = setdiff(vlist()$factor_vars, input$group_vars)[unlist(vars.fisher)]


      res = jstable::CreateTableOneJS(data = data(),
                             vars = vars.tb1, strata = input$group_vars, includeNA = F, test = T,
                             testApprox = chisq.test, argsApprox = list(correct = TRUE),
                             testExact = fisher.test, argsExact = list(workspace = 2 * 10^5),
                             testNormal = oneway.test, argsNormal = list(var.equal = F),
                             testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                             showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                             catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label())

      return(res)
    } else {
      vars.tb1 = setdiff(vars, c(input$group2_vars, input$group_vars))
      #vars.group = paste(data()[[input$group_vars]], data()[[input$group2_vars]], sep= ":")

      #vars.fisher = sapply(setdiff(vlist()$factor_vars, c(input$group2_vars, input$group_vars)), function(x){is(tryCatch(chisq.test(table(vars.group, data()[[x]])),error=function(e) e, warning=function(w) w), "warning")})
      #vars.fisher = setdiff(vlist()$factor_vars, c(input$group2_vars, input$group_vars))[vars.fisher]

      res = jstable::CreateTableOneJS(data = data(),
                             vars = vars.tb1, strata = input$group_vars, strata2 = input$group2_vars, includeNA = F, test = T,
                             testApprox = chisq.test, argsApprox = list(correct = TRUE),
                             testExact = fisher.test, argsExact = list(workspace = 2 * 10^5, hybrid = T),
                             testNormal = oneway.test, argsNormal = list(var.equal = F),
                             testNonNormal = kruskal.test, argsNonNormal = list(NULL),
                             showAllLevels = T, printToggle = F, quote = F, smd = input$smd, Labels = T, exact = NULL, nonnormal = input$nonnormal_vars,
                             catDigits = input$decimal_tb1_cat, contDigits = input$decimal_tb1_con, pDigits = input$decimal_tb1_p, labeldata = data_label(), psub = input$psub)

      return(res)
    }

  })

  return(out)


}

