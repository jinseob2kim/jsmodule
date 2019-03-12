#' @title kaplanUI: shiny module UI for kaplan-meier plot
#' @description Shiny module UI for kaplan-meier plot
#' @param id id
#' @return Shiny module UI for kaplan-meier plot
#' @details Shiny module UI for kaplan-meier plot
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable);library(ggplot2)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      kaplanUI("kaplan")
#'    ),
#'    mainPanel(
#'      plotOutput("kaplan_plot"),
#'      ggplotdownUI("kaplan")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label,
#'                            data_varStruct = NULL)
#'
#'   output$kaplan_plot <- renderPlot({
#'     print(out_kaplan())
#'   })
#'}
#' @rdname kaplanUI
#' @export

kaplanUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("eventtime")),
    uiOutput(ns("indep")),
    uiOutput(ns("cutconti")),
    checkboxInput(ns("cumhaz"), "Show cumulative hazard", F),
    checkboxInput(ns("pval"), "Show p-value(log-rank test)", T),
    checkboxInput(ns("table"), "Show table", T),
    checkboxInput(ns("subcheck"), "Sub-group analysis"),
    uiOutput(ns("ranges")),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval"))
  )
}



#' @title ggplotdownUI: Option & download module UI for ggplot
#' @description Option & download module UI for ggplot
#' @param id id
#' @return Option & download module UI for ggplot
#' @details Option & download module UI for ggplot
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable);library(ggplot2)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      kaplanUI("kaplan")
#'    ),
#'    mainPanel(
#'      plotOutput("kaplan_plot"),
#'      ggplotdownUI("kaplan")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label,
#'                            data_varStruct = NULL)
#'
#'   output$kaplan_plot <- renderPlot({
#'     print(out_kaplan())
#'   })
#'}
#' @rdname ggplotdownUI
#' @export

ggplotdownUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    h3("Download options"),
    wellPanel(
      uiOutput(ns("downloadControls")),
      downloadButton(ns("downloadButton"), label = "Download the plot")
    )
  )
}




#' @title kaplanModule: shiny module server for kaplan-meier plot.
#' @description Shiny module server for kaplan-meier plot.
#' @param input input
#' @param output output
#' @param session session
#' @param data Reactive data
#' @param data_label Reactuve data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey Reactive survey data. default: NULL
#' @param id.cluster Reactive cluster variable if marginal model, Default: NULL
#' @param timeby timeby, Default: NULL
#' @param range.x range of x axis, Default: NULL
#' @param range.y range of y axis, Default: NULL
#' @param pval.coord pval.coord, Default: NULL
#' @return Shiny module server for kaplan-meier plot.
#' @details Shiny module server for kaplan-meier plot.
#' @examples
#' library(shiny);library(DT);library(data.table);library(jstable);library(ggplot2)
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      kaplanUI("kaplan")
#'    ),
#'    mainPanel(
#'      plotOutput("kaplan_plot"),
#'      ggplotdownUI("kaplan")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label,
#'                            data_varStruct = NULL)
#'
#'   output$kaplan_plot <- renderPlot({
#'     print(out_kaplan())
#'   })
#'}
#' @rdname kaplanModule
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame na.omit
#' @importFrom epiDisplay regress.display
#' @importFrom jstable LabelepiDisplay
#' @importFrom purrr map_lgl


kaplanModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL,
                         timeby = NULL, range.x = NULL, range.y = NULL, pval.coord = NULL) {

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
      need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
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
      selectInput(session$ns("event_km"), "Event",
                  choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = F,
                  selected = NULL
      ),
      selectInput(session$ns("time_km"), "Time",
                  choices = vlist()$conti_list, multiple = F,
                  selected = NULL
      )
    )
  })

  output$indep <- renderUI({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
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


    if (!is.null(design.survey)){
      indep.km <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, input$time_km, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
    } else if (!is.null(id.cluster)){
      indep.km <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, input$time_km, id.cluster()))
    } else{
      indep.km <- setdiff(names(data()), c(vlist()$except_vars, input$event_km, input$time_km ))
    }


    tagList(
      selectInput(session$ns("indep_km"), "Independent variables",
                  choices = c("None", mklist(data_varStruct(), indep.km)), multiple = F,
                  selected = "None"
      )
    )
  })

  observeEvent(input$indep_km,{
    output$cutconti = renderUI({
      if (input$indep_km %in% c("None", vlist()$factor_vars)){
        return(NULL)
      } else if (!is.null(design.survey) | !is.null(id.cluster)){
        return(NULL)
      } else{
        req(!is.null(input$event_km))
        req(!is.null(input$time_km))
        data.km <- data()
        data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
        mstat <- maxstat::maxstat.test(as.formula(paste("survival::Surv(",input$time_km,",", input$event_km,") ~ ", input$indep_km, sep="")), data= data.km, smethod="LogRank", pmethod="condMC", B=999)
        cut5 <- mstat$cuts[order(-mstat$stats)][1:5]
        tagList(
          selectizeInput(session$ns("cut5"), "Best cut (Top 5)",
                         choices = cut5, multiple = F,
                         selected = cut5[1]
          )
        )
      }
    })


  })




  observeEvent(input$subcheck, {
    output$subvar <- renderUI({
      req(input$subcheck == T)

      var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$time_km, input$event_km, input$indep_km))
      if (!is.null(id.cluster)){
        var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$time_km, input$event_km, input$indep_km, id.cluster()))
      } else if (!is.null(design.survey)){
        var_subgroup <- setdiff(names(data()), union(c(names(design.survey()$strata), names(design.survey()$cluster), names(design.survey()$allprob)), c(vlist()$except_vars, input$time_km, input$event_km, input$indep_km)))
      }

      var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
      validate(
        need(length(var_subgroup) > 0 , "No variables for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_km"), "Sub-group variable",
                    choices = var_subgroup_list, multiple = F,
                    selected = var_subgroup[1])
      )


    })

  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(input$subvar_km)

    if (input$subvar_km %in% vlist()$factor_vars){
      selectInput(session$ns("subval_km"), "Sub-group value",
                  choices = data_label()[variable == input$subvar_km, level], multiple = T,
                  selected = data_label()[variable == input$subvar_km, level][1])
    } else{
      val <- stats::quantile(data()[[input$subvar_km]], na.rm = T)
      sliderInput(session$ns("subval_km"), "Sub-group range",
                  min = val[1], max = val[5],
                  value = c(val[2], val[4]))
    }

  })





  form.km <- reactive({
    validate(
      need(!is.null(input$indep_km), "Please select at least 1 independent variable.")
    )
    if (input$indep_km == "None"){
      return(as.formula(paste("survival::Surv(",input$time_km,",", input$event_km,") ~ ", "1", sep="")))
    } else if (input$indep_km %in% vlist()$factor_vars) {
      return(as.formula(paste("survival::Surv(",input$time_km,",", input$event_km,") ~ ", input$indep_km, sep="")))
    } else{
      return(as.formula(paste("survival::Surv(",input$time_km,",", input$event_km,") ~ ", "xcat", sep="")))
    }
  })


  kmList <- reactive({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
    req(input$indep_km)
    data.km <- data()
    label.regress <- data_label()
    data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
    if(input$subcheck == T){
      req(input$subvar_km)
      req(input$subval_km)
      if (input$subvar_km %in% vlist()$factor_vars){
        data.km <- data.km[get(input$subvar_km) %in% input$subval_km]
      } else{
        data.km <- data.km[get(input$subvar_km) >= input$subval_km[1] & get(input$subvar_km) <= input$subval_km[2]]
      }
      data.km[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
      label.regress2 <- mk.lev(data.km)[, c("variable", "class", "level")]
      data.table::setkey(data_label(), "variable", "class", "level")
      data.table::setkey(label.regress2, "variable", "class", "level")
      label.regress <- data_label()[label.regress2]
      data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))

    }

    if (input$indep_km %in% vlist()$conti_vars){
      data.km$xcat <- ifelse(data.km[[input$indep_km]] > input$cut5, 1, 0)
    }

    mf <- model.frame(form.km(), data.km)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness."))
    )

    if (is.null(design.survey)){
      cc <- substitute(survival::survfit(.form, data= data.km), list(.form= form.km()))
      res.km <- eval(cc)
      if (input$indep_km == "None"){
        yst.name <- ""
        yst.lab <- "All"
      } else if (input$indep_km %in% vlist()$factor_vars){
        yst.name <- label.regress[variable == input$indep_km, var_label][1]
        yst.lab <- label.regress[variable == input$indep_km, val_label]
      } else{
        yst.name <- paste(label.regress[variable == input$indep_km, var_label], "group")
        yst.lab <- paste(label.regress[variable == input$indep_km, var_label], paste(c(">", "\u2264"), input$cut5, sep=""))
      }
      ylab <- ifelse(input$cumhaz, "Cumulative hazard", "Survival")
      return(list(res = res.km, ylab = ylab, yst.name = yst.name, yst.lab = yst.lab, data = data.km))

    } else{
      data.design <- design.survey()
      label.regress <- data_label()
      data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))
      if(input$subcheck == T){
        req(input$subvar_km)
        req(input$subval_km)

        if (input$subvar_km %in% vlist()$factor_vars){
          data.design <- subset(data.design, get(input$subvar_km) %in% input$subval_km)
        } else{
          data.design <- subset(data.design, get(input$subvar_km) >= input$subval_km[1] & get(input$subvar_km) <= input$subval_km[2])
        }
        data.design$variables[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
        label.regress2 <- mk.lev(data.design$variables)[, c("variable", "class", "level")]
        data.table::setkey(data_label(), "variable", "class", "level")
        data.table::setkey(label.regress2, "variable", "class", "level")
        label.regress <- data_label()[label.regress2]
        data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))

      }
      cc <- substitute(survey::svykm(.form, design= data.design, se = F), list(.form= form.km()))
      res.km <- eval(cc)
      if (input$indep_km == "None"){
        yst.name <- ""
        yst.lab <- "All"
      } else{
        yst.name <- label.regress[variable == input$indep_km, var_label][1]
        yst.lab <- label.regress[variable == input$indep_km, val_label]
      }
      ylab = ifelse(input$cumhaz, "Cumulative hazard", "Survival")
      return(list(res = res.km, ylab = ylab, yst.name = yst.name, yst.lab = yst.lab, data = data.design))
    }
  })

  observeEvent(kmList(), {
    output$ranges = renderUI({
      res.km <- kmList()$res
      if (is.null(design.survey)){
        xmax <- max(res.km$time)
      } else{
        if (class(res.km) == "svykmlist"){
          xmax <- max(sapply(res.km, function(x){max(x$time)}))
        } else if(class(res.km) == "svykm"){

          xmax <- max(res.km$time)
        }
      }

      value.timeby <- signif(xmax/7, 1)
      if (!is.null(timeby)){
        value.timeby <- timeby
      }

      if(is.null(range.x)){
        range.x <- c(xmin, xmax)
      }
      if(is.null(range.y)){
        range.y <- c(0, 1)
      }



      tagList(
        sliderInput(session$ns("timeby"), "Time by",
                    min = 1, max = xmax, value = value.timeby, step = 5),

        sliderInput(session$ns("xlims"), "X axis range(time)",
                    min = 0, max = xmax, value = range.x, step = 5),
        sliderInput(session$ns("ylims"), "Y axis range(probability)",
                    min = 0, max = 1, value = range.y , step = 0.05)
      )
    })
  })

  kmInput <- reactive({
    req(kmList())
    req(input$timeby)
    req(input$xlims)
    req(input$ylims)
    res.km <- kmList()$res
    ylab <- kmList()$ylab
    yst.name <- kmList()$yst.name
    yst.lab <- kmList()$yst.lab
    data.km <- kmList()$data

    if (is.null(design.survey)){
      if (is.null(id.cluster)){
        return(
          jskm::jskm(res.km, pval = input$pval, mark=F, table= input$table, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F, timeby = input$timeby, xlims = input$xlims, ylims = input$ylims,
                     cumhaz= input$cumhaz, cluster.option = "None", cluster.var = NULL, data = data.km, pval.coord = pval.coord)
        )
      } else{
        return(
          jskm::jskm(res.km, pval = input$pval, mark=F, table= input$table, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F, timeby = input$timeby, xlims = input$xlims, ylims = input$ylims,
                     cumhaz= input$cumhaz, cluster.option = "cluster", cluster.var = id.cluster(), data = data.km, pval.coord = pval.coord)
        )
      }
    } else{
      return(
        jskm::svyjskm(res.km, pval = input$pval, table= input$table, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F, timeby = input$timeby, xlims = input$xlims, ylims = input$ylims,
                      cumhaz= input$cumhaz, design = data.km, pval.coord = pval.coord)
      )
    }
  })




  output$downloadControls <- renderUI({
    tagList(
      column(4,
             selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
                            choices = c("jpg","pdf", "tiff", "svg"), multiple = F,
                            selected = "jpg"
             )
      ),
      column(4,
             sliderInput(session$ns("fig_width"), "Width (in):",
                         min = 5, max = 15, value = 8
             )
      ),
      column(4,
             sliderInput(session$ns("fig_height"), "Height (in):",
                         min = 5, max = 15, value = 6
             )
      )
    )
  })

  output$downloadButton <- downloadHandler(
    filename =  function() {
      if (is.null(design.survey)){
        if (is.null(id.cluster)){
          return(paste(input$event_km, "_", input$indep_km,"_kaplan_meier.",input$file_ext ,sep=""))
        } else{
          return(paste(input$event_km, "_", input$indep_km,"_kaplan_meier_marginal.",input$file_ext ,sep=""))
        }
      } else{
        return(paste(input$event_km, "_", input$indep_km,"_surveykaplan_meier.",input$file_ext ,sep=""))
      }

    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }

                     ggsave(file, kmInput(), dpi = 300, units = "in", width = input$fig_width, height =input$fig_height)
                   })

    }
  )

  return(kmInput)




}

