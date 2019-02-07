#' @title kaplanUI: ModuleUI for kaplan-meier plot
#' @description ModuleUI for kaplan-meier plot
#' @param id id
#' @return kaplanUI
#' @details DETAILS
#' @examples
#'  #EXAMPLE1
#' @rdname kaplanUI
#' @export

kaplanUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("eventtime")),
    uiOutput(ns("indep")),
    checkboxInput(ns("cumhaz"), "Show cumulative hazard", F),
    checkboxInput(ns("pval"), "Show p-value(log-rank test)", T),
    checkboxInput(ns("table"), "Show table", T),
    checkboxInput(ns("subcheck"), "Sub-group analysis"),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval"))
  )
}



#' @title ggplotdownUI: Option & download module UI for ggplot
#' @description Option & download module UI for ggplot
#' @param id id
#' @return ggplotdownUI
#' @details DETAILS
#' @examples
#'  #EXAMPLE1
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




#' @title kaplanModule: Module for kaplan-meier plot
#' @description Module for kaplan-meier plot
#' @param input input
#' @param output output
#' @param session session
#' @param data reactive data
#' @param data_label reactuve data_label
#' @param data_varStruct reactive data_varStruct, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey reactive survey data. default: NULL
#' @param id.cluster reactive cluster variable if marginal model, Default: NULL
#' @return kaplanModule
#' @details DETAILS
#' @examples
#'  #EXAMPLE1
#' @rdname kaplanModule
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom labelled var_label<-
#' @importFrom stats glm as.formula model.frame
#' @importFrom epiDisplay regress.display
#' @importFrom jstable LabelepiDisplay
#' @importFrom purrr map_lgl


kaplanModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, id.cluster = NULL) {

  ## To remove NOTE.
  val_label <- variable <- NULL

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


    if (is.null(design.survey)){
      indep.km <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, input$time_km ))

    } else{
      indep.km <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, input$time_km, names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))

    }


    tagList(
      selectInput(session$ns("indep_km"), "Independent variables",
                  choices = c("None", mklist(data_varStruct(), indep.km)), multiple = F,
                  selected = "None"
      )
    )
  })

  observeEvent(input$indep_km, {
    output$subvar <- renderUI({
      req(input$subcheck == T)
      factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
      factor_subgroup <- setdiff(factor_vars, c(input$event_km, input$indep_km))
      factor_subgroup_list <- mklist(data_varStruct(), factor_subgroup)
      validate(
        need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
      )

      tagList(
        selectInput(session$ns("subvar_km"), "Sub-group variable",
                    choices = factor_subgroup_list, multiple = F,
                    selected = factor_subgroup[1])
      )
    })

  })


  output$subval <- renderUI({
    req(input$subcheck == T)
    req(input$subvar_km)
    selectInput(session$ns("subval_km"), "Sub-group value",
                choices = data_label()[variable == input$subvar_km, val_label], multiple = F,
                selected = data_label()[variable == input$subvar_km, val_label][1])
  })



  form.km <- reactive({
    validate(
      need(!is.null(input$indep_km), "Please select at least 1 independent variable.")
    )
    if (input$indep_km == "None"){
      return(as.formula(paste("survival::Surv(",input$time_km,",", input$event_km,") ~ ", "1", sep="")))
    } else{
      return(as.formula(paste("survival::Surv(",input$time_km,",", input$event_km,") ~ ", input$indep_km, sep="")))
    }
  })


  kmInput <- reactive({
    req(!is.null(input$event_km))
    req(!is.null(input$time_km))
    data.km <- data()
    data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
    if(input$subcheck == T){
      req(input$subvar_km)
      req(input$subval_km)
      data.km <- data.km[get(input$subvar_km) == input$subval_km, ]
    }
    mf <- model.frame(form.km(), data.km)
    validate(
      need(nrow(mf) > 0, paste("No complete data due to missingness."))
    )

    if (is.null(design.survey)){
      cc = substitute(survival::survfit(.form, data= data.km), list(.form= form.km()))
      res.km = eval(cc)
      if (input$indep_km == "None"){
        yst.name <- ""
        yst.lab <- "All"
      } else{
        yst.name <- data_label()[variable == input$indep_km, var_label][1]
        yst.lab <- data_label()[variable == input$indep_km, val_label]
      }
      ylab = ifelse(input$cumhaz, "Cumulative hazard", "Survival")
      if (is.null(id.cluster)){
        return(
          jskm::jskm(res.km, pval = input$pval, mark=F, table= input$table, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F,
                     cumhaz= input$cumhaz, cluster.option = "None", cluster.var = NULL, data = data.km)
        )
      } else{
        return(
          jskm::jskm(res.km, pval = input$pval, mark=F, table= input$table, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F,
                     cumhaz= input$cumhaz, cluster.option = "cluster", cluster.var = id.cluster(), data = data.km)
        )
      }



    } else{
      data.design <- design.survey()
      data.design$variables[[input$event_km]] <- as.numeric(as.vector(data.design$variables[[input$event_km]]))
      if(input$subcheck == T){
        req(input$subvar_km)
        req(input$subval_km)
        data.design <- subset(data.design, get(input$subvar_km) == input$subval_km)
      }
      cc <- substitute(survey::svykm(.form, design= data.design, se = T), list(.form= form.km()))
      res.km <- eval(cc)
      if (input$indep_km == "None"){
        yst.name <- ""
        yst.lab <- "All"
      } else{
        yst.name <- data_label()[variable == input$indep_km, var_label][1]
        yst.lab <- data_label()[variable == input$indep_km, val_label]
      }
      ylab = ifelse(input$cumhaz, "Cumulative hazard", "Survival")
      return(
        jskm::svyjskm(res.km, pval = input$pval, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F,
                   cumhaz= input$cumhaz, design = data.design)
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
      ggsave(file, kmInput(), dpi = 300, units = "in", width = input$fig_width, height =input$fig_height)

    }
  )

  return(kmInput)




}

