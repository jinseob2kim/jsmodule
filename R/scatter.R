#' @title scatterUI: shiny module UI for scatterplot
#' @description Shiny module UI for scatterplot
#' @param id id
#' @param label label
#' @return Shiny module UI for scatterplot
#' @details Shiny module UI for scatterplot
#' @examples
#' library(shiny);library(ggplot2);library(ggpubr);
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      scatterUI("scatter")
#'    ),
#'    mainPanel(
#'      plotOutput("scatter_plot"),
#'      ggplotdownUI("scatter")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_scatter <- scatterServer("scatter", data = data, data_label = data.label,
#'     data_varStruct = NULL)
#'
#'   output$scatter_plot <- renderPlot({
#'     print(out_scatter())
#'   })
#'}
#' @rdname scatterUI
#' @export

scatterUI <- function(id, label = "scatterplot") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("vars_scatter")),
    uiOutput(ns("strata_scatter")),
    radioButtons(ns("line"), "Line", choices = c("None", "Reg.line", "Loess"), selected = "None", inline = T),
    conditionalPanel("input.line != 'None'", ns = ns,
                     tagList(
                       fluidRow(
                         column(6, checkboxInput(ns("lineci"), "95% CI", F)),
                         column(6, checkboxInput(ns("lineall"), "Overall line only", F))
                       )
                     )),
    radioButtons(ns("stat_cor"), "Correlation coefficients", choices = c("None", "Pearson", "Spearman"), selected = "None", inline = T),
    conditionalPanel("input.stat_cor != 'None'", ns = ns,
                     checkboxInput(ns("stat_all"), "Overall correlation only", F)),

    checkboxInput(ns("subcheck"), "Sub-group analysis"),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval"))

  )
}



#' @title scatterServer: shiny module server for scatterplot.
#' @description Shiny module server for scatterplot.
#' @param id id
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @return Shiny module server for scatterplot.
#' @details Shiny module server for scatterplot.
#' @examples
#' library(shiny);library(ggplot2);library(ggpubr);
#' ui <- fluidPage(
#'    sidebarLayout(
#'    sidebarPanel(
#'      scatterUI("scatter")
#'    ),
#'    mainPanel(
#'      plotOutput("scatter_plot"),
#'      ggplotdownUI("scatter")
#'    )
#'  )
#')
#'
#' server <- function(input, output, session) {
#'
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_scatter <- scatterServer("scatter", data = data, data_label = data.label,
#'     data_varStruct = NULL)
#'
#'   output$scatter_plot <- renderPlot({
#'     print(out_scatter())
#'   })
#'}
#' @rdname scatterServer
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom ggpubr ggscatter
#' @importFrom ggplot2 ggsave
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location

scatterServer <- function(id, data, data_label, data_varStruct = NULL, nfactor.limit = 10) {
  moduleServer(id,
               function(input, output, session) {
                 ## To remove NOTE.
                 level <- val_label <- variable <- NULL

                 if (is.null(data_varStruct)){
                   data_varStruct = reactive(list(variable = names(data())))
                 }


                 vlist <- reactive({

                   data <- data.table(data(), stringsAsFactors = T)

                   factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
                   #data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
                   factor_list <- mklist(data_varStruct(), factor_vars)

                   nclass_factor <- unlist(data[, lapply(.SD, function(x){length(levels(x))}), .SDcols = factor_vars])

                   group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data)]
                   group_list <- mklist(data_varStruct(), group_vars)

                   except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data)]

                   select_vars <- setdiff(names(data), factor_vars)
                   select_list <- mklist(data_varStruct(), select_vars)

                   return(list(factor_vars = factor_vars, factor_list = factor_list, nclass_factor = nclass_factor, group_vars = group_vars, group_list = group_list, except_vars = except_vars,
                               select_vars = select_vars, select_list = select_list))
                 })

                 output$vars_scatter <- renderUI({
                   tagList(
                     selectizeInput(session$ns("x_scatter"), "X variable",
                                    choices = vlist()$select_list, multiple = F,
                                    selected = vlist()$select_vars[1]
                     ),
                     selectizeInput(session$ns("y_scatter"), "Y variable",
                                    choices = vlist()$select_list, multiple = F,
                                    selected = ifelse(length(vlist()$select_vars) > 1, vlist()$select_vars[2], vlist()$select_vars[1])
                     )
                   )

                 })

                 output$strata_scatter <- renderUI({
                   strata_vars <- setdiff(vlist()$factor_vars, vlist()$except_vars)
                   strata_list <- mklist(data_varStruct(), strata_vars)
                   selectizeInput(session$ns("strata"), "Strata",
                                  choices = c("None", strata_list), multiple = F,
                                  selected = "None"
                   )
                 })


                 observeEvent(input$subcheck, {
                   output$subvar <- renderUI({
                     req(input$subcheck == T)
                     req(!is.null(input$x_scatter))

                     var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$x_scatter, input$y_scatter, input$strata))

                     var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
                     validate(
                       need(length(var_subgroup) > 0 , "No variables for sub-group analysis")
                     )

                     tagList(
                       selectInput(session$ns("subvar_km"), "Sub-group variables",
                                   choices = var_subgroup_list, multiple = T,
                                   selected = var_subgroup[1])
                     )


                   })

                 })


                 output$subval <- renderUI({
                   req(input$subcheck == T)
                   req(length(input$subvar_km) > 0)

                   outUI <- tagList()

                   for (v in seq_along(input$subvar_km)){
                     if (input$subvar_km[[v]] %in% vlist()$factor_vars){
                       outUI[[v]] <- selectInput(session$ns(paste0("subval_km", v)), paste0("Sub-group value: ", input$subvar_km[[v]]),
                                                 choices = data_label()[variable == input$subvar_km[[v]], level], multiple = T,
                                                 selected = data_label()[variable == input$subvar_km[[v]], level][1])
                     } else{
                       val <- stats::quantile(data()[[input$subvar_km[[v]]]], na.rm = T)
                       outUI[[v]] <- sliderInput(session$ns(paste0("subval_km", v)), paste0("Sub-group range: ", input$subvar_km[[v]]),
                                                 min = val[1], max = val[5],
                                                 value = c(val[2], val[4]))
                     }

                   }
                   outUI

                 })

                 scatterInput <- reactive({
                   req(c(input$x_scatter, input$y_scatter, input$strata))
                   data <- data()
                   label <- data_label()
                   if(input$subcheck == T){
                     validate(
                       need(length(input$subvar_km) > 0 , "No variables for subsetting"),
                       need(all(sapply(1:length(input$subvar_km), function(x){length(input[[paste0("subval_km", x)]])})), "No value for subsetting")
                     )

                     for (v in seq_along(input$subvar_km)){
                       if (input$subvar_km[[v]] %in% vlist()$factor_vars){
                         data <- data[get(input$subvar_km[[v]]) %in% input[[paste0("subval_km", v)]]]
                       } else{
                         data <- data[get(input$subvar_km[[v]]) >= input[[paste0("subval_km", v)]][1] & get(input$subvar_km[[v]]) <= input[[paste0("subval_km", v)]][2]]
                       }
                     }

                     data[, (vlist()$factor_vars) := lapply(.SD, factor), .SDcols = vlist()$factor_vars]
                     label2 <- mk.lev(data)[, c("variable", "level")]
                     data.table::setkey(label, "variable", "level")
                     data.table::setkey(label2, "variable", "level")
                     label <- label[label2]

                   }

                   add <- switch(input$line,
                                 "None" = "none",
                                 "Reg.line" = "reg.line",
                                 "Loess" = "loess")

                   cor.coef <- ifelse(input$stat_cor != "None", T, F)
                   cor.method <- ifelse(input$stat_cor == "Spearman", "spearman", "pearson")
                   color <- ifelse(input$strata == "None", "black", input$strata)
                   if (input$strata != "None"){
                     data <- data[!is.na(get(input$strata))]
                   }
                   add.params <- list()
                   cor.coeff.args <- list(p.accuracy = 0.001)
                   if (input$lineall == T){
                     add.params <- list(color = "black")
                   }

                   if (input$stat_all == F & input$strata != "None"){
                     cor.coeff.args <- list(aes_string(color = input$strata), p.accuracy = 0.001)
                   }



                   ggpubr::ggscatter(data, input$x_scatter, input$y_scatter, color= color, add = add, add.params = add.params, conf.int = input$lineci,
                                     cor.coef = cor.coef, cor.method = cor.method, cor.coeff.args = cor.coeff.args, xlab = label[variable == input$x_scatter, var_label][1],
                                     ylab = label[variable == input$y_scatter, var_label][1], na.rm = T)

                 })

                 output$downloadControls <- renderUI({
                   tagList(
                     column(4,
                            selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
                                           choices = c("jpg","pdf", "tiff", "svg", "pptx"), multiple = F,
                                           selected = "pptx"
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
                     paste(input$x_scatter, "_", input$y_scatter,"_scatterplot.",input$file_ext ,sep="")

                   },
                   # content is a function with argument file. content writes the plot to the device
                   content = function(file) {
                     withProgress(message = 'Download in progress',
                                  detail = 'This may take a while...', value = 0, {
                                    for (i in 1:15) {
                                      incProgress(1/15)
                                      Sys.sleep(0.01)
                                    }

                                    if (input$file_ext == "pptx"){
                                      my_vec_graph <- rvg::dml(ggobj  = scatterInput())

                                      doc <- officer::read_pptx()
                                      doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                                      doc <- officer::ph_with(doc, my_vec_graph, location = officer:: ph_location(width = input$fig_width, height = input$fig_height))
                                      print(doc, target = file)

                                    } else{
                                      ggplot2::ggsave(file, scatterInput(), dpi = 300, units = "in", width = input$fig_width, height =input$fig_height)
                                    }
                                  })

                   }
                 )

                 return(scatterInput)





                 }
  )


















}

