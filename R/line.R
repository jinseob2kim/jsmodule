#' @title lineUI: shiny module UI for lineplot
#' @description Shiny module UI for lineplot
#' @param id id
#' @param label label
#' @return Shiny module UI for lineplot
#' @details Shiny module UI for lineplot
#' @examples
#' library(shiny)
#' library(ggplot2)
#' library(ggpubr)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       lineUI("line")
#'     ),
#'     mainPanel(
#'       plotOutput("line_plot"),
#'       ggplotdownUI("line")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_line <- lineServer("line",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$line_plot <- renderPlot({
#'     print(out_line())
#'   })
#' }
#' @rdname lineUI
#' @export


lineUI <- function(id, label = "lineplot") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("vars_line")),
    uiOutput(ns("strata_line")),
    radioButtons(ns("options"), "Option", choices = c("Mean_SE", "Mean_SD", "Median_IQR"), selected = "Mean_SE", inline = T),
    checkboxInput(ns("linetype"), "Linetype"),
    checkboxInput(ns("jitter"), "Jitter"),
    checkboxInput(ns("rev_y"), "Reverse Y-axis"),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval")),
    uiOutput(ns("size")),
    uiOutput(ns("pointsize"))
  )
}


#' @title lineServer: shiny module server for lineplot.
#' @description Shiny module server for lineplot.
#' @param id id
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @return Shiny module server for lineplot.
#' @details Shiny module server for lineplot.
#' @examples
#' library(shiny)
#' library(ggplot2)
#' library(ggpubr)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       lineUI("line")
#'     ),
#'     mainPanel(
#'       plotOutput("line_plot"),
#'       ggplotdownUI("line")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_line <- lineServer("line",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$line_plot <- renderPlot({
#'     print(out_line())
#'   })
#' }
#' @rdname lineServer
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom ggpubr ggline
#' @importFrom ggplot2 ggsave scale_y_reverse
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location



lineServer <- function(id, data, data_label, data_varStruct = NULL, nfactor.limit = 10) {
  moduleServer(
    id,
    function(input, output, session) {
      ## To remove NOTE.
      level <- val_label <- variable <- NULL

      if (is.null(data_varStruct)) {
        data_varStruct <- reactive(list(variable = names(data())))
      }


      vlist <- reactive({
        data <- data.table(data(), stringsAsFactors = T)

        factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
        # data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
        factor_list <- mklist(data_varStruct(), factor_vars)

        nclass_factor <- unlist(data[, lapply(.SD, function(x) {
          length(levels(x))
        }), .SDcols = factor_vars])

        group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data)]
        group_list <- mklist(data_varStruct(), group_vars)

        except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data)]

        select_vars <- setdiff(names(data), factor_vars)
        select_list <- mklist(data_varStruct(), select_vars)

        return(list(
          factor_vars = factor_vars, factor_list = factor_list, nclass_factor = nclass_factor, group_vars = group_vars, group_list = group_list, except_vars = except_vars,
          select_vars = select_vars, select_list = select_list
        ))
      })

      output$vars_line <- renderUI({
        tagList(
          selectizeInput(session$ns("x_line"), "X variable",
            choices = vlist()$factor_vars, multiple = F,
            selected = vlist()$select_vars[1]
          ),
          selectizeInput(session$ns("y_line"), "Y variable",
            choices = vlist()$select_list, multiple = F,
            selected = ifelse(length(vlist()$select_vars) > 1, vlist()$select_vars[2], vlist()$select_vars[1])
          )
        )
      })

      output$strata_line <- renderUI({
        strata_vars <- setdiff(vlist()$factor_vars, vlist()$except_vars)
        strata_vars <- setdiff(strata_vars, input$x_line)
        strata_list <- mklist(data_varStruct(), strata_vars)
        strata_select <- c("None", strata_list)
        selectizeInput(session$ns("strata"), "Strata",
          choices = strata_select, multiple = F,
          selected = unlist(strata_select)[1]
        )
      })


      observeEvent(input$subcheck, {
        output$subvar <- renderUI({
          req(input$subcheck == T)
          req(!is.null(input$x_line))

          var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$x_line, input$y_line, input$strata))

          var_subgroup_list <- mklist(data_varStruct(), var_subgroup)
          validate(
            need(length(var_subgroup) > 0, "No variables for sub-group analysis")
          )

          tagList(
            selectInput(session$ns("subvar_km"), "Sub-group variables",
              choices = var_subgroup_list, multiple = T,
              selected = var_subgroup[1]
            )
          )
        })
      })
      output$size<-renderUI({
        tagList(
          fluidRow(
            column(6,numericInput(session$ns('size'), 'line size', value= 0.5)),
            column(6,numericInput(session$ns('pointsize'), 'point size', value= 0.5))

          )

        )
      }
      )

      output$subval <- renderUI({
        req(input$subcheck == T)
        req(length(input$subvar_km) > 0)

        outUI <- tagList()

        for (v in seq_along(input$subvar_km)) {
          if (input$subvar_km[[v]] %in% vlist()$factor_vars) {
            outUI[[v]] <- selectInput(session$ns(paste0("subval_km", v)), paste0("Sub-group value: ", input$subvar_km[[v]]),
              choices = data_label()[variable == input$subvar_km[[v]], level], multiple = T,
              selected = data_label()[variable == input$subvar_km[[v]], level][1]
            )
          } else {
            val <- stats::quantile(data()[[input$subvar_km[[v]]]], na.rm = T)
            outUI[[v]] <- sliderInput(session$ns(paste0("subval_km", v)), paste0("Sub-group range: ", input$subvar_km[[v]]),
              min = val[1], max = val[5],
              value = c(val[2], val[4])
            )
          }
        }
        outUI
      })

      lineInput <- reactive({
        req(c(input$x_line, input$y_line, input$strata))
        data <- data.table(data())
        label <- data_label()
        add <- switch(input$options,
          "Mean_SE" = "mean_se",
          "Mean_SD" = "mean_sd",
          "Median_IQR" = "median_iqr"
        )
        if (input$jitter) {
          add <- switch(input$options,
            "Mean_SE" = c("jitter", "mean_se"),
            "Mean_SD" = c("jitter", "mean_sd"),
            "Median_IQR" = c("jitter", "median_iqr")
          )
        }


        color <- ifelse(input$strata == "None", "black", input$strata)
        fill <- ifelse(input$strata == "None", input$x_line, input$strata)
        if (input$strata != "None") {
          data <- data[!is.na(get(input$strata))]
        }
        add.params <- list()
        cor.coeff.args <- list(p.accuracy = 0.001)



        linetype <- 19
        if (input$linetype) {
          if (input$strata == "None") {
            linetype <- 20
          } else {
            linetype <- input$strata
          }
        }


        res.plot <- ggpubr::ggline(data, input$x_line, input$y_line,
          color = color, add = add, add.params = add.params, conf.int = input$lineci,
          xlab = label[variable == input$x_line, var_label][1],
          ylab = label[variable == input$y_line, var_label][1], na.rm = T,
          position=position_dodge(0.5),
          size=input$size,
          point.size = input$pointsize,
          linetype = linetype
        )

        if (input$rev_y) {
          res.plot <- res.plot + ggplot2::scale_y_reverse()
        }

        return(res.plot)
      })

      output$downloadControls <- renderUI({
        tagList(
          column(
            4,
            selectizeInput(session$ns("file_ext"), "File extension (dpi = 300)",
              choices = c("jpg", "pdf", "tiff", "svg", "pptx"), multiple = F,
              selected = "pptx"
            )
          ),
          column(
            4,
            sliderInput(session$ns("fig_width"), "Width (in):",
              min = 5, max = 15, value = 8
            )
          ),
          column(
            4,
            sliderInput(session$ns("fig_height"), "Height (in):",
              min = 5, max = 15, value = 6
            )
          )
        )
      })

      output$downloadButton <- downloadHandler(
        filename = function() {
          paste(input$x_line, "_", input$y_line, "_lineplot.", input$file_ext, sep = "")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          withProgress(
            message = "Download in progress",
            detail = "This may take a while...",
            value = 0,
            {
              for (i in 1:15) {
                incProgress(1 / 15)
                Sys.sleep(0.01)
              }

              if (input$file_ext == "pptx") {
                my_vec_graph <- rvg::dml(ggobj = lineInput())

                doc <- officer::read_pptx()
                doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
                print(doc, target = file)
              } else {
                ggplot2::ggsave(file, lineInput(), dpi = 300, units = "in", width = input$fig_width, height = input$fig_height)
              }
            }
          )
        }
      )

      return(lineInput)
    }
  )
}
#####




# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       lineUI("line")
#     ),
#     mainPanel(
#       plotOutput("line_plot"),
#       ggplotdownUI("line")
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   mtcars$am <- as.factor(mtcars$am)
#   mtcars$vs <- as.factor(mtcars$vs)
#   mtcars$gear <- as.factor(mtcars$gear)
#   mtcars$carb <- as.factor(mtcars$carb)
#   mtcars$cyl <- as.factor(mtcars$cyl)
#   data <- reactive(mtcars)
#   data.label <- reactive(jstable::mk.lev(mtcars))
#   out_line <- lineServer("line", data = data, data_label = data.label,
#                              data_varStruct = NULL)
#
#   output$line_plot <- renderPlot({
#     print(out_line())
#   })
# }
#
# shinyApp(ui, server)
