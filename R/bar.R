#' @title barUI: shiny module UI for barplot
#' @description Shiny module UI for barplot
#' @param id id
#' @param label label
#' @return Shiny module UI for barplot
#' @details Shiny module UI for barplot
#' @examples
#' library(shiny)
#' library(ggplot2)
#' library(ggpubr)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       barUI("bar")
#'     ),
#'     mainPanel(
#'       optionUI("bar"),
#'       plotOutput("bar_plot"),
#'       ggplotdownUI("bar")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_bar <- barServer("bar",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$bar_plot <- renderPlot({
#'     print(out_bar())
#'   })
#' }
#' @rdname barUI
#' @export



barUI <- function(id, label = "barplot") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("vars_bar")),
    uiOutput(ns("strata_bar")),
    checkboxInput(ns("fill"), "Fill"),
    checkboxInput(ns("mean"), "Mean_SE"),
    checkboxInput(ns("jitter"), "Jitter"),
    uiOutput(ns("pvalue")),
    uiOutput(ns("subvar")),
    uiOutput(ns("subval"))
  )
}


# optionUI <- function(id) {
#   # Create a namespace function using the provided id
#   ns <- NS(id)
#
#   shinyWidgets::dropdownButton(
#     uiOutput(ns("option_bar")),
#     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
#     tooltip = shinyWidgets::tooltipOptions(title = "Click to see other options !")
#   )
# }


#' @title barServer: shiny module server for barplot.
#' @description Shiny module server for barplot.
#' @param id id
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @return Shiny module server for barplot.
#' @details Shiny module server for barplot.
#' @examples
#' library(shiny)
#' library(ggplot2)
#' library(ggpubr)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       barUI("bar")
#'     ),
#'     mainPanel(
#'       optionUI("bar"),
#'       plotOutput("bar_plot"),
#'       ggplotdownUI("bar")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_bar <- barServer("bar",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$bar_plot <- renderPlot({
#'     print(out_bar())
#'   })
#' }
#' @rdname barServer
#' @export
#' @import shiny
#' @importFrom data.table data.table .SD :=
#' @importFrom ggpubr ggbarplot stat_compare_means geom_pwc
#' @importFrom ggplot2 ggsave
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location
#' @importFrom scales label_pvalue
#' @importFrom shinyWidgets dropdownButton tooltipOptions



barServer <- function(id, data, data_label, data_varStruct = NULL, nfactor.limit = 10) {
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

      output$vars_bar <- renderUI({
        tagList(
          selectizeInput(session$ns("x_bar"), "X variable",
            choices = vlist()$factor_vars, multiple = F,
            selected = vlist()$select_vars[1]
          ),
          selectizeInput(session$ns("y_bar"), "Y variable",
            choices = vlist()$select_list, multiple = F,
            selected = ifelse(length(vlist()$select_vars) > 1, vlist()$select_vars[2], vlist()$select_vars[1])
          )
        )
      })

      output$strata_bar <- renderUI({
        strata_vars <- setdiff(vlist()$factor_vars, vlist()$except_vars)
        strata_vars <- setdiff(strata_vars, input$x_bar)
        strata_list <- mklist(data_varStruct(), strata_vars)
        strata_select <- c("None", strata_list)
        selectizeInput(session$ns("strata"), "Strata",
          choices = strata_select, multiple = F,
          selected = unlist(strata_select)[1]
        )
      })


      output$pvalue <- renderUI({
        req(!is.null(input$x_bar))

        tglist <- tagList()

        if (vlist()$nclass_factor[input$x_bar] < 3) {
          pval.choices <- c("T-test" = "t.test", "Wilcoxon" = "wilcox.test")
        } else {
          pval.choices <- c("ANOVA" = "anova", "Kruskal-Wallis" = "kruskal.test")
        }

        tglist <- tagAppendChildren(
          tglist,
          div("P value Option") %>% strong(),
          tabsetPanel(
            id = session$ns("side_tabset_pval"),
            type = "hidden",
            selected = "strataFalse",
            tabPanel(
              "strataTrue",
              NULL
            ),
            tabPanel(
              "strataFalse",
              checkboxInput(session$ns("isPvalue"), "P value"),
            )
          ),
          tabsetPanel(
            id = session$ns("side_tabset_pvalradio"),
            type = "hidden",
            selected = "isPvalueFalse",
            tabPanel(
              "isPvalueTrue",
              radioButtons(
                session$ns("pvalue"),
                label = NULL,
                inline = TRUE,
                choices = pval.choices
              ),
            ),
            tabPanel(
              "isPvalueFalse",
              NULL
            )
          ),
          tabsetPanel(
            id = session$ns("side_tabset_ppval"),
            type = "hidden",
            selected = "under_three",
            tabPanel(
              "under_three",
              NULL
            ),
            tabPanel(
              "over_three",
              checkboxInput(session$ns("isPair"), "Pairwise P value"),
            )
          ),
          tabsetPanel(
            id = session$ns("side_tabset_ppvalradio"),
            type = "hidden",
            selected = "isPairFalse",
            tabPanel(
              "isPairTrue",
              radioButtons(
                session$ns("p_pvalue"),
                label = NULL,
                inline = TRUE,
                choices = c("T-test" = "t_test", "Wilcoxon" = "wilcox_test")
              ),
            ),
            tabPanel(
              "isPairFalse",
              NULL
            )
          ),
          tabsetPanel(
            id = session$ns("side_tabset_isstrata"),
            type = "hidden",
            selected = "strataFalse",
            tabPanel(
              "strataTrue",
              checkboxInput(session$ns("isStrata"), "Pairwise P value"),
            ),
            tabPanel(
              "strataFalse",
              NULL
            )
          ),
          tabsetPanel(
            id = session$ns("side_tabset_spvalradio"),
            type = "hidden",
            selected = "isStrataFalse",
            tabPanel(
              "isStrataTrue",
              radioButtons(
                session$ns("s_pvalue"),
                label = NULL,
                inline = TRUE,
                choices = c("T-test" = "t_test", "Wilcoxon" = "wilcox_test")
              )
            ),
            tabPanel(
              "isStrataFalse",
              NULL
            )
          )
        )

        return(tglist)
      })


      # Error message popup
      barInputError <- reactive({
        msg <- tryCatch(
          {
            print(barInput() %>% suppressWarnings())
          },
          warning = function(e) {
            res <- e
            temp <- e
            while (!is.null(temp$message)) {
              res <- temp
              temp <- temp$parent
            }
            return(res$message)
          },
          error = function(e) {
            return(e$message)
          }
        )

        ifelse(!is.ggplot(msg), msg, "Success")
      })


      observeEvent(input$subcheck, {
        output$subvar <- renderUI({
          req(input$subcheck == T)
          req(!is.null(input$x_bar))

          var_subgroup <- setdiff(names(data()), c(vlist()$except_vars, input$x_bar, input$y_bar, input$strata))

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


      observeEvent(input$x_bar, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")

        nclass.factor <- vlist()$nclass_factor[input$x_bar]
        if (nclass.factor > 2 & input$strata == "None") {
          tabset.selected <- "over_three"
        } else {
          tabset.selected <- "under_three"
        }
        updateTabsetPanel(session, "side_tabset_ppval", selected = tabset.selected)
      })


      observeEvent(input$strata, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")

        updateTabsetPanel(session, "side_tabset_ppval", selected = "under_three")
        updateCheckboxInput(session, "isPvalue", value = FALSE)
        updateCheckboxInput(session, "isPair", value = FALSE)
        updateCheckboxInput(session, "isStrata", value = FALSE)

        nclass.factor <- vlist()$nclass_factor[input$x_bar]
        if (input$strata != "None") {
          tabset.selected.strata <- "strataTrue"
          tabset.selected.nclass <- "under_three"
        } else if (nclass.factor > 2) {
          tabset.selected.strata <- "strataFalse"
          tabset.selected.nclass <- "over_three"
        } else {
          tabset.selected.strata <- "strataFalse"
          tabset.selected.nclass <- "under_three"
        }
        updateTabsetPanel(session, "side_tabset_pval", selected = tabset.selected.strata)
        updateTabsetPanel(session, "side_tabset_isstrata", selected = tabset.selected.strata)
        updateTabsetPanel(session, "side_tabset_ppval", selected = tabset.selected.nclass)
      })


      observeEvent(input$isPvalue, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")
        updateTabsetPanel(session, "side_tabset_pvalradio", selected = ifelse(input$isPvalue, "isPvalueTrue", "isPvalueFalse"))
      })


      observeEvent(input$isPair, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")
        updateTabsetPanel(session, "side_tabset_ppvalradio", selected = ifelse(input$isPair, "isPairTrue", "isPairFalse"))
      })


      observeEvent(input$isStrata, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")
        updateTabsetPanel(session, "side_tabset_spvalradio", selected = ifelse(input$isStrata, "isStrataTrue", "isStrataFalse"))
      })


      observeEvent(input$pvalue, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")
      })


      observeEvent(input$p_pvalue, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")
      })


      observeEvent(input$s_pvalue, {
        msg <- barInputError()
        if (msg != "" & msg != "Success") showNotification(msg, type = "warning")
      })


      # Reset button
      observeEvent(input$pval_reset, {
        updateSliderInput(session, "pvalfont", value = 4)
        updateSliderInput(session, "pvalx", value = 0.5)
        updateSliderInput(session, "pvaly", value = 1)
        updateSliderInput(session, "p_pvalfont", value = 4)
      })


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

      barInput <- reactive({
        req(c(input$x_bar, input$y_bar, input$strata, input$pvalue, input$pvalx, input$pvaly, input$pvalfont, input$p_pvalue, input$p_pvalfont, input$s_pvalue))
        req(input$isPair != "None")
        req(input$isPvalue != "None")
        req(input$isStrata != "None")

        data <- data.table(data())
        label <- data_label()
        color <- ifelse(input$strata == "None", "black", input$strata)
        fill <- "white"
        if (input$fill) {
          fill <- ifelse(input$strata == "None", input$x_bar, input$strata)
        }

        if (input$strata != "None") {
          data <- data[!is.na(get(input$strata))]
        }
        add.params <- list()
        cor.coeff.args <- list(p.accuracy = 0.001)

        add <- "mean"
        if (input$jitter) {
          add <- c("mean", "jitter")
        }
        if (input$mean) {
          add <- "mean_se"
        }
        if (input$mean & input$jitter) {
          add <- c("jitter", "mean_se")
        }

        pval.font.size <- c(input$pvalfont, input$p_pvalfont, input$p_pvalfont / 10)
        pval.coord <- c(input$pvalx, input$pvaly)
        pval.name <- input$pvalue
        ppval.name <- input$p_pvalue
        spval.name <- input$s_pvalue


        res.plot <- ggpubr::ggbarplot(data, input$x_bar, input$y_bar,
          color = color, add = add, add.params = add.params, conf.int = input$lineci,
          xlab = label[variable == input$x_bar, var_label][1],
          ylab = label[variable == input$y_bar, var_label][1], na.rm = T,
          position = position_dodge(), fill = fill,
        )

        if (input$isPvalue & input$strata == "None") {
          res.plot <- res.plot +
            ggpubr::stat_compare_means(
              method = pval.name,
              size = pval.font.size[1],
              label.x.npc = pval.coord[1],
              label.y.npc = pval.coord[2],
              aes(
                label = scales::label_pvalue(add_p = TRUE)(after_stat(p))
              ),
            )
        }

        if (input$isPair & vlist()$nclass_factor[input$x_bar] > 2 & input$strata == "None") {
          res.plot <- res.plot +
            ggpubr::geom_pwc(
              method = ppval.name,
              size = pval.font.size[3],
              label.size = pval.font.size[2],
              aes(label = scales::label_pvalue(add_p = TRUE)(after_stat(p))),
            )
        }

        if (input$isStrata & input$strata != "None") {
          res.plot <- res.plot +
            ggpubr::geom_pwc(
              method = spval.name,
              size = pval.font.size[3],
              label.size = pval.font.size[2],
              aes(label = scales::label_pvalue(add_p = TRUE)(after_stat(p)), group = !!sym(input$strata))
            )
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
          paste(input$x_bar, "_", input$y_bar, "_barplot.", input$file_ext, sep = "")
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
                my_vec_graph <- rvg::dml(ggobj = barInput())

                doc <- officer::read_pptx()
                doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
                print(doc, target = file)
              } else {
                ggplot2::ggsave(file, barInput(), dpi = 300, units = "in", width = input$fig_width, height = input$fig_height)
              }
            }
          )
        }
      )


      # option dropdown menu
      output$option_kaplan <- renderUI({
        tagList(
          h3("P-value position"),
          sliderInput(session$ns("pvalfont"), "P-value font size",
            min = 1, max = 10, value = 4
          ),
          sliderInput(session$ns("pvalx"), "x-axis",
            min = 0, max = 1, value = 0.5
          ),
          sliderInput(session$ns("pvaly"), "y-axis",
            min = 0, max = 1, value = 0.7
          ),
          h3("Pair P-value position"),
          sliderInput(session$ns("p_pvalfont"), "P-value font size",
            min = 1, max = 10, value = 4
          ),
          actionButton(session$ns("pval_reset"), "reset"),
        )
      })

      # Dropdown button default option
      outputOptions(x = output, name = "option_kaplan", suspendWhenHidden = FALSE)

      return(barInput)
    }
  )
}
#####


#
#
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       barUI("bar")
#     ),
#     mainPanel(
#       plotOutput("bar_plot"),
#       ggplotdownUI("bar")
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
#   out_bar <- barServer("bar", data = data, data_label = data.label,
#                            data_varStruct = NULL)
#
#   output$bar_plot <- renderPlot({
#     print(out_bar())
#   })
# }
#
# shinyApp(ui, server)
