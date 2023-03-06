#' @title ggpairsModuleUI1: Variable selection module UI for ggpairs
#' @description Variable selection module UI for ggpairs
#' @param id id
#' @return Variable selection module UI for ggpairs
#' @details Variable selection module UI for ggpairs
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' library(ggplot2)
#' library(GGally)
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       ggpairsModuleUI1("ggpairs")
#'     ),
#'     mainPanel(
#'       plotOutput("ggpairs_plot"),
#'       ggpairsModuleUI2("ggpairs")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_ggpairs <- callModule(ggpairsModule2, "ggpairs",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$kaplan_plot <- renderPlot({
#'     print(out_ggpairs())
#'   })
#' }
#' @rdname ggpairsModuleUI1
#' @export

ggpairsModuleUI1 <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("vars_ggpairs")),
    uiOutput(ns("strata_ggpairs")),
    selectInput(ns("theme"), "Theme",
      c("default", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
      multiple = F,
      selected = "default"
    )
  )
}


#' @title ggpairsModuleUI2: Option & download module UI for ggpairs
#' @description Option & download module UI for ggpairs
#' @param id id
#' @return Option & download module UI for ggpairs
#' @details Option & download module UI for ggpairs
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' library(ggplot2)
#' library(GGally)
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       ggpairsModuleUI1("ggpairs")
#'     ),
#'     mainPanel(
#'       plotOutput("ggpairs_plot"),
#'       ggpairsModuleUI2("ggpairs")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_ggpairs <- callModule(ggpairsModule2, "ggpairs",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$kaplan_plot <- renderPlot({
#'     print(out_ggpairs())
#'   })
#' }
#' @rdname ggpairsModuleUI2
#' @export

ggpairsModuleUI2 <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    h3("Graph option"),
    wellPanel(
      uiOutput(ns("gtype"))
    ),
    br(),
    h3("Download options"),
    wellPanel(
      uiOutput(ns("downloadControls")),
      downloadButton(ns("downloadButton"), label = "Download the plot")
    )
  )
}




#' @title ggpairsModule: shiny module server for basic/scatter plot.
#' @description Shiny module server for basic/scatter plot.
#' @param input input
#' @param output output
#' @param session session
#' @param data data
#' @param data_label data label
#' @param data_varStruct List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit for categorical variables, Default: 20
#' @return Shiny module server for basic/scatter plot.
#' @details Shiny module server for basic/scatter plot.
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' library(ggplot2)
#' library(GGally)
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       ggpairsModuleUI1("ggpairs")
#'     ),
#'     mainPanel(
#'       plotOutput("ggpairs_plot"),
#'       ggpairsModuleUI2("ggpairs")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- mtcars
#'   data.label <- jstable::mk.lev(mtcars)
#'
#'   out_ggpairs <- callModule(ggpairsModule, "ggpairs",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$kaplan_plot <- renderPlot({
#'     print(out_ggpairs())
#'   })
#' }
#' @rdname ggpairsModule
#' @export
#' @import shiny
#' @import ggplot2
#' @importFrom data.table data.table .SD :=
#' @importFrom GGally ggpairs
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location


ggpairsModule <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 20) {
  ## To remove NOTE.
  val_label <- variable <- NULL

  if (is.null(data_varStruct)) {
    data_varStruct <- list(variable = names(data))
  }

  if (!("data.table" %in% class(data))) {
    data <- data.table(data)
  }
  if (!("data.table" %in% class(data_label))) {
    data_label <- data.table(data_label)
  }

  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  data[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
  factor_list <- mklist(data_varStruct, factor_vars)

  nclass_factor <- unlist(data[, lapply(.SD, function(x) {
    length(levels(x))
  }), .SDcols = factor_vars])

  group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data)]
  group_list <- mklist(data_varStruct, group_vars)

  except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data)]

  select_vars <- setdiff(names(data), except_vars)
  select_list <- mklist(data_varStruct, select_vars)


  output$vars_ggpairs <- renderUI({
    selectizeInput(session$ns("vars"), "Variables",
      choices = select_list, multiple = T,
      selected = select_vars[1]
    )
  })

  output$strata_ggpairs <- renderUI({
    strata_vars <- setdiff(setdiff(factor_vars, except_vars), input$vars)
    strata_list <- mklist(data_varStruct, strata_vars)
    selectizeInput(session$ns("strata"), "Strata",
      choices = c("None", strata_list), multiple = F,
      selected = "None"
    )
  })





  output$gtype <- renderUI({
    tagList(
      h4("Upper"),
      column(
        4,
        selectizeInput(session$ns("gytpe_upper_conti"), "conti",
          choices = c("points", "smooth", "smooth_loess", "density", "cor", "blank"), multiple = F,
          selected = "cor"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_upper_combo"), "combo",
          choices = c("box", "box_no_facet", "dot", "dot_no_facet", "facethist", "facetdensity", "denstrip", "blank"), multiple = F,
          selected = "box_no_facet"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_upper_discrete"), "discrete",
          choices = c("facetbar", "ratio", "blank"), multiple = F,
          selected = "facetbar"
        )
      ),
      h4("Lower"),
      column(
        4,
        selectizeInput(session$ns("gytpe_lower_conti"), "conti",
          choices = c("points", "smooth", "smooth_loess", "density", "cor", "blank"), multiple = F,
          selected = "smooth_loess"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_lower_combo"), "combo",
          choices = c("box", "box_no_facet", "dot", "dot_no_facet", "facethist", "facetdensity", "denstrip", "blank"), multiple = F,
          selected = "facethist"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_lower_discrete"), "discrete",
          choices = c("facetbar", "ratio", "blank"), multiple = F,
          selected = "facetbar"
        )
      ),
      h4("Diag"),
      column(
        4,
        selectizeInput(session$ns("gytpe_diag_conti"), "conti",
          choices = c("densityDiag", "barDiag", "blankDiag"), multiple = F,
          selected = "densityDiag"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_diag_discrete"), "discrete",
          choices = c("barDiag", "blankDiag"), multiple = F,
          selected = "barDiag"
        )
      )
    )
  })


  ggpairsInput <- reactive({
    data.val <- data
    for (i in factor_vars) {
      levels(data.val[[i]]) <- data_label[variable == i, val_label]
    }

    if (is.null(input$vars)) {
      return(NULL)
    } else if (input$strata == "None") {
      p <- GGally::ggpairs(data.val,
        columns = input$vars, columnLabels = sapply(input$vars, function(x) {
          data_label[variable == x, var_label][1]
        }),
        upper = list(continuous = input$gytpe_upper_conti, combo = input$gytpe_upper_combo, discrete = input$gytpe_upper_discrete, na = "na"),
        lower = list(continuous = input$gytpe_lower_conti, combo = input$gytpe_lower_combo, discrete = input$gytpe_lower_discrete, na = "na"),
        diag = list(continuous = input$gytpe_diag_conti, discrete = input$gytpe_diag_discrete, na = "na"),
        axisLabels = "show"
      )
    } else {
      p <- GGally::ggpairs(data.val,
        columns = input$vars, columnLabels = sapply(input$vars, function(x) {
          data_label[variable == x, var_label][1]
        }),
        mapping = aes_string(color = input$strata),
        upper = list(continuous = input$gytpe_upper_conti, combo = input$gytpe_upper_combo, discrete = input$gytpe_upper_discrete, na = "na"),
        lower = list(continuous = input$gytpe_lower_conti, combo = input$gytpe_lower_combo, discrete = input$gytpe_lower_discrete, na = "na"),
        diag = list(continuous = input$gytpe_diag_conti, discrete = input$gytpe_diag_discrete, na = "na"),
        axisLabels = "show"
      )
    }
    class(p) <- c(class(p), "ggplot")
    if (is.null(input$theme)) {
      return(p)
    }
    if (input$theme == "default") {
      return(p)
    } else if (input$theme == "bw") {
      return(p + theme_bw())
    } else if (input$theme == "linedraw") {
      return(p + theme_linedraw())
    } else if (input$theme == "light") {
      return(p + theme_light())
    } else if (input$theme == "dark") {
      return(p + theme_dark())
    } else if (input$theme == "minimal") {
      return(p + theme_minimal())
    } else if (input$theme == "classic") {
      return(p + theme_classic())
    } else if (input$theme == "void") {
      return(p + theme_void())
    }
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
      paste(input$vars, "_by_", input$strata, "_ggpairs.", input$file_ext, sep = "")
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
            my_vec_graph <- rvg::dml(ggobj = ggpairsInput())
            doc <- officer::read_pptx()
            doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
            doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
            print(doc, target = file)
          } else {
            ggsave(file, ggpairsInput(), dpi = 300, units = "in", width = input$fig_width, height = input$fig_height)
          }
        }
      )
    }
  )

  return(ggpairsInput)
}



#' @title ggpairsModule2: shiny module server for basic/scatter plot for reactive data.
#' @description Shiny module server for basic/scatter plot for reactive data.
#' @param input input
#' @param output output
#' @param session session
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param data_varStruct List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit for categorical variables, Default: 20
#' @return Shiny module server for basic/scatter plot
#' @details Shiny module server for basic/scatter plot for reactive data.
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(jstable)
#' library(ggplot2)
#' library(GGally)
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       ggpairsModuleUI1("ggpairs")
#'     ),
#'     mainPanel(
#'       plotOutput("ggpairs_plot"),
#'       ggpairsModuleUI2("ggpairs")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(mtcars)
#'   data.label <- reactive(jstable::mk.lev(mtcars))
#'
#'   out_ggpairs <- callModule(ggpairsModule2, "ggpairs",
#'     data = data, data_label = data.label,
#'     data_varStruct = NULL
#'   )
#'
#'   output$kaplan_plot <- renderPlot({
#'     print(out_ggpairs())
#'   })
#' }
#' @rdname ggpairsModule2
#' @export
#' @import shiny
#' @import ggplot2
#' @importFrom data.table data.table .SD :=
#' @importFrom GGally ggpairs
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location

ggpairsModule2 <- function(input, output, session, data, data_label, data_varStruct = NULL, nfactor.limit = 20) {
  ## To remove NOTE.
  val_label <- variable <- NULL

  if (is.null(data_varStruct)) {
    data_varStruct <- reactive(list(variable = names(data())))
  }


  vlist <- reactive({
    mklist <- function(varlist, vars) {
      lapply(
        varlist,
        function(x) {
          inter <- intersect(x, vars)
          if (length(inter) == 1) {
            inter <- c(inter, "")
          }
          return(inter)
        }
      )
    }

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

    select_vars <- setdiff(names(data), except_vars)
    select_list <- mklist(data_varStruct(), select_vars)



    return(list(
      factor_vars = factor_vars, factor_list = factor_list, nclass_factor = nclass_factor, group_vars = group_vars, group_list = group_list, except_vars = except_vars,
      select_vars = select_vars, select_list = select_list
    ))
  })




  output$vars_ggpairs <- renderUI({
    selectizeInput(session$ns("vars"), "Variables",
      choices = vlist()$select_list, multiple = T,
      selected = vlist()$select_vars[1]
    )
  })

  output$strata_ggpairs <- renderUI({
    mklist <- function(varlist, vars) {
      lapply(
        varlist,
        function(x) {
          inter <- intersect(x, vars)
          if (length(inter) == 1) {
            inter <- c(inter, "")
          }
          return(inter)
        }
      )
    }
    strata_vars <- setdiff(setdiff(vlist()$factor_vars, vlist()$except_vars), input$vars)
    strata_list <- mklist(data_varStruct(), strata_vars)
    selectizeInput(session$ns("strata"), "Strata",
      choices = c("None", strata_list), multiple = F,
      selected = "None"
    )
  })





  output$gtype <- renderUI({
    tagList(
      h4("Upper"),
      column(
        4,
        selectizeInput(session$ns("gytpe_upper_conti"), "conti",
          choices = c("points", "smooth", "smooth_loess", "density", "cor", "blank"), multiple = F,
          selected = "cor"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_upper_combo"), "combo",
          choices = c("box", "box_no_facet", "dot", "dot_no_facet", "facethist", "facetdensity", "denstrip", "blank"), multiple = F,
          selected = "box_no_facet"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_upper_discrete"), "discrete",
          choices = c("facetbar", "ratio", "blank"), multiple = F,
          selected = "facetbar"
        )
      ),
      h4("Lower"),
      column(
        4,
        selectizeInput(session$ns("gytpe_lower_conti"), "conti",
          choices = c("points", "smooth", "smooth_loess", "density", "cor", "blank"), multiple = F,
          selected = "smooth_loess"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_lower_combo"), "combo",
          choices = c("box", "box_no_facet", "dot", "dot_no_facet", "facethist", "facetdensity", "denstrip", "blank"), multiple = F,
          selected = "facethist"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_lower_discrete"), "discrete",
          choices = c("facetbar", "ratio", "blank"), multiple = F,
          selected = "facetbar"
        )
      ),
      h4("Diag"),
      column(
        4,
        selectizeInput(session$ns("gytpe_diag_conti"), "conti",
          choices = c("densityDiag", "barDiag", "blankDiag"), multiple = F,
          selected = "densityDiag"
        )
      ),
      column(
        4,
        selectizeInput(session$ns("gytpe_diag_discrete"), "discrete",
          choices = c("barDiag", "blankDiag"), multiple = F,
          selected = "barDiag"
        )
      )
    )
  })


  ggpairsInput <- reactive({
    data.val <- data()
    for (i in vlist()$factor_vars) {
      levels(data.val[[i]]) <- data_label()[variable == i, val_label]
    }

    if (is.null(input$vars)) {
      return(NULL)
    } else if (input$strata == "None") {
      p <- GGally::ggpairs(data.val,
        columns = input$vars, columnLabels = sapply(input$vars, function(x) {
          data_label()[variable == x, var_label][1]
        }),
        upper = list(continuous = input$gytpe_upper_conti, combo = input$gytpe_upper_combo, discrete = input$gytpe_upper_discrete, na = "na"),
        lower = list(continuous = input$gytpe_lower_conti, combo = input$gytpe_lower_combo, discrete = input$gytpe_lower_discrete, na = "na"),
        diag = list(continuous = input$gytpe_diag_conti, discrete = input$gytpe_diag_discrete, na = "na"),
        axisLabels = "show"
      )
    } else {
      p <- GGally::ggpairs(data.val,
        columns = input$vars, columnLabels = sapply(input$vars, function(x) {
          data_label()[variable == x, var_label][1]
        }),
        mapping = aes_string(color = input$strata),
        upper = list(continuous = input$gytpe_upper_conti, combo = input$gytpe_upper_combo, discrete = input$gytpe_upper_discrete, na = "na"),
        lower = list(continuous = input$gytpe_lower_conti, combo = input$gytpe_lower_combo, discrete = input$gytpe_lower_discrete, na = "na"),
        diag = list(continuous = input$gytpe_diag_conti, discrete = input$gytpe_diag_discrete, na = "na"),
        axisLabels = "show"
      )
    }
    class(p) <- c(class(p), "ggplot")
    if (is.null(input$theme)) {
      return(p)
    }
    if (input$theme == "default") {
      return(p)
    } else if (input$theme == "bw") {
      return(p + theme_bw())
    } else if (input$theme == "linedraw") {
      return(p + theme_linedraw())
    } else if (input$theme == "light") {
      return(p + theme_light())
    } else if (input$theme == "dark") {
      return(p + theme_dark())
    } else if (input$theme == "minimal") {
      return(p + theme_minimal())
    } else if (input$theme == "classic") {
      return(p + theme_classic())
    } else if (input$theme == "void") {
      return(p + theme_void())
    }
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
      paste(input$vars, "_by_", input$strata, "_ggpairs.", input$file_ext, sep = "")
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
            my_vec_graph <- rvg::dml(ggobj = ggpairsInput())
            doc <- officer::read_pptx()
            doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
            doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$fig_width, height = input$fig_height))
            print(doc, target = file)
          } else {
            ggsave(file, ggpairsInput(), dpi = 300, units = "in", width = input$fig_width, height = input$fig_height)
          }
        }
      )
    }
  )

  return(ggpairsInput)
}
