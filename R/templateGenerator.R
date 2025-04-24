#' @title templateGenerator: Shiny Gadget for global/app.R template.
#' @description Opens a Shiny app that allows users to generate a Shiny project template.
#' @return Shiny Gadget including Data, Label info, Table 1, Regression(linear, logistic), Basic plots
#' @details Shiny Gadget including Data, Label info, Table 1, Regression(linear, logistic), Basic plots
#' @examples
#' if (interactive()) {
#'   templateGenerator()
#' }
#' @rdname templateGenerator
#' @export
#' @import shiny
#' @import bslib
#' @import data.table
templateGenerator <- function(){

ui <- fluidPage(
  titlePanel("Shiny Generator"),

  sidebarLayout(
    sidebarPanel(
      textInput("author", "author", value = "LHJ"),
      textInput("folder_name", "folder_name", value = "my_app"),
      textInput("save_path", "path", value = getwd()),
      actionButton("generate", "generate template", class = "btn-primary")
    ),

    mainPanel(
      verbatimTextOutput("status"),

      tags$hr(),
      h4("select options"),
      helpText("Data and table 1 are always included."),

      fluidRow(
        column(12, strong("Regression")),
        column(12,
               checkboxGroupInput("regression_panels", label = NULL,
                                  choices = list(
                                    "Linear regression" = "linear",
                                    "Logistic regression" = "logistic",
                                    "Cox model" = "cox"
                                  ))
        )
      ),

      fluidRow(
        column(12, strong("Plots")),
        column(12,
               checkboxGroupInput("plot_panels", label = NULL,
                                  choices = list(
                                    "Basic plot" = "basic_plot",
                                    "Histogram" = "histogram",
                                    "Scatterplot" = "scatter",
                                    "Boxplot" = "box",
                                    "Barplot" = "bar",
                                    "Lineplot" = "line",
                                    "Kaplan-meier plot" = "kaplan"
                                  ))
        )
      ),

      fluidRow(
        column(12, strong("ROC analysis")),
        column(12,
               checkboxGroupInput("roc_panels", label = NULL,
                                  choices = list(
                                    "ROC" = "roc",
                                    "Time-dependent ROC" = "timeroc"
                                  ))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$generate, {
    app_dir <- file.path(input$save_path, input$folder_name)
    dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

    selected_panels <- c(input$regression_panels, input$plot_panels, input$roc_panels)

    ## global.R
    global_code <- paste(
      paste0( "Created by ", input$author, " on ", Sys.Date()),

      '# global.R ----

library(data.table); library(dplyr); library(jstable)
setwd("~/ShinyApps/working directory")

board <- pins::board_s3("~~", prefix = "pins/~~")
alist <- pins::pin_read(board, "~~")
b <- alist$data %>% data.table(check.names = TRUE)
vinfo <- alist$vinfo
a <- copy(b)

varlist <- list(
  Base = c("", ""),
  Event = c("", ""),
  Time = c("", "")
)

out <- a[, .SD, .SDcols = c(unlist(varlist))]
out <- data.table::as.data.table(out)

factor_vars <- c(names(out)[sapply(out, function(x) length(unique(x))) <= 5])
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
conti_vars <- setdiff(names(out), c(factor_vars))
out[, (conti_vars) := lapply(.SD, as.numeric), .SDcols = conti_vars]

out.label <- jstable::mk.lev(out)
out.label <- data.table::as.data.table(out.label)
out.label[variable == "Sex", `:=`(var_label = "Sex", val_label = c("Male", "Female"))]

vars01 <- sapply(factor_vars, function(v) identical(levels(out[[v]]), c("0", "1")))
for (v in names(vars01)[vars01 == TRUE]) {
  out.label[variable == v, val_label := c("No", "Yes")]
}
vars02 <- sapply(factor_vars, function(v) identical(levels(out[[v]]), c("FALSE", "TRUE")))
for (v in names(vars02)[vars02 == TRUE]) {
  out.label[variable == v, val_label := c("No", "Yes")]
}

var.subgroup <- NULL', sep = '\n')


  # app_code> interactive> ui_parts
  ## app.R
  ui_parts <- list()

  ## Data tab  ( UI)
  ui_parts <- append(ui_parts, list(
    'tabPanel("Data", icon = icon("table"),
        sidebarLayout(
          sidebarPanel(
            uiOutput("factor"),
            uiOutput("binary_check"),
            uiOutput("binary_var"),
            uiOutput("binary_val"),
            uiOutput("ref_check"),
            uiOutput("ref_var"),
            uiOutput("ref_val"),
            uiOutput("subset_check"),
            uiOutput("subset_var"),
            uiOutput("subset_val"),
            bookmarkButton()
          ),
          mainPanel(
            tabsetPanel(type = "pills",
              tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
              tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
            )
          )
        )
    )'
  ))

  # Table 1 ()
  ui_parts <- append(ui_parts, list(
    'tabPanel("Table 1", icon = icon("percentage"),
    sidebarLayout(
      sidebarPanel(tb1moduleUI("tb1")),
      mainPanel(
        withLoader(DTOutput("table1"), type = "html", loader = "loader6"),
        wellPanel(
          h5("Normal continuous variables are summarized with Mean (SD) and t-test (2 groups) or ANOVA (> 2 groups)"),
          h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
          h5("Categorical variables are summarized with table")
        )
      )
    )
  )'
  ))

  # Regression panels
  if ("linear" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Linear regression",
      sidebarLayout(
        sidebarPanel(regressModuleUI("linear")),
        mainPanel(
          withLoader(DTOutput("lineartable"), type = "html", loader = "loader6"),
          br(),
          uiOutput("warning_linear")
        )
      )
    )'
    ))
  }
  if ("logistic" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Logistic regression",
      sidebarLayout(
        sidebarPanel(regressModuleUI("logistic")),
        mainPanel(
          withLoader(DTOutput("logistictable"), type = "html", loader = "loader6")
        )
      )
    )'
    ))
  }
  if ("cox" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Cox model",
      sidebarLayout(
        sidebarPanel(coxUI("cox")),
        mainPanel(
          withLoader(DTOutput("coxtable"), type = "html", loader = "loader6")
        )
      )
    )'
    ))
  }

  # Plot panels
  if ("basic_plot" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Basic plot",
      sidebarLayout(
        sidebarPanel(ggpairsModuleUI1("ggpairs")),
        mainPanel(
          withLoader(plotOutput("ggpairs_plot"), type = "html", loader = "loader6"),
          ggpairsModuleUI2("ggpairs")
        )
      )
    )'
    ))
  }
  if ("histogram" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Histogram",
      sidebarLayout(
        sidebarPanel(histogramUI("histogram")),
        mainPanel(
          withLoader(plotOutput("histogram"), type = "html", loader = "loader6"),
          ggplotdownUI("histogram")
        )
      )
    )'
    ))
  }
  if ("scatter" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Scatterplot",
      sidebarLayout(
        sidebarPanel(scatterUI("scatter")),
        mainPanel(
          withLoader(plotOutput("scatter_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("scatter")
        )
      )
    )'
    ))
  }
  if ("box" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Boxplot",
      sidebarLayout(
        sidebarPanel(boxUI("box")),
        mainPanel(
          withLoader(plotOutput("box_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("box")
        )
      )
    )'
    ))
  }
  if ("bar" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Barplot",
      sidebarLayout(
        sidebarPanel(barUI("bar")),
        mainPanel(
          withLoader(plotOutput("bar_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("bar")
        )
      )
    )'
    ))
  }
  if ("line" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Lineplot",
      sidebarLayout(
        sidebarPanel(lineUI("line")),
        mainPanel(
          withLoader(plotOutput("line_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("line")
        )
      )
    )'
    ))
  }
  if ("kaplan" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Kaplan-meier plot",
      sidebarLayout(
        sidebarPanel(kaplanUI("kaplan")),
        mainPanel(
          optionUI("kaplan"),
          withLoader(plotOutput("kaplan_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("kaplan")
        )
      )
    )'
    ))
  }

  # ROC panels
  if ("roc" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("ROC",
      sidebarLayout(
        sidebarPanel(rocUI("roc")),
        mainPanel(
          withLoader(plotOutput("plot_roc"), type = "html", loader = "loader6"),
          ggplotdownUI("roc"),
          withLoader(DTOutput("table_roc"), type = "html", loader = "loader6")
        )
      )
    )'
    ))
  }
  if ("timeroc" %in% selected_panels) {
    ui_parts <- append(ui_parts, list(
      'tabPanel("Time-dependent ROC",
      sidebarLayout(
        sidebarPanel(timerocUI("timeroc")),
        mainPanel(
          withLoader(plotOutput("plot_timeroc"), type = "html", loader = "loader6"),
          withLoader(tableOutput("cut_timeroc"), type = "html", loader = "loader6"),
          ggplotdownUI("timeroc"),
          withLoader(DTOutput("table_timeroc"), type = "html", loader = "loader6")
        )
      )
    )'
    ))
  }

  # 1
  ui_code <- paste(ui_parts, collapse = ",\n")




  server_parts <- c()
  server_parts <-  c(
    'output$factor <- renderUI({',
    '  selectInput("factor_vname",',
    '              label = "Additional categorical variables",',
    '              choices = names(out), multiple = TRUE,',
    '              #selected = NULL',
    '  )',
    '})',

    'output$binary_check <- renderUI({',
    '  checkboxInput("check_binary", "Make binary variables")',
    '})',

    'output$ref_check <- renderUI({',
    '  checkboxInput("check_ref", "Change reference of categorical variables")',
    '})',

    'output$subset_check <- renderUI({',
    '  checkboxInput("check_subset", "Subset data")',
    '})',

    'observeEvent(input$check_binary, {',
    '  var.conti <- setdiff(names(out), factor_vars)',
    '  output$binary_var <- renderUI({',
    '    req(input$check_binary == TRUE)',
    '    selectInput("var_binary", "Variables to dichotomize",',
    '                choices = var.conti, multiple = TRUE,',
    '                selected = var.conti[1])',
    '  })',

    '  output$binary_val <- renderUI({',
    '    req(input$check_binary == TRUE)',
    '    req(length(input$var_binary) > 0)',
    '    outUI <- tagList()',
    '    for (v in seq_along(input$var_binary)){',
    '      med <- stats::quantile(out[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = TRUE)',
    '      outUI[[v]] <- splitLayout(cellWidths = c("25%", "75%"),',
    '                                selectInput(paste0("con_binary", v), paste0("Define reference:"),',
    '                                            choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"',
    '                                ),',
    '                                numericInput(paste0("cut_binary", v), input$var_binary[[v]],',
    '                                             value = med[2], min = med[1], max = med[3]',
    '                                )',
    '      )',
    '    }',
    '    outUI',
    '  })',
    '})',

    'observeEvent(input$check_ref, {',
    '  var.factor <- factor_vars',
    '  output$ref_var <- renderUI({',
    '    req(input$check_ref == TRUE)',
    '    selectInput("var_ref", "Variables to change reference",',
    '                choices = var.factor, multiple = TRUE,',
    '                selected = var.factor[1])',
    '  })',

    '  output$ref_val <- renderUI({',
    '    req(input$check_ref == TRUE)',
    '    req(length(input$var_ref) > 0)',
    '    outUI <- tagList()',
    '    for (v in seq_along(input$var_ref)){',
    '      outUI[[v]] <- selectInput(paste0("con_ref", v), paste0("Reference: ", input$var_ref[[v]]),',
    '                                choices = levels(factor(out[[input$var_ref[[v]]]])), selected = levels(factor(out[[input$var_ref[[v]]]]))[2])',
    '    }',
    '    outUI',
    '  })',
    '})',

    'observeEvent(input$check_subset, {',
    '  output$subset_var <- renderUI({',
    '    req(input$check_subset == TRUE)',
    '    tagList(',
    '      selectInput("var_subset", "Subset variables",',
    '                  choices = names(out), multiple = TRUE,',
    '                  selected = "Rheumatic_0_Degenerative_1")',
    '    )',
    '  })',

    '  output$subset_val <- renderUI({',
    '    req(input$check_subset == TRUE)',
    '    req(length(input$var_subset) > 0)',
    '    var.factor <- factor_vars',
    '    outUI <- tagList()',
    '    for (v in seq_along(input$var_subset)){',
    '      if (input$var_subset[[v]] %in% var.factor){',
    '        varlevel <- levels(as.factor(out[[input$var_subset[[v]]]]))',
    '        outUI[[v]] <- selectInput(paste0("val_subset", v), paste0("Subset value: ", input$var_subset[[v]]),',
    '                                  choices = varlevel, multiple = TRUE,',
    '                                  selected = varlevel[2])',
    '      } else{',
    '        val <- stats::quantile(out[[input$var_subset[[v]]]], na.rm = TRUE)',
    '        outUI[[v]] <- sliderInput(paste0("val_subset", v), paste0("Subset range: ", input$var_subset[[v]]),',
    '                                  min = val[1], max = val[5],',
    '                                  value = c(val[2], val[4]))',
    '      }',
    '    }',
    '    outUI',
    '  })',
    '})',

    'data.info <- reactive({',
    '  out1 <- as.data.table(out[, .SD])', # Ensure out is treated as data.table
    '  out1[, (conti_vars) := lapply(.SD, as.numeric), .SDcols = conti_vars]',
    '  out.label1 <- as.data.table(out.label[, .SD])',

    '  if (!is.null(input$factor_vname)) {',
    '    out1[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols = input$factor_vname]',
    '    out.label1 <- rbind(out.label1[!(variable %in% input$factor_vname)], mk.lev(out1[, .SD, .SDcols = input$factor_vname]))',
    '  }',

    '  req(!is.null(input$check_binary))',
    '  if (input$check_binary == TRUE) {',
    '    validate(need(length(input$var_binary) > 0 , "No variables to dichotomize"))',
    '    sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")',
    '    names(sym.ineq) <- sym.ineq[4:1]',
    '    sym.ineq2 <- c("le", "ge", "l", "g")',
    '    names(sym.ineq2) <- sym.ineq',
    '    for (v in seq_along(input$var_binary)) {',
    '      req(input[[paste0("con_binary", v)]])',
    '      req(input[[paste0("cut_binary", v)]])',
    '      if (input[[paste0("con_binary", v)]] == "\u2264") {',
    '        out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) <= input[[paste0("cut_binary", v)]]))]',
    '      } else if (input[[paste0("con_binary", v)]] == "\u2265") {',
    '        out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) >= input[[paste0("cut_binary", v)]]))]',
    '      } else if (input[[paste0("con_binary", v)]] == "\u003c") {',
    '        out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) < input[[paste0("cut_binary", v)]]))]',
    '      } else {',
    '        out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) > input[[paste0("cut_binary", v)]]))]',
    '      }',

    '      cn.new <- paste0(input$var_binary[[v]], "_group_", sym.ineq2[input[[paste0("con_binary", v)]]], input[[paste0("cut_binary", v)]])',
    '      setnames(out1, "BinaryGroupRandom", cn.new)',

    '      label.binary <- mk.lev(out1[, .SD, .SDcols = cn.new])',
    '      label.binary <- as.data.table(label.binary)',
    '      label.binary[, var_label := paste0(input$var_binary[[v]], " _group")]',
    '      label.binary[, val_label := paste0(c(input[[paste0("con_binary", v)]], sym.ineq[input[[paste0("con_binary", v)]]]), " ", input[[paste0("cut_binary", v)]])]',
    '      out.label1 <- rbind(out.label1, label.binary)',
    '    }',
    '  }',

    '  if (!is.null(input$check_ref)) {',
    '    if (input$check_ref) {',
    '      validate(need(length(input$var_ref) > 0 , "No variables to change reference"))',
    '      for (v in seq_along(input$var_ref)) {',
    '        req(input[[paste0("con_ref", v)]])',
    '        out1[[input$var_ref[[v]]]] <- stats::relevel(out1[[input$var_ref[[v]]]], ref = input[[paste0("con_ref", v)]])',
    '        out.label1[variable == input$var_ref[[v]], ":="(level = levels(out1[[input$var_ref[[v]]]]), val_label = levels(out1[[input$var_ref[[v]]]]))]',
    '      }',
    '    }',
    '  }',

    '  if (!is.null(input$check_subset)) {',
    '    if (input$check_subset) {',
    '      validate(need(length(input$var_subset) > 0 , "No variables for subsetting"),',
    '               need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting"))',
    '      var.factor <- factor_vars',
    '      for (v in seq_along(input$var_subset)) {',
    '        if (input$var_subset[[v]] %in% var.factor) {',
    '          out1 <- out1[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]',
    '          out1 <- as.data.table(out1)',
    '          out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]',
    '          out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]',
    '          setkey(out.label1, "variable", "class", "level")',
    '          setkey(out.label2, "variable", "class", "level")',
    '          out.label1 <- out.label1[out.label2]',
    '        } else {',
    '          out1 <- out1[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]',
    '          out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]',
    '          out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]',
    '          setkey(out.label1, "variable", "class", "level")',
    '          setkey(out.label2, "variable", "class", "level")',
    '          out.label1 <- out.label1[out.label2]',
    '        }',
    '      }',
    '    }',
    '  }',

    '  return(list(data = out1, label = out.label1))',
    '})',

    'data <- reactive({',
    '  data.info()$data',
    '})',

    'data.label <- reactive(data.info()$label)',

    'varlist_new <- reactive(list(New = setdiff(names(data()), names(out))))',

    'output$data <- renderDT({',
    '  datatable(data(), rownames = FALSE, editable = FALSE, caption = "Data",',
    '            options = c(jstable::opt.data("data"), list(scrollX = TRUE)))',
    '})',

    'output$data_label <- renderDT({',
    '  datatable(data.label(), rownames = FALSE, editable = FALSE, extensions = "Buttons", caption = "Label of data",',
    '            options = c(jstable::opt.data("label"), list(scrollX = TRUE)))',
    '})'
  )

  server_parts <- paste(server_parts, collapse = "\n")


  # Table 1
  server_parts <- c(server_parts, '
out_tb1 <- callModule(tb1module2, "tb1",
                      data = data,
                      data_label = data.label,
                      data_varStruct = reactive(c(varlist_new(), varlist)),
                      nfactor.limit = 20,
                      showAllLevels = TRUE)

output$table1 <- renderDT({
  tb <- out_tb1()$table
  cap <- out_tb1()$caption
  out.tb1 <- datatable(tb, rownames = TRUE, extensions = "Buttons", caption = cap,
                       options = c(jstable::opt.tb1("tb1"), list(scrollX = TRUE)))
  if ("sig" %in% colnames(tb)) {
    out.tb1 <- out.tb1 %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
  }
  out.tb1
})
')

  # server_add.
  if ("linear" %in% selected_panels) {
    server_parts <- c(server_parts, '
  out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct =  reactive(c(varlist[c(2, 1)], varlist_new())), default.unires = F, nfactor.limit = nfactor.limit)

  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == "sig")
    datatable(out_linear()$table, rownames=T, extensions= "Buttons", caption = out_linear()$caption,
              options = c(opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = "row",backgroundColor = styleEqual("**", "yellow"))
  })

  output$warning_linear <- renderText({
    paste("<b>", out_linear()$warning, "</b>")
  })
')
  }

  if ("logistic" %in% selected_panels) {
    server_parts <- c(server_parts, '
  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = reactive(c(varlist[c(2, 1)], varlist_new())), nfactor.limit = nfactor.limit, default.unires = F)

  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == "sig")
    datatable(out_logistic()$table, rownames=T, extensions= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          ),
                          list(scrollX = TRUE)
              )
    ) %>% formatStyle("sig", target = "row",backgroundColor = styleEqual("**", "yellow"))
  })
')
  }

  if ("cox" %in% selected_panels) {
    server_parts <- c(server_parts, '
  out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = T, nfactor.limit = nfactor.limit)

    output$coxtable <- renderDT({
      hide <- which(colnames(out_cox()$table) == c("sig"))
      datatable(out_cox()$table,
        rownames = T, extensions = "Buttons", caption = out_cox()$caption,
        options = c(
          opt.tbreg(out_cox()$caption),
          list(columnDefs = list(list(visible = FALSE, targets = hide)))
        )
      ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
    })

')
  }

  if ("basic_plot" %in% selected_panels) {
    server_parts <- c(server_parts, '
    out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$ggpairs_plot <- renderPlot({
      print(out_ggpairs())
    })
')
  }

  if ("histogram" %in% selected_panels) {
    server_parts <- c(server_parts, '
  out_histogram <- histogramServer("histogram", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$histogram <- renderPlot({
      print(out_histogram())
    })
  ')
  }


  if ("scatter" %in% selected_panels) {
    server_parts <- c(server_parts, '
out_scatter <- scatterServer("scatter", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$scatter_plot <- renderPlot({
      print(out_scatter())
    })
')
  }


  if ("box" %in% selected_panels) {
    server_parts <- c(server_parts, '
out_box <- boxServer("box", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$box_plot <- renderPlot({
      print(out_box())
    })
')
  }


  if ("bar" %in% selected_panels) {
    server_parts <- c(server_parts, '
 out_bar <- barServer("bar", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$bar_plot <- renderPlot({
      print(out_bar())
    })
')
  }


  if ("line" %in% selected_panels) {
    server_parts <- c(server_parts, '
out_line <- lineServer("line", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$line_plot <- renderPlot({
      print(out_line())
    })
')
  }


  if ("kaplan" %in% selected_panels) {
    server_parts <- c(server_parts, '
out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$kaplan_plot <- renderPlot({
      print(out_kaplan())
    })
')
  }


  if ("roc" %in% selected_panels) {
    server_parts <- c(server_parts, '
 out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$plot_roc <- renderPlot({
      print(out_roc()$plot)
    })

    output$table_roc <- renderDT({
      datatable(out_roc()$tb,
        rownames = F, editable = F, extensions = "Buttons",
        caption = "ROC results",
        options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
      )
    })
')
  }


  if ("timeroc" %in% selected_panels) {
    server_parts <- c(server_parts, '
out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$plot_timeroc <- renderPlot({
      print(out_timeroc()$plot)
    })

    output$table_timeroc <- renderDT({
      datatable(out_timeroc()$tb,
        rownames = F, editable = F, extensions = "Buttons", caption = "ROC results",
        options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
      )
    })

    output$cut_timeroc <- renderTable(
      {
        out_timeroc()$cut
      },
      caption = "Best cutoff",
      caption.placement = "top"
    )
')
  }



  # further options.


  app_code <- paste('
# app.R ----
library(shiny)
library(shinycustomloader)
library(ggpubr)
library(DT)
library(survival)
library(jstable)
library(shinymanager)
library(officer)

source("global.R")
nfactor.limit <- 20
ui <- navbarPage("Zarathu App",
  theme = bslib::bs_theme(version = 3),
  ',ui_code,'
)



server <- function(input, output, session) {', "\n",
  paste(server_parts, collapse = "\n\n"),
  '

    session$onSessionEnded(function() {
    session$close()
  })
}

shinyApp(ui, server)
', collapse = "\n")

## save
writeLines(global_code, file.path(app_dir, "global.R"))
writeLines(app_code, file.path(app_dir, "app.R"))

output$status <- renderText({
  paste0("App creation complete!\n", app_dir, "\n Included tabs: Data, Table 1, ", paste(selected_panels, collapse = ", "))
})
  })
}

shinyApp(ui, server)
}
