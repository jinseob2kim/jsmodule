#' @title jsRepeatedGadget: Shiny Gadget of Repeated measure analysis.
#' @description Shiny Gadget including Data, Label info, Table 1, GEE(linear, logistic), Basic plot
#' @param data data
#' @param nfactor.limit nlevels limit for categorical variables
#' @return Shiny Gadget including Data, Label info, Table 1, GEE(linear, logistic), Basic plot
#' @details Shiny Gadget including Data, Label info, Table 1, GEE(linear, logistic), Basic plot
#' @examples
#' if (interactive()) {
#'   jsRepeatedGadget(mtcars)
#' }
#' @rdname jsRepeatedGadget
#' @export
#' @importFrom GGally ggpairs
#' @importFrom stats as.formula binomial
#' @importFrom data.table data.table := .SD
#' @importFrom DT datatable %>% formatStyle styleEqual renderDT DTOutput
#' @importFrom shinycustomloader withLoader
#' @importFrom jstable opt.data opt.tb1 opt.tbreg
#' @importFrom geepack geeglm
#' @import ggplot2
#' @import shiny

jsRepeatedGadget <- function(data, nfactor.limit = 20) {
  requireNamespace("survival")
  # requireNamespace("survC1")

  ## To remove NOTE.
  val_label <- BinaryGroupRandom <- variable <- NULL

  change.vnlist <- list(
    c(" ", "_"), c("=<", "_le_"), c("=>", "_ge_"), c("=", "_eq_"), c("\\(", "_open_"), c("\\)", "_close_"), c("%", "_percent_"), c("-", "_"), c("/", "_"),
    c("\r\n", "_"), c(",", "_comma_")
  )

  out <- data.table(data, check.names = F)
  name.old <- names(out)
  out <- data.table(data, check.names = T)
  name.new <- names(out)
  # ref <- data.table(name.old = name.old, name.new = name.new);setkey(ref, name.new)
  ref <- list(name.old = name.old, name.new = name.new)

  ## factor variable
  factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
  out[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]
  conti_vars <- setdiff(names(out), factor_vars)
  nclass <- unlist(out[, lapply(.SD, function(x) {
    length(unique(x))
  }), .SDcols = conti_vars])
  # except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
  add_vars <- names(nclass)[nclass >= 1 & nclass <= 5]

  data.list <- list(data = out, factor_original = factor_vars, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= nfactor.limit], factor_adds = add_vars)

  ui <- navbarPage(
    "Repeated measure analysis",
    tabPanel("Data",
      icon = icon("table"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("factor"),
          uiOutput("repeated"),
          uiOutput("binary_check"),
          uiOutput("binary_var"),
          uiOutput("binary_val"),
          uiOutput("ref_check"),
          uiOutput("ref_var"),
          uiOutput("ref_val"),
          uiOutput("subset_check"),
          uiOutput("subset_var"),
          uiOutput("subset_val")
        ),
        mainPanel(
          tabsetPanel(
            type = "pills",
            tabPanel("Data", withLoader(DTOutput("data"), type = "html", loader = "loader6")),
            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type = "html", loader = "loader6"))
          )
        )
      )
    ),
    tabPanel("Table 1",
      icon = icon("percentage"),
      sidebarLayout(
        sidebarPanel(
          tb1moduleUI("tb1")
        ),
        mainPanel(
          withLoader(DTOutput("table1"), type = "html", loader = "loader6"),
          wellPanel(
            h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
            h5("Non-normal continuous variables are summarized with median [IQR or min,max] and wilcox(2 groups)/kruskal-wallis(>3 groups) test"),
            h5("Categorical variables  are summarized with table")
          )
        )
      )
    ),
    navbarMenu("GEE",
      icon = icon("list-alt"),
      tabPanel(
        "Linear",
        sidebarLayout(
          sidebarPanel(
            GEEModuleUI("linear")
          ),
          mainPanel(
            withLoader(DTOutput("lineartable"), type = "html", loader = "loader6")
          )
        )
      ),
      tabPanel(
        "Binomial",
        sidebarLayout(
          sidebarPanel(
            GEEModuleUI("logistic")
          ),
          mainPanel(
            withLoader(DTOutput("logistictable"), type = "html", loader = "loader6")
          )
        )
      ),
      tabPanel(
        "Marginal cox model",
        sidebarLayout(
          sidebarPanel(
            coxUI("cox")
          ),
          mainPanel(
            withLoader(DTOutput("coxtable"), type = "html", loader = "loader6")
          )
        )
      )
    ),
    navbarMenu("Plot",
      icon = icon("bar-chart-o"),
      tabPanel(
        "Scatter plot",
        sidebarLayout(
          sidebarPanel(
            ggpairsModuleUI1("ggpairs")
          ),
          mainPanel(
            withLoader(plotOutput("ggpairs_plot"), type = "html", loader = "loader6"),
            ggpairsModuleUI2("ggpairs")
          )
        )
      ),
      tabPanel(
        "Kaplan-meier plot",
        sidebarLayout(
          sidebarPanel(
            kaplanUI("kaplan")
          ),
          mainPanel(
            optionUI("kaplan"),
            withLoader(plotOutput("kaplan_plot"), type = "html", loader = "loader6"),
            ggplotdownUI("kaplan")
          )
        )
      )
    ),
    navbarMenu("ROC analysis",
      icon = icon("check"),
      tabPanel(
        "ROC",
        sidebarLayout(
          sidebarPanel(
            rocUI("roc")
          ),
          mainPanel(
            withLoader(plotOutput("plot_roc"), type = "html", loader = "loader6"),
            ggplotdownUI("roc"),
            withLoader(DTOutput("table_roc"), type = "html", loader = "loader6")
          )
        )
      ),
      tabPanel(
        "Time-dependent ROC",
        sidebarLayout(
          sidebarPanel(
            timerocUI("timeroc")
          ),
          mainPanel(
            withLoader(plotOutput("plot_timeroc"), type = "html", loader = "loader6"),
            ggplotdownUI("timeroc"),
            withLoader(DTOutput("table_timeroc"), type = "html", loader = "loader6")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$factor <- renderUI({
      selectInput("factor_vname",
        label = "Additional categorical variables",
        choices = data.list$factor_adds_list, multiple = T,
        selected = data.list$factor_adds
      )
    })

    output$repeated <- renderUI({
      selectInput("repeated_vname",
        label = "Repeated measure variables",
        choices = names(data.list$data), multiple = F,
        selected = names(data.list$data)[1]
      )
    })

    observeEvent(c(data.list$factor_original, input$factor_vname, input$repeated_vname), {
      output$binary_check <- renderUI({
        checkboxInput("check_binary", "Make binary variables")
      })

      output$ref_check <- renderUI({
        checkboxInput("check_ref", "Change reference of categorical variables")
      })

      output$subset_check <- renderUI({
        checkboxInput("check_subset", "Subset data")
      })
    })

    observeEvent(input$check_binary, {
      var.conti <- setdiff(names(data.list$data), c(data.list$factor_original, input$factor_vname))
      output$binary_var <- renderUI({
        req(input$check_binary == T)
        selectInput("var_binary", "Variables to dichotomize",
          choices = var.conti, multiple = T,
          selected = var.conti[1]
        )
      })

      output$binary_val <- renderUI({
        req(input$check_binary == T)
        req(length(input$var_binary) > 0)
        outUI <- tagList()
        for (v in seq_along(input$var_binary)) {
          med <- stats::quantile(data.list$data[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = T)
          outUI[[v]] <- splitLayout(
            cellWidths = c("25%", "75%"),
            selectInput(paste0("con_binary", v), paste0("Define reference:"),
              choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"
            ),
            numericInput(paste0("cut_binary", v), input$var_binary[[v]],
              value = med[2], min = med[1], max = med[3]
            )
          )
        }
        outUI
      })
    })

    observeEvent(input$check_ref, {
      var.factor <- c(data.list$factor_original, input$factor_vname)
      output$ref_var <- renderUI({
        req(input$check_ref == T)
        selectInput("var_ref", "Variables to change reference",
          choices = var.factor, multiple = T,
          selected = var.factor[1]
        )
      })

      output$ref_val <- renderUI({
        req(input$check_ref == T)
        req(length(input$var_ref) > 0)
        outUI <- tagList()
        for (v in seq_along(input$var_ref)) {
          outUI[[v]] <- selectInput(paste0("con_ref", v), paste0("Reference: ", input$var_ref[[v]]),
            choices = levels(factor(data.list$data[[input$var_ref[[v]]]])), selected = levels(factor(data.list$data[[input$var_ref[[v]]]]))[2]
          )
        }
        outUI
      })
    })

    observeEvent(input$check_subset, {
      output$subset_var <- renderUI({
        req(input$check_subset == T)
        # factor_subset <- setdiff(c(data.list$factor_original, input$factor_vname), input$repeated_vname)

        # validate(
        #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
        # )

        tagList(
          selectInput("var_subset", "Subset variables",
            choices = names(data.list$data), multiple = T,
            selected = names(data.list$data)[1]
          )
        )
      })

      output$subset_val <- renderUI({
        req(input$check_subset == T)
        req(input$var_subset)
        var.factor <- c(data.list$factor_original, input$factor_vname)

        outUI <- tagList()

        for (v in seq_along(input$var_subset)) {
          if (input$var_subset[[v]] %in% var.factor) {
            varlevel <- levels(as.factor(data.list$data[[input$var_subset[[v]]]]))
            outUI[[v]] <- selectInput(paste0("val_subset", v), paste0("Subset value: ", input$var_subset[[v]]),
              choices = varlevel, multiple = T,
              selected = varlevel[1]
            )
          } else {
            val <- stats::quantile(data.list$data[[input$var_subset[[v]]]], na.rm = T)
            outUI[[v]] <- sliderInput(paste0("val_subset", v), paste0("Subset range: ", input$var_subset[[v]]),
              min = val[1], max = val[5],
              value = c(val[2], val[4])
            )
          }
        }
        outUI
      })
    })

    data.info <- reactive({
      out <- data.table::data.table(data.list$data)
      out[, (data.list$conti_original) := lapply(.SD, function(x) {
        as.numeric(as.vector(x))
      }), .SDcols = data.list$conti_original]
      if (!is.null(input$factor_vname)) {
        out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols = input$factor_vname]
      }
      out.label <- mk.lev(out)

      req(!is.null(input$check_binary))
      if (input$check_binary == T) {
        validate(
          need(length(input$var_binary) > 0, "No variables to dichotomize")
        )
        sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
        names(sym.ineq) <- sym.ineq[4:1]
        sym.ineq2 <- c("le", "ge", "l", "g")
        names(sym.ineq2) <- sym.ineq
        for (v in seq_along(input$var_binary)) {
          req(input[[paste0("con_binary", v)]])
          req(input[[paste0("cut_binary", v)]])
          if (input[[paste0("con_binary", v)]] == "\u2264") {
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) <= input[[paste0("cut_binary", v)]]))]
          } else if (input[[paste0("con_binary", v)]] == "\u2265") {
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) >= input[[paste0("cut_binary", v)]]))]
          } else if (input[[paste0("con_binary", v)]] == "\u003c") {
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) < input[[paste0("cut_binary", v)]]))]
          } else {
            out[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) > input[[paste0("cut_binary", v)]]))]
          }

          cn.new <- paste0(input$var_binary[[v]], "_group_", sym.ineq2[input[[paste0("con_binary", v)]]], input[[paste0("cut_binary", v)]])
          data.table::setnames(out, "BinaryGroupRandom", cn.new)

          label.binary <- mk.lev(out[, .SD, .SDcols = cn.new])
          label.binary[, var_label := paste0(input$var_binary[[v]], " _group")]
          label.binary[, val_label := paste0(c(input[[paste0("con_binary", v)]], sym.ineq[input[[paste0("con_binary", v)]]]), " ", input[[paste0("cut_binary", v)]])]
          out.label <- rbind(out.label, label.binary)
        }
      }

      if (!is.null(input$check_ref)) {
        if (input$check_ref) {
          validate(
            need(length(input$var_ref) > 0, "No variables to change reference")
          )
          for (v in seq_along(input$var_ref)) {
            req(input[[paste0("con_ref", v)]])
            out[[input$var_ref[[v]]]] <- stats::relevel(out[[input$var_ref[[v]]]], ref = input[[paste0("con_ref", v)]])
            out.label[variable == input$var_ref[[v]], ":="(level = levels(out[[input$var_ref[[v]]]]), val_label = levels(out[[input$var_ref[[v]]]]))]
          }
        }
      }

      if (!is.null(input$check_subset)) {
        if (input$check_subset) {
          validate(
            need(length(input$var_subset) > 0, "No variables for subsetting"),
            need(all(sapply(1:length(input$var_subset), function(x) {
              length(input[[paste0("val_subset", x)]])
            })), "No value for subsetting")
          )
          var.factor <- c(data.list$factor_original, input$factor_vname)
          # var.conti <- setdiff(data()$conti_original, input$factor_vname)

          for (v in seq_along(input$var_subset)) {
            if (input$var_subset[[v]] %in% var.factor) {
              out <- out[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
              # var.factor <- c(data()$factor_original, input$factor_vname)
              out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
              out.label2 <- mk.lev(out)[, c("variable", "level")]
              data.table::setkey(out.label, "variable", "level")
              data.table::setkey(out.label2, "variable", "level")
              out.label <- out.label[out.label2]
            } else {
              out <- out[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
              # var.factor <- c(data()$factor_original, input$factor_vname)
              out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
              out.label2 <- mk.lev(out)[, c("variable", "level")]
              data.table::setkey(out.label, "variable", "level")
              data.table::setkey(out.label2, "variable", "level")
              out.label <- out.label[out.label2]
            }
          }
        }
      }

      for (vn in ref[["name.new"]]) {
        w <- which(ref[["name.new"]] == vn)
        out.label[variable == vn, var_label := ref[["name.old"]][w]]
      }

      return(list(data = out, label = out.label))
    })

    data <- reactive(data.info()$data)
    data.label <- reactive(data.info()$label)
    id.gee <- reactive(input$repeated_vname)

    output$data <- renderDT({
      datatable(data(),
        rownames = F, editable = F, extensions = "Buttons", caption = "Data",
        options = c(jstable::opt.data("data"), list(scrollX = TRUE))
      )
    })

    output$data_label <- renderDT({
      datatable(data.label(),
        rownames = F, editable = F, extensions = "Buttons", caption = "Label of data",
        options = c(jstable::opt.data("label"), list(scrollX = TRUE))
      )
    })

    out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, showAllLevels = T)

    output$table1 <- renderDT({
      tb <- out_tb1()$table
      cap <- out_tb1()$caption
      out.tb1 <- datatable(tb,
        rownames = T, extensions = "Buttons", caption = cap,
        options = c(
          jstable::opt.tb1("tb1"),
          list(columnDefs = list(list(visible = FALSE, targets = which(colnames(tb) %in% c("test", "sig"))))),
          list(scrollX = TRUE)
        )
      )
      if ("sig" %in% colnames(tb)) {
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
      }
      return(out.tb1)
    })

    out_linear <- callModule(GEEModuleLinear, "linear", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, id.gee = id.gee)

    output$lineartable <- renderDT({
      hide <- which(colnames(out_linear()$table) == "sig")
      datatable(out_linear()$table,
        rownames = T, extensions = "Buttons", caption = out_linear()$caption,
        options = c(
          jstable::opt.tbreg(out_linear()$caption),
          list(columnDefs = list(list(visible = FALSE, targets = hide))),
          list(scrollX = TRUE)
        )
      ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
    })

    out_logistic <- callModule(GEEModuleLogistic, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, id.gee = id.gee)

    output$logistictable <- renderDT({
      hide <- which(colnames(out_logistic()$table) == "sig")
      datatable(out_logistic()$table,
        rownames = T, extensions = "Buttons", caption = out_logistic()$caption,
        options = c(
          jstable::opt.tbreg(out_logistic()$caption),
          list(columnDefs = list(list(visible = FALSE, targets = hide))),
          list(scrollX = TRUE)
        )
      ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
    })

    out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, default.unires = T, id.cluster = id.gee)

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

    out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$ggpairs_plot <- renderPlot({
      print(out_ggpairs())
    })

    out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, nfactor.limit = nfactor.limit, data_varStruct = NULL, id.cluster = id.gee)

    output$kaplan_plot <- renderPlot({
      print(out_kaplan())
    })

    out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)

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

    out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)

    output$plot_timeroc <- renderPlot({
      print(out_timeroc()$plot)
    })

    output$table_timeroc <- renderDT({
      datatable(out_timeroc()$tb,
        rownames = F, editable = F, extensions = "Buttons", caption = "ROC results",
        options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
      )
    })

    session$onSessionEnded(function() {
      stopApp()
    })
  }

  # viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}

#' @title jsRepeatedAddin: Rstudio addin of jsRepeatedGadget
#' @description Rstudio addin of jsRepeatedGadget
#' @return Rstudio addin of jsRepeatedGadget
#' @details Rstudio addin of jsRepeatedGadget
#' @examples
#' if (interactive()) {
#'   jsRepeatedAddin()
#' }
#' @seealso
#'  \code{\link[rstudioapi]{rstudio-editors}}
#' @rdname jsRepeatedAddin
#' @export
#' @importFrom rstudioapi getActiveDocumentContext

jsRepeatedAddin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  # Set the default data to use based on the selection.
  dataString <- context$selection[[1]]$text
  data <- get(dataString, envir = .GlobalEnv)
  # viewer <- dialogViewer("Subset", width = 1000, height = 800)
  jsRepeatedGadget(data, nfactor.limit = 20)
}

#' @title jsRepeatedExtAddin: RStudio Addin for repeated measure analysis with external data.
#' @description RStudio Addin for repeated measure analysis with external csv/xlsx/sas7bdat/sav/dta file.
#' @param nfactor.limit nlevels limit for categorical variables, Default: 20
#' @param max.filesize Maximum file size to upload (MB), Default: 2048 (2 GB)
#' @return RStudio Addin for repeated measure analysis with external data.
#' @details RStudio Addin for repeated measure analysis with external csv/xlsx/sas7bdat/sav/dta file.
#' @examples
#' if (interactive()) {
#'   jsRepeatedExtAddin()
#' }
#' @seealso
#'  \code{\link[data.table]{fwrite}}
#'  \code{\link[survival]{colon}}
#'  \code{\link[jstable]{opt.tbreg}}
#' @rdname jsRepeatedExtAddin
#' @export
#' @importFrom data.table fwrite
#' @importFrom jstable opt.tbreg
#' @importFrom DT datatable %>% formatStyle styleEqual renderDT DTOutput
#' @importFrom shinycustomloader withLoader
#' @import shiny
#' @importFrom shinyjs useShinyjs click
#' @import flextable

jsRepeatedExtAddin <- function(nfactor.limit = 20, max.filesize = 2048) {
  options(shiny.maxRequestSize = max.filesize * 1024^2)

  ui <- navbarPage(
    "Repeated measure analysis",
    header = tagList(
      shinyjs::useShinyjs()
    ),
    inverse = TRUE,
    tabPanel("Data",
      icon = icon("table"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("import"),
          downloadButton("downloadData", "Example data")
        ),
        mainPanel(
          tabsetPanel(
            type = "pills",
            tabPanel("Data", withLoader(DTOutput("data"), type = "html", loader = "loader6")),
            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type = "html", loader = "loader6"))
          ),
          htmlOutput("naomit")
        )
      )
    ),
    tabPanel("Table 1",
      icon = icon("percentage"),
      sidebarLayout(
        sidebarPanel(
          tb1moduleUI("tb1")
        ),
        mainPanel(
          downloadButton(outputId = "dl.table1", style = "display:none;"),
          actionButton("dl.table1.clk", NULL, style = "display:none;"),
          withLoader(DTOutput("table1"), type = "html", loader = "loader6"),
          wellPanel(
            h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
            h5("Non-normal continuous variables are summarized with median [IQR or min,max] and wilcox(2 groups)/kruskal-wallis(>3 groups) test"),
            h5("Categorical variables  are summarized with table")
          )
        )
      )
    ),
    navbarMenu("GEE",
      icon = icon("list-alt"),
      tabPanel(
        "Linear",
        sidebarLayout(
          sidebarPanel(
            GEEModuleUI("linear")
          ),
          mainPanel(
            downloadButton(outputId = "dl.lingee", style = "display:none;"),
            actionButton("dl.lingee.clk", NULL, style = "display:none;"),
            withLoader(DTOutput("lineartable"), type = "html", loader = "loader6")
          )
        )
      ),
      tabPanel(
        "Binomial",
        sidebarLayout(
          sidebarPanel(
            GEEModuleUI("logistic")
          ),
          mainPanel(
            downloadButton(outputId = "dl.loggee", style = "display:none;"),
            actionButton("dl.loggee.clk", NULL, style = "display:none;"),
            withLoader(DTOutput("logistictable"), type = "html", loader = "loader6")
          )
        )
      ),
      tabPanel(
        "Marginal cox model",
        sidebarLayout(
          sidebarPanel(
            coxUI("cox")
          ),
          mainPanel(
            downloadButton(outputId = "dl.coxgee", style = "display:none;"),
            actionButton("dl.coxgee.clk", NULL, style = "display:none;"),
            withLoader(DTOutput("coxtable"), type = "html", loader = "loader6")
          )
        )
      )
    ),
    navbarMenu("Plot",
      icon = icon("chart-column"),
      tabPanel(
        "Scatter plot",
        sidebarLayout(
          sidebarPanel(
            ggpairsModuleUI1("ggpairs")
          ),
          mainPanel(
            withLoader(plotOutput("ggpairs_plot"), type = "html", loader = "loader6"),
            ggpairsModuleUI2("ggpairs")
          )
        )
      ),
      tabPanel(
        "Kaplan-meier plot",
        sidebarLayout(
          sidebarPanel(
            kaplanUI("kaplan")
          ),
          mainPanel(
            optionUI("kaplan"),
            withLoader(plotOutput("kaplan_plot"), type = "html", loader = "loader6"),
            ggplotdownUI("kaplan")
          )
        )
      )
    ),
    navbarMenu("ROC analysis",
      icon = icon("check"),
      tabPanel(
        "ROC",
        sidebarLayout(
          sidebarPanel(
            rocUI("roc")
          ),
          mainPanel(
            withLoader(plotOutput("plot_roc"), type = "html", loader = "loader6"),
            ggplotdownUI("roc"),
            withLoader(DTOutput("table_roc"), type = "html", loader = "loader6")
          )
        )
      ),
      tabPanel(
        "Time-dependent ROC",
        sidebarLayout(
          sidebarPanel(
            timerocUI("timeroc")
          ),
          mainPanel(
            withLoader(plotOutput("plot_timeroc"), type = "html", loader = "loader6"),
            ggplotdownUI("timeroc"),
            withLoader(DTOutput("table_timeroc"), type = "html", loader = "loader6")
          )
        )
      )
    ),
    navbarMenu(
      title = "Subgroup analysis",
      icon = icon("chart-bar"),
      tabPanel(
        title = "subgroup marginal cox",
        sidebarLayout(
          sidebarPanel(
            forestcoxUI("Forest")
          ),
          mainPanel(
            tabsetPanel(
              type = "pills",
              tabPanel(
                title = "Data",
                withLoader(
                  DTOutput("tablesub"),
                  type = "html",
                  loader = "loader6"
                )
              ),
              tabPanel(
                title = "figure",
                plotOutput("forestplot", width = "100%"),
                ggplotdownUI("Forest")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "subgroup linear mixed model",
        sidebarLayout(
          sidebarPanel(
            forestglmUI("Forest_glm")
          ),
          mainPanel(
            tabsetPanel(
              type = "pills",
              tabPanel(
                title = "Data",
                withLoader(
                  DTOutput("tablesub_glm"),
                  type = "html",
                  loader = "loader6"
                )
              ),
              tabPanel(
                title = "figure",
                plotOutput("forestplot_glm", width = "100%"),
                ggplotdownUI("Forest_glm")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "subgroup logistic GLMM",
        sidebarLayout(
          sidebarPanel(
            forestglmUI("Forest_glmbi")
          ),
          mainPanel(
            tabsetPanel(
              type = "pills",
              tabPanel(
                title = "Data",
                withLoader(
                  DTOutput("tablesub_glmbi"),
                  type = "html",
                  loader = "loader6"
                )
              ),
              tabPanel(
                title = "figure",
                plotOutput("forestplot_glmbi", width = "100%"),
                ggplotdownUI("Forest_glmbi")
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("example_repeated", ".csv", sep = "")
      },
      content = function(file) {
        data.table::fwrite(survival::colon[, -2], file)
      }
    )

    output$import <- renderUI({
      FileRepeatedInput("datafile")
    })

    data.info <- callModule(FileRepeated, "datafile", nfactor.limit = nfactor.limit)
    data <- reactive(data.info()$data)
    data.label <- reactive(data.info()$label)
    id.gee <- reactive(data.info()$id.gee)

    output$data <- renderDT({
      datatable(data(),
        rownames = F, editable = F, extensions = "Buttons", caption = "Data",
        options = c(opt.data("data"), list(scrollX = TRUE))
      )
    })

    output$data_label <- renderDT({
      datatable(data.label(),
        rownames = F, editable = F, extensions = "Buttons", caption = "Label of data",
        options = c(opt.data("label"), list(scrollX = TRUE))
      )
    })

    output$naomit <- renderText({
      data.info()$naomit
    })

    out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    observeEvent(input$dl.table1.clk, {
      shinyjs::click(id = "dl.table1")
    })

    output$dl.table1 <- downloadHandler(
      filename = "table1.docx",
      content = function(file) {
        tb <- out_tb1()$table
        rn <- rownames(tb)
        tb <- cbind(rn, data.frame(tb))
        colnames(tb)[1] <- " "

        officer::read_docx() %>%
          body_add_flextable(
            tb %>%
              flextable() %>%
              autofit() %>%
              theme_booktabs(bold_header = TRUE)
          ) %>%
          print(target = file)
      }
    )

    outputOptions(output, "dl.table1", suspendWhenHidden = FALSE)

    output$table1 <- renderDT({
      tb <- out_tb1()$table
      cap <- out_tb1()$caption
      out.tb1 <- datatable(tb,
        rownames = T, extensions = "Buttons", caption = cap,
        options = c(
          list(
            dom = "<lf<rt>Bip>",
            lengthMenu = list(
              c(10, 25, -1),
              c("10", "25", "All")
            ),
            pageLength = 25,
            ordering = F,
            buttons = list(
              "copy",
              "print",
              list(
                text = "Download",
                extend = "collection",
                buttons = list(
                  list(extend = "csv", filename = "tb1"),
                  list(extend = "excel", filename = "tb1"),
                  list(extend = "pdf", filename = "tb1")
                ) # ,
              ),
              list(
                text = "Word",
                extend = "collection",
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                  Shiny.setInputValue('dl.table1.clk', true, {priority: 'event'});
                  }"
                )
              )
            )
          ),
          list(columnDefs = list(list(visible = FALSE, targets = which(colnames(tb) %in% c("test", "sig"))))),
          list(scrollX = TRUE)
        )
      )
      if ("sig" %in% colnames(tb)) {
        out.tb1 <- out.tb1 %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
      }
      return(out.tb1)
    })

    out_linear <- callModule(GEEModuleLinear, "linear", data = data, data_label = data.label, data_varStruct = NULL, id.gee = id.gee, nfactor.limit = nfactor.limit)

    observeEvent(input$dl.lingee.clk, {
      shinyjs::click(id = "dl.lingee")
    })

    output$dl.lingee <- downloadHandler(
      filename = "lingee.docx",
      content = function(file) {
        tb <- out_linear()$table
        rn <- rownames(tb)
        cn <- colnames(tb)
        tb <- cbind(rn, data.frame(tb))
        colnames(tb) <- c(" ", cn)

        officer::read_docx() %>%
          body_add_flextable(
            tb %>%
              flextable() %>%
              autofit() %>%
              theme_booktabs(bold_header = TRUE)
          ) %>%
          print(target = file)
      }
    )

    outputOptions(output, "dl.lingee", suspendWhenHidden = FALSE)

    output$lineartable <- renderDT({
      hide <- which(colnames(out_linear()$table) == "sig")
      datatable(out_linear()$table,
        rownames = T, extensions = "Buttons", caption = out_linear()$caption,
        options = c(
          list(
            dom = "<lf<rt>Bip>",
            lengthMenu = list(
              c(10, 25, -1),
              c("10", "25", "All")
            ),
            pageLength = 25,
            ordering = F,
            buttons = list(
              "copy",
              "print",
              list(
                text = "Download",
                extend = "collection",
                buttons = list(
                  list(extend = "csv", filename = out_linear()$caption),
                  list(extend = "excel", filename = out_linear()$caption),
                  list(extend = "pdf", filename = out_linear()$caption)
                ) # ,
              ),
              list(
                text = "Word",
                extend = "collection",
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                  Shiny.setInputValue('dl.lingee.clk', true, {priority: 'event'});
                  }"
                )
              )
            )
          ),
          list(columnDefs = list(list(visible = FALSE, targets = hide))),
          list(scrollX = TRUE)
        )
      ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
    })

    out_logistic <- callModule(GEEModuleLogistic, "logistic", data = data, data_label = data.label, data_varStruct = NULL, id.gee = id.gee, nfactor.limit = nfactor.limit)

    observeEvent(input$dl.loggee.clk, {
      shinyjs::click(id = "dl.loggee")
    })

    output$dl.loggee <- downloadHandler(
      filename = "loggee.docx",
      content = function(file) {
        tb <- out_logistic()$table
        rn <- rownames(tb)
        cn <- colnames(tb)
        tb <- cbind(rn, data.frame(tb))
        colnames(tb) <- c(" ", cn)

        officer::read_docx() %>%
          body_add_flextable(
            tb %>%
              flextable() %>%
              autofit() %>%
              theme_booktabs(bold_header = TRUE)
          ) %>%
          print(target = file)
      }
    )

    outputOptions(output, "dl.loggee", suspendWhenHidden = FALSE)

    output$logistictable <- renderDT({
      hide <- which(colnames(out_logistic()$table) == "sig")
      datatable(out_logistic()$table,
        rownames = T, extensions = "Buttons", caption = out_logistic()$caption,
        options = c(
          list(
            dom = "<lf<rt>Bip>",
            lengthMenu = list(
              c(10, 25, -1),
              c("10", "25", "All")
            ),
            pageLength = 25,
            ordering = F,
            buttons = list(
              "copy",
              "print",
              list(
                text = "Download",
                extend = "collection",
                buttons = list(
                  list(extend = "csv", filename = out_logistic()$caption),
                  list(extend = "excel", filename = out_logistic()$caption),
                  list(extend = "pdf", filename = out_logistic()$caption)
                ) # ,
              ),
              list(
                text = "Word",
                extend = "collection",
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                  Shiny.setInputValue('dl.loggee.clk', true, {priority: 'event'});
                  }"
                )
              )
            )
          ),
          list(columnDefs = list(list(visible = FALSE, targets = hide))),
          list(scrollX = TRUE)
        )
      ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
    })

    out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = T, id.cluster = id.gee, nfactor.limit = nfactor.limit)

    observeEvent(input$dl.coxgee.clk, {
      shinyjs::click(id = "dl.coxgee")
    })

    output$dl.coxgee <- downloadHandler(
      filename = "coxgee.docx",
      content = function(file) {
        tb <- out_cox()$table
        rn <- rownames(tb)
        cn <- colnames(tb)
        tb <- cbind(rn, data.frame(tb))
        colnames(tb) <- c(" ", cn)

        officer::read_docx() %>%
          body_add_flextable(
            tb %>%
              flextable() %>%
              autofit() %>%
              theme_booktabs(bold_header = TRUE)
          ) %>%
          print(target = file)
      }
    )

    outputOptions(output, "dl.coxgee", suspendWhenHidden = FALSE)

    output$coxtable <- renderDT({
      hide <- which(colnames(out_cox()$table) == c("sig"))
      datatable(out_cox()$table,
        rownames = T, extensions = "Buttons", caption = out_cox()$caption,
        options = c(
          list(
            dom = "<lf<rt>Bip>",
            lengthMenu = list(
              c(10, 25, -1),
              c("10", "25", "All")
            ),
            pageLength = 25,
            ordering = F,
            buttons = list(
              "copy",
              "print",
              list(
                text = "Download",
                extend = "collection",
                buttons = list(
                  list(extend = "csv", filename = out_cox()$caption),
                  list(extend = "excel", filename = out_cox()$caption),
                  list(extend = "pdf", filename = out_cox()$caption)
                ) # ,
              ),
              list(
                text = "Word",
                extend = "collection",
                action = DT::JS(
                  "function ( e, dt, node, config ) {
                  Shiny.setInputValue('dl.coxgee.clk', true, {priority: 'event'});
                  }"
                )
              )
            )
          ),
          list(columnDefs = list(list(visible = FALSE, targets = hide)))
        )
      ) %>% formatStyle("sig", target = "row", backgroundColor = styleEqual("**", "yellow"))
    })

    out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

    output$ggpairs_plot <- renderPlot({
      print(out_ggpairs())
    })

    out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)

    output$kaplan_plot <- renderPlot({
      print(out_kaplan())
    })

    out_roc <- callModule(rocModule, "roc", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)

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

    out_timeroc <- callModule(timerocModule, "timeroc", data = data, data_label = data.label, data_varStruct = NULL, id.cluster = id.gee, nfactor.limit = nfactor.limit)

    output$plot_timeroc <- renderPlot({
      print(out_timeroc()$plot)
    })

    output$table_timeroc <- renderDT({
      datatable(out_timeroc()$tb,
        rownames = F, editable = F, extensions = "Buttons", caption = "ROC results",
        options = c(jstable::opt.tbreg("roctable"), list(scrollX = TRUE))
      )
    })


    observe({
      outtable <- forestcoxServer(
        id = "Forest",
        data = data,
        data_label = data.label,
        cluster_id = id.gee() # Reactive 값 호출
      )

      output$tablesub <- renderDT({
        outtable()[[1]]
      })

      output$forestplot <- renderPlot({
        outtable()[[2]]
      })
    })

    observe({
      outtable_glm <- forestglmServer("Forest_glm", data = data, data_label = data.label, family = "gaussian", repeated_id = id.gee())
      output$tablesub_glm <- renderDT({
        outtable_glm()[[1]]
      })
      output$forestplot_glm <- renderPlot({
        outtable_glm()[[2]]
      })
    })

    observe({
      outtable_glmbi <- forestglmServer("Forest_glmbi", data = data, data_label = data.label, family = "binomial", repeated_id = id.gee())
      output$tablesub_glmbi <- renderDT({
        outtable_glmbi()[[1]]
      })
      output$forestplot_glmbi <- renderPlot({
        outtable_glmbi()[[2]]
      })
    })
  }

  # viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}
