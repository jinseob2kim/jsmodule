source("global.R")
options(shiny.sanitize.errors = F)



ui <- navbarPage(
  "Basic statistics",
  tabPanel("Data",
    icon = icon("table"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("factor"),
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
          h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
          h5("Categorical variables  are summarized with table")
        )
      )
    )
  ),
  navbarMenu("Regression",
    icon = icon("list-alt"),
    tabPanel(
      "Linear regression",
      sidebarLayout(
        sidebarPanel(
          regressModuleUI("linear")
        ),
        mainPanel(
          withLoader(DTOutput("lineartable"), type = "html", loader = "loader6"),
          br(),
          uiOutput("warning_linear")
        )
      )
    ),
    tabPanel(
      "Logistic regression",
      sidebarLayout(
        sidebarPanel(
          regressModuleUI("logistic")
        ),
        mainPanel(
          withLoader(DTOutput("logistictable"), type = "html", loader = "loader6")
        )
      )
    ),
    tabPanel(
      "Cox model",
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
    ),
    tabPanel(
      "Box plot",
      sidebarLayout(
        sidebarPanel(
          boxUI("box")
        ),
        mainPanel(
          optionUI("box"),
          withLoader(plotOutput("box_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("box")
        )
      )
    ),
    tabPanel(
      "Bar plot",
      sidebarLayout(
        sidebarPanel(
          barUI("bar")
        ),
        mainPanel(
          optionUI("bar"),
          withLoader(plotOutput("bar_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("bar")
        )
      )
    ),
    tabPanel(
      "Line plot",
      sidebarLayout(
        sidebarPanel(
          lineUI("line")
        ),
        mainPanel(
          optionUI("line"),
          withLoader(plotOutput("line_plot"), type = "html", loader = "loader6"),
          ggplotdownUI("line")
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

  observeEvent(c(data.list$factor_original, input$factor_vname), {
    output$subset_check <- renderUI({
      checkboxInput("check_subset", "Subset data")
    })
  })

  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      # factor_subset <- c(data.list$factor_original, input$factor_vname)

      # validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      # )

      tagList(
        selectInput("var_subset", "Subset variable",
          choices = names(data.list$data), multiple = F,
          selected = names(data.list$data)[1]
        )
      )
    })

    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(input$var_subset)
      var.factor <- c(data.list$factor_original, input$factor_vname)

      if (input$var_subset %in% var.factor) {
        varlevel <- levels(as.factor(data.list$data[[input$var_subset]]))
        selectInput(session$ns("val_subset"), "Subset value",
          choices = varlevel, multiple = T,
          selected = varlevel[1]
        )
      } else {
        val <- stats::quantile(data.list$data[[input$var_subset]], na.rm = T)
        sliderInput(session$ns("val_subset"), "Subset range",
          min = val[1], max = val[5],
          value = c(val[2], val[4])
        )
      }
    })
  })


  data.info <- reactive({
    out <- data.list$data
    out[, (data.list$conti_original) := lapply(.SD, function(x) {
      as.numeric(as.vector(x))
    }), .SDcols = data.list$conti_original]
    if (!is.null(input$factor_vname)) {
      out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols = input$factor_vname]
    }
    out.label <- mk.lev(out)

    if (!is.null(input$check_subset)) {
      if (input$check_subset) {
        validate(
          need(length(input$var_subset) > 0, "No variables for subsetting")
        )
        var.factor <- c(data.list$factor_original, input$factor_vname)
        # var.conti <- setdiff(data()$conti_original, input$factor_vname)

        if (input$var_subset %in% var.factor) {
          out <- out[get(input$var_subset) %in% input$val_subset]
          # var.factor <- c(data()$factor_original, input$factor_vname)
          out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
          out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
          data.table::setkey(out.label, "variable", "class", "level")
          data.table::setkey(out.label2, "variable", "class", "level")
          out.label <- out.label[out.label2]
        } else {
          out <- out[get(input$var_subset) >= input$val_subset[1] & get(input$var_subset) <= input$val_subset[2]]
          # var.factor <- c(data()$factor_original, input$factor_vname)
          out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
          out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
          data.table::setkey(out.label, "variable", "class", "level")
          data.table::setkey(out.label2, "variable", "class", "level")
          out.label <- out.label[out.label2]
        }
      }
    }

    return(list(data = out, label = out.label))
  })

  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)

  output$data <- renderDT({
    datatable(data(),
      rownames = F, editable = F, extensions = "Buttons", caption = "Data",
      options = c(list(scrollX = TRUE))
    )
  })


  output$data_label <- renderDT({
    datatable(data.label(),
      rownames = F, editable = F, extensions = "Buttons", caption = "Label of data",
      options = c(jstable::opt.data("label"), list(scrollX = TRUE))
    )
  })




  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

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

  out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, default.unires = F)

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

  output$warning_linear <- renderText({
    paste("<b>", out_linear()$warning, "</b>")
  })

  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit, default.unires = F)

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

  out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = NULL, default.unires = F, nfactor.limit = nfactor.limit)

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


  out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL)

  output$ggpairs_plot <- renderPlot({
    print(out_ggpairs())
  })

  out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = NULL, nfactor.limit = nfactor.limit)

  output$kaplan_plot <- renderPlot({
    print(out_kaplan())
  })

  out_box <- boxServer("box", data = data, data_label = data.label, data_varStruct = NULL)

  output$box_plot <- renderPlot({
    print(out_box())
  })

  out_bar <- barServer("bar", data = data, data_label = data.label, data_varStruct = NULL)

  output$bar_plot <- renderPlot({
    print(out_bar())
  })

  out_line <- lineServer("line", data = data, data_label = data.label, data_varStruct = NULL)

  output$line_plot <- renderPlot({
    print(out_line())
  })
}


shinyApp(ui, server)
