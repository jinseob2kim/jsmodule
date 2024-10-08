#' @title FileRepeatedInput: File upload UI for repeated measure analysis.
#' @description File upload UI for repeated measure analysis.
#' @param id id
#' @param label label, Default: 'csv/xlsx/sav/sas7bdat/dta file'
#' @return File upload UI for repeated measure analysis.
#' @details File upload UI for repeated measure analysis.
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(readxl)
#' library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       FileRepeatedInput("datafile")
#'     ),
#'     mainPanel(
#'       tabsetPanel(
#'         type = "pills",
#'         tabPanel("Data", DTOutput("data")),
#'         tabPanel("Label", DTOutput("data_label", width = "100%"))
#'       )
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- callModule(FileRepeated, "datafile")
#'
#'   output$data <- renderDT({
#'     data()$data
#'   })
#'
#'   output$label <- renderDT({
#'     data()$label
#'   })
#' }
#' @rdname FileRepeatedInput
#' @export
#' @import shiny

FileRepeatedInput <- function(id, label = "Upload data (csv/xlsx/sav/sas7bdat/dta)") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label),
    uiOutput(ns("factor")),
    uiOutput(ns("repeated")),
    uiOutput(ns("binary_check")),
    uiOutput(ns("binary_var")),
    uiOutput(ns("binary_val")),
    uiOutput(ns("ref_check")),
    uiOutput(ns("ref_var")),
    uiOutput(ns("ref_val")),
    uiOutput(ns("subset_check")),
    uiOutput(ns("subset_var")),
    uiOutput(ns("subset_val"))
  )
}



#' @title FileRepeated: File upload server module for repeated measure analysis.
#' @description File upload server module for repeated measure analysis.
#' @param input input
#' @param output output
#' @param session session
#' @param nfactor.limit nfactor limit to include, Default: 20
#' @return File upload server module for repeated measure analysis.
#' @details File upload server module for repeated measure analysis.
#' @examples
#' library(shiny)
#' library(DT)
#' library(data.table)
#' library(readxl)
#' library(jstable)
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       FileRepeatedInput("datafile")
#'     ),
#'     mainPanel(
#'       tabsetPanel(
#'         type = "pills",
#'         tabPanel("Data", DTOutput("data")),
#'         tabPanel("Label", DTOutput("data_label", width = "100%"))
#'       )
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- callModule(FileRepeated, "datafile")
#'
#'   output$data <- renderDT({
#'     data()$data
#'   })
#'
#'   output$label <- renderDT({
#'     data()$label
#'   })
#' }
#' @rdname FileRepeated
#' @export
#' @import shiny
#' @importFrom data.table fread data.table .SD :=
#' @importFrom readxl read_excel
#' @importFrom readr guess_encoding
#' @importFrom utils read.csv
#' @importFrom jstable mk.lev
#' @importFrom haven read_sav read_sas read_dta


FileRepeated <- function(input, output, session, nfactor.limit = 20) {
  ## To remove NOTE.
  val_label <- BinaryGroupRandom <- variable <- NULL

  # The selected file, if any
  userFile <- eventReactive(input$file, {
    # If no file is selected, don't do anything
    # validate(need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  # change.vnlist = list(c(" ", "_"), c("=<", "_le_"), c("=>", "_ge_"), c("=", "_eq_"), c("\\(", "_open_"), c("\\)", "_close_"), c("%", "_percent_"), c("-", "_"),  c("/", "_"),
  #                     c("\r\n", "_"), c(",", "_comma_")
  # )

  data <- eventReactive(input$file, {
    validate(need((grepl("csv", userFile()$name) == T) | (grepl("xlsx", userFile()$name) == T) | (grepl("sav", userFile()$name) == T) | (grepl("sas7bdat", userFile()$name) == T), message = "Please upload csv/xlsx/sav/sas7bdat file"))
    if (grepl("csv", userFile()$name) == T) {
      out <- data.table::fread(userFile()$datapath, check.names = F, integer64 = "double")
      if (readr::guess_encoding(userFile()$datapath)[1, 1] == "EUC-KR") {
        out <- data.table::data.table(utils::read.csv(userFile()$datapath, check.names = F, fileEncoding = "EUC-KR"))
      }
    } else if (grepl("xlsx", userFile()$name) == T) {
      out <- data.table::data.table(readxl::read_excel(userFile()$datapath), check.names = F, integer64 = "double")
    } else if (grepl("sav", userFile()$name) == T) {
      out <- data.table::data.table(tryCatch(haven::read_sav(userFile()$datapath), error = function(e) {
        return(haven::read_sav(userFile()$datapath, encoding = "latin1"))
      }), check.names = F)
      # out = data.table::data.table(haven::read_sav(userFile()$datapath, encoding = "latin1"), check.names = F, integer64 = "double")
    } else if (grepl("sas7bdat", userFile()$name) == T) {
      out <- data.table::data.table(tryCatch(haven::read_sas(userFile()$datapath), error = function(e) {
        return(haven::read_sas(userFile()$datapath, encoding = "latin1"))
      }), check.names = F)
      # out = data.table::data.table(haven::read_sas(userFile()$datapath), check.names = F, integer64 = "double")
    } else if (grepl("dta", userFile()$name) == T) {
      out <- data.table::data.table(tryCatch(haven::read_dta(userFile()$datapath), error = function(e) {
        return(haven::read_dta(userFile()$datapath, encoding = "latin1"))
      }), check.names = F)
      # out = data.table::data.table(haven::read_dta(userFile()$datapath), check.names = F, integer64 = "double")
    } else {
      stop("Not supported format.")
    }
    # for (x in change.vnlist){
    #  names(out) <- gsub(x[1], x[2], names(out))
    # }

    naCol <- names(out)[unlist(out[, lapply(.SD, function(x) {
      all(is.na(x))
    })])]
    if (length(naCol) == 0) {
      naomit <- NULL
    } else {
      out <- out[, .SD, .SDcols = -naCol]
      naomit <- paste("Column <B>", paste(naCol, collapse = ", "), "</B> are(is) excluded because it is empty.", sep = "")
    }

    out.old <- out
    name.old <- names(out.old)
    out <- data.table::data.table(out, check.names = T)
    name.new <- names(out)
    ref <- list(name.old = name.old, name.new = name.new)


    # numstart.vnum <- suppressWarnings(sapply(names(out),function(x){!is.na(as.numeric(substr(x, 1,1)))}))
    # names(out)[numstart.vnum] <- paste("n_", names(out)[numstart.vnum], sep = "")

    factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
    if (length(factor_vars) > 0) {
      out[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]
    }
    conti_vars <- setdiff(names(out), factor_vars)
    nclass <- unlist(out[, lapply(.SD, function(x) {
      length(unique(x)[!is.na(unique(x))])
    }), .SDcols = conti_vars])
    # except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
    add_vars <- names(nclass)[nclass >= 2 & nclass <= 5]
    # factor_vars_ini <- union(factor_vars, add_vars)
    return(list(data = out, data.old = out.old, factor_original = factor_vars, conti_original = conti_vars, factor_adds_list = names(nclass)[nclass <= nfactor.limit], factor_adds = add_vars, ref = ref, naomit = naomit))
  })




  output$factor <- renderUI({
    selectInput(session$ns("factor_vname"),
      label = "Additional categorical variables",
      choices = data()$factor_adds_list, multiple = T,
      selected = data()$factor_adds
    )
  })

  output$repeated <- renderUI({
    selectInput(session$ns("repeated_vname"),
      label = "Repeated measure variables",
      choices = names(data()$data), multiple = F,
      selected = names(data()$data)[1]
    )
  })


  observeEvent(c(data()$factor_original, input$factor_vname, input$repeated_vname), {
    output$binary_check <- renderUI({
      checkboxInput(session$ns("check_binary"), "Make binary variables")
    })

    output$ref_check <- renderUI({
      checkboxInput(session$ns("check_ref"), "Change reference of categorical variables")
    })

    output$subset_check <- renderUI({
      checkboxInput(session$ns("check_subset"), "Subset data")
    })
  })

  observeEvent(input$check_binary, {
    var.conti <- setdiff(names(data()$data), c(data()$factor_original, input$factor_vname))
    output$binary_var <- renderUI({
      req(input$check_binary == T)
      selectInput(session$ns("var_binary"), "Variables to dichotomize",
        choices = var.conti, multiple = T,
        selected = var.conti[1]
      )
    })

    output$binary_val <- renderUI({
      req(input$check_binary == T)
      req(length(input$var_binary) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_binary)) {
        med <- stats::quantile(data()$data[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = T)
        outUI[[v]] <- splitLayout(
          cellWidths = c("25%", "75%"),
          selectInput(session$ns(paste0("con_binary", v)), paste0("Define reference:"),
            choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"
          ),
          numericInput(session$ns(paste0("cut_binary", v)), input$var_binary[[v]],
            value = med[2], min = med[1], max = med[3]
          )
        )
      }
      outUI
    })
  })

  observeEvent(input$check_ref, {
    var.factor <- c(data()$factor_original, input$factor_vname)
    output$ref_var <- renderUI({
      req(input$check_ref == T)
      selectInput(session$ns("var_ref"), "Variables to change reference",
        choices = var.factor, multiple = T,
        selected = var.factor[1]
      )
    })

    output$ref_val <- renderUI({
      req(input$check_ref == T)
      req(length(input$var_ref) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_ref)) {
        outUI[[v]] <- selectInput(session$ns(paste0("con_ref", v)), paste0("Reference: ", input$var_ref[[v]]),
          choices = levels(factor(data()$data[[input$var_ref[[v]]]])), selected = levels(factor(data()$data[[input$var_ref[[v]]]]))[2]
        )
      }
      outUI
    })
  })

  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      # factor_subset <- setdiff(c(data()$factor_original, input$factor_vname), input$repeated_vname)

      # validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      # )

      tagList(
        selectInput(session$ns("var_subset"), "Subset variables",
          choices = names(data()$data), multiple = T,
          selected = names(data()$data)[1]
        )
      )
    })

    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(input$var_subset)
      var.factor <- c(data()$factor_original, input$factor_vname)
      outUI <- tagList()

      for (v in seq_along(input$var_subset)) {
        if (input$var_subset[[v]] %in% var.factor) {
          varlevel <- levels(as.factor(data()$data[[input$var_subset[[v]]]]))
          outUI[[v]] <- selectInput(session$ns(paste0("val_subset", v)), paste0("Subset value: ", input$var_subset[[v]]),
            choices = varlevel, multiple = T,
            selected = varlevel[1]
          )
        } else {
          val <- stats::quantile(data()$data[[input$var_subset[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(session$ns(paste0("val_subset", v)), paste0("Subset range: ", input$var_subset[[v]]),
            min = val[1], max = val[5],
            value = c(val[2], val[4])
          )
        }
      }
      outUI
    })
  })



  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  outdata <- reactive({
    req(input$repeated_vname)
    out <- data()$data
    out[, (data()$conti_original) := lapply(.SD, function(x) {
      as.numeric(as.vector(x))
    }), .SDcols = data()$conti_original]
    if (length(input$factor_vname) > 0) {
      out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols = input$factor_vname]
    }
    out <- out[!is.na(get(input$repeated_vname))][order(get(input$repeated_vname))]

    ref <- data()$ref
    out.label <- mk.lev(out)

    if (tools::file_ext(input$file$name) == "sav") {
      out.label <- mk.lev2(data()$data.old, out.label)
    }

    if (!is.null(input$check_binary)) {
      if (input$check_binary) {
        validate(
          need(length(input$var_binary) > 0, "No variables to dichotomize")
        )
        sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
        names(sym.ineq) <- sym.ineq[4:1]
        sym.ineq2 <- c("le", "ge", "l", "g")
        names(sym.ineq2) <- sym.ineq
        for (v in seq_along(input$var_binary)) {
          req(input[[paste0("con_binary", v)]])
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
        var.factor <- c(data()$factor_original, input$factor_vname)
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

    if (tools::file_ext(input$file$name) != "sav") {
      for (vn in ref[["name.new"]]) {
        w <- which(ref[["name.new"]] == vn)
        out.label[variable == vn, var_label := ref[["name.old"]][w]]
      }
    }

    return(list(data = out, label = out.label, naomit = data()$naomit, id.gee = input$repeated_vname))
  })



  # Return the reactive that yields the data frame
  return(outdata)
}
