# /R/DataManager.R

#' @title DataManager R6 Class for Shiny Modules
#' @description
#' An R6 class to manage data loading, preprocessing, and common UI elements
#' for various Shiny modules in the package. It handles file reading (csv, xlsx,
#' sav, sas7bdat, dta), initial variable analysis, and the rendering and logic for
#' common data manipulations like creating binary variables, changing factor
#' references, and subsetting data. This class is designed to be used internally
#' by other Shiny server modules to reduce code duplication.
#'
#' @keywords internal
#' @importFrom R6 R6Class
#' @import shiny
#' @import data.table
#' @importFrom readxl read_excel
#' @importFrom readr guess_encoding
#' @importFrom haven read_sav read_sas read_dta
#' @importFrom jstable mk.lev
#' @importFrom utils read.csv
#' @importFrom stats quantile relevel
#' @importFrom tools file_ext

DataManager <- R6::R6Class("DataManager",
  public = list(
    # ================================#
    #         Public Fields
    # ================================#
    #' @field input Shiny module's input object.
    input = NULL,
    #' @field output Shiny module's output object.
    output = NULL,
    #' @field session Shiny module's session object.
    session = NULL,
    #' @field ns Shiny module's namespace function.
    ns = NULL,
    #' @field nfactor.limit The threshold for unique values to suggest a numeric
    #' variable as a factor.
    nfactor.limit = 20,

    # Reactive values holding the state
    #' @field initial_data_info A reactive value holding the initially loaded
    #' data and its metadata.
    initial_data_info = NULL,
    #' @field processed_data A reactive value holding the data after all
    #' transformations have been applied.
    processed_data = NULL,

    # ================================#
    #           Constructor
    # ================================#
    #' @description
    #' Create a new DataManager object.
    #' @param input Shiny module's input object.
    #' @param output Shiny module's output object.
    #' @param session Shiny module's session object.
    #' @param nfactor.limit The maximum number of unique values for a continuous
    #' variable to be suggested as a factor.
    #' @return A new `DataManager` object.
    initialize = function(input, output, session, nfactor.limit = 20) {
      self$input <- input
      self$output <- output
      self$session <- session
      self$ns <- session$ns
      self$nfactor.limit <- nfactor.limit

      # --- Initialize reactive data flows ---
      userFile <- eventReactive(self$input$file, {
        req(self$input$file)
        self$input$file
      })

      self$initial_data_info <- eventReactive(userFile(), {
        private$load_data_from_file(userFile())
      })

      self$processed_data <- reactive({
        req(self$initial_data_info())
        private$apply_transformations(self$initial_data_info())
      })

      # --- Render all common UI elements ---
      private$render_common_ui_observers()
    },

    # ================================#
    #         Public Methods
    # ================================#
    #' @description
    #' Returns the final processed data as a reactive expression.
    #' This is the main output to be used by the calling module.
    #' @return A reactive expression that returns a list containing the
    #' processed `data`, `label` information, and `naomit` message.
    get_reactive_data = function() {
      return(self$processed_data)
    }
  ),
  private = list(
    # ================================#
    #        Private Methods
    # ================================#
    load_data_from_file = function(file) {
      ext <- tools::file_ext(file$name)
      validate(need(ext %in% c("csv", "xlsx", "sav", "sas7bdat", "dta"),
        message = "Please upload csv/xlsx/sav/sas7bdat/dta file"
      ))

      path <- file$datapath
      out <- NULL

      if (ext == "csv") {
        out <- data.table::fread(path, check.names = FALSE, integer64 = "double")
        if (readr::guess_encoding(path)[1, 1] == "EUC-KR") {
          out <- data.table::data.table(utils::read.csv(path, check.names = FALSE, fileEncoding = "EUC-KR"))
        }
      } else if (ext == "xlsx") {
        out <- data.table::data.table(readxl::read_excel(path), check.names = FALSE, integer64 = "double")
      } else if (ext == "sav") {
        out <- data.table::data.table(tryCatch(haven::read_sav(path), error = function(e) haven::read_sav(path, encoding = "latin1")), check.names = FALSE)
      } else if (ext == "sas7bdat") {
        out <- data.table::data.table(tryCatch(haven::read_sas(path), error = function(e) haven::read_sas(path, encoding = "latin1")), check.names = FALSE)
      } else if (ext == "dta") {
        out <- data.table::data.table(tryCatch(haven::read_dta(path), error = function(e) haven::read_dta(path, encoding = "latin1")), check.names = FALSE)
      } else {
        stop("Not supported format.")
      }

      naCol <- names(out)[sapply(out, function(x) all(is.na(x)))]
      naomit_msg <- if (length(naCol) == 0) {
        "There are no empty columns."
      } else {
        out <- out[, .SD, .SDcols = -naCol]
        paste("Column(s) <B>", paste(naCol, collapse = ", "), "</B> were excluded because they are empty.", sep = "")
      }

      out.old <- out
      name.old <- names(out.old)

      out <- data.table::data.table(out, check.names = TRUE)
      name.new <- names(out)
      ref <- list(name.old = name.old, name.new = name.new)

      factor_vars <- names(out)[sapply(out, function(x) is.factor(x) || is.character(x))]
      if (length(factor_vars) > 0) {
        out[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]
      }
      conti_vars <- setdiff(names(out), factor_vars)
      nclass <- sapply(out[, ..conti_vars, with = FALSE], function(x) length(unique(x[!is.na(x)])))

      factor_adds_list <- names(nclass)[nclass <= self$nfactor.limit]
      add_vars <- names(nclass)[nclass >= 2 & nclass <= 5]
      except_vars <- names(nclass)[nclass == 1]

      return(list(
        data = out, data.old = out.old, factor_original = factor_vars,
        conti_original = conti_vars, factor_adds_list = factor_adds_list,
        factor_adds = add_vars, except_vars = except_vars, ref = ref,
        naomit = naomit_msg, file_ext = ext
      ))
    },
    apply_transformations = function(initial_info) {
      ## To remove NOTE.
      val_label <- BinaryGroupRandom <- variable <- NULL

      out <- data.table::copy(initial_info$data)
      out[, (initial_info$conti_original) := lapply(.SD, function(x) {
        as.numeric(as.vector(x))
      }), .SDcols = initial_info$conti_original]

      if (length(self$input$factor_vname) > 0) {
        out[, (self$input$factor_vname) := lapply(.SD, as.factor), .SDcols = self$input$factor_vname]
      }

      out.label <- jstable::mk.lev(out)
      if (initial_info$file_ext == "sav") {
        out.label <- mk.lev2(initial_info$data.old, out.label)
      }

      # Apply binary conversion
      if (!is.null(self$input$check_binary) && self$input$check_binary) {
        validate(need(length(self$input$var_binary) > 0, "No variables to dichotomize"))
        sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
        names(sym.ineq) <- sym.ineq[c(4, 3, 2, 1)]
        sym.ineq2 <- c("le", "ge", "l", "g")
        names(sym.ineq2) <- c("\u2264", "\u2265", "\u003c", "\u003e")

        for (v in seq_along(self$input$var_binary)) {
          var_name <- self$input$var_binary[[v]]
          con_input <- self$input[[paste0("con_binary", v)]]
          cut_input <- self$input[[paste0("cut_binary", v)]]
          req(con_input, cut_input)

          new_val <- switch(con_input,
            "\u2264" = 1 - as.integer(out[[var_name]] <= cut_input),
            "\u2265" = 1 - as.integer(out[[var_name]] >= cut_input),
            "\u003c" = 1 - as.integer(out[[var_name]] < cut_input),
            "\u003e" = 1 - as.integer(out[[var_name]] > cut_input)
          )
          # Create binary factor variable
          out[, BinaryGroupRandom := factor(new_val, levels = c(0, 1))]

          cn.new <- paste0(var_name, "_group_", sym.ineq2[con_input], cut_input)
          data.table::setnames(out, "BinaryGroupRandom", cn.new)

          label.binary <- jstable::mk.lev(out[, .SD, .SDcols = cn.new])
          label.binary[, var_label := paste0(var_name, " _group")]
          label.binary[, val_label := paste0(c(con_input, sym.ineq[con_input]), " ", cut_input)]
          out.label <- rbind(out.label, label.binary)
        }
      }

      # Apply reference change
      if (!is.null(self$input$check_ref) && self$input$check_ref) {
        validate(need(length(self$input$var_ref) > 0, "No variables to change reference"))
        for (v in seq_along(self$input$var_ref)) {
          var_name <- self$input$var_ref[[v]]
          ref_val <- self$input[[paste0("con_ref", v)]]
          req(ref_val)
          out[[var_name]] <- stats::relevel(out[[var_name]], ref = ref_val)
          out.label[variable == var_name, ":="(level = levels(out[[var_name]]), val_label = levels(out[[var_name]]))]
        }
      }

      # Apply subsetting
      if (!is.null(self$input$check_subset) && self$input$check_subset) {
        validate(
          need(length(self$input$var_subset) > 0, "No variables for subsetting"),
          need(all(sapply(1:length(self$input$var_subset), function(x) length(self$input[[paste0("val_subset", x)]]))), "No value for subsetting")
        )
        var.factor <- c(initial_info$factor_original, self$input$factor_vname)
        for (v in seq_along(self$input$var_subset)) {
          var_name <- self$input$var_subset[[v]]
          sub_val <- self$input[[paste0("val_subset", v)]]

          if (var_name %in% var.factor) {
            out <- out[get(var_name) %in% sub_val]
          } else {
            out <- out[get(var_name) >= sub_val[1] & get(var_name) <= sub_val[2]]
          }
          # Refactor levels after subsetting
          factor_cols <- names(out)[sapply(out, is.factor)]
          if (length(factor_cols) > 0) {
            out[, (factor_cols) := lapply(.SD, factor), .SDcols = factor_cols]
          }
        }
        out.label2 <- jstable::mk.lev(out)[, c("variable", "level")]
        data.table::setkey(out.label, "variable", "level")
        data.table::setkey(out.label2, "variable", "level")
        out.label <- out.label[out.label2]
      }

      # Update labels from original names
      if (initial_info$file_ext != "sav") {
        for (vn in initial_info$ref[["name.new"]]) {
          w <- which(initial_info$ref[["name.new"]] == vn)
          out.label[variable == vn, var_label := initial_info$ref[["name.old"]][w]]
        }
      }

      return(list(data = out, label = out.label, naomit = initial_info$naomit, except_vars = initial_info$except_vars))
    },
    render_common_ui_observers = function() {
      self$output$factor <- renderUI({
        req(self$initial_data_info())
        selectInput(self$ns("factor_vname"),
          label = "Additional categorical variables",
          choices = self$initial_data_info()$factor_adds_list,
          multiple = TRUE,
          selected = self$initial_data_info()$factor_adds
        )
      })

      observeEvent(self$initial_data_info(), {
        self$output$binary_check <- renderUI({
          checkboxInput(self$ns("check_binary"), "Make binary variables")
        })
        self$output$ref_check <- renderUI({
          checkboxInput(self$ns("check_ref"), "Change reference of categorical variables")
        })
        self$output$subset_check <- renderUI({
          checkboxInput(self$ns("check_subset"), "Subset data")
        })
      })

      observeEvent(self$input$check_binary,
        {
          data_info <- self$initial_data_info()
          var.conti <- setdiff(names(data_info$data), c(data_info$factor_original, self$input$factor_vname))
          self$output$binary_var <- renderUI({
            req(self$input$check_binary == TRUE)
            selectInput(self$ns("var_binary"), "Variables to dichotomize",
              choices = var.conti, multiple = TRUE, selected = var.conti[1]
            )
          })
          self$output$binary_val <- renderUI({
            req(self$input$check_binary == TRUE)
            req(length(self$input$var_binary) > 0)
            tagList(lapply(seq_along(self$input$var_binary), function(v) {
              med <- stats::quantile(data_info$data[[self$input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = TRUE)
              splitLayout(
                cellWidths = c("25%", "75%"),
                selectInput(self$ns(paste0("con_binary", v)), "Ref:", choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"),
                numericInput(self$ns(paste0("cut_binary", v)), self$input$var_binary[[v]], value = med[2], min = med[1], max = med[3])
              )
            }))
          })
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(self$input$check_ref,
        {
          data_info <- self$initial_data_info()
          var.factor <- c(data_info$factor_original, self$input$factor_vname)
          self$output$ref_var <- renderUI({
            req(self$input$check_ref == TRUE)
            selectInput(self$ns("var_ref"), "Variables to change reference",
              choices = var.factor, multiple = TRUE, selected = var.factor[1]
            )
          })
          self$output$ref_val <- renderUI({
            req(self$input$check_ref == TRUE)
            req(length(self$input$var_ref) > 0)
            tagList(lapply(seq_along(self$input$var_ref), function(v) {
              selectInput(self$ns(paste0("con_ref", v)), paste0("Reference: ", self$input$var_ref[[v]]),
                choices = levels(factor(data_info$data[[self$input$var_ref[[v]]]]))
              )
            }))
          })
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(self$input$check_subset,
        {
          data_info <- self$initial_data_info()
          self$output$subset_var <- renderUI({
            req(self$input$check_subset == TRUE)
            selectInput(self$ns("var_subset"), "Subset variables",
              choices = names(data_info$data), multiple = TRUE, selected = names(data_info$data)[1]
            )
          })
          self$output$subset_val <- renderUI({
            req(self$input$check_subset == TRUE)
            req(length(self$input$var_subset) > 0)
            var.factor <- c(data_info$factor_original, self$input$factor_vname)
            tagList(lapply(seq_along(self$input$var_subset), function(v) {
              var_name <- self$input$var_subset[[v]]
              if (var_name %in% var.factor) {
                varlevel <- levels(factor(data_info$data[[var_name]]))
                selectInput(self$ns(paste0("val_subset", v)), paste0("Subset value: ", var_name),
                  choices = varlevel, multiple = TRUE, selected = varlevel[1]
                )
              } else {
                val <- stats::quantile(data_info$data[[var_name]], na.rm = TRUE)
                sliderInput(self$ns(paste0("val_subset", v)), paste0("Subset range: ", var_name),
                  min = val[1], max = val[5], value = c(val[2], val[4])
                )
              }
            }))
          })
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )
    }
  )
)
