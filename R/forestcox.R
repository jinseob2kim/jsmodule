#' @title forestcoxUI:shiny module UI for forestcox
#' @description Shiny module UI for forestcox
#' @param id id
#' @param label label, Default: 'forestplot'
#' @return Shinymodule UI
#' @details Shinymodule UI for forestcox
#' @examples
#'
#' library(shiny)
#' library(DT)
#' mtcars$vs <- factor(mtcars$vs)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$kk <- factor(as.integer(mtcars$disp >= 150))
#' mtcars$kk1 <- factor(as.integer(mtcars$disp >= 200))
#'
#' library(shiny)
#' library(DT)
#' mtcars$vs <- factor(mtcars$vs)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$kk <- factor(as.integer(mtcars$disp >= 150))
#' mtcars$kk1 <- factor(as.integer(mtcars$disp >= 200))
#'
#' out <- mtcars
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       forestcoxUI("Forest")
#'     ),
#'     mainPanel(
#'       tabsetPanel(
#'         type = "pills",
#'         tabPanel(
#'           title = "Data",
#'           DTOutput("tablesub")
#'         ),
#'         tabPanel(
#'           title = "figure",
#'           plotOutput("forestplot", width = "100%"),
#'           ggplotdownUI("Forest")
#'         )
#'       )
#'     )
#'   )
#' )
#'
#'
#' server <- function(input, output, session) {
#'   data <- reactive(out)
#'   label <- reactive(jstable::mk.lev(out))
#'   outtable <- forestcoxServer("Forest", data = data, data_label = label)
#'   output$tablesub <- renderDT({
#'     outtable()[[1]]
#'   })
#'   output$forestplot <- renderPlot({
#'     outtable()[[2]]
#'   })
#' }
#'
#' @rdname forestcoxUI
#' @export

forestcoxUI <- function(id, label = "forestplot") {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cluster_id_ui")),
    uiOutput(ns("group_tbsub")),
    uiOutput(ns("dep_tbsub")),
    uiOutput(ns("day_tbsub")),
    uiOutput(ns("subvar_tbsub")),
    uiOutput(ns("cov_tbsub")),
    uiOutput(ns("time_tbsub")),
    checkboxInput(ns("cmp_risk_check"), "Competing Risk Analysis(Fine-Gray)"),
    uiOutput(ns("cmp_eventtime")),
    checkboxInput(ns("custom_forest"), "Custom X axis ticks in forest plot"),
    uiOutput(ns("hr_points")),
    uiOutput(ns("numeric_inputs"))
  )
}



#' @title forestcoxServer:shiny module server for forestcox
#' @description Shiny module server for forestcox
#' @param id id
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey reactive survey data. default: NULL
#' @param cluster_id cluster option variable for marginal cox model
#' @param vec.event event variables as vector for survival analysis,  Default: NULL
#' @param vec.time time variables as vector for survival analysis,  Default: NULL
#' @return Shiny module server for forestcox
#' @details Shiny module server for forestcox
#' @examples
#'
#' library(shiny)
#' library(DT)
#' mtcars$vs <- factor(mtcars$vs)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$kk <- factor(as.integer(mtcars$disp >= 150))
#' mtcars$kk1 <- factor(as.integer(mtcars$disp >= 200))
#'
#' library(shiny)
#' library(DT)
#' mtcars$vs <- factor(mtcars$vs)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$kk <- factor(as.integer(mtcars$disp >= 150))
#' mtcars$kk1 <- factor(as.integer(mtcars$disp >= 200))
#'
#' out <- mtcars
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       forestcoxUI("Forest")
#'     ),
#'     mainPanel(
#'       tabsetPanel(
#'         type = "pills",
#'         tabPanel(
#'           title = "Data",
#'           DTOutput("tablesub"),
#'         ),
#'         tabPanel(
#'           title = "figure",
#'           plotOutput("forestplot", width = "100%"),
#'           ggplotdownUI("Forest")
#'         )
#'       )
#'     )
#'   )
#' )
#'
#'
#' server <- function(input, output, session) {
#'   data <- reactive(out)
#'   label <- reactive(jstable::mk.lev(out))
#'   outtable <- forestcoxServer("Forest", data = data, data_label = label)
#'   output$tablesub <- renderDT({
#'     outtable()[[1]]
#'   })
#'   output$forestplot <- renderPlot({
#'     a
#'     outtable()[[2]]
#'   })
#' }
#'
#' @seealso
#'  \code{\link[data.table]{data.table-package}}, \code{\link[data.table]{setDT}}, \code{\link[data.table]{setattr}}
#'  \code{\link[jstable]{TableSubgroupMultiCox}}
#'  \code{\link[forestploter]{forest_theme}}, \code{\link[forestploter]{forest}}
#'  \code{\link[rvg]{dml}}
#'  \code{\link[officer]{read_pptx}}, \code{\link[officer]{add_slide}}, \code{\link[officer]{ph_with}}, \code{\link[officer]{ph_location}}
#' @rdname forestcoxServer
#' @export
#' @importFrom data.table data.table setDT setnames
#' @importFrom jstable TableSubgroupMultiCox
#' @importFrom forestploter forest_theme forest
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location

forestcoxServer <- function(id, data, data_label, data_varStruct = NULL, nfactor.limit = 10, design.survey = NULL, cluster_id = NULL, vec.event = NULL, vec.time = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      N <- V1 <- V2 <- .N <- HR <- Lower <- Upper <- level <- val_label <- variable <- NULL
      fix_et <- !is.null(vec.event) && !is.null(vec.time) && (length(vec.event) == length(vec.time))
      if (is.null(data_varStruct)) {
        data_varStruct <- reactive(list(variable = names(data())))
      }

      vlist <- reactive({
        label <- data.table::data.table(data_label(), stringsAsFactors = T)

        data <- data.table::data.table(data(), stringsAsFactors = T)

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
        factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
        # data[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
        factor_list <- mklist(data_varStruct(), factor_vars)

        conti_vars <- setdiff(names(data()), factor_vars)
        if (!is.null(design.survey)) {
          conti_vars <- setdiff(conti_vars, c(names(design.survey()$allprob), names(design.survey()$strata), names(design.survey()$cluster)))
        }
        conti_vars_positive <- conti_vars[unlist(data[, lapply(.SD, function(x) {
          min(x, na.rm = T) >= 0
        }), .SDcols = conti_vars])]
        conti_list <- mklist(data_varStruct(), conti_vars)

        nclass_factor <- unlist(data[, lapply(.SD, function(x) {
          length(levels(x))
        }), .SDcols = factor_vars])
        # nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})
        class01_factor <- unlist(data[, lapply(.SD, function(x) {
          identical(levels(x), c("0", "1"))
        }), .SDcols = factor_vars])

        validate(
          need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
        )
        factor_01vars <- factor_vars[class01_factor]

        factor_01_list <- mklist(data_varStruct(), factor_01vars)

        group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
        group_list <- mklist(data_varStruct(), group_vars)

        except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]
        group2_vars <- factor_vars[nclass_factor == 2]
        return(list(
          factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list, conti_vars_positive = conti_vars_positive,
          group2_vars = group2_vars,
          factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_vars = group_vars, group_list = group_list, except_vars = except_vars
        ))
      })

      output$group_tbsub <- renderUI({
        req(input$dep)
        selectInput(session$ns("group"), "Group", choices = setdiff(c(vlist()$group2_vars, vlist()$conti_vars), input$dep), selected = setdiff(c(vlist()$group2_vars, vlist()$conti_vars), c(input$dep, vlist()$factor_01vars[1]))[1])
      })
      output$dep_tbsub <- renderUI({
        if (!fix_et) {
          selectInput(session$ns("dep"), "Outcome", choices = vlist()$factor_01vars, selected = vlist()$factor_01vars[1])
        } else {
          selectInput(session$ns("dep"), "Outcome", choices = vec.event, selected = vec.event[1])
        }
      })
      output$subvar_tbsub <- renderUI({
        req(input$dep)
        selectInput(session$ns("subvar"), "Subgroup to include", choices = setdiff(vlist()$group_vars, c(input$group, input$dep)), selected = NULL, multiple = T)
      })
      output$cov_tbsub <- renderUI({
        selectInput(session$ns("cov"), "Addtional covariates", choices = vlist()$group_vars, selected = NULL, multiple = T)
      })
      output$day_tbsub <- renderUI({
        if (!fix_et) {
          selectInput(session$ns("day"), "Time", choices = vlist()$conti_vars_positive, selected = vlist()$conti_vars_positive[1])
        } else {
          selectInput(session$ns("day"), "Time", choices = vec.time, selected = vec.time[1])
        }
      })
      output$time_tbsub <- renderUI({
        req(input$day)
        day <- input$day
        sliderInput(session$ns("time"), "Select time range", min = min(data()[[day]], na.rm = TRUE), max = max(data()[[day]], na.rm = TRUE), value = c(min(data()[[day]], na.rm = TRUE), max(data()[[day]], na.rm = TRUE)))
      })




      if (fix_et) {
        paired_forest <- setNames(vec.time, vec.event)
        paired_rev_forest <- setNames(vec.event, vec.time)


        observeEvent(input$dep, {
          req(input$dep)
          new_day <- paired_forest[[input$dep]]
          if (!is.null(new_day) && new_day != input$day) {
            updateSelectInput(session, "day", selected = new_day)
          }
        })

        observeEvent(input$day, {
          req(input$day)
          new_dep <- paired_rev_forest[[input$day]]
          if (!is.null(new_dep) && new_dep != input$dep) {
            updateSelectInput(session, "dep", selected = new_dep)
          }
        })
      }



      # print(cluster_id)



      observeEvent(input$custom_forest, {
        output$hr_points <- renderUI({
          req(input$custom_forest == TRUE)
          tagList(
            sliderInput(session$ns("num_points"), "select number of x axis ticks", min = 2, max = 8, value = 3)
          )
        })
      })

      output$numeric_inputs <- renderUI({
        req(input$num_points)
        fluidRow(
          lapply(seq_len(input$num_points), function(i) {
            column(
              width = floor(12 / input$num_points),
              numericInput(
                session$ns(paste0("point_", i)),
                paste("Point", i, ":"),
                value = 1.0
              )
            )
          })
        )
      })



      observeEvent(input$cmp_risk_check, {
        output$cmp_eventtime <- renderUI({
          req(input$cmp_risk_check == TRUE)
          validate(
            need(length(vlist()$factor_01vars) >= 1, "No candidate event variables coded as 0, 1"),
            need(length(vlist()$conti_vars_positive) >= 1, "No candidate time variables")
          )
          tagList(
            selectInput(session$ns("cmp_event_cox"), "Competing Event",
              choices = mklist(data_varStruct(), vlist()$factor_01vars), multiple = FALSE,
              selected = NULL
            ),
            selectInput(session$ns("cmp_time_cox"), "Competing Time",
              choices = mklist(data_varStruct(), vlist()$conti_vars_positive), multiple = FALSE,
              selected = NULL
            )
          )
        })
      })

      tbsub <- reactive({
        label <- data_label()
        if (is.null(design.survey)) {
          data <- data()
        } else {
          data <- design.survey()$variables
        }

        req(input$group, input$dep, input$day)
        group.tbsub <- input$group
        var.event <- input$dep
        var.day <- input$day

        vs <- input$subvar
        var.time <- input$time

        isgroup <- ifelse(group.tbsub %in% vlist()$group_vars, 1, 0)

        # data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
        data <- data[data[[var.day]] >= var.time[1]]
        data[[var.event]] <- ifelse(data[[var.day]] >= var.time[2] & data[[var.event]] == "1", 0, as.numeric(as.vector(data[[var.event]])))
        data[[var.day]] <- ifelse(data[[var.day]] >= var.time[2], var.time[2], data[[var.day]])

        if (is.null(design.survey)) {
          coxdata <- data
        } else {
          coxdata <- design.survey()
          coxdata$variables <- data
        }
        if (input$cmp_risk_check) {
          req(input$cmp_event_cox)
          req(input$cmp_time_cox)
          form <- as.formula(paste("survival::Surv(fgstart, fgstop, fgstatus) ~ ", group.tbsub, sep = ""))
          cox_data <- data
          cox_data[[input$cmp_event_cox]] <- as.numeric(as.vector(cox_data[[input$cmp_event_cox]]))
          cox_data$cmpp_time <- with(cox_data, ifelse(cox_data[[input$dep]] == 0, cox_data[[input$cmp_time_cox]], cox_data[[input$day]]))
          cox_data$cmpp_event <- with(cox_data, ifelse(cox_data[[input$dep]] == 0, 2 * cox_data[[input$cmp_event_cox]], 1))
          cox_data$cmpp_event <- factor(cox_data$cmpp_event)
          fg_data <- survival::finegray(formula = survival::Surv(cmpp_time, cmpp_event) ~ ., data = cox_data)
          tbsub <- TableSubgroupMultiCox(form, var_subgroups = vs, var_cov = setdiff(input$cov, vs), data = fg_data, time_eventrate = var.time[2], line = F, decimal.hr = 3, decimal.percent = 1, weights = "fgwt")
          if (!is.null(cluster_id)) {
            form <- as.formula(
              paste(
                "survival::Surv(fgstart, fgstop, fgstatus) ~ ",
                group.tbsub,
                " + cluster(", cluster_id, ")",
                sep = ""
              )
            )
            tbsub <- TableSubgroupMultiCox(form, var_subgroups = vs, var_cov = setdiff(input$cov, vs), data = fg_data, time_eventrate = var.time[2], line = F, decimal.hr = 3, decimal.percent = 1, weights = "fgwt")
            names(tbsub) <- gsub(paste0("\\s*\\+\\s*cluster\\(", cluster_id, "\\)"), "", names(tbsub))
          }
        } else {
          form <- as.formula(paste("Surv(", var.day, ",", var.event, ") ~ ", group.tbsub, sep = ""))

          tbsub <- TableSubgroupMultiCox(form, var_subgroups = vs, var_cov = setdiff(input$cov, vs), data = coxdata, time_eventrate = var.time[2], line = F, decimal.hr = 3, decimal.percent = 1)
          if (!is.null(cluster_id)) {
            form <- paste("Surv(", var.day, ",", var.event, ") ~ ", group.tbsub, sep = "")
            form <- as.formula(paste(form, " + cluster(", cluster_id, ")", sep = ""))
            print(form)
            tbsub <- TableSubgroupMultiCox(form, var_subgroups = vs, var_cov = setdiff(input$cov, vs), data = coxdata, time_eventrate = var.time[2], line = F, decimal.hr = 3, decimal.percent = 1)
            names(tbsub) <- gsub(paste0("\\s*\\+\\s*cluster\\(", cluster_id, "\\)"), "", names(tbsub))
          }
        }



        # data[[var.event]] <- ifelse(data[[var.day]] > 365 * 5 & data[[var.event]] == 1, 0,  as.numeric(as.vector(data[[var.event]])))
        # tbsub<-TableSubgroupMultiCox(as.formula('Surv(mpg,vs)~am'), var_subgroups = 'kk',  data=out, time_eventrate = 365 , line = F, decimal.hr = 3, decimal.percent = 1)
        len <- nrow(label[variable == group.tbsub])
        data <- data.table::setDT(data)
        if (!isgroup) {
          tbsub <- data.table::setnames(tbsub, "Point Estimate", "HR")
          tbsub <- tbsub[, c(1, 4:8)]
          return(tbsub)
        }
        if (is.null(design.survey)) {
          ev.ov <- data[!is.na(get(group.tbsub)), sum(as.numeric(as.vector(get(var.event))), na.rm = TRUE), keyby = get(group.tbsub)][, V1]
          nn.ov <- data[!is.na(get(group.tbsub)), .N, keyby = get(group.tbsub)][, N]
        } else {
          ev.ov <- round(svytable(as.formula(paste0("~", var.event, "+", group.tbsub)), design = coxdata)[2, ], 2)
          nn.ov <- round(svytable(as.formula(paste0("~", group.tbsub)), design = coxdata), 2)
        }
        ov <- data.table::data.table(t(c("OverAll", paste0(ev.ov, "/", nn.ov, " (", round(ev.ov / nn.ov * 100, 2), "%)"))))
        if (!is.null(vs)) {
          rbindlist(lapply(
            vs,
            function(x) {
              cc <- data.table::data.table(matrix(ncol = len + 1))
              cc[[1]] <- x

              dd.bind <- " "
              getlev <- data.table::data.table(get = levels(data[[x]]))
              for (y in levels(data[[group.tbsub]])) {
                if (is.null(design.survey)) {
                  ev <- data[!is.na(get(x)) & get(group.tbsub) == y, sum(as.numeric(as.vector(get(var.event))), na.rm = TRUE), keyby = get(x)]
                  nn <- data[!is.na(get(x)) & get(group.tbsub) == y, .N, keyby = get(x)]
                  vv <- data.table::data.table(get = ev[, get], paste0(ev[, V1], "/", nn[, N], " (", round(ev[, V1] / nn[, N] * 100, 1), "%)"))
                  ee <- merge(data.table::data.table(get = levels(ev[, get])), vv, all.x = TRUE)
                  dd.bind <- cbind(dd.bind, ee[, V2])
                } else {
                  svy <- svytable(as.formula(paste0("~", var.event, "+", x)), design = subset(coxdata, !is.na(get(x)) & get(group.tbsub) == y))
                  ev <- round(svy[2, ], 2)
                  nn <- round(svytable(as.formula(paste0("~", x)), design = subset(coxdata, !is.na(get(x)) & get(group.tbsub) == y)), 2)
                  vv <- data.table::data.table(get = colnames(svy), paste0(ev, "/", nn, " (", round(ev / nn * 100, 2), "%)"))
                  ee <- merge(getlev, vv, all.x = TRUE)
                  dd.bind <- cbind(dd.bind, ee[, V2])
                }
              }
              names(cc) <- names(dd.bind)
              rbind(cc, dd.bind)
            }
          )) -> ll

          names(ov) <- names(ll)
          cn <- rbind(ov, ll)


          names(cn)[-1] <- label[variable == group.tbsub, val_label]
          tbsub <- cbind(Variable = tbsub[, 1], cn[, -1], tbsub[, c(paste0(group.tbsub, "=", label[variable == group.tbsub, level]), names(tbsub)[4:6], "P value", "P for interaction")])

          tbsub[-(len - 1), 1] <- unlist(lapply(vs, function(x) {
            c(label[variable == x, var_label][1], paste0("     ", label[variable == x, val_label]))
          }))
          colnames(tbsub)[1:(2 + 2 * len)] <- c("Subgroup", paste0("N(%): ", label[variable == group.tbsub, val_label]), paste0(var.time[2], "-", input$day, "\n", " KM rate(%): ", label[variable == group.tbsub, val_label]), "HR")
        } else {
          cn <- ov
          names(cn)[-1] <- label[variable == group.tbsub, val_label]
          tbsub <- cbind(Variable = tbsub[, 1], cn[, -1], tbsub[, c(paste0(group.tbsub, "=", label[variable == group.tbsub, level]), names(tbsub)[4:6], "P value", "P for interaction")])

          colnames(tbsub)[1:(2 + 2 * nrow(label[variable == group.tbsub]))] <- c("Subgroup", paste0("N(%): ", label[variable == group.tbsub, val_label]), paste0(var.time[2], "-", input$day, "\n", " KM rate(%): ", label[variable == group.tbsub, val_label]), "HR")
        }

        return(tbsub)
      })




      ticks <- reactive({
        if (is.null(input$num_points)) {
          a <- c(0, 1, 2)
        } else {
          a <- sapply(seq_len(input$num_points), function(i) input[[paste0("point_", i)]])
        }
        return(a)
      })




      figure <- reactive({
        group.tbsub <- input$group
        label <- data_label()
        data <- data.table::setDT(tbsub())
        len <- ncol(data)

        ll <- ifelse(group.tbsub %in% vlist()$group_vars, nrow(label[variable == group.tbsub]), 0)
        data[HR == 0 | Lower == 0, ":="(HR = NA, Lower = NA, Upper = NA)]
        data_est <- data$`HR`
        data[is.na(data)] <- " "
        data$HR <- ifelse(data$HR == " ", " ", paste0(data$HR, " (", data$Lower, "-", data$Upper, ")"))
        data.table::setnames(data, "HR", "HR (95% CI)")
        data$` ` <- paste(rep(" ", 20), collapse = " ")
        tm <- forestploter::forest_theme(base_size = input$font, ci_Theight = 0.2)
        selected_columns <- c(c(1:(2 + 2 * ll)), len + 1, (len - 1):(len))
        xlim <- c(1 / input$xMax, input$xMax)
        xlim <- round(xlim[order(xlim)], 2)
        if (is.null(input$xMax) || any(is.na(xlim))) {
          xlim <- c(0, 2)
        }
        forestploter::forest(data[, .SD, .SDcols = selected_columns],
          lower = as.numeric(data$Lower),
          upper = as.numeric(data$Upper),
          ci_column = 3 + 2 * ll,
          est = as.numeric(data_est),
          ref_line = 1,
          ticks_digits = 1,
          x_trans = "log",
          xlim = NULL,
          arrow_lab = c(input$arrow_left, input$arrow_right),
          ticks_at = ticks(),
          theme = tm
        ) -> zz

        l <- dim(zz)
        h <- zz$height[(l[1] - 2):(l[1] - 1)]
        zz <- print(zz[, 2:(l[2] - 1)], autofit = TRUE)
        zz$heights[(l[1] - 2):(l[1] - 1)] <- h
        return(zz)
      })

      res <- reactive({
        list(
          datatable(tbsub(),
            caption = paste0(input$dep, " subgroup analysis"), rownames = F, extensions = "Buttons",
            options = c(
              opt.tb1(paste0("tbsub_", input$dep)),
              list(scrollX = TRUE, columnDefs = list(list(className = "dt-right", targets = 0)))
            )
          ),
          figure()
        )
      })

      output$downloadControls <- renderUI({
        tagList(
          fluidRow(
            column(
              3,
              numericInput(session$ns("font"), "font-size", value = 12)
            ),
            column(
              3,
              textInput(session$ns("arrow_left"), "arrow left", value = "Better")
            ),
            column(
              3,
              textInput(session$ns("arrow_right"), "arrow right", value = "Worse")
            )
          ),
          fluidRow(
            column(
              5,
              sliderInput(session$ns("width_forest"), "Plot width(inch)", min = 1, max = 30, value = 16)
            ),
            column(
              5,
              sliderInput(session$ns("height_forest"), "Plot height(inch)", min = 1, max = 30, value = 3)
            )
          )
        )
      })
      output$downloadButton <- downloadHandler(
        filename = function() {
          paste(input$dep, "_forestplot.pptx", sep = "")
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
              zz <- figure()
              my_vec_graph <- rvg::dml(code = print(zz))

              doc <- officer::read_pptx()
              doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
              doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$width_forest, height = input$height_forest, top = 0, left = 0))
              print(doc, target = file)
            }
          )
        }
      )

      return(res)
    }
  )
}
