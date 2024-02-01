#' @title forestcoxUI:shiny module UI for forestcox
#' @description Shiny module UI for forestcox
#' @param id id
#' @param label label, Default: 'forestplot'
#' @return Shinymodule UI
#' @details Shinymodule UI for forestcox
#' @examples
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       forestcoxUI('Forest')
#'     ),
#'     mainPanel(
#'       DTOutput('tablesub')
#'     )
#'   )
#' )
#'
#'
#' server <- function(input, output, session) {
#'   data<-reactive(out)
#'
#'   outtable<-forestcoxServer('Forest',data=data,data_label=mk.lev(data) )
#'   output$tablesub<-renderDT({
#'     outtable()
#'   })
#' }
#' @rdname forestcoxUI
#' @export

forestcoxUI<-function(id,label='forestplot'){
  ns<-NS(id)
  outcome<-c('status')
  subvar<-c('kk','kk1')
  cov<-c('asdf','asdf')
  day<-c('time')


  tagList(

    uiOutput(ns('group_tbsub')),
    uiOutput(ns('dep_tbsub')),
    uiOutput(ns('day_tbsub')),
    uiOutput(ns('subvar_tbsub')),
    uiOutput(ns('cov_tbsub')),
    uiOutput(ns('time_tbsub')),
    downloadButton(ns('forest'), 'Download forest plot'),
    sliderInput(ns('width_forest'), 'Plot width(inch)', min = 1 , max = 30, value = 15),
    sliderInput(ns('height_forest'), 'Plot width(inch)', min = 1 , max = 30, value = 20),


  )

}


#' @title forestcoxServer:shiny module server for forestcox
#' @description Shiny module server for forestcox
#' @param id id
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @return Shiny module server for forestcox
#' @details Shiny module server for forestcox
#' @examples
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       forestcoxUI('Forest')
#'     ),
#'     mainPanel(
#'       DTOutput('tablesub')
#'     )
#'   )
#' )
#'
#'
#' server <- function(input, output, session) {
#'   data<-reactive(out)
#'
#'   outtable<-forestcoxServer('Forest',data=data,data_label=mk.lev(data) )
#'   output$tablesub<-renderDT({
#'     outtable()
#'   })
#' }
#' @seealso
#'  \code{\link[forestploter]{forest}}
#'  \code{\link[rvg]{dml}}
#'  \code{\link[officer]{read_pptx}}, \code{\link[officer]{add_slide}}, \code{\link[officer]{ph_with}}, \code{\link[officer]{ph_location}}
#' @rdname forestcoxServer
#' @export
#' @importFrom forestploter forest
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location

forestcoxServer<-function(id,data,data_label,data_varStruct=NULL,nfactor.limit=10){
  moduleServer(
    id,
    function(input, output, session) {

      label<-data_label
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
      output$group_tbsub<-renderUI({
        selectInput(session$ns('group'), 'Group', choices = vlist()$group_vars, selected = vlist()$group_vars[1])
      })
      output$dep_tbsub<-renderUI({
        selectInput(session$ns('dep'), 'Outcome', choices = vlist()$select_vars, selected = vlist()$select_vars[1])
      })
      output$subvar_tbsub<-renderUI({
        selectInput(session$ns('subvar'), 'Subgroup to include', choices =setdiff(vlist()$group_vars, input$group), selected = setdiff(vlist()$group_vars, input$group), multiple = T)

      })
      output$cov_tbsub<-renderUI({
        selectInput(session$ns('cov'), 'Addtional covariates', choices = vlist()$group_vars, selected = NULL, multiple = T)
      })
      output$day_tbsub<-renderUI({
        selectInput(session$ns('day'), 'Day', choices = vlist()$select_vars, selected = vlist()$select_vars[1])
      })
      output$time_tbsub<-renderUI({
        day <- input$day
        sliderInput(session$ns('time'), 'Time', min = min(data()[[day]]) , max = max(data()[[day]]), value = c(min(data()[[day]]), max(data()[[day]])))
      })


      tbsub<-reactive({

        data<-data()
        #  req(input$dep)
        # req(input$day)
        # req(input$subvar)
        # req(input$cov)
        # req(input$group)
        group.tbsub<-input$group
        var.event <- input$dep
        var.day <- input$day
        var.time<-input$time
        #data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
        data <- data[!(var.day < var.time[1])]
        data[[var.event]] <- ifelse(data[[var.day]] >= var.time[2] & data[[var.event]] == "1", 0, as.numeric(as.vector(data[[var.event]])))
        data[[var.day]] <- ifelse(data[[var.day]] >= var.time[2], var.time[2], data[[var.day]])
        data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))


        form <- as.formula(paste("Surv(", var.day, ",", var.event, ") ~ ", group.tbsub, sep = ""))
        vs <- input$subvar

        tbsub <-  TableSubgroupMultiCox(form, var_subgroups = vs,var_cov = setdiff(input$cov, vs), data=data, time_eventrate = 365 , line = F, decimal.hr = 3, decimal.percent = 1)
        #data[[var.event]] <- ifelse(data[[var.day]] > 365 * 5 & data[[var.event]] == 1, 0,  as.numeric(as.vector(data[[var.event]])))
        #tbsub <-  TableSubgroupMultiCox(form, var_subgroups = vs, data=data, time_eventrate = 365 , line = F, decimal.hr = 3, decimal.percent = 1)

        data<-setDT(data)
        lapply(vs,
               function(x){
                 cc <- data.table(t(c(x, NA, NA)))

                 dd <- lapply(levels(data[[group.tbsub]]),
                              function(y){
                                ev <- data[!is.na(get(x)) & get(group.tbsub) == y, sum(as.numeric(as.vector(get(var.event)))), keyby = get(x)]
                                nn <- data[!is.na(get(x)) & get(group.tbsub) == y, .N, keyby = get(x)]
                                vv <- paste0(ev[, V1], "/", nn[, N], " (", round(ev[, V1]/ nn[, N] * 100, 1), "%)")
                                data.table(ev[, 1], vv)
                              })
                 dd.bind <- cbind(dd[[1]], dd[[2]][, -1])
                 names(cc) <- names(dd.bind)
                 rbind(cc, dd.bind)
               }) %>% rbindlist -> ll

        ev.ov <- data[, sum(as.numeric(as.vector(get(var.event)))), keyby = get(group.tbsub)][, V1]
        nn.ov <- data[, .N, keyby = get(group.tbsub)][, N]

        ov <- data.table(t(c("OverAll", paste0(ev.ov, "/", nn.ov, " (", round(ev.ov/nn.ov * 100, 1), "%)"))))
        names(ov) <- names(ll)
        cn <- rbind(ov, ll)


        names(cn)[-1] <- label[variable == group.tbsub, val_label]
        tbsub <- cbind(Variable = tbsub[, 1], cn[, -1], tbsub[, c(8, 7, 4, 5, 6, 9, 10)])

        tbsub[-1, 1] <- unlist(lapply(vs, function(x){c(label[variable == x, var_label][1], paste0("     ", label[variable == x, val_label]))}))
        colnames(tbsub)[1:6] <- c("Subgroup", paste0("N(%): ", label[variable == group.tbsub, val_label]), paste0( var.time[2],"-",input$day,"KM rate(%): ", label[variable == group.tbsub, val_label]), "HR")



        return(tbsub)

      })

      res <- reactive({
        datatable(tbsub(), caption = paste0(input$dep_tbsub, " subgroup analysis: ", input$data_tbsub), rownames = F, extensions= "Buttons",
                  options = c(opt.tb1(paste0("tbsub_", input$data_tbsub)),
                              list(scrollX = TRUE, columnDefs = list(list(className = 'dt-right', targets = 0)))
                  ))
        # list({
        #   data <- tbsub()
        #   caption <- paste0(input$dep, " subgroup analysis: ", input$data)
        #   return(list(
        #     table = data,
        #     caption = caption
        #   ))
        # })
      })


      output$forest <- downloadHandler(
        filename =  function() {
          paste(input$dep,"_forestplot.pptx", sep="")

        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
          withProgress(message = 'Download in progress',
                       detail = 'This may take a while...', value = 0, {
                         for (i in 1:15) {
                           incProgress(1/15)
                           Sys.sleep(0.01)
                         }

                         data <- tbsub()
                         data_est<-data$`Absolute difference(%)`
                         data[[6]]<-paste0(data[[6]], " (", data[[7]], "-", data[[8]], ")")
                         data$` ` <- paste(rep(" ", 20), collapse = " ")
                         tm <- forest_theme(ci_Theight = 0.2)
                         forestploter::forest(data[,c(c(1:6),11,c(9:10))],
                                              lower=as.numeric(data$LCI),
                                              upper=as.numeric(data$UCI),
                                              ci_column =7,
                                              est=as.numeric(data_est),
                                              xlim=c(0, 5),
                                              theme=tm
                         )-> zz
                         my_vec_graph <- rvg::dml(code = print(zz))

                         doc <- officer::read_pptx()
                         doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                         doc <- officer::ph_with(doc, my_vec_graph, location = officer::ph_location(width = input$width_forest, height = input$height_forest, top = 0, left = 0))
                         print(doc, target = file)


                       })

        }
      )



      return(res)

    }
  )
}





