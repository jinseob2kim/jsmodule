#' @title forestglmUI:Shiny module UI for forestglm
#' @description Shiny module UI for forestcox
#' @param id id
#' @param label label, Default: 'forestplot'
#' @return Shinymodule UI
#' @details Shinymodule UI for forestglm
#' @examples
#' \dontrun{
#' if(interactive()){
#' library(survival);library(jstable);library(data.table);library(dplyr);library(shiny);library(DT)
#'
#' lung %>%
#'   mutate(
#'     status = factor(as.integer(status == 1)),
#'     sex = factor(sex),
#'     kk = factor(as.integer(pat.karno >= 70)),
#'     kk1 = factor(as.integer(pat.karno >= 60))
#'   ) -> lung
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       forestglmUI('Forest')
#'     ),
#'     mainPanel(
#'       DTOutput('tablesub')
#'     )
#'   )
#' )
#'
#' out<-lung
#'
#' server <- function(input, output, session) {
#'   data<-reactive(out)
#'   label<-reactive(mk.lev(out))
#'   outtable<-forestglmServer('Forest',data=data,data_label=label,family='gaussian')
#'   output$tablesub<-renderDT({
#'     outtable()
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#'  }
#' }
#' @rdname forestglmUI
#' @export
#'
#'
forestglmUI<-function(id,label='forestplot'){
  ns<-NS(id)
  tagList(

    uiOutput(ns('group_tbsub')),
    uiOutput(ns('dep_tbsub')),
    uiOutput(ns('subvar_tbsub')),
    uiOutput(ns('cov_tbsub')),
    downloadButton(ns('forest'), 'Download forest plot'),
    sliderInput(ns('width_forest'), 'Plot width(inch)', min = 1 , max = 30, value = 15),
    sliderInput(ns('height_forest'), 'Plot width(inch)', min = 1 , max = 30, value = 20),


  )

}



#' @title forestglmServer:shiny module server for forestglm
#' @description Shiny module server for forestglm
#' @param id id
#' @param data Reactive data
#' @param data_label Reactive data label
#' @param family family, "gaussian" or "binomial"
#' @param data_varStruct Reactive List of variable structure, Default: NULL
#' @param nfactor.limit nlevels limit in factor variable, Default: 10
#' @param design.survey reactive survey data. default: NULL
#' @return Shiny module server for forestglm
#' @details Shiny module server for forestglm
#' @examples
#' \dontrun{
#' if(interactive()){
#' library(survival);library(jstable);library(data.table);library(dplyr);library(shiny);library(DT)
#'
#' lung %>%
#'   mutate(
#'     status = factor(as.integer(status == 1)),
#'     sex = factor(sex),
#'     kk = factor(as.integer(pat.karno >= 70)),
#'     kk1 = factor(as.integer(pat.karno >= 60))
#'   ) -> lung
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       forestglmUI('Forest')
#'     ),
#'     mainPanel(
#'       DTOutput('tablesub')
#'     )
#'   )
#' )
#'
#' out<-lung
#'
#' server <- function(input, output, session) {
#'   data<-reactive(out)
#'   label<-reactive(mk.lev(out))
#'   outtable<-forestglmServer('Forest',data=data,data_label=label,family='gaussian')
#'   output$tablesub<-renderDT({
#'     outtable()
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{setDT}}
#'  \code{\link[survey]{surveysummary}}, \code{\link[survey]{svytable}}
#'  \code{\link[forestploter]{forest_theme}}, \code{\link[forestploter]{forest}}
#'  \code{\link[rvg]{dml}}
#'  \code{\link[officer]{read_pptx}}, \code{\link[officer]{add_slide}}, \code{\link[officer]{ph_with}}, \code{\link[officer]{ph_location}}
#' @rdname forestglmServer
#' @export
#' @importFrom data.table setDT
#' @importFrom survey svymean svyvar svytable
#' @importFrom forestploter forest_theme forest
#' @importFrom rvg dml
#' @importFrom officer read_pptx add_slide ph_with ph_location
forestglmServer<-function(id,data,data_label,family,data_varStruct=NULL,nfactor.limit=10,design.survey=NULL){
  moduleServer(
    id,
    function(input, output, session) {


      level <- val_label <- variable <- NULL

      if (is.null(data_varStruct)) {
        data_varStruct <- reactive(list(variable = names(data())))
      }


      vlist <- reactive({

        label <- data.table(data_label(), stringsAsFactors = T)
        data <- data.table(data(), stringsAsFactors = T)

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
        isNA_factor <- unlist(data[, lapply(.SD, function(x) {
          return (sum(is.na(x))!=0)
        }) ])
        validate(
          need(length(class01_factor) >= 1, "No categorical variables coded as 0, 1 in data")
        )
        isNA_vars<-names(data)[isNA_factor]
        factor_01vars <- factor_vars[class01_factor]

        factor_01_list <- mklist(data_varStruct(), factor_01vars)

        group_vars <- factor_vars[nclass_factor >= 2 & nclass_factor <= nfactor.limit & nclass_factor < nrow(data())]
        group_list <- mklist(data_varStruct(), group_vars)
        group2_vars<-factor_vars[nclass_factor==2]
        except_vars <- factor_vars[nclass_factor > nfactor.limit | nclass_factor == 1 | nclass_factor == nrow(data())]
        return(list(
          factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list, conti_vars_positive = conti_vars_positive,
          isNA_vars=isNA_vars,group2_vars= group2_vars,factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_vars = group_vars, group_list = group_list, except_vars = except_vars
        ))
      })
      dep<-reactive({
        if(family=='binomial'){
          return(setdiff(vlist()$factor_01vars,vlist()$isNA_vars))
        }
        return(setdiff(names(data()),vlist()$isNA_vars))
      })

      output$group_tbsub<-renderUI({
        req(input$dep)
        selectInput(session$ns('group'), 'Group', choices = vlist()$group2_vars, selected = setdiff(vlist()$group2_vars,input$dep)[1])
      })
      output$dep_tbsub<-renderUI({

        selectInput(session$ns('dep'), 'Outcome', choices = dep(), selected = dep()[1])
      })
      output$subvar_tbsub<-renderUI({
        req(input$group)
        req(input$dep)
        selectInput(session$ns('subvar'), 'Subgroup to include', choices =setdiff(vlist()$group_vars, c(input$group,input$dep)), selected = setdiff(vlist()$group_vars, c(input$group,input$dep)), multiple = T)

      })
      output$cov_tbsub<-renderUI({
        selectInput(session$ns('cov'), 'Addtional covariates', choices = vlist()$group_vars, selected = NULL, multiple = T)
      })




      tbsub<-reactive({
        label <- data_label()
        var.event <- input$dep
        group.tbsub<-input$group
        if(is.null(design.survey)){
          data<-data()
          data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
          coxdata<-data
        }else{
          data<-design.survey()$variables
          data[[var.event]] <- as.numeric(as.vector(data[[var.event]]))
          coxdata<-design.survey()
          coxdata$variables<-data
        }




        form <- as.formula(paste( var.event, " ~ ", group.tbsub, sep = ""))
        vs <- input$subvar

        #data[[var.event]] <- ifelse(data[[var.day]] > 365 * 5 & data[[var.event]] == 1, 0,  as.numeric(as.vector(data[[var.event]])))
        tbsub <-  TableSubgroupMultiGLM(form, var_subgroups = vs,var_cov = setdiff(input$cov, vs), data=coxdata,family=family)
        #tbsub <-  TableSubgroupMultiGLM(form, var_subgroups = vs, data=coxdata,family=family)
        len<-nrow(label[variable==group.tbsub])
        data<-data.table::setDT(data)
        if(family=='gaussian'){
          setnames(tbsub,'Point.Estimate','Beta')
          if(is.null(design.survey)){
            meanvar<-data[, .(round(mean(get(var.event),na.rm=TRUE),2),round(var(get(var.event),na.rm=TRUE),2))]%>%mutate(mean=paste(V1,'±',V2))
            meanvar<-meanvar[,3]
          }else{
            ss<-paste('~',var.event)
            meanvar<-data.table(mean=paste(round(ftable(survey::svymean(as.formula(ss),coxdata))[,1],2),'±',round(ftable(survey::svyvar(as.formula(ss),coxdata))[,1],2)))
          }
          meanvar<-rbind(meanvar,
          lapply(vs,
                function(x){
                  cc<-data.table(mean=NA)
                  for( y in levels(data[[x]])){
                    if(is.null(design.survey)){
                    ev <- data[!is.na(get(x)) & get(x) == y, .(round(mean(get(var.event),na.rm=TRUE),2),round(var(get(var.event),na.rm=TRUE),2))]%>%
                      mutate(mean=paste(V1,'±',V2))
                    cc<-rbind(cc,ev[,3])
                    }else{
                      sub<-subset(coxdata, !is.na(get(x)) & get(group.tbsub) == y)
                      ss<-paste('~',var.event)
                      ev<-data.table(mean=paste(round(ftable(survey::svymean(as.formula(ss),sub))[,1],2),'±',round(ftable(survey::svyvar(as.formula(ss),sub))[,1],2)))
                      cc<-rbind(cc,ev)

                    }
                  }
                  cc
                })%>%rbindlist
          )
          tbsub<-cbind(tbsub[,1],meanvar,tbsub[4:8])
          colnames(tbsub)[1]<-'Subgroup'
          return(tbsub)
        }
        if(is.null(design.survey)){
        ev.ov <- data[!is.na(get(group.tbsub)), sum(as.numeric(as.vector(get(var.event))),na.rm=TRUE), keyby = get(group.tbsub)][, V1]
        nn.ov <- data[!is.na(get(group.tbsub)), .N, keyby = get(group.tbsub)][, N]
        }else{
          ev.ov <- round(survey::svytable(as.formula(paste0("~", var.event, "+", group.tbsub)), design = coxdata)[2, ], 1)
          nn.ov <- round(survey::svytable(as.formula(paste0("~", group.tbsub)), design = coxdata), 1)

        }
        ov <- data.table(t(c("OverAll", paste0(ev.ov, "/", nn.ov, " (", round(ev.ov/nn.ov * 100, 1), "%)"))))

        if(!is.null(vs)){
          lapply(vs,
                 function(x){
                   cc<-data.table(matrix(ncol=len+1))
                   cc[[1]]<-x

                   dd.bind<-' '
                   getlev<-data.table(get=levels(data[[x]]))
                   for( y in levels(data[[group.tbsub]])){
                     if(is.null(design.survey)){
                     ev <- data[!is.na(get(x)) & get(group.tbsub) == y, sum(as.numeric(as.vector(get(var.event))),na.rm=TRUE), keyby = get(x)]
                     nn <- data[!is.na(get(x)) & get(group.tbsub) == y, .N, keyby = get(x)]
                     vv<-data.table(get=ev[,get],paste0(ev[, V1], "/", nn[, N], " (", round(ev[, V1]/ nn[, N] * 100, 1), "%)"))
                     ee<-merge(data.table(get=levels(ev[,get])),vv,all.x = TRUE)
                     dd.bind<-cbind(dd.bind,ee[,V2])
                     }else{
                       svy<-survey::svytable(as.formula(paste0("~", var.event, "+", x)), design = subset(coxdata, !is.na(get(x)) & get(group.tbsub) == y))
                       ev <- round(svy[2, ], 1)
                       nn <- round(survey::svytable(as.formula(paste0("~", x)), design = subset(coxdata, !is.na(get(x)) & get(group.tbsub) == y)), 1)
                       vv <- data.table(get=colnames(svy),paste0(ev, "/", nn, " (", round(ev/ nn * 100, 1), "%)"))
                       ee<-merge(getlev,vv,all.x=TRUE)
                       dd.bind<-cbind(dd.bind,ee[,V2])
                     }
                   }
                   names(cc) <- names(dd.bind)
                   rbind(cc, dd.bind)
                 }) %>% rbindlist -> ll


          names(ov) <- names(ll)
          cn <- rbind(ov, ll)


          names(cn)[-1] <- label[variable == group.tbsub, val_label]
          tbsub <- cbind(Variable = paste0(tbsub[,1]," ",rownames(tbsub)), cn[, -1], tbsub[, c( names(tbsub)[4:6], 'P value','P for interaction')])

          tbsub[-(len-1), 1] <- unlist(lapply(vs, function(x){c(label[variable == x, var_label][1], paste0("     ", label[variable == x, val_label]))}))
          colnames(tbsub)[1:(1+len)] <- c("Subgroup", paste0("N(%): ", label[variable == group.tbsub, val_label]))

        }else{
          cn<-ov
          names(cn)[-1] <- label[variable == group.tbsub, val_label]
          tbsub <- cbind(Variable = paste0(tbsub[,1]," ",rownames(tbsub)), cn[, -1], tbsub[, c( names(tbsub)[4:6], 'P value','P for interaction')])

          colnames(tbsub)[1:(1+nrow(label[variable==group.tbsub]))] <- c("Subgroup", paste0("N(%): ", label[variable == group.tbsub, val_label]))

        }
        return(tbsub)

      })

      res <- reactive({
        datatable(tbsub(), caption = paste0(input$dep, " subgroup analysis"), rownames = F, extensions= "Buttons",
                  options = c(opt.tb1(paste0("tbsub_",input$dep)),
                              list(scrollX = TRUE, columnDefs = list(list(className = 'dt-right', targets = 0)))
                  ))
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

                         data <- data.table::setDT(tbsub())
                         group.tbsub<-input$group
                         if(family=='gaussian'){
                           r<-'Beta'
                           ll<-1
                         }else{
                          r<-'OR'
                          ll<-nrow(data_label()[variable==group.tbsub])
                          data[OR==0|Lower==0,':='(OR=NA,Lower=NA,Upper=NA)]
                          data<-mutate(data,OR=round(log(as.numeric(OR)),2),
                                       Lower=round(log(as.numeric(Lower)),2),Upper=round(log(as.numeric(Upper)),2))
                         }

                         len<-ncol(data)
                         data_est<-data[,get(r)]
                         data<-data%>%mutate(!!r:=paste0(get(r), " (", Lower, "-", Upper, ")"))
                         data$` ` <- paste(rep(" ", 20), collapse = " ")
                         tm <- forestploter::forest_theme(ci_Theight = 0.2)
                         selected_columns<-c(c(1:(2+ll)),len+1,(len-1):(len))
                         forestploter::forest(data[,..selected_columns],
                                              lower=as.numeric(data$Lower),
                                              upper=as.numeric(data$Upper),
                                              ci_column =3+ll,
                                              est=as.numeric(data_est),
                                              ref_line = 1,
                                              xlim=c(min(as.numeric(data$Lower),na.rm=TRUE), max(as.numeric(data$Upper),na.rm=TRUE)),
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





