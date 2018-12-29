
#' @title jsPropensityGadget: Shiny Gadget for propensity score analysis.
#' @description Shiny Gadget for propensity score analysis.
#' @param data data
#' @return Gadget
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  jsPropensityGadget(mtcars)
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{data.table}}
#'  \code{\link[MatchIt]{matchit}},\code{\link[MatchIt]{match.data}}
#'  \code{\link[jstable]{cox2.display}},\code{\link[jstable]{svycox.display}}
#'  \code{\link[survival]{survfit}},\code{\link[survival]{coxph}},\code{\link[survival]{Surv}}
#'  \code{\link[jskm]{jskm}},\code{\link[jskm]{svyjskm}}
#'  \code{\link[ggplot2]{ggsave}}
#'  \code{\link[survey]{svykm}}
#' @rdname jsPropensityGadget
#' @export
#' @importFrom data.table data.table
#' @importFrom MatchIt matchit match.data
#' @importFrom jstable cox2.display svycox.display
#' @importFrom survival survfit
#' @importFrom jskm jskm svyjskm
#' @importFrom ggplot2 ggsave
#' @importFrom survey svykm
#' @importFrom purrr map_lgl
#' @importFrom stats model.frame

jsPropensityGadget <- function(data){

  ## Data label
  out.old <- data.table::data.table(data)
  name.old <- names(out.old)
  out <- data.table::data.table(data, check.names = T)
  name.new <- names(out)
  ref <- list(name.old = name.old, name.new = name.new)

  data_varStruct1 = list(variable = names(out))

  ## Vars
  naCol <- names(out)[colSums(is.na(out)) > 0]
  out <- out[, .SD, .SDcols = -naCol]

  factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
  if (!is.null(factor_vars) & length(factor_vars) > 0){
    out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
  }

  conti_original <- setdiff(names(out), factor_vars)
  nclass <- unlist(out[, lapply(.SD, function(x){length(unique(x))}), .SDcols = conti_original])
  factor_adds_list = mklist(data_varStruct1, names(nclass)[(nclass <= 20) & (nclass < nrow(out))])

  except_vars <- names(nclass)[ nclass== 1 | nclass >= 10]
  factor_adds <- names(nclass)[nclass >= 1 &  nclass <= 5]





  ui <- navbarPage("Propensity score analysis",
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("factor"),
                                uiOutput("group_ps"),
                                uiOutput("indep_ps"),
                                radioButtons("pcut_ps", label = "Default p-value cut for ps calculation",
                                             choices = c(0.05, 0.1, 0.2),
                                             selected = 0.1, inline =T)
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                            tabPanel("Matching data", withLoader(DTOutput("matdata"), type="html", loader="loader6")),
                                            tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                ),
                                htmlOutput("naomit")
                              )
                            )
                   ),
                   tabPanel("Table 1",
                            sidebarLayout(
                              sidebarPanel(
                                tb1simpleUI("tb1")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Original",
                                                     withLoader(DTOutput("table1_original"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("Matching",
                                                     withLoader(DTOutput("table1_ps"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            ),
                                            tabPanel("IPTW",
                                                     withLoader(DTOutput("table1_iptw"), type="html", loader="loader6"),
                                                     wellPanel(
                                                       h5("Normal continuous variables  are summarized with Mean (SD) and complex survey regression"),
                                                       h5("Non-normal continuous variables are summarized with median [IQR] and complex sampling rank test"),
                                                       h5("Categorical variables  are summarized with table")
                                                     )
                                            )
                                )
                              )
                            )
                   ),
                   tabPanel("Cox model",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("coxUI_eventtime"),
                                uiOutput("coxUI_indep"),
                                checkboxInput("coxUI_subcheck", "Sub-group analysis"),
                                uiOutput("coxUI_subvar"),
                                uiOutput("coxUI_subval")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Original",
                                                     withLoader(DTOutput("cox_original"), type="html", loader="loader6")
                                            ),
                                            tabPanel("Matching",
                                                     withLoader(DTOutput("cox_ps"), type="html", loader="loader6")
                                            ),
                                            tabPanel("IPTW",
                                                     withLoader(DTOutput("cox_iptw"), type="html", loader="loader6")
                                            )
                                )
                              )
                            )
                   ),
                   tabPanel("Kaplan-meier plot",
                            sidebarLayout(
                              sidebarPanel(
                                uiOutput("kmUI_eventtime"),
                                uiOutput("kmUI_indep"),
                                checkboxInput("km_cumhaz", "Show cumulative hazard", F),
                                checkboxInput("km_pval", "Show p-value(log-rank test)", T),
                                checkboxInput("km_table", "Show table", T),
                                checkboxInput("km_subcheck", "Sub-group analysis"),
                                uiOutput("km_subvar"),
                                uiOutput("km_subval")
                              ),
                              mainPanel(
                                tabsetPanel(type = "pills",
                                            tabPanel("Original",
                                                     withLoader(plotOutput("km_original"), type="html", loader="loader6"),
                                                     h3("Download options"),
                                                     wellPanel(
                                                       uiOutput("downloadControls_km_original"),
                                                       downloadButton("downloadButton_km_original", label = "Download the plot")
                                                     )
                                            ),
                                            tabPanel("Matching",
                                                     withLoader(plotOutput("km_ps"), type="html", loader="loader6"),
                                                     h3("Download options"),
                                                     wellPanel(
                                                       uiOutput("downloadControls_km_ps"),
                                                       downloadButton("downloadButton_km_ps", label = "Download the plot")
                                                     )
                                            ),
                                            tabPanel("IPTW",
                                                     withLoader(plotOutput("km_iptw"), type="html", loader="loader6"),
                                                     h3("Download options"),
                                                     wellPanel(
                                                       uiOutput("downloadControls_km_iptw"),
                                                       downloadButton("downloadButton_km_iptw", label = "Download the plot")
                                                     )
                                            )
                                )
                              )
                            )
                   )

  )

  server <- function(input, output, session) {

    output$factor <- renderUI({
      selectInput("factor_vname", label = "Additional categorical variables",
                  choices = factor_adds_list, multiple = T,
                  selected = factor_adds)
    })

    data1 <- reactive({
      out1 <- out
      out1[, (conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = conti_original]
      if (!is.null(input$factor_vname) & length(input$factor_vname) > 0){
        out1[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
      }
      return(out1)
    })

    data.label <- eventReactive(data(), {
      labeldata <- mk.lev(data1())
      for (vn in ref[["name.new"]]){
        w <- which(ref[["name.new"]] == vn)
        labeldata[variable ==vn, var_label := ref[["name.old"]][w]]
      }
      return(labeldata)
    })

    output$group_ps <- renderUI({
      factor_vars <- names(data1())[data1()[, lapply(.SD, class) %in% c("factor", "character")]]
      validate(
        need(!is.null(factor_vars) & length(factor_vars) > 0, "No categorical variables in data")
      )

      class01_factor <- unlist(data1()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}), .SDcols = factor_vars])
      #nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])
      #factor_2vars <- names(nclass_factor)[nclass_factor == 2]


      validate(
        need(!is.null(class01_factor), "No categorical variables coded as 0, 1 in data")
      )

      factor_01vars <- factor_vars[class01_factor]
      factor_01vars_case_small <- factor_01vars[unlist(sapply(factor_01vars, function(x){diff(table(data1()[[x]])) <= 0}))]

      selectInput("group_pscal", label = "Group variable for PS calculation (0, 1 coding)",
                  choices = mklist(data_varStruct1, factor_01vars_case_small), multiple = F,
                  selected = factor_01vars_case_small[1])
    })

    observeEvent(input$group_pscal , {
      output$indep_ps <- renderUI({
        if (is.null(input$group_pscal)){
          return(NULL)
        }
        validate(
          need(length(input$group_pscal) > 0, "No group variables in data")
        )

        vars <- setdiff(setdiff(names(data1()), except_vars),  input$group_pscal)
        varsIni <- sapply(vars,
                          function(v){
                            forms <- as.formula(paste(input$group_pscal, "~", v))
                            coef <- summary(glm(forms, data = data1(), family = binomial))$coefficients
                            sigOK <- !all(coef[-1, 4] > as.numeric(input$pcut_ps))
                            return(sigOK)
                          })
        tagList(
          selectInput(session$ns("indep_pscal"), label = "Independent variables for PS calculation",
                      choices = mklist(data_varStruct1, vars), multiple = T,
                      selected = vars[varsIni])
        )
      })
    })

    mat.info <- eventReactive(input$indep_pscal, {
      if (is.null(input$group_pscal) | is.null(input$indep_pscal)){
        return(NULL)
      }

      forms <- as.formula(paste(input$group_pscal, " ~ ", paste(input$indep_pscal, collapse = "+"), sep=""))
      m.out <- MatchIt::matchit(forms, data = data1())
      pscore <- m.out$distance
      iptw <- ifelse(m.out$treat == levels(m.out$treat)[2], 1/pscore,  1/(1-pscore))
      wdata <- cbind(data1(), pscore, iptw)

      mdata <- MatchIt::match.data(m.out, distance = "pscore")
      return(list(data = wdata, matdata = mdata[, -grep("weights", names(mdata))]))
    })


    ## For Other analysis
    group_var = reactive(input$group_pscal)



    output$data <- renderDT({
      datatable(mat.info()$data, rownames=F, editable = F, extensions= "Buttons", caption = "Data with ps, iptw",
                options = c(opt.data("data"), list(scrollX = TRUE))
      )
    })

    output$matdata <- renderDT({
      datatable(mat.info()$matdata, rownames=F, editable = F, extensions= "Buttons", caption = "Matching data",
                options = c(opt.data("matching_data"), list(scrollX = TRUE))
      )
    })

    output$data_label <- renderDT({
      datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
                options = c(opt.data("data_label"), list(scrollX = TRUE))
      )
    })

    output$naomit <- renderText({
      if (length(naCol) == 0) {
        return("Data has <B>no</B> missing values.")
      } else{
        txt_miss <- paste(naCol, collapse = ", ")
        return(paste("Column <B>", txt_miss, "</B> are(is) excluded due to missing value.", sep = ""))
      }
    })


    ## Table 1
    data <- reactive(mat.info()$data)
    matdata <- reactive(mat.info()$matdata)
    data_varStruct <- reactive(list(variable = names(mat.info()$matdata)))


    vlist <- eventReactive(mat.info(), {

      mklist <- function(varlist, vars){
        lapply(varlist,
               function(x){
                 inter <- intersect(x, vars)
                 if (length(inter) == 1){
                   inter <- c(inter, "")
                 }
                 return(inter)
               })
      }


      factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
      #factor_vars <- names(data())[sapply(names(data()), function(x){class(data()[[x]]) %in% c("factor", "character")})]
      factor_list <- mklist(data_varStruct(), factor_vars)


      conti_vars <- setdiff(names(data()), c(factor_vars, "pscore", "iptw"))
      conti_list <- mklist(data_varStruct(), conti_vars)

      nclass_factor <- unlist(data()[, lapply(.SD, function(x){length(unique(x)[!is.na(unique(x))])}), .SDcols = factor_vars])
      #nclass_factor <- sapply(factor_vars, function(x){length(unique(data()[[x]]))})
      class01_factor <- unlist(data()[, lapply(.SD, function(x){identical(levels(x), c("0", "1"))}), .SDcols = factor_vars])

      validate(
        need(!is.null(class01_factor), "No categorical variables coded as 0, 1 in data")
      )
      factor_01vars <- factor_vars[class01_factor]

      factor_01_list <- mklist(data_varStruct(), factor_01vars)

      group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10 & nclass_factor < nrow(data())]
      group_list <- mklist(data_varStruct(), group_vars)


      except_vars <- factor_vars[nclass_factor > 10 | nclass_factor == 1 | nclass_factor == nrow(data())]

      ## non-normal: shapiro test
      f <- function(x) {
        if (diff(range(x, na.rm = T)) == 0) return(F) else return(shapiro.test(x)$p.value <= 0.05)
      }

      non_normal <- ifelse(nrow(data()) <=3 | nrow(data()) >= 5000,
                           rep(F, length(conti_vars)),
                           sapply(conti_vars, function(x){f(data()[[x]])})
      )

      return(list(factor_vars = factor_vars, factor_list = factor_list, conti_vars = conti_vars, conti_list = conti_list,
                  factor_01vars = factor_01vars, factor_01_list = factor_01_list, group_list = group_list, except_vars = except_vars, non_normal = non_normal)
      )

    })




    out.tb1 <- callModule(tb1simple2, "tb1", data = data, matdata = matdata, data_label = data.label, data_varStruct = data_varStruct,
                          vlist = vlist, group_var = group_var)

    output$table1_original <- renderDT({
      tb = out.tb1()$original$table
      cap = out.tb1()$original$caption
      out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                          options = c(opt.tb1("tb1_original"),
                                      list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                      ),
                                      list(scrollX = TRUE)
                          )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })

    output$table1_ps <- renderDT({
      tb = out.tb1()$ps$table
      cap = out.tb1()$ps$caption
      out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = paste(cap, "- PS matching"),
                          options = c(opt.tb1("tb1_ps"),
                                      list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                      ),
                                      list(scrollX = TRUE)
                          )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })

    output$table1_iptw <- renderDT({
      tb = out.tb1()$iptw$table
      cap = out.tb1()$iptw$caption
      out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                          options = c(opt.tb1("tb1_iptw"),
                                      list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                      ),
                                      list(scrollX = TRUE)
                          )
      )
      if ("sig" %in% colnames(tb)){
        out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
      }
      return(out.tb1)
    })


    ## Cox

    output$coxUI_eventtime <- renderUI({
      mklist <- function(varlist, vars){
        lapply(varlist,
               function(x){
                 inter <- intersect(x, vars)
                 if (length(inter) == 1){
                   inter <- c(inter, "")
                 }
                 return(inter)
               })
      }

      tagList(
        selectInput("event_cox", "Event",
                    choices = mklist(data_varStruct(), setdiff(vlist()$factor_01vars, group_var())), multiple = F,
                    selected = NULL
        ),
        selectInput("time_cox", "Time",
                    choices = vlist()$conti_list, multiple = F,
                    selected = NULL
        )
      )
    })


    output$coxUI_indep <- renderUI({
      req(!is.null(input$event_cox))
      req(!is.null(input$time_cox))
      indep.cox <- setdiff(names(mat.info()$matdata), c(vlist()$except_vars, input$event_cox, input$time_cox ))
      mklist <- function(varlist, vars){
        lapply(varlist,
               function(x){
                 inter <- intersect(x, vars)
                 if (length(inter) == 1){
                   inter <- c(inter, "")
                 }
                 return(inter)
               })
      }

      tagList(
        selectInput("indep_cox", "Independent variables",
                    choices = mklist(data_varStruct(), indep.cox), multiple = T,
                    selected = group_var()
        )
      )
    })

    observeEvent(input$indep_cox, {
      output$coxUI_subvar <- renderUI({
        req(input$coxUI_subcheck == T)
        factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
        factor_subgroup <- setdiff(factor_vars, c(input$event_cox, input$indep_cox))
        factor_subgroup_list <- mklist(data_varStruct(), factor_subgroup)
        validate(
          need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
        )

        tagList(
          selectInput("subvar_cox", "Sub-group variable",
                      choice = factor_subgroup_list, multiple = F,
                      selected = factor_subgroup[1])
        )
      })

    })


    output$coxUI_subval <- renderUI({
      req(input$coxUI_subcheck == T)
      req(input$subvar_cox)
      selectInput("subval_cox", "Sub-group value",
                  choice = data.label()[variable == input$subvar_cox, val_label], multiple = F,
                  selected = data.label()[variable == input$subvar_cox, val_label][1])
    })



    form.cox <- reactive({
      validate(
        need(!is.null(input$indep_cox), "Please select at least 1 independent variable.")
      )
      as.formula(paste("survival::Surv(",input$time_cox,",", input$event_cox,") ~ ", paste(input$indep_cox, collapse="+"),sep=""))
    })



    output$cox_original <- renderDT({
      data.cox <- mat.info()$data
      data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))
      if(input$coxUI_subcheck == T){
        req(input$subvar_cox)
        data.cox <- data.cox[get(input$subvar_cox) == input$subval_cox, ]
      }
      mf <- model.frame(form.cox(), data.cox)
      validate(
        need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
      )
      lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
      validate(
        need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
      )

      cc = substitute(survival::coxph(.form, data= data.cox, model = T), list(.form= form.cox()))
      res.cox = eval(cc)
      tb.cox <- jstable::cox2.display(res.cox)
      tb.cox <- jstable::LabeljsCox(tb.cox, data.label())
      out.cox <- rbind(tb.cox$table, tb.cox$metric)
      sig <- out.cox[, ncol(out.cox)]
      sig <- gsub("< ", "", sig)
      sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
      out.cox <- cbind(out.cox, sig)
      cap.cox <- paste("Cox's proportional hazard model on time ('", data.label()[variable == input$time_cox, var_label][1] , "') to event ('", data.label()[variable == input$event_cox, var_label][1], "')", sep="")
      if(input$coxUI_subcheck == T){
        cap.cox <- paste(cap.cox, " - ", data.label()[variable == input$subvar_cox, var_label][1], ": ", data.label()[variable == input$subvar_cox & level == input$subval_cox, val_label], sep = "")
      }
      hide = which(colnames(out.cox) == c("sig"))
      datatable(out.cox, rownames=T, extensions= "Buttons", caption = cap.cox,
                options = c(opt.tbreg(cap.cox),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))

    })


    output$cox_ps <- renderDT({
      data.cox <- mat.info()$matdata
      data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))
      if(input$coxUI_subcheck == T){
        req(input$subvar_cox)
        data.cox <- data.cox[get(input$subvar_cox) == input$subval_cox, ]
      }
      mf <- model.frame(form.cox(), data.cox)
      validate(
        need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
      )
      lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
      validate(
        need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
      )
      cc = substitute(survival::coxph(.form, data= data.cox, model = T), list(.form= form.cox()))
      res.cox = eval(cc)
      tb.cox <- jstable::cox2.display(res.cox)
      tb.cox <- jstable::LabeljsCox(tb.cox, data.label())
      out.cox <- rbind(tb.cox$table, tb.cox$metric)
      sig <- out.cox[, ncol(out.cox)]
      sig <- gsub("< ", "", sig)
      sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
      out.cox <- cbind(out.cox, sig)
      cap.cox <- paste("Cox's proportional hazard model on time ('", data.label()[variable == input$time_cox, var_label][1] , "') to event ('", data.label()[variable == input$event_cox, var_label][1], "') - Matching data", sep="")
      if(input$coxUI_subcheck == T){
        cap.cox <- paste(cap.cox, " - ", data.label()[variable == input$subvar_cox, var_label][1], ": ", data.label()[variable == input$subvar_cox & level == input$subval_cox, val_label], sep = "")
      }
      hide = which(colnames(out.cox) == c("sig"))
      datatable(out.cox, rownames=T, extensions= "Buttons", caption = cap.cox,
                options = c(opt.tbreg(cap.cox),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))

    })

    output$cox_iptw <- renderDT({
      data.cox <- mat.info()$data
      data.cox[[input$event_cox]] <- as.numeric(as.vector(data.cox[[input$event_cox]]))
      if(input$coxUI_subcheck == T){
        req(input$subvar_cox)
        data.cox <- data.cox[get(input$subvar_cox) == input$subval_cox, ]
      }
      data.design <- survey::svydesign(ids = ~ 1, data = data.cox, weights = ~ iptw)
      mf <- model.frame(form.cox(), data.cox)
      validate(
        need(nrow(mf) > 0, paste("No complete data due to missingness. Please remove some variables from independent variables"))
      )
      lgl.1level <- purrr::map_lgl(mf, ~length(unique(.x)) == 1)
      validate(
        need(sum(lgl.1level) == 0, paste(paste(names(lgl.1level)[lgl.1level], collapse =" ,"), "has(have) a unique value. Please remove that from independent variables"))
      )

      cc = substitute(survey::svycoxph(.form, design= data.design), list(.form= form.cox()))
      res.cox = eval(cc)
      tb.cox <- jstable::svycox.display(res.cox)
      tb.cox <- jstable::LabeljsCox(tb.cox, data.label())
      out.cox <- rbind(tb.cox$table, tb.cox$metric)
      sig <- out.cox[, ncol(out.cox)]
      sig <- gsub("< ", "", sig)
      sig <- ifelse(as.numeric(as.vector(sig)) <= 0.05, "**", NA)
      out.cox <- cbind(out.cox, sig)
      cap.cox <- paste("Weighted cox's proportional hazard model on time ('", data.label()[variable == input$time_cox, var_label][1] , "') to event ('", data.label()[variable == input$event_cox, var_label][1], "') ", sep="")
      if(input$coxUI_subcheck == T){
        cap.cox <- paste(cap.cox, " - ", data.label()[variable == input$subvar_cox, var_label][1], ": ", data.label()[variable == input$subvar_cox & level == input$subval_cox, val_label], sep = "")
      }
      hide = which(colnames(out.cox) == c("sig"))
      datatable(out.cox, rownames=T, extensions= "Buttons", caption = cap.cox,
                options = c(opt.tbreg(cap.cox),
                            list(columnDefs = list(list(visible=FALSE, targets= hide))
                            )
                )
      )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))

    })

    ## Kaplan

    output$kmUI_eventtime <- renderUI({
      mklist <- function(varlist, vars){
        lapply(varlist,
               function(x){
                 inter <- intersect(x, vars)
                 if (length(inter) == 1){
                   inter <- c(inter, "")
                 }
                 return(inter)
               })
      }

      tagList(
        selectInput("event_km", "Event",
                    choices = mklist(data_varStruct(), setdiff(vlist()$factor_01vars, group_var())), multiple = F,
                    selected = NULL
        ),
        selectInput("time_km", "Time",
                    choices = vlist()$conti_list, multiple = F,
                    selected = NULL
        )
      )
    })


    output$kmUI_indep <- renderUI({
      indep.km <- setdiff(vlist()$factor_vars, c(vlist()$except_vars, input$event_km, input$time_km ))
      mklist <- function(varlist, vars){
        lapply(varlist,
               function(x){
                 inter <- intersect(x, vars)
                 if (length(inter) == 1){
                   inter <- c(inter, "")
                 }
                 return(inter)
               })
      }

      tagList(
        selectInput("indep_km", "Independent variables",
                    choices = mklist(data_varStruct(), indep.km), multiple = F,
                    selected = group_var()
        )
      )
    })

    observeEvent(input$indep_km,{
      output$km_subvar <- renderUI({
        req(input$km_subcheck == T)
        factor_vars <- names(data())[data()[, lapply(.SD, class) %in% c("factor", "character")]]
        factor_subgroup <- setdiff(factor_vars, c(input$event_km, input$indep_km))
        factor_subgroup_list <- mklist(data_varStruct(), factor_subgroup)
        validate(
          need(length(factor_subgroup) > 0 , "No factor variable for sub-group analysis")
        )

        tagList(
          selectInput("subvar_km", "Sub-group variable",
                      choice = factor_subgroup_list, multiple = F,
                      selected = factor_subgroup[1])
        )
      })
    })


    output$km_subval <- renderUI({
      req(input$km_subcheck == T)
      req(input$subvar_km)
      selectInput("subval_km", "Sub-group value",
                  choice = data.label()[variable == input$subvar_km, val_label], multiple = F,
                  selected = data.label()[variable == input$subvar_km, val_label][1])
    })



    form.km <- reactive({
      validate(
        need(!is.null(input$indep_km), "Please select at least 1 independent variable.")
      )
      as.formula(paste("survival::Surv(",input$time_km,",", input$event_km,") ~ ", input$indep_km ,sep=""))
    })

    ## KM original
    km_original_input <- reactive({
      data.km <- mat.info()$data
      data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
      if (input$km_subcheck == T){
        req(input$subvar_km)
        data.km <- data.km[get(input$subvar_km) == input$subval_km, ]
      }
      cc = substitute(survival::survfit(.form, data= data.km), list(.form= form.km()))
      res.km = eval(cc)
      yst.name = data.label()[variable == input$indep_km, var_label][1]
      yst.lab = data.label()[variable == input$indep_km, val_label]
      ylab = ifelse(input$km_cumhaz, "Cumulative hazard(%)", "Survival(%)")

      return(
        jskm::jskm(res.km, pval = input$km_pval, mark=F, table= input$km_table, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F, legendposition = c(0.9, 0.2),
             cumhaz= input$km_cumhaz, cluster.option = "None", cluster.var = NULL, data = data.km)
      )

    })

    output$km_original <- renderPlot({
      print(km_original_input())
    })

    output$downloadControls_km_original <- renderUI({
      fluidRow(
        column(4,
               selectizeInput("km_original_file_ext", "File extension (dpi = 300)",
                              choices = c("jpg","pdf", "tiff", "svg"), multiple = F,
                              selected = "jpg"
               )
        ),
        column(4,
               sliderInput("fig_width_km_original", "Width (in):",
                           min = 5, max = 20, value = 8
               )
        ),
        column(4,
               sliderInput("fig_height_km_original", "Height (in):",
                           min = 5, max = 20, value = 6
               )
        )
      )
    })

    output$downloadButton_km_original <- downloadHandler(
      filename =  function() {
        paste(input$event_km, input$indep_km,"_kaplan_meier.",input$km_original_file_ext ,sep="")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggplot2::ggsave(file ,km_original_input(), dpi = 300, units = "in", width = input$fig_width_km_original, height =input$fig_height_km_original)

      }
    )


    ## KM ps
    km_ps_input <- reactive({
      data.km <- mat.info()$matdata
      data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
      if (input$km_subcheck == T){
        req(input$subvar_km)
        data.km <- data.km[get(input$subvar_km) == input$subval_km, ]
      }
      cc = substitute(survival::survfit(.form, data= data.km), list(.form= form.km()))
      res.km = eval(cc)
      yst.name = data.label()[variable == input$indep_km, var_label][1]
      yst.lab = data.label()[variable == input$indep_km, val_label]
      ylab = ifelse(input$km_cumhaz, "Cumulative hazard(%)", "Survival(%)")

      return(
        jskm::jskm(res.km, pval = input$km_pval, mark=F, table= input$km_table, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= F, legendposition = c(0.9, 0.2),
             cumhaz= input$km_cumhaz, cluster.option = "None", cluster.var = NULL, data = data.km)
      )

    })

    output$km_ps <- renderPlot({
      print(km_ps_input())
    })

    output$downloadControls_km_ps <- renderUI({
      fluidRow(
        column(4,
               selectizeInput("km_ps_file_ext", "File extension (dpi = 300)",
                              choices = c("jpg","pdf", "tiff", "svg"), multiple = F,
                              selected = "jpg"
               )
        ),
        column(4,
               sliderInput("fig_width_km_ps", "Width (in):",
                           min = 5, max = 20, value = 8
               )
        ),
        column(4,
               sliderInput("fig_height_km_ps", "Height (in):",
                           min = 5, max = 20, value = 6
               )
        )
      )
    })

    output$downloadButton_km_ps <- downloadHandler(
      filename =  function() {
        paste(input$event_km, input$indep_km,"_kaplan_meier_PSmatching.", input$km_original_file_ext  ,sep="")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggplot2::ggsave(file ,km_ps_input(), dpi = 300, units = "in", width = input$fig_width_km_ps, height =input$fig_height_km_ps)

      }
    )


    ## KM iptw
    km_iptw_input <- reactive({
      data.km <- mat.info()$data
      data.km[[input$event_km]] <- as.numeric(as.vector(data.km[[input$event_km]]))
      if (input$km_subcheck == T){
        req(input$subvar_km)
        data.km <- data.km[get(input$subvar_km) == input$subval_km, ]
      }
      data.design <- survey::svydesign(ids = ~ 1, data = data.km, weights = ~ iptw)
      cc = substitute(survey::svykm(.form, design = data.design), list(.form= form.km()))
      res.km = eval(cc)
      yst.name = data.label()[variable == input$indep_km, var_label][1]
      yst.lab = data.label()[variable == input$indep_km, val_label]
      ylab = ifelse(input$km_cumhaz, "Cumulative hazard(%)", "Survival(%)")

      return(
        jskm::svyjskm(res.km, xlabs = "Time-to-event", ylabs = ylab, ystratalabs = yst.lab, ystrataname = yst.name, cumhaz = input$km_cumhaz, design = data.design, pval = input$km_pval, legendposition = c(0.9, 0.2))
      )

    })

    output$km_iptw <- renderPlot({
      print(km_iptw_input())
    })

    output$downloadControls_km_iptw <- renderUI({
      fluidRow(
        column(4,
               selectizeInput("km_iptw_file_ext", "File extension (dpi = 300)",
                              choices = c("jpg","pdf", "tiff", "svg"), multiple = F,
                              selected = "jpg"
               )
        ),
        column(4,
               sliderInput("fig_width_km_iptw", "Width (in):",
                           min = 5, max = 20, value = 8
               )
        ),
        column(4,
               sliderInput("fig_height_km_iptw", "Height (in):",
                           min = 5, max = 20, value = 6
               )
        )
      )
    })

    output$downloadButton_km_iptw <- downloadHandler(
      filename =  function() {
        paste(input$event_km, input$indep_km,"_kaplan_meier_IPTW.", input$km_original_file_ext  ,sep="")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        ggplot2::ggsave(file ,km_iptw_input(), dpi = 300, units = "in", width = input$fig_width_km_iptw, height =input$fig_height_km_iptw)

      }
    )













  }









  #viewer <- dialogViewer("Descriptive statistics", width = 1100, height = 850)
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}



#' @title jsPropensityAddin: Rstudio addin of jsPropensityGadget
#' @description Rstudio addin of jsPropensityGadget
#' @return Rstudio addin of jsPropensityGadget
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rstudioapi]{rstudio-editors}}
#' @rdname jsPropensityAddin
#' @export
#' @importFrom rstudioapi getActiveDocumentContext


jsPropensityAddin <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  # Set the default data to use based on the selection.
  dataString <- context$selection[[1]]$text
  data <- get(dataString, envir = .GlobalEnv)
  #viewer <- dialogViewer("Subset", width = 1000, height = 800)
  jsPropensityGadget(data)
}


