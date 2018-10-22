# jsmodule

[![Build Status](https://travis-ci.org/jinseob2kim/jsmodule.svg?branch=master)](https://travis-ci.org/jinseob2kim/jsmodule)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/jsmodule)](http://cran.r-project.org/package=jsmodule)
[![codecov](https://codecov.io/github/jinseob2kim/jsmodule/branch/master/graphs/badge.svg)](https://codecov.io/github/jinseob2kim/jsmodule)
[![GitHub issues](https://img.shields.io/github/issues/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/issues)
[![GitHub forks](https://img.shields.io/github/forks/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/network)
[![GitHub stars](https://img.shields.io/github/stars/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/stargazers)
[![GitHub license](https://img.shields.io/github/license/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/blob/master/LICENSE)
[![GitHub last commit](https://img.shields.io/github/last-commit/google/skia.svg)](https://github.com/jinseob2kim/jsmodule)
[![GitHub contributors](https://img.shields.io/github/contributors/jinseob2kim/jsmodule.svg?maxAge=2592000)](https://github.com/jinseob2kim/jsmodule/graphs/contributors)

Shiny modules for medical research

## Install

```r
devtools::install_github(c('jinseob2kim/jstable', 'jinseob2kim/jsmodule'))
```

## Gadget

### Basic statistics 

```r
jsBasicGadget(mtcars)
```

### Propensity score analysis

```r
jsPropensityGadget(mtcars)
```


## Modules

### Example 1: Shiny app for `csv/xlsx` input

```r
library(jsmodule)
library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      csvFileInput("datafile")
    ),
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                  tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- callModule(csvFile, "datafile")

  output$data <- renderDT({
    datatable(data()$data, rownames=F, editable = F, extension= "Buttons", caption = "Labels of data",
              options = opt.data("data")
    )
  })


  output$data_label <- renderDT({
    datatable(data()$label, rownames=F, editable = F, extension= "Buttons", caption = "Labels of data",
              options = opt.data("label")
    )
  })
}

shinyApp(ui, server)
```

### Example 2: Table 1

```r
library(shiny);library(data.table);library(DT)
library(jstable);library(shinycustomloader);library(tableone);library(labelled)

data = data.table(mtcars)
data$vs = as.factor(data$vs)
data$am = as.factor(data$am)
data$cyl = as.factor(data$cyl)
data.label = mk.lev(data)


ui <- navbarPage("Basic statistics",
                 tabPanel("Data",
                          tabsetPanel(type = "pills",
                                      tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                      tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                          )
                 ),
                 tabPanel("Table 1",
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )

                 )
)

server <- function(input, output, session) {
  out_tb1 <- callModule(tb1module, "tb1", data = data, data_label = data.label, data_varStruct = NULL)
  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
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
}

shinyApp(ui, server)

```

### Example3: Table 1 for **reactive** data

```r
library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader);library(tableone);library(labelled)


ui <- navbarPage("Basic statistics",
                 tabPanel("Data",
                          sidebarLayout(
                            sidebarPanel(
                              csvFileInput("datafile")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                          )
                              )
                            )
                 ),
                 tabPanel("Table 1",
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )

                 )
)




server <- function(input, output, session) {
  data.info <- callModule(csvFile, "datafile")
  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)

  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extension= "Buttons", caption = "Data",
              options = opt.data("data")
    )
  })


  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extension= "Buttons", caption = "Label of data",
              options = opt.data("label")
    )
  })




  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL)

  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
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



}

shinyApp(ui, server)
```

### Example 4: Add linear & logistic regression

```r
library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader);library(tableone);library(labelled)
library(epiDisplay)

data = data.table(mtcars)
data$vs = as.factor(data$vs)
data$am = as.factor(data$am)
data$cyl = as.factor(data$cyl)
data.label = mk.lev(data)


ui <- navbarPage("Basic statistics",
                 tabPanel("Data",
                          tabsetPanel(type = "pills",
                                      tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                      tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                          )
                 ),
                 tabPanel("Table 1",
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )

                 ),
                 navbarMenu("Regression",
                            tabPanel("Linear regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("linear")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Logistic regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("logistic")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                       )
                                     )
                            )

                 )
)

server <- function(input, output, session) {

  output$data <- renderDT({
    datatable(data, rownames=F, editable = F, extension= "Buttons", caption = "Data",
              options = opt.data("data")
    )
  })


  output$data_label <- renderDT({
    datatable(data.label, rownames=F, editable = F, extension= "Buttons", caption = "Labels of data",
              options = opt.data("label")
    )
  })

  out_tb1 <- callModule(tb1module, "tb1", data = data, data_label = data.label, data_varStruct = NULL)
  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
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


  out_linear <- callModule(regressModule, "linear", data = data, data_label = data.label, data_varStruct = NULL)

  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == c("P(F-test)",  "sig"))
    datatable(out_linear()$table, rownames=T, extension= "Buttons", caption = out_linear()$caption,
              options = c(opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })

  out_logistic <- callModule(logisticModule, "logistic", data = data, data_label = data.label, data_varStruct = NULL)

  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == c("P(F-test)",  "sig"))
    datatable(out_logistic()$table, rownames=T, extension= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
}

shinyApp(ui, server)

```


### Example 5: Add linear & logistic regression for **reactive** data

```r
library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader);library(tableone);library(labelled)
library(epiDisplay)

ui <- navbarPage("Basic statistics",
                 tabPanel("Data",
                          sidebarLayout(
                            sidebarPanel(
                              csvFileInput("datafile")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                          )
                              )
                            )
                 ),
                 tabPanel("Table 1",
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )

                 ),
                 navbarMenu("Regression",
                            tabPanel("Linear regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("linear")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Logistic regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("logistic")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                       )
                                     )
                            )

                 )
)




server <- function(input, output, session) {
  data.info <- callModule(csvFile, "datafile")
  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)

  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extension= "Buttons", caption = "Data",
              options = opt.data("data")
    )
  })


  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extension= "Buttons", caption = "Label of data",
              options = opt.data("label")
    )
  })




  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL)

  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
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

  out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL)

  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == c("P(F-test)",  "sig"))
    datatable(out_linear()$table, rownames=T, extension= "Buttons", caption = out_linear()$caption,
              options = c(opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })

  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL)

  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == c("P(F-test)",  "sig"))
    datatable(out_logistic()$table, rownames=T, extension= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })



}

shinyApp(ui, server)

```


### Example 6: Add scatterplot

```r
library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader);library(tableone);library(labelled)
library(epiDisplay)
library(ggplot2);library(GGally)

data = mtcars
data$vs = as.factor(data$vs)
data$am = as.factor(data$am)
data.label = mk.lev(data)


ui <- navbarPage("Basic statistics",
                 tabPanel("Data",
                          tabsetPanel(type = "pills",
                                      tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                      tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                          )
                 ),
                 tabPanel("Table 1",
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )

                 ),
                 navbarMenu("Regression",
                            tabPanel("Linear regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("linear")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Logistic regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("logistic")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                       )
                                     )
                            )

                 ),
                 navbarMenu("Plot",
                            tabPanel("Scatter plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         ggpairsModuleUI1("ggpairs")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("ggpairs_plot"), type="html", loader="loader6"),
                                         ggpairsModuleUI2("ggpairs")
                                       )
                                     )
                            )

                 )

)

server <- function(input, output, session) {

  output$data <- renderDT({
    datatable(data, rownames=F, editable = F, extension= "Buttons", caption = "Data",
              options = opt.data("data")
    )
  })


  output$data_label <- renderDT({
    datatable(data.label, rownames=F, editable = F, extension= "Buttons", caption = "Labels of data",
              options = opt.data("label")
    )
  })

  out_tb1 <- callModule(tb1module, "tb1", data = data, data_label = data.label, data_varStruct = NULL)
  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
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


  out_linear <- callModule(regressModule, "linear", data = data, data_label = data.label, data_varStruct = NULL)

  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == c("P(F-test)",  "sig"))
    datatable(out_linear()$table, rownames=T, extension= "Buttons", caption = out_linear()$caption,
              options = c(opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })

  out_logistic <- callModule(logisticModule, "logistic", data = data, data_label = data.label, data_varStruct = NULL)

  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == c("P(F-test)",  "sig"))
    datatable(out_logistic()$table, rownames=T, extension= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })

  out_ggpairs <- callModule(ggpairsModule, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL)

  output$ggpairs_plot <- renderPlot({
    print(out_ggpairs())
  })



}

shinyApp(ui, server)

```


### Example 7: Add scatterplot for **reactive data**

```r
library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader);library(tableone);library(labelled)
library(epiDisplay)
library(ggplot2);library(GGally)

ui <- navbarPage("Basic statistics",
                 tabPanel("Data",
                          sidebarLayout(
                            sidebarPanel(
                              csvFileInput("datafile")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                                          )
                              )
                            )
                 ),
                 tabPanel("Table 1",
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )

                 ),
                 navbarMenu("Regression",
                            tabPanel("Linear regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("linear")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("lineartable"), type="html", loader="loader6")
                                       )
                                     )
                            ),
                            tabPanel("Logistic regression",
                                     sidebarLayout(
                                       sidebarPanel(
                                         regressModuleUI("logistic")
                                       ),
                                       mainPanel(
                                         withLoader(DTOutput("logistictable"), type="html", loader="loader6")
                                       )
                                     )
                            )

                 ),
                 navbarMenu("Plot",
                            tabPanel("Scatter plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         ggpairsModuleUI1("ggpairs")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("ggpairs_plot"), type="html", loader="loader6"),
                                         ggpairsModuleUI2("ggpairs")
                                       )
                                     )
                            )

                 )
)




server <- function(input, output, session) {
  data.info <- callModule(csvFile, "datafile")
  data <- reactive(data.info()$data)
  data.label <- reactive(data.info()$label)

  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extension= "Buttons", caption = "Data",
              options = opt.data("data")
    )
  })


  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extension= "Buttons", caption = "Label of data",
              options = opt.data("label")
    )
  })




  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = NULL)

  output$table1 <- renderDT({
    tb = out_tb1()$table
    cap = out_tb1()$caption
    out.tb1 = datatable(tb, rownames = T, extension= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
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

  out_linear <- callModule(regressModule2, "linear", data = data, data_label = data.label, data_varStruct = NULL)

  output$lineartable <- renderDT({
    hide = which(colnames(out_linear()$table) == c("P(F-test)",  "sig"))
    datatable(out_linear()$table, rownames=T, extension= "Buttons", caption = out_linear()$caption,
              options = c(opt.tbreg(out_linear()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })

  out_logistic <- callModule(logisticModule2, "logistic", data = data, data_label = data.label, data_varStruct = NULL)

  output$logistictable <- renderDT({
    hide = which(colnames(out_logistic()$table) == c("P(F-test)",  "sig"))
    datatable(out_logistic()$table, rownames=T, extension= "Buttons", caption = out_logistic()$caption,
              options = c(opt.tbreg(out_logistic()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets =hide))
                          )
              )
    ) %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })


  out_ggpairs <- callModule(ggpairsModule2, "ggpairs", data = data, data_label = data.label, data_varStruct = NULL)

  output$ggpairs_plot <- renderPlot({
    print(out_ggpairs())
  })



}

shinyApp(ui, server)


```
