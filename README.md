# jsmodule
[![GitHub issues](https://img.shields.io/github/issues/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/issues)
[![GitHub forks](https://img.shields.io/github/forks/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/network)
[![GitHub stars](https://img.shields.io/github/stars/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/stargazers)
[![GitHub license](https://img.shields.io/github/license/jinseob2kim/jsmodule.svg)](https://github.com/jinseob2kim/jsmodule/blob/master/LICENSE)
[![GitHub last commit](https://img.shields.io/github/last-commit/google/skia.svg)](https://github.com/jinseob2kim/jsmodule)

Shiny modules for medical research

## Install

```r
devtools::install_github('jinseob2kim/jsmodule')
```

## Example: Shiny app for `csv/xlsx` input

```r
library(jsmodule)
library(shiny);library(data.table);library(readxl);library(DT);library(jstable);library(shinycustomloader)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      csvFileInput("datafile", "Upload data (csv/xlsx format)")
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
