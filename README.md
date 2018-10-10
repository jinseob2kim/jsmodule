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
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      csvFileInput("datafile", "Upload data (csv/xlsx format)")
    ),
    mainPanel(
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  datafile <- callModule(csvFile, "datafile")

  output$table <- renderDataTable({
    datafile()
  })
}

shinyApp(ui, server)
```
