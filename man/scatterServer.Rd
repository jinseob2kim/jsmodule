% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatter.R
\name{scatterServer}
\alias{scatterServer}
\title{scatterServer: shiny module server for scatterplot.}
\usage{
scatterServer(id, data, data_label, data_varStruct = NULL, nfactor.limit = 10)
}
\arguments{
\item{id}{id}

\item{data}{Reactive data}

\item{data_label}{Reactive data label}

\item{data_varStruct}{Reactive List of variable structure, Default: NULL}

\item{nfactor.limit}{nlevels limit in factor variable, Default: 10}
}
\value{
Shiny module server for scatterplot.
}
\description{
Shiny module server for scatterplot.
}
\details{
Shiny module server for scatterplot.
}
\examples{
library(shiny)
library(ggplot2)
library(ggpubr)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      scatterUI("scatter")
    ),
    mainPanel(
      plotOutput("scatter_plot"),
      ggplotdownUI("scatter")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive(mtcars)
  data.label <- reactive(jstable::mk.lev(mtcars))

  out_scatter <- scatterServer("scatter",
    data = data, data_label = data.label,
    data_varStruct = NULL
  )

  output$scatter_plot <- renderPlot({
    print(out_scatter())
  })
}
}
