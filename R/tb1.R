
tb1moduleUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    uiOutput(ns("base")),
    uiOutput(ns("sub1")),
    uiOutput(ns("sub2"))
  )
}



tb1module <- function(input, output, session, data, data_label){
  factor_vars <- names(data)[data[, lapply(.SD, class) %in% c("factor", "character")]]
  conti_vars <- setdiff(names(data), factor_vars)
  nclass_factor <- unlist(data[, lapply(.SD, function(x){length(unique(x))}), .SDcols = factor_vars])
  group_vars <- factor_vars[nclass_factor >=2 & nclass_factor <=10]

  output$base <- renderUI({
    tagList(
      selectInput(session$ns("group_vars"), "Stratified by",
                     choices =  c("None", group_vars), multiple = F,
                     selected = "None"
      ),
      selectInput(session$ns("nonnormal_vars"), "Non-normal variable (continuous)",
                     choices = conti_vars, multiple = T,
                     selected = NULL
      ),
      sliderInput(session$ns("decimal_tb1_con"), "Digits (continuous)",
                  min = 1, max = 3, value = 1
      ),
      sliderInput(session$ns("decimal_tb1_cat"), "Digits (categorical, %)",
                  min = 1, max = 3, value = 1
      )
    )
  })


  output$sub1 <- renderUI({
    req(!is.null(input$group_vars))
    if(input$group_vars == "None") return(NULL)

    tagList(
      sliderInput(session$ns("decimal_tb1_p"), "Digits (p)",
                  min = 3, max = 5, value = 3
      ),
      selectInput(session$ns("exact_vars"), "Fisher's test (categorical)",
                  choices = group_vars, multiple = T,
                  selected = NULL
      ),
      checkboxInput(session$ns("smd"), "Show SMD", F),
      selectInput(session$ns("group2_vars"), "2nd group (optional)",
                  choices = c("None", group_vars), multiple = F,
                  selected = "None"
      )
    )
  })

  output$sub2 <- renderUI({
    req(!is.null(input$group_vars), req(!is.null(input$group2_vars)))
    if ((input$group_vars == 'None') | (input$group2_vars == 'None') | (input$group2_vars == input$group_vars)) return(NULL)
    tagList(
      checkboxInput(session$ns("psub"), "Subgroup p-values", F)
    )

  })




}
