#' @title aiAssistantUI: AI Assistant module UI
#' @description AI-powered statistical analysis assistant module UI
#' @param id id
#' @param show_api_config If TRUE, shows API configuration UI. If FALSE, uses only env vars. Default: TRUE
#' @return AI Assistant module UI
#' @details Provides an interactive chat interface with AI for statistical analysis code generation
#' @examples
#' library(shiny)
#' library(DT)
#' library(survival)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(12, aiAssistantUI("ai"))
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(colon)
#'   data.label <- reactive(jstable::mk.lev(colon))
#'
#'   callModule(aiAssistant, "ai",
#'     data = data,
#'     data_label = data.label,
#'     data_varStruct = NULL,
#'     api_key = Sys.getenv("ANTHROPIC_API_KEY")
#'   )
#' }
#' @rdname aiAssistantUI
#' @export
#' @import shiny
#' @import shinyjs

aiAssistantUI <- function(id, show_api_config = TRUE) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    shinyjs::useShinyjs(),

    # Custom CSS for tooltip (Bootstrap style)
    tags$style(HTML("
      .info-tooltip {
        position: relative;
        display: inline-block;
        cursor: help;
        color: #0d6efd;
        margin-left: 5px;
      }
      .info-tooltip .tooltiptext {
        visibility: hidden;
        width: 280px;
        background-color: #fff;
        color: #212529;
        text-align: left;
        border-radius: 0.375rem;
        border: 1px solid rgba(0,0,0,.15);
        padding: 12px;
        position: absolute;
        z-index: 1000;
        top: -5px;
        left: 105%;
        opacity: 0;
        transition: opacity 0.2s;
        font-size: 13px;
        line-height: 1.5;
        box-shadow: 0 0.5rem 1rem rgba(0,0,0,.15);
      }
      .info-tooltip .tooltiptext::before {
        content: '';
        position: absolute;
        top: 15px;
        right: 100%;
        margin-top: -6px;
        border-width: 6px;
        border-style: solid;
        border-color: transparent #fff transparent transparent;
        z-index: 1;
      }
      .info-tooltip .tooltiptext::after {
        content: '';
        position: absolute;
        top: 15px;
        right: 100%;
        margin-top: -7px;
        border-width: 7px;
        border-style: solid;
        border-color: transparent rgba(0,0,0,.15) transparent transparent;
      }
      .info-tooltip:hover .tooltiptext {
        visibility: visible;
        opacity: 1;
      }
    ")),

    # API Configuration Section (conditional on show_api_config)
    if (show_api_config) {
      tagList(
        fluidRow(
          column(12,
            wellPanel(
              h4(icon("key"), " API Configuration"),
              fluidRow(
                column(6,
                  selectInput(
                    ns("provider"),
                    "AI Provider",
                    choices = c(
                      "Anthropic (Claude)" = "anthropic",
                      "OpenAI (GPT)" = "openai",
                      "Google (Gemini)" = "google"
                    ),
                    selected = "anthropic"
                  ),
                  tags$div(
                    style = "margin-top: 10px;",
                    uiOutput(ns("model_selector"))
                  )
                ),
                column(6,
                  passwordInput(
                    ns("api_key_input"),
                    "API Key",
                    placeholder = "Enter your API key or leave empty for env var"
                  ),
                  helpText(
                    tags$small(
                      "Env vars: ",
                      tags$code("ANTHROPIC_API_KEY"),
                      ", ",
                      tags$code("OPENAI_API_KEY"),
                      ", ",
                      tags$code("GOOGLE_API_KEY")
                    )
                  ),
                  actionButton(
                    ns("check_api_key"),
                    "Check API Key",
                    icon = icon("key"),
                    class = "btn-info",
                    style = "width: 100%; margin-top: 10px;"
                  )
                )
              ),
              uiOutput(ns("api_status"))
            )
          )
        ),
        hr()
      )
    } else {
      # Show minimal status when API config UI is hidden
      tagList(
        fluidRow(
          column(12,
            uiOutput(ns("env_config_status"))
          )
        ),
        hr()
      )
    },

    # Main Interface
    fluidRow(
      column(6,
        wellPanel(
          h4(icon("comments"), " Chat"),
          p(
            "Ask for statistical analysis. ",
            tags$em("Example: 'Create survival curve by rx'")
          ),
          # Token usage display
          uiOutput(ns("token_display")),
          hr(),
          # Chat history
          div(
            id = ns("chat_container"),
            style = paste0(
              "max-height: 400px; overflow-y: auto; ",
              "border: 1px solid #ddd; padding: 10px; ",
              "border-radius: 5px; background-color: #fafafa;"
            ),
            uiOutput(ns("chat_history"))
          ),
          hr(),
          # Input area
          fluidRow(
            column(9,
              textAreaInput(
                ns("user_input"),
                NULL,
                placeholder = "Enter your analysis request... (Enter to send, Shift+Enter for new line)",
                width = "100%",
                rows = 2
              ),
              tags$script(HTML(sprintf("
                $(document).ready(function() {
                  $('#%s').on('keydown', function(e) {
                    if (e.keyCode === 13 && !e.shiftKey) {
                      e.preventDefault();
                      $('#%s').click();
                    }
                  });
                });
              ", ns("user_input"), ns("send_btn"))))
            ),
            column(3,
              actionButton(
                ns("send_btn"),
                "Send",
                icon = icon("paper-plane"),
                class = "btn-primary",
                style = "width: 100%; margin-top: 5px;"
              ),
              actionButton(
                ns("clear_chat"),
                "Clear",
                icon = icon("trash"),
                class = "btn-warning",
                style = "width: 100%; margin-top: 5px;"
              )
            )
          )
        )
      ),
      column(6,
        wellPanel(
          h4(icon("code"), " Generated Code"),
          shinyAce::aceEditor(
            outputId = ns("code_editor"),
            value = "",
            mode = "r",
            theme = "monokai",
            height = "400px",
            fontSize = 13,
            showLineNumbers = TRUE,
            highlightActiveLine = TRUE,
            readOnly = FALSE,
            showPrintMargin = FALSE,
            placeholder = "No code generated yet. You can edit the code here."
          ),
          tags$style(HTML(sprintf("
            /* Editable code editor style */
            #%s.ace_editor {
              cursor: text !important;
              opacity: 1;
              border: 2px solid #2196F3 !important;
              box-shadow: 0 0 8px rgba(33, 150, 243, 0.3);
            }
            #%s.ace_editor .ace_content {
              cursor: text !important;
            }
            #%s.ace_editor .ace_text-layer {
              cursor: text !important;
            }
            #%s.ace_editor .ace_scroller {
              cursor: text !important;
            }
          ",
          ns("code_editor"), ns("code_editor"), ns("code_editor"), ns("code_editor")))),
          fluidRow(
            column(6,
              actionButton(
                ns("run_code"),
                "Run Code",
                icon = icon("play"),
                class = "btn-success",
                style = "width: 100%; margin-top: 10px;"
              )
            ),
            column(6,
              actionButton(
                ns("copy_code"),
                "Copy Code",
                icon = icon("copy"),
                class = "btn-secondary",
                style = "width: 100%; margin-top: 10px;"
              )
            )
          ),
          tags$script(HTML(sprintf("
            $(document).on('click', '#%s', function() {
              var editor = ace.edit('%s');
              var code = editor.getValue();
              navigator.clipboard.writeText(code).then(function() {
                // Show success notification
                Shiny.setInputValue('%s', Math.random());
              }).catch(function(err) {
                console.error('Failed to copy: ', err);
              });
            });

            // Auto-scroll chat to bottom
            Shiny.addCustomMessageHandler('scrollChat', function(message) {
              setTimeout(function() {
                var container = document.getElementById(message.id);
                if (container) {
                  container.scrollTop = container.scrollHeight;
                }
              }, 100);
            });
          ", ns("copy_code"), ns("code_editor"), ns("copy_success"))))
        ),
        wellPanel(
          h4(icon("chart-line"), " Results"),
          uiOutput(ns("result_output")),
          hr(),
          h5("Download Options"),
          fluidRow(
            column(3, uiOutput(ns("download_pptx_ui"))),
            column(3, uiOutput(ns("download_word_ui"))),
            column(3, uiOutput(ns("download_excel_ui"))),
            column(3, uiOutput(ns("download_txt_ui")))
          ),
          uiOutput(ns("ppt_size_ui"))
        )
      )
    )
  )
}


#' @title aiAssistant: AI Assistant module server
#' @description AI-powered statistical analysis assistant module server
#' @param input input
#' @param output output
#' @param session session
#' @param data Data (reactive)
#' @param data_label Data label (reactive)
#' @param data_varStruct Variable structure list of data, Default: NULL
#' @param api_key API key for AI service. If NULL, reads from provider-specific env var
#' @param stats_guide Optional custom statistical guide text. If NULL, uses default guide
#' @param show_api_config If TRUE, shows API config UI. If FALSE, uses only env vars. Default: TRUE
#' @param analysis_context Optional reactive or list containing previous analysis results. AI can reference these when user asks follow-up questions. Default: NULL
#' @return AI Assistant module server
#' @details Provides interactive statistical analysis code generation using AI
#' @examples
#' library(shiny)
#' library(DT)
#' library(survival)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(12, aiAssistantUI("ai"))
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   data <- reactive(colon)
#'   data.label <- reactive(jstable::mk.lev(colon))
#'
#'   callModule(aiAssistant, "ai",
#'     data = data,
#'     data_label = data.label,
#'     data_varStruct = NULL,
#'     api_key = Sys.getenv("ANTHROPIC_API_KEY")
#'   )
#' }
#' @rdname aiAssistant
#' @export
#' @import shiny
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom officer read_pptx add_slide ph_with ph_location read_docx
#' @importFrom rvg dml
#' @importFrom flextable flextable autofit body_add_flextable
#' @importFrom openxlsx write.xlsx
#' @importFrom gridExtra grid.arrange

aiAssistant <- function(input, output, session, data, data_label,
                        data_varStruct = NULL,
                        api_key = NULL, stats_guide = NULL,
                        show_api_config = TRUE,
                        analysis_context = NULL) {

  # Reactive values for model list
  available_models <- reactiveVal(list())
  selected_model <- reactiveVal(NULL)

  # Get provider from environment or input
  get_provider <- reactive({
    if (!show_api_config) {
      # When API config UI is hidden, use environment variable
      provider <- Sys.getenv("AI_PROVIDER", unset = "anthropic")
      return(provider)
    } else {
      # When API config UI is shown, use input or default
      if (!is.null(input$provider)) {
        return(input$provider)
      }
      return("anthropic")
    }
  })

  # Fetch available models from provider
  fetch_models <- function(provider, api_key) {
    if (is.null(api_key) || api_key == "") {
      return(NULL)
    }

    tryCatch({
      if (provider == "anthropic") {
        response <- httr::GET(
          url = "https://api.anthropic.com/v1/models",
          httr::add_headers(
            "x-api-key" = api_key,
            "anthropic-version" = "2023-06-01"
          )
        )
        content <- httr::content(response, "parsed")
        if (!is.null(content$data)) {
          models <- sapply(content$data, function(m) m$id)
          return(models)
        }
      } else if (provider == "openai") {
        response <- httr::GET(
          url = "https://api.openai.com/v1/models",
          httr::add_headers(
            "Authorization" = paste("Bearer", api_key)
          )
        )
        content <- httr::content(response, "parsed")
        if (!is.null(content$data)) {
          models <- sapply(content$data, function(m) m$id)
          return(sort(models, decreasing = TRUE))
        }
      } else if (provider == "google") {
        # Google Gemini API
        response <- httr::GET(
          url = paste0(
            "https://generativelanguage.googleapis.com/v1beta/",
            "models?key=", api_key
          )
        )
        content <- httr::content(response, "parsed")
        if (!is.null(content$models)) {
          # Extract model names and filter for generation models
          # Gemini API provides supportedGenerationMethods metadata
          all_models <- sapply(content$models, function(m) {
            # Remove "models/" prefix
            name <- sub("^models/", "", m$name)
            # Check if supports generateContent
            methods <- unlist(m$supportedGenerationMethods)
            if ("generateContent" %in% methods) {
              return(name)
            }
            return(NA)
          })
          models <- all_models[!is.na(all_models)]
          return(models)
        }
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  }

  # Reactive API key - from input or parameter or environment
  get_api_key <- reactive({
    # When API config UI is hidden, only use parameter or environment variable
    if (!show_api_config) {
      if (!is.null(api_key) && api_key != "") {
        return(api_key)
      }
      # Environment variable based on provider
      provider <- get_provider()
      env_var <- switch(provider,
        "anthropic" = "ANTHROPIC_API_KEY",
        "openai" = "OPENAI_API_KEY",
        "google" = "GOOGLE_API_KEY",
        "ANTHROPIC_API_KEY"
      )
      return(Sys.getenv(env_var))
    }

    # When API config UI is shown: Priority: UI input > function parameter > environment variable
    if (!is.null(input$api_key_input) && input$api_key_input != "") {
      return(input$api_key_input)
    }

    if (!is.null(api_key) && api_key != "") {
      return(api_key)
    }

    # Environment variable based on provider
    provider <- get_provider()
    env_var <- switch(provider,
      "anthropic" = "ANTHROPIC_API_KEY",
      "openai" = "OPENAI_API_KEY",
      "google" = "GOOGLE_API_KEY",
      "ANTHROPIC_API_KEY"
    )

    return(Sys.getenv(env_var))
  })

  # Environment configuration status (when API config UI is hidden)
  output$env_config_status <- renderUI({
    if (show_api_config) return(NULL)

    provider <- get_provider()
    api_key_val <- get_api_key()
    model <- selected_model()

    provider_name <- switch(provider,
      "anthropic" = "Anthropic Claude",
      "openai" = "OpenAI GPT",
      "google" = "Google Gemini",
      "Unknown"
    )

    if (is.null(api_key_val) || api_key_val == "") {
      return(tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        sprintf(" AI Provider: %s | Status: API key not configured", provider_name)
      ))
    }

    model_text <- if (!is.null(model)) model else "auto-detect"

    tags$div(
      class = "alert alert-success",
      style = "margin-bottom: 15px;",
      icon("check-circle"),
      sprintf(" AI Provider: %s | Model: %s | Status: Ready", provider_name, model_text)
    )
  })

  # Check API key and fetch models when button clicked
  observeEvent(input$check_api_key, {
    provider <- get_provider()
    api_key_val <- get_api_key()

    if (is.null(api_key_val) || api_key_val == "") {
      showNotification(
        "Please enter an API key first.",
        type = "warning",
        duration = 3
      )
      return()
    }

    # Show loading notification
    showNotification(
      "Fetching available models...",
      id = "fetching_models",
      type = "message",
      duration = NULL
    )

    # Fetch models
    models <- fetch_models(provider, api_key_val)

    # Remove loading notification
    removeNotification("fetching_models")

    if (is.null(models) || length(models) == 0) {
      showNotification(
        "Failed to fetch models. Please check your API key.",
        type = "error",
        duration = 5
      )
      available_models(NULL)
      selected_model(NULL)
      return()
    }

    # Success - update available models
    available_models(models)

    # Set default model
    env_model <- Sys.getenv("AI_MODEL", unset = "")
    if (env_model != "" && env_model %in% models) {
      selected_model(env_model)
    } else {
      default_model <- switch(provider,
        "anthropic" = models[grepl("sonnet-4", models)][1],
        "openai" = models[grepl("gpt-4", models)][1],
        "google" = "gemini-1.5-flash",
        models[1]
      )
      if (!is.na(default_model)) {
        selected_model(default_model)
      } else {
        selected_model(models[1])
      }
    }

    showNotification(
      sprintf("Successfully loaded %d models", length(models)),
      type = "message",
      duration = 3
    )
  })

  # Auto-fetch models when API config UI is hidden (environment-based config)
  observe({
    if (!show_api_config) {
      provider <- get_provider()
      api_key_val <- get_api_key()

      if (!is.null(provider) && !is.null(api_key_val) && api_key_val != "") {
        env_model <- Sys.getenv("AI_MODEL", unset = "")
        models <- fetch_models(provider, api_key_val)
        available_models(models)

        if (!is.null(models) && length(models) > 0) {
          if (env_model != "" && env_model %in% models) {
            selected_model(env_model)
          } else {
            default_model <- switch(provider,
              "anthropic" = models[grepl("sonnet-4", models)][1],
              "openai" = models[grepl("gpt-4", models)][1],
              "google" = "gemini-1.5-flash",
              models[1]
            )
            if (!is.na(default_model)) {
              selected_model(default_model)
            } else {
              selected_model(models[1])
            }
          }
        }
      }
    }
  })

  # Model selector UI
  output$model_selector <- renderUI({
    models <- available_models()

    if (is.null(models) || length(models) == 0) {
      return(tags$div(
        style = "color: #888; font-size: 12px;",
        icon("info-circle"),
        " Click 'Check API Key' to load models"
      ))
    }

    tagList(
      selectInput(
        session$ns("selected_model"),
        "Model",
        choices = models,
        selected = selected_model()
      ),
      actionButton(
        session$ns("apply_config"),
        "Apply Configuration",
        icon = icon("check"),
        class = "btn-primary",
        style = "width: 100%; margin-top: 10px;"
      )
    )
  })

  # Update selected model when user changes selection
  observeEvent(input$selected_model, {
    if (!is.null(input$selected_model)) {
      selected_model(input$selected_model)
    }
  })

  # Apply configuration when button clicked
  observeEvent(input$apply_config, {
    provider <- get_provider()
    api_key_val <- get_api_key()
    model <- selected_model()

    if (is.null(api_key_val) || api_key_val == "") {
      showNotification(
        "Please enter an API key first.",
        type = "warning",
        duration = 3
      )
      return()
    }

    if (is.null(model)) {
      showNotification(
        "Please select a model first.",
        type = "warning",
        duration = 3
      )
      return()
    }

    # Configuration successful
    provider_name <- switch(provider,
      "anthropic" = "Anthropic Claude",
      "openai" = "OpenAI GPT",
      "google" = "Google Gemini",
      "Unknown"
    )

    showNotification(
      sprintf("Configuration applied: %s - %s", provider_name, model),
      type = "message",
      duration = 3
    )
  })

  # API status indicator
  output$api_status <- renderUI({
    api_key_val <- get_api_key()
    models <- available_models()
    model <- selected_model()
    provider <- get_provider()

    provider_name <- switch(provider,
      "anthropic" = "Anthropic Claude",
      "openai" = "OpenAI GPT",
      "google" = "Google Gemini",
      "Unknown"
    )

    if (is.null(api_key_val) || api_key_val == "") {
      tags$div(
        class = "alert alert-warning",
        style = "margin-top: 15px;",
        icon("exclamation-triangle"),
        sprintf(" Step 1: Enter your %s API key", provider_name)
      )
    } else if (is.null(models) || length(models) == 0) {
      tags$div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        icon("info-circle"),
        sprintf(" Step 2: Click 'Check API Key' to load %s models", provider_name)
      )
    } else if (is.null(model)) {
      tags$div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        icon("info-circle"),
        sprintf(" Step 3: Select a model and click 'Apply Configuration'")
      )
    } else {
      tags$div(
        class = "alert alert-success",
        style = "margin-top: 15px;",
        icon("check-circle"),
        sprintf(" Ready: %s - %s (%d models available)", provider_name, model, length(models))
      )
    }
  })

  # Load prompt from package
  default_prompt <- tryCatch({
    get_prompt("default")
  }, error = function(e) {
    # Fallback if prompt file not found
    paste(
      "You are an R/Shiny medical statistics expert.",
      "Generate executable R code using jskm, jstable, jsmodule packages.",
      "Store results in 'result' variable.",
      sep = "\n"
    )
  })

  stats_guide_text <- if (!is.null(stats_guide)) {
    stats_guide
  } else {
    default_prompt
  }

  # Reactive values
  chat_history <- reactiveVal(list())
  display_history <- reactiveVal(list())
  current_code <- reactiveVal("")
  execution_result <- reactiveVal(NULL)
  result_type <- reactiveVal("none")  # "plot", "table", "text", "error", "none"

  # Token tracking
  token_usage <- reactiveVal(list(
    input_tokens = 0,
    output_tokens = 0,
    total_tokens = 0,
    total_cost = 0
  ))

  # Tool execution status
  tool_execution_log <- reactiveVal(list())

  # Define available tools for AI function calling
  get_tools_schema <- function() {
    list(
      list(
        name = "execute_r_code",
        description = "Execute R code in the analysis environment and return the result. Use this to run statistical analysis code. The code should store the final result in a variable named 'result'.",
        input_schema = list(
          type = "object",
          properties = list(
            code = list(
              type = "string",
              description = "The R code to execute. Must be valid R syntax. Store final output in 'result' variable."
            ),
            description = list(
              type = "string",
              description = "Brief description of what this code does (for logging purposes)"
            )
          ),
          required = list("code")
        )
      ),
      list(
        name = "get_data_summary",
        description = "Get comprehensive summary of the dataset including structure, variable types, and basic statistics",
        input_schema = list(
          type = "object",
          properties = list(
            detailed = list(
              type = "boolean",
              description = "If true, includes statistical summaries for all variables. Default false."
            )
          ),
          required = list()
        )
      ),
      list(
        name = "get_column_info",
        description = "Get detailed information about specific column(s) including unique values, missing data, and distribution",
        input_schema = list(
          type = "object",
          properties = list(
            columns = list(
              type = "array",
              items = list(type = "string"),
              description = "Column names to get information about"
            )
          ),
          required = list("columns")
        )
      ),
      list(
        name = "get_data_sample",
        description = "Get sample rows from the dataset to understand the actual data values",
        input_schema = list(
          type = "object",
          properties = list(
            n_rows = list(
              type = "integer",
              description = "Number of rows to sample. Default 10, max 50."
            )
          ),
          required = list()
        )
      )
    )
  }

  # Helper: Determine result type and store result
  # Unified logic for classifying and storing execution results
  determine_result_type <- function(res, store_result = TRUE) {
    result_info <- list()

    # Single plot
    if (inherits(res, c("ggplot", "gg", "gtable", "grob", "recordedplot"))) {
      result_info$type <- "plot"
      result_info$value <- list(res)
      result_info$message <- "Plot generated successfully. The plot is now displayed in the Results panel."

    # Multiple plots
    } else if (is.list(res) && length(res) > 0 &&
               all(sapply(res, function(x) inherits(x, c("ggplot", "gg", "gtable", "grob", "recordedplot"))))) {
      result_info$type <- "plot"
      result_info$value <- res
      result_info$message <- sprintf("Generated %d plots successfully. All plots are displayed in the Results panel.", length(res))

    # Flextable
    } else if (inherits(res, "flextable")) {
      result_info$type <- "flextable"
      result_info$value <- res
      result_info$message <- "Flextable generated successfully."

    # Table object (from table() function)
    } else if (inherits(res, "table")) {
      result_info$type <- "table"
      result_info$value <- as.data.frame(res)
      result_info$message <- sprintf("Table generated: %d rows × %d columns",
                                     nrow(as.data.frame(res)), ncol(as.data.frame(res)))

    # Data frame or matrix
    } else if (is.data.frame(res) || is.matrix(res)) {
      result_info$type <- "table"
      result_info$value <- res
      result_info$message <- sprintf(
        "Table generated: %d rows × %d columns\nFirst few rows:\n%s",
        nrow(res), ncol(res),
        paste(capture.output(print(head(res, 3))), collapse = "\n")
      )

    # List with $table element (e.g., CreateTableOneJS result)
    } else if (is.list(res) && !is.null(res$table)) {
      result_info$type <- "table"
      result_info$value <- res$table
      result_info$message <- sprintf("Table extracted from list result: %d rows × %d columns",
                                     nrow(res$table), ncol(res$table))

    # Everything else as text
    } else {
      result_info$type <- "text"
      result_info$value <- res
      result_info$message <- paste(capture.output(print(res)), collapse = "\n")
    }

    # Store in reactive values if requested
    if (store_result) {
      execution_result(result_info$value)
      result_type(result_info$type)
    }

    return(result_info)
  }

  # Helper: Build analysis context section for prompts
  build_analysis_context <- function(context_info) {
    if (is.null(context_info) || length(context_info) == 0) {
      return("")
    }

    context_section <- "\n\n## ANALYSIS CONTEXT\n"
    context_section <- paste0(context_section,
      "The user has already performed the following analyses in the application. ",
      "You can reference these previous results when the user mentions 'the previous analysis', ",
      "'the table/plot I showed you', or asks for follow-up analyses:\n\n")

    for (name in names(context_info)) {
      item <- context_info[[name]]

      # Show description if available
      if (!is.null(item$description)) {
        context_section <- paste0(context_section, "- ", name, ": ", item$description, "\n")
      }

      # Show code if available
      if (!is.null(item$code)) {
        context_section <- paste0(context_section, "  Code:\n```r\n", item$code, "\n```\n")
      }
    }
    context_section <- paste0(context_section, "\n")

    return(context_section)
  }

  # Helper: Get default model for provider
  get_default_model <- function(provider) {
    switch(provider,
      "anthropic" = "claude-sonnet-4-20250514",
      "openai" = "gpt-4-turbo",
      "google" = "gemini-1.5-flash",
      "claude-sonnet-4-20250514"
    )
  }

  # Validate R code for safety
  validate_code_safety <- function(code) {
    # List of dangerous functions to block
    dangerous_patterns <- c(
      "system", "system2", "shell", "shell.exec",  # System calls
      "Sys\\.setenv", "Sys\\.unsetenv",  # Environment modification
      "unlink", "file\\.remove", "file\\.create", "dir\\.create",  # File operations
      "setwd", "source",  # Directory/source changes
      "options", "par",  # Global settings (partially allowed for plots)
      "install\\.packages", "remove\\.packages",  # Package management
      "library\\((?!jstable|jskm|jsmodule|survival|ggplot2|ggpubr|pROC|data\\.table)",  # Restrict libraries
      "require\\((?!jstable|jskm|jsmodule|survival|ggplot2|ggpubr|pROC|data\\.table)",
      "eval\\(", "evalq\\(",  # Nested eval
      "assign\\(.+envir\\s*=\\s*\\.GlobalEnv",  # Global env assignment
      "<<-"  # Super assignment
    )

    for (pattern in dangerous_patterns) {
      if (grepl(pattern, code, perl = TRUE)) {
        # Allow par() for plot settings
        if (pattern == "par" && grepl("par\\(", code)) {
          next
        }
        return(list(
          safe = FALSE,
          reason = sprintf("Blocked pattern detected: %s", pattern)
        ))
      }
    }

    # Try to parse to check syntax
    tryCatch({
      parse(text = code)
      return(list(safe = TRUE))
    }, error = function(e) {
      return(list(
        safe = FALSE,
        reason = sprintf("Syntax error: %s", e$message)
      ))
    })
  }

  # Execute tools called by AI
  execute_tool <- function(tool_name, tool_input) {
    tryCatch({
      if (tool_name == "execute_r_code") {
        code <- tool_input$code
        description <- if (!is.null(tool_input$description)) tool_input$description else "Code execution"

        # Validate code safety
        safety_check <- validate_code_safety(code)
        if (!safety_check$safe) {
          return(list(
            success = FALSE,
            result = sprintf("Code validation failed: %s", safety_check$reason)
          ))
        }

        # Create restricted environment
        env <- new.env(parent = emptyenv())

        # Add only allowed base functions
        allowed_functions <- c(
          # Data manipulation
          "c", "list", "data.frame", "matrix", "cbind", "rbind",
          "names", "colnames", "rownames", "nrow", "ncol", "dim",
          "head", "tail", "subset", "merge", "transform",
          # Math & stats
          "sum", "mean", "median", "sd", "var", "min", "max", "range",
          "quantile", "cor", "cov", "t.test", "chisq.test", "fisher.test",
          # Logical & control
          "if", "else", "for", "while", "function", "return",
          "ifelse", "switch", "which", "any", "all",
          # Type conversion
          "as.numeric", "as.character", "as.factor", "as.integer",
          "as.data.frame", "as.matrix", "as.list",
          "is.na", "is.null", "is.factor", "is.numeric",
          # Utility
          "print", "cat", "paste", "paste0", "sprintf",
          "length", "unique", "table", "summary", "str",
          "grep", "grepl", "sub", "gsub", "strsplit"
        )

        for (fn in allowed_functions) {
          if (exists(fn, envir = baseenv())) {
            env[[fn]] <- get(fn, envir = baseenv())
          }
        }

        # Add data objects
        env$out <- data()
        env$out.label <- if (!is.null(data_label)) data_label() else NULL

        # Add allowed packages to search path
        env$library <- function(package, ...) {
          pkg_name <- as.character(substitute(package))
          allowed_pkgs <- c("jstable", "jskm", "jsmodule", "survival",
                           "ggplot2", "ggpubr", "pROC", "data.table",
                           "DT", "gridExtra", "GGally")
          if (pkg_name %in% allowed_pkgs) {
            base::library(package, character.only = TRUE, ...)
            # Copy package functions to environment
            pkg_env <- as.environment(paste0("package:", pkg_name))
            for (obj in ls(pkg_env)) {
              env[[obj]] <- get(obj, envir = pkg_env)
            }
          } else {
            stop(sprintf("Package '%s' is not allowed", pkg_name))
          }
        }

        # Execute code in restricted environment
        parsed <- parse(text = code)
        res <- NULL
        for (expr in parsed) {
          res <- eval(expr, envir = env)
        }

        # Check for result variable
        if (exists("result", envir = env)) {
          res <- get("result", envir = env)
        }

        # Store successful code
        current_code(code)

        # Determine result type and store (using unified helper)
        result_info <- determine_result_type(res, store_result = TRUE)

        return(list(
          success = TRUE,
          result = result_info$message
        ))

      } else if (tool_name == "get_data_summary") {
        d <- data()
        detailed <- if (!is.null(tool_input$detailed)) tool_input$detailed else FALSE

        factor_vars <- names(d)[sapply(d, is.factor)]
        numeric_vars <- names(d)[sapply(d, is.numeric)]

        summary_text <- sprintf(
          "Dataset Summary:\n- Rows: %d\n- Columns: %d\n- Factor variables (%d): %s\n- Numeric variables (%d): %s",
          nrow(d), ncol(d),
          length(factor_vars), paste(factor_vars, collapse = ", "),
          length(numeric_vars), paste(numeric_vars, collapse = ", ")
        )

        if (detailed) {
          summary_text <- paste0(
            summary_text, "\n\nStatistical Summary:\n",
            paste(capture.output(summary(d)), collapse = "\n")
          )
        }

        return(list(success = TRUE, result = summary_text))

      } else if (tool_name == "get_column_info") {
        d <- data()
        columns <- tool_input$columns

        info_list <- lapply(columns, function(col) {
          if (!col %in% names(d)) {
            return(sprintf("Column '%s' not found", col))
          }

          vec <- d[[col]]
          n_missing <- sum(is.na(vec))

          if (is.factor(vec) || is.character(vec)) {
            unique_vals <- unique(vec)
            n_unique <- length(unique_vals)
            top_values <- head(sort(table(vec), decreasing = TRUE), 5)

            sprintf(
              "Column: %s\nType: %s\nMissing: %d (%.1f%%)\nUnique values: %d\nTop 5 values:\n%s",
              col, class(vec)[1], n_missing, 100 * n_missing / length(vec),
              n_unique,
              paste(capture.output(print(top_values)), collapse = "\n")
            )
          } else {
            sprintf(
              "Column: %s\nType: %s\nMissing: %d (%.1f%%)\nMin: %.2f\nMax: %.2f\nMean: %.2f\nMedian: %.2f",
              col, class(vec)[1], n_missing, 100 * n_missing / length(vec),
              min(vec, na.rm = TRUE), max(vec, na.rm = TRUE),
              mean(vec, na.rm = TRUE), median(vec, na.rm = TRUE)
            )
          }
        })

        return(list(
          success = TRUE,
          result = paste(info_list, collapse = "\n\n")
        ))

      } else if (tool_name == "get_data_sample") {
        d <- data()
        n_rows <- if (!is.null(tool_input$n_rows)) min(tool_input$n_rows, 50) else 10

        sample_idx <- if (nrow(d) <= n_rows) {
          1:nrow(d)
        } else {
          sample(nrow(d), n_rows)
        }

        sample_data <- d[sample_idx, ]

        result_text <- sprintf(
          "Sample of %d rows from dataset:\n%s",
          nrow(sample_data),
          paste(capture.output(print(sample_data)), collapse = "\n")
        )

        return(list(success = TRUE, result = result_text))

      } else {
        return(list(
          success = FALSE,
          result = paste("Unknown tool:", tool_name)
        ))
      }
    }, error = function(e) {
      # Store error for debugging
      if (tool_name == "execute_r_code") {
        result_type("error")
        execution_result(list(error = TRUE, message = e$message))
      }

      return(list(
        success = FALSE,
        result = sprintf("Error executing tool: %s", e$message)
      ))
    })
  }

  # Build context from data
  data_context <- reactive({
    req(data())
    d <- data()
    dl <- if (!is.null(data_label)) data_label() else NULL
    vs <- if (!is.null(data_varStruct)) data_varStruct else NULL

    factor_vars <- names(d)[sapply(d, is.factor)]
    numeric_vars <- names(d)[sapply(d, is.numeric)]

    context <- paste0(
      "## Current Data Information\n",
      "- Data: out (", nrow(d), " rows x ", ncol(d), " columns)\n",
      "- Factor variables: ", paste(head(factor_vars, 10), collapse = ", "),
      if (length(factor_vars) > 10) " ..." else "", "\n",
      "- Numeric variables: ", paste(head(numeric_vars, 10), collapse = ", "),
      if (length(numeric_vars) > 10) " ..." else "", "\n"
    )

    if (!is.null(vs)) {
      context <- paste0(context,
        "\n## Variable Structure\n",
        "- Base: ", paste(head(vs$Base, 5), collapse = ", "), " ...\n"
      )
      if (!is.null(vs$Event)) {
        context <- paste0(context, "- Event: ", paste(vs$Event, collapse = ", "), "\n")
      }
      if (!is.null(vs$Time)) {
        context <- paste0(context, "- Time: ", paste(vs$Time, collapse = ", "), "\n")
      }
    }

    if (!is.null(dl)) {
      context <- paste0(context,
        "\n## Available Labels (first 10)\n",
        paste(capture.output(print(head(dl, 10))), collapse = "\n")
      )
    }

    return(context)
  })

  # AI API call function with tool use (function calling)
  call_ai_with_tools <- function(user_message, conversation_history = list(), max_iterations = 5, context_info = NULL) {
    API_KEY <- get_api_key()
    provider <- get_provider()
    model <- selected_model()

    message("[DEBUG] call_ai_with_tools - Provider: ", provider, ", Model: ", model %||% "default")

    if (API_KEY == "" || is.null(API_KEY)) {
      message("[ERROR] API key is not configured")
      return(list(
        success = FALSE,
        message = "API key not configured."
      ))
    }

    # Use default model if not selected
    if (is.null(model)) {
      model <- get_default_model(provider)
      message("[DEBUG] Using default model: ", model)
    }

    # Build analysis context section (using helper)
    context_section <- build_analysis_context(context_info)

    # Build system prompt (simplified for tool use)
    system_prompt <- paste0(
      "You are an R statistical analysis expert. ",
      "Use the provided tools to help users analyze their data:\n",
      "- execute_r_code: Run R code to perform analysis\n",
      "- get_data_summary: Get dataset overview\n",
      "- get_column_info: Get detailed column information\n",
      "- get_data_sample: See actual data values\n\n",
      "IMPORTANT GUIDELINES:\n",
      "1. Always use jsmodule/jstable/jskm functions for analysis\n",
      "2. Store final results in 'result' variable\n",
      "3. If code execution fails, analyze the error and fix it\n",
      "4. Use get_data_summary or get_column_info to explore data first\n",
      "5. Never write file-saving code (write.xlsx, ggsave, etc.)\n\n",
      stats_guide_text,
      context_section
    )

    # Get tools schema
    tools <- get_tools_schema()

    # Initialize conversation
    messages <- c(
      conversation_history,
      list(list(role = "user", content = user_message))
    )

    # Tool use loop
    iteration <- 0
    accumulated_tokens <- list(input = 0, output = 0)
    tool_log <- list()

    while (iteration < max_iterations) {
      iteration <- iteration + 1

      message("[DEBUG] Iteration ", iteration, "/", max_iterations)

      # Call API based on provider
      if (provider == "anthropic") {
        message("[DEBUG] Calling Anthropic API with model: ", model)
        response <- tryCatch({
          httr::POST(
            url = "https://api.anthropic.com/v1/messages",
            httr::add_headers(
              "x-api-key" = API_KEY,
              "anthropic-version" = "2023-06-01",
              "content-type" = "application/json"
            ),
            body = jsonlite::toJSON(list(
              model = model,
              max_tokens = 4096,
              system = system_prompt,
              messages = messages,
              tools = tools
            ), auto_unbox = TRUE),
            encode = "json"
          )
        }, error = function(e) {
          return(list(success = FALSE, message = paste("API error:", e$message)))
        })

        if (inherits(response, "list") && !is.null(response$success) && !response$success) {
          return(response)
        }

        parsed <- httr::content(response, "parsed")

        if (!is.null(parsed$error)) {
          return(list(success = FALSE, message = parsed$error$message))
        }

        # Track tokens
        if (!is.null(parsed$usage)) {
          accumulated_tokens$input <- accumulated_tokens$input + (parsed$usage$input_tokens %||% 0)
          accumulated_tokens$output <- accumulated_tokens$output + (parsed$usage$output_tokens %||% 0)
        }

        # Check stop reason
        stop_reason <- parsed$stop_reason

        if (stop_reason == "end_turn") {
          # Final answer
          final_text <- ""
          for (content_block in parsed$content) {
            if (content_block$type == "text") {
              final_text <- paste0(final_text, content_block$text)
            }
          }

          return(list(
            success = TRUE,
            message = final_text,
            usage = list(
              input_tokens = accumulated_tokens$input,
              output_tokens = accumulated_tokens$output
            ),
            tool_log = tool_log
          ))

        } else if (stop_reason == "tool_use") {
          # Execute tools
          tool_results <- list()

          for (content_block in parsed$content) {
            if (content_block$type == "tool_use") {
              tool_name <- content_block$name
              tool_input <- content_block$input
              tool_id <- content_block$id

              # Execute tool
              result <- execute_tool(tool_name, tool_input)

              # Log tool use
              tool_log[[length(tool_log) + 1]] <- list(
                name = tool_name,
                input = tool_input,
                result = result
              )

              # Add tool result
              tool_results[[length(tool_results) + 1]] <- list(
                type = "tool_result",
                tool_use_id = tool_id,
                content = if (result$success) result$result else paste("Error:", result$result)
              )
            }
          }

          # Add assistant message and tool results to conversation
          messages[[length(messages) + 1]] <- list(
            role = "assistant",
            content = parsed$content
          )

          messages[[length(messages) + 1]] <- list(
            role = "user",
            content = tool_results
          )

          # Continue loop to get next response
          next

        } else {
          return(list(
            success = FALSE,
            message = paste("Unexpected stop reason:", stop_reason)
          ))
        }

      } else if (provider == "openai") {
        # OpenAI tool calling implementation
        # Convert tools to OpenAI format
        openai_tools <- lapply(tools, function(tool) {
          list(
            type = "function",
            `function` = list(
              name = tool$name,
              description = tool$description,
              parameters = tool$input_schema
            )
          )
        })

        openai_messages <- c(
          list(list(role = "system", content = system_prompt)),
          messages
        )

        response <- tryCatch({
          httr::POST(
            url = "https://api.openai.com/v1/chat/completions",
            httr::add_headers(
              "Authorization" = paste("Bearer", API_KEY),
              "Content-Type" = "application/json"
            ),
            body = jsonlite::toJSON(list(
              model = model,
              messages = openai_messages,
              max_tokens = 4096,
              tools = openai_tools,
              tool_choice = "auto"
            ), auto_unbox = TRUE),
            encode = "json"
          )
        }, error = function(e) {
          return(list(success = FALSE, message = paste("API error:", e$message)))
        })

        if (inherits(response, "list") && !is.null(response$success) && !response$success) {
          return(response)
        }

        parsed <- httr::content(response, "parsed")

        if (!is.null(parsed$error)) {
          return(list(success = FALSE, message = parsed$error$message %||% "Unknown error"))
        }

        # Track tokens
        if (!is.null(parsed$usage)) {
          accumulated_tokens$input <- accumulated_tokens$input + (parsed$usage$prompt_tokens %||% 0)
          accumulated_tokens$output <- accumulated_tokens$output + (parsed$usage$completion_tokens %||% 0)
        }

        message_obj <- parsed$choices[[1]]$message

        # Check if tool calls exist
        if (!is.null(message_obj$tool_calls) && length(message_obj$tool_calls) > 0) {
          # Execute tools
          tool_messages <- list()

          for (tool_call in message_obj$tool_calls) {
            tool_name <- tool_call$`function`$name
            tool_input <- jsonlite::fromJSON(tool_call$`function`$arguments)

            # Execute tool
            result <- execute_tool(tool_name, tool_input)

            # Log tool use
            tool_log[[length(tool_log) + 1]] <- list(
              name = tool_name,
              input = tool_input,
              result = result
            )

            # Add tool result message
            tool_messages[[length(tool_messages) + 1]] <- list(
              role = "tool",
              tool_call_id = tool_call$id,
              name = tool_name,
              content = if (result$success) result$result else paste("Error:", result$result)
            )
          }

          # Add assistant message and tool results
          messages[[length(messages) + 1]] <- message_obj
          messages <- c(messages, tool_messages)

          # Continue loop
          next

        } else {
          # Final answer
          final_text <- message_obj$content %||% ""

          return(list(
            success = TRUE,
            message = final_text,
            usage = list(
              input_tokens = accumulated_tokens$input,
              output_tokens = accumulated_tokens$output
            ),
            tool_log = tool_log
          ))
        }

      } else if (provider == "google") {
        # Google Gemini function calling implementation
        # Convert tools to Gemini format
        gemini_tools <- list(
          function_declarations = lapply(tools, function(tool) {
            list(
              name = tool$name,
              description = tool$description,
              parameters = tool$input_schema
            )
          })
        )

        # Convert messages to Gemini format
        gemini_contents <- list()
        for (msg in messages) {
          if (msg$role == "user") {
            if (is.list(msg$content) && !is.null(msg$content[[1]]$type)) {
              # This is a tool result
              gemini_contents[[length(gemini_contents) + 1]] <- list(
                role = "model",
                parts = lapply(msg$content, function(tool_res) {
                  list(
                    functionResponse = list(
                      name = tool_res$name %||% "unknown",
                      response = list(
                        result = tool_res$content
                      )
                    )
                  )
                })
              )
            } else {
              # Regular user message
              gemini_contents[[length(gemini_contents) + 1]] <- list(
                role = "user",
                parts = list(list(text = msg$content))
              )
            }
          } else if (msg$role == "assistant") {
            # Assistant message (may contain function calls)
            gemini_contents[[length(gemini_contents) + 1]] <- list(
              role = "model",
              parts = if (is.list(msg$content)) msg$content else list(list(text = msg$content))
            )
          }
        }

        response <- tryCatch({
          httr::POST(
            url = paste0(
              "https://generativelanguage.googleapis.com/v1beta/",
              "models/", model, ":generateContent?key=", API_KEY
            ),
            httr::add_headers(
              "Content-Type" = "application/json"
            ),
            body = jsonlite::toJSON(list(
              contents = gemini_contents,
              systemInstruction = list(
                parts = list(list(text = system_prompt))
              ),
              tools = list(gemini_tools),
              generationConfig = list(
                temperature = 0.7,
                maxOutputTokens = 4096
              )
            ), auto_unbox = TRUE),
            encode = "json"
          )
        }, error = function(e) {
          return(list(success = FALSE, message = paste("API error:", e$message)))
        })

        if (inherits(response, "list") && !is.null(response$success) && !response$success) {
          return(response)
        }

        parsed <- httr::content(response, "parsed")

        if (!is.null(parsed$error)) {
          error_msg <- if (!is.null(parsed$error$message)) {
            parsed$error$message
          } else {
            "Unknown Google API error"
          }
          return(list(success = FALSE, message = error_msg))
        }

        # Track tokens
        if (!is.null(parsed$usageMetadata)) {
          accumulated_tokens$input <- accumulated_tokens$input + (parsed$usageMetadata$promptTokenCount %||% 0)
          accumulated_tokens$output <- accumulated_tokens$output + (parsed$usageMetadata$candidatesTokenCount %||% 0)
        }

        # Check if response has function calls
        if (!is.null(parsed$candidates) && length(parsed$candidates) > 0) {
          candidate <- parsed$candidates[[1]]

          if (!is.null(candidate$content$parts)) {
            parts <- candidate$content$parts

            # Check for function calls
            has_function_call <- FALSE
            function_call_parts <- list()
            tool_results_for_response <- list()

            for (part in parts) {
              if (!is.null(part$functionCall)) {
                has_function_call <- TRUE
                tool_name <- part$functionCall$name
                tool_input <- part$functionCall$args

                # Execute tool
                result <- execute_tool(tool_name, tool_input)

                # Log tool use
                tool_log[[length(tool_log) + 1]] <- list(
                  name = tool_name,
                  input = tool_input,
                  result = result
                )

                # Store function call part
                function_call_parts[[length(function_call_parts) + 1]] <- part

                # Prepare tool result for next request
                tool_results_for_response[[length(tool_results_for_response) + 1]] <- list(
                  type = "tool_result",
                  name = tool_name,
                  content = if (result$success) result$result else paste("Error:", result$result)
                )
              }
            }

            if (has_function_call) {
              # Add assistant message with function calls
              messages[[length(messages) + 1]] <- list(
                role = "assistant",
                content = parts
              )

              # Add user message with function results
              messages[[length(messages) + 1]] <- list(
                role = "user",
                content = tool_results_for_response
              )

              # Continue loop
              next

            } else {
              # Final answer - extract text
              final_text <- ""
              for (part in parts) {
                if (!is.null(part$text)) {
                  final_text <- paste0(final_text, part$text)
                }
              }

              return(list(
                success = TRUE,
                message = final_text,
                usage = list(
                  input_tokens = accumulated_tokens$input,
                  output_tokens = accumulated_tokens$output
                ),
                tool_log = tool_log
              ))
            }
          }
        }

        # If we get here, something went wrong
        return(list(
          success = FALSE,
          message = "Failed to parse Google Gemini response"
        ))

      } else {
        return(list(
          success = FALSE,
          message = paste("Unknown provider:", provider)
        ))
      }
    }

    # Max iterations reached
    return(list(
      success = FALSE,
      message = sprintf("Maximum iterations (%d) reached without final answer", max_iterations),
      tool_log = tool_log
    ))
  }

  # Helper function for null coalescing
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # AI API call function (legacy - without tool use)
  call_ai <- function(user_message, conversation_history = list(), context_info = NULL) {
    API_KEY <- get_api_key()
    provider <- get_provider()
    model <- selected_model()

    if (API_KEY == "" || is.null(API_KEY)) {
      return(list(
        success = FALSE,
        message = paste(
          "API key not configured.",
          "Please enter your API key in the Settings tab."
        )
      ))
    }

    # Use default model if not selected
    if (is.null(model)) {
      model <- get_default_model(provider)
    }

    # Build analysis context section (using helper)
    context_section <- build_analysis_context(context_info)

    # Build system prompt from template + context
    system_prompt <- paste0(
      stats_guide_text, "\n\n",
      "## Current Project Context\n",
      data_context(),
      context_section
    )

    messages <- c(
      conversation_history,
      list(list(role = "user", content = user_message))
    )

    # Call appropriate API based on provider
    if (provider == "anthropic") {
      # Anthropic Claude API
      response <- tryCatch({
        httr::POST(
          url = "https://api.anthropic.com/v1/messages",
          httr::add_headers(
            "x-api-key" = API_KEY,
            "anthropic-version" = "2023-06-01",
            "content-type" = "application/json"
          ),
          body = jsonlite::toJSON(list(
            model = model,
            max_tokens = 4096,
            system = system_prompt,
            messages = messages
          ), auto_unbox = TRUE),
          encode = "json"
        )
      }, error = function(e) {
        return(list(success = FALSE, message = paste("API error:", e$message)))
      })

      if (inherits(response, "list") &&
          !is.null(response$success) && !response$success) {
        return(response)
      }

      parsed_content <- httr::content(response, "parsed")

      if (!is.null(parsed_content$error)) {
        return(list(success = FALSE, message = parsed_content$error$message))
      }

      assistant_message <- parsed_content$content[[1]]$text

      # Extract token usage
      usage <- list(
        input_tokens = if (!is.null(parsed_content$usage$input_tokens)) parsed_content$usage$input_tokens else 0,
        output_tokens = if (!is.null(parsed_content$usage$output_tokens)) parsed_content$usage$output_tokens else 0
      )

      return(list(success = TRUE, message = assistant_message, usage = usage))

    } else if (provider == "openai") {
      # OpenAI API
      # Convert messages format (add system message to messages array)
      openai_messages <- c(
        list(list(role = "system", content = system_prompt)),
        messages
      )

      response <- tryCatch({
        httr::POST(
          url = "https://api.openai.com/v1/chat/completions",
          httr::add_headers(
            "Authorization" = paste("Bearer", API_KEY),
            "Content-Type" = "application/json"
          ),
          body = jsonlite::toJSON(list(
            model = model,
            messages = openai_messages,
            max_tokens = 4096,
            temperature = 0.7
          ), auto_unbox = TRUE),
          encode = "json"
        )
      }, error = function(e) {
        return(list(success = FALSE, message = paste("API error:", e$message)))
      })

      if (inherits(response, "list") &&
          !is.null(response$success) && !response$success) {
        return(response)
      }

      parsed_content <- httr::content(response, "parsed")

      if (!is.null(parsed_content$error)) {
        error_msg <- if (!is.null(parsed_content$error$message)) {
          parsed_content$error$message
        } else {
          "Unknown OpenAI API error"
        }
        return(list(success = FALSE, message = error_msg))
      }

      assistant_message <- parsed_content$choices[[1]]$message$content

      # Extract token usage
      usage <- list(
        input_tokens = if (!is.null(parsed_content$usage$prompt_tokens)) parsed_content$usage$prompt_tokens else 0,
        output_tokens = if (!is.null(parsed_content$usage$completion_tokens)) parsed_content$usage$completion_tokens else 0
      )

      return(list(success = TRUE, message = assistant_message, usage = usage))

    } else if (provider == "google") {
      # Google Gemini API
      # Convert messages format
      gemini_contents <- lapply(messages, function(msg) {
        list(
          role = if (msg$role == "assistant") "model" else "user",
          parts = list(list(text = msg$content))
        )
      })

      response <- tryCatch({
        httr::POST(
          url = paste0(
            "https://generativelanguage.googleapis.com/v1beta/",
            "models/", model, ":generateContent?key=", API_KEY
          ),
          httr::add_headers(
            "Content-Type" = "application/json"
          ),
          body = jsonlite::toJSON(list(
            contents = gemini_contents,
            systemInstruction = list(
              parts = list(list(text = system_prompt))
            ),
            generationConfig = list(
              temperature = 0.7,
              maxOutputTokens = 4096
            )
          ), auto_unbox = TRUE),
          encode = "json"
        )
      }, error = function(e) {
        return(list(success = FALSE, message = paste("API error:", e$message)))
      })

      if (inherits(response, "list") &&
          !is.null(response$success) && !response$success) {
        return(response)
      }

      parsed_content <- httr::content(response, "parsed")

      if (!is.null(parsed_content$error)) {
        error_msg <- if (!is.null(parsed_content$error$message)) {
          parsed_content$error$message
        } else {
          "Unknown Google API error"
        }
        return(list(success = FALSE, message = error_msg))
      }

      # Extract text from Gemini response
      if (!is.null(parsed_content$candidates) &&
          length(parsed_content$candidates) > 0) {
        candidate <- parsed_content$candidates[[1]]
        if (!is.null(candidate$content$parts) &&
            length(candidate$content$parts) > 0) {
          assistant_message <- candidate$content$parts[[1]]$text

          # Extract token usage
          usage <- list(
            input_tokens = if (!is.null(parsed_content$usageMetadata$promptTokenCount)) parsed_content$usageMetadata$promptTokenCount else 0,
            output_tokens = if (!is.null(parsed_content$usageMetadata$candidatesTokenCount)) parsed_content$usageMetadata$candidatesTokenCount else 0
          )

          return(list(success = TRUE, message = assistant_message, usage = usage))
        }
      }

      return(list(
        success = FALSE,
        message = "Failed to parse Google API response"
      ))

    } else {
      return(list(
        success = FALSE,
        message = paste("Unknown provider:", provider)
      ))
    }
  }

  # Calculate cost based on provider and model
  calculate_cost <- function(provider, model, input_tokens, output_tokens) {
    # Pricing per 1M tokens (as of 2024)
    pricing <- list(
      anthropic = list(
        default = list(input = 3, output = 15),
        "claude-opus" = list(input = 15, output = 75),
        "claude-sonnet" = list(input = 3, output = 15),
        "claude-haiku" = list(input = 0.25, output = 1.25)
      ),
      openai = list(
        default = list(input = 30, output = 60),
        "gpt-4" = list(input = 30, output = 60),
        "gpt-3.5" = list(input = 0.5, output = 1.5)
      ),
      google = list(
        default = list(input = 0.5, output = 1.5),
        "gemini-pro" = list(input = 0.5, output = 1.5),
        "gemini-flash" = list(input = 0.075, output = 0.3)
      )
    )

    # Get provider pricing
    provider_pricing <- pricing[[provider]]
    if (is.null(provider_pricing)) {
      return(0)
    }

    # Find matching model price
    price <- provider_pricing$default
    for (model_key in names(provider_pricing)) {
      if (model_key != "default" && grepl(model_key, model, ignore.case = TRUE)) {
        price <- provider_pricing[[model_key]]
        break
      }
    }

    # Calculate cost
    input_cost <- (input_tokens / 1000000) * price$input
    output_cost <- (output_tokens / 1000000) * price$output
    return(input_cost + output_cost)
  }

  # Extract R code from markdown
  extract_r_code <- function(text) {
    pattern <- "(?s)```r\\s*\\n(.*?)\\n```"
    match <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
    if (length(match) > 1) {
      return(match[2])
    }
    return(NULL)
  }

  # Send message to AI
  observeEvent(input$send_btn, {
    req(input$user_input)
    user_msg <- input$user_input

    # Immediately clear and disable input field
    updateTextAreaInput(session, "user_input", value = "")
    shinyjs::disable("user_input")
    shinyjs::disable("send_btn")

    # Update display history immediately (show user message right away)
    new_display <- c(display_history(),
                     list(list(role = "user", content = user_msg)))
    display_history(new_display)

    # Prepare analysis context info
    context_info <- NULL
    if (!is.null(analysis_context)) {
      # Handle both reactive and non-reactive analysis_context
      if (is.reactive(analysis_context)) {
        context_info <- analysis_context()
      } else {
        context_info <- analysis_context
      }
    }

    # Call AI API with function calling
    response <- tryCatch({
      withProgress(message = "Processing request...", {
        # Check API key first
        api_key <- get_api_key()
        if (is.null(api_key) || api_key == "") {
          showNotification("API key not configured", type = "error", duration = 5)
          return(list(
            success = FALSE,
            message = "API key not configured. Please set your API key in the configuration panel or environment variable."
          ))
        }

        # Debug output to console
        cat("[DEBUG] Calling AI API...\n", file = stderr())
        showNotification("Calling AI API...", type = "message", duration = 2)

        result <- call_ai(user_msg, chat_history(), context_info = context_info)

        cat("[DEBUG] AI API returned: ", if (result$success) "SUCCESS" else "FAILED", "\n", file = stderr())
        if (result$success) {
          showNotification("AI response received", type = "message", duration = 2)
        } else {
          showNotification(paste("AI API failed:", result$message), type = "error", duration = 5)
        }

        result
      })
    }, error = function(e) {
      cat("[ERROR] Exception in AI API call: ", e$message, "\n", file = stderr())
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      list(
        success = FALSE,
        message = paste("Error:", e$message)
      )
    })

    if (response$success) {
      # Update API history
      new_history <- c(chat_history(),
                       list(list(role = "user", content = user_msg)),
                       list(list(role = "assistant", content = response$message)))
      chat_history(new_history)

      # Update display
      new_display <- c(display_history(),
                       list(list(role = "assistant", content = response$message)))
      display_history(new_display)

      # Update token usage
      if (!is.null(response$usage)) {
        current_usage <- token_usage()
        provider <- get_provider()
        model <- selected_model()

        new_input <- current_usage$input_tokens + response$usage$input_tokens
        new_output <- current_usage$output_tokens + response$usage$output_tokens
        new_total <- new_input + new_output
        new_cost <- current_usage$total_cost + calculate_cost(provider, model,
                                                               response$usage$input_tokens,
                                                               response$usage$output_tokens)

        token_usage(list(
          input_tokens = new_input,
          output_tokens = new_output,
          total_tokens = new_total,
          total_cost = new_cost
        ))
      }

      # Extract R code from markdown response
      code <- extract_r_code(response$message)
      if (!is.null(code)) {
        current_code(code)
      }

    } else {
      new_display <- c(display_history(),
                       list(list(role = "error", content = response$message)))
      display_history(new_display)
    }

    # Re-enable input field after response
    shinyjs::enable("user_input")
    shinyjs::enable("send_btn")
  })

  # Display token usage
  output$token_display <- renderUI({
    usage <- token_usage()

    if (usage$total_tokens == 0) {
      return(NULL)
    }

    tags$div(
      class = "alert alert-info",
      style = "padding: 8px; margin-bottom: 10px; font-size: 12px;",
      tags$strong(icon("chart-bar"), " Token Usage: "),
      tags$span(
        sprintf("Input: %s | Output: %s | Total: %s",
                format(usage$input_tokens, big.mark = ","),
                format(usage$output_tokens, big.mark = ","),
                format(usage$total_tokens, big.mark = ","))
      ),
      tags$br(),
      tags$strong(icon("dollar-sign"), " Estimated Cost: "),
      tags$span(sprintf("$%.4f", usage$total_cost))
    )
  })

  # Display chat history
  output$chat_history <- renderUI({
    history <- display_history()
    if (length(history) == 0) {
      return(p("Start a conversation...", style = "color: gray; font-style: italic;"))
    }

    tagList(
      lapply(history, function(msg) {
        if (msg$role == "user") {
          tags$div(
            style = paste0(
              "background-color: #e3f2fd; padding: 8px; margin: 5px 0; ",
              "border-radius: 8px; word-wrap: break-word; overflow-wrap: break-word;"
            ),
            tags$strong(icon("user"), " You: "),
            tags$span(
              style = "white-space: pre-wrap; word-break: break-word;",
              msg$content
            )
          )
        } else if (msg$role == "assistant") {
          tags$div(
            style = paste0(
              "background-color: #f5f5f5; padding: 8px; margin: 5px 0; ",
              "border-radius: 8px; word-wrap: break-word; overflow-wrap: break-word;"
            ),
            tags$strong(icon("robot"), " AI: "),
            tags$pre(
              style = paste0(
                "white-space: pre-wrap; font-size: 11px; margin-top: 5px; ",
                "word-break: break-word; overflow-wrap: break-word; margin-bottom: 0;"
              ),
              msg$content
            )
          )
        } else {
          tags$div(
            style = paste0(
              "background-color: #ffebee; padding: 8px; margin: 5px 0; ",
              "border-radius: 8px; word-wrap: break-word; overflow-wrap: break-word;"
            ),
            tags$strong(icon("exclamation-triangle"), " Error: "),
            tags$span(
              style = "white-space: pre-wrap; word-break: break-word;",
              msg$content
            )
          )
        }
      })
    )
  })

  # Auto-scroll chat to bottom when history updates
  observe({
    display_history()  # Create dependency on history changes
    session$sendCustomMessage(
      type = "scrollChat",
      message = list(id = session$ns("chat_container"))
    )
  })

  # Update code editor when current_code changes
  observe({
    code <- current_code()
    shinyAce::updateAceEditor(session, "code_editor", value = code)
  })

  # Copy code notification
  observeEvent(input$copy_success, {
    showNotification("Code copied to clipboard!", type = "message", duration = 2)
  })

  # Run code
  observeEvent(input$run_code, {
    # Get code from editor (user may have edited it)
    code <- input$code_editor
    req(code != "")

    # Update current_code with the edited version
    current_code(code)

    # Prepare environment with data
    env <- new.env()
    env$out <- data()
    env$out.label <- if (!is.null(data_label)) data_label() else NULL

    # Execute code
    exec_result <- tryCatch({
      parsed <- parse(text = code)
      res <- NULL
      for (expr in parsed) {
        res <- eval(expr, envir = env)
      }
      # Check for result variable
      if (exists("result", envir = env)) {
        res <- get("result", envir = env)
      }
      list(success = TRUE, value = res)
    }, error = function(e) {
      list(success = FALSE, message = e$message)
    })

    if (!exec_result$success) {
      result_type("error")
      execution_result(list(error = TRUE, message = exec_result$message))
      return()
    }

    result <- exec_result$value

    # Determine result type and store (using unified helper)
    determine_result_type(result, store_result = TRUE)
  })

  # Render plot
  output$result_plot <- renderPlot({
    plots <- execution_result()
    rtype <- result_type()
    req(rtype == "plot")

    n <- length(plots)
    if (n == 1) {
      print(plots[[1]])
    } else {
      gridExtra::grid.arrange(grobs = plots, ncol = min(n, 2))
    }
  }, height = 400)

  # Render table
  output$result_table <- renderDT({
    result <- execution_result()
    rtype <- result_type()
    req(rtype == "table")
    DT::datatable(as.data.frame(result), rownames = TRUE,
                  options = list(scrollX = TRUE, pageLength = 10))
  })

  # Render result output
  output$result_output <- renderUI({
    result <- execution_result()
    rtype <- result_type()

    if (is.null(result) || rtype == "none") {
      return(p("Run code to see results here.", style = "color: gray;"))
    }

    if (rtype == "error") {
      return(tags$div(
        style = "color: red;",
        tags$strong("Execution Error: "), result$message
      ))
    }

    if (rtype == "plot") {
      return(plotOutput(session$ns("result_plot"), height = "400px"))
    }

    if (rtype == "flextable") {
      return(htmltools::HTML(flextable::htmltools_value(result)))
    }

    if (rtype == "table") {
      return(DTOutput(session$ns("result_table")))
    }

    # text
    return(tags$pre(
      style = "max-height: 400px; overflow-y: auto; font-size: 11px;",
      paste(capture.output(print(result)), collapse = "\n")
    ))
  })

  # Download UI elements
  output$download_pptx_ui <- renderUI({
    rtype <- result_type()
    if (rtype == "plot") {
      downloadButton(session$ns("download_pptx"), "PPT", icon = icon("file-powerpoint"), class = "btn-info btn-sm")
    }
  })

  output$download_word_ui <- renderUI({
    rtype <- result_type()
    if (rtype == "table" || rtype == "flextable") {
      downloadButton(session$ns("download_word"), "Word", icon = icon("file-word"), class = "btn-info btn-sm")
    }
  })

  output$download_excel_ui <- renderUI({
    rtype <- result_type()
    if (rtype == "table") {
      downloadButton(session$ns("download_excel"), "Excel", icon = icon("file-excel"), class = "btn-info btn-sm")
    }
  })

  output$download_txt_ui <- renderUI({
    result <- execution_result()
    code <- current_code()
    # Show TXT download if there's any result or code
    if (!is.null(result) || (!is.null(code) && code != "")) {
      downloadButton(session$ns("download_txt"), "TXT", icon = icon("file-alt"), class = "btn-info btn-sm")
    }
  })

  # PPT size settings UI
  output$ppt_size_ui <- renderUI({
    rtype <- result_type()
    plots <- execution_result()
    if (rtype == "plot") {
      n <- length(plots)
      tagList(
        if (n > 1) p(tags$b(paste0(n, " plots → ", n, " slides")), style = "color: #2196F3;"),
        fluidRow(
          column(6, sliderInput(session$ns("ppt_width"), "PPT Width", min = 5, max = 20, value = 10, step = 0.5)),
          column(6, sliderInput(session$ns("ppt_height"), "PPT Height", min = 5, max = 15, value = 7.5, step = 0.5))
        )
      )
    }
  })

  # Download handlers
  output$download_pptx <- downloadHandler(
    filename = function() {
      paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx")
    },
    content = function(file) {
      withProgress(
        message = "Generating PowerPoint...",
        detail = "This may take a while...",
        value = 0,
        {
          result <- execution_result()
          rtype <- result_type()

          w <- if (!is.null(input$ppt_width)) input$ppt_width else 10
          h <- if (!is.null(input$ppt_height)) input$ppt_height else 7.5

          doc <- officer::read_pptx()
          incProgress(0.1, detail = "Creating slides...")

          tryCatch({
            # Handle different result types
            if (rtype == "plot") {
              # Single or multiple plots
              plots <- if (is.list(result) && !inherits(result, c("gg", "ggplot"))) {
                result
              } else {
                list(result)
              }

              for (i in seq_along(plots)) {
                doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
                doc <- officer::ph_with(doc, rvg::dml(code = print(plots[[i]])),
                                       location = officer::ph_location(width = w, height = h, left = 0, top = 0.5))
                incProgress(0.6 / length(plots), detail = paste("Adding plot", i))
              }

            } else if (rtype == "table") {
              # Table result
              df <- as.data.frame(result)
              ft <- flextable::flextable(df)
              ft <- flextable::autofit(ft)

              doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
              doc <- officer::ph_with(doc, ft, location = officer::ph_location_type(type = "body"))
              incProgress(0.7, detail = "Adding table")
            }

            incProgress(0.2, detail = "Saving file...")
            print(doc, target = file)

          }, error = function(e) {
            showNotification(paste("Error creating PPT:", e$message), type = "error", duration = 10)
          })
        }
      )
    }
  )

  output$download_word <- downloadHandler(
    filename = function() {
      paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      withProgress(
        message = "Generating Word document...",
        detail = "This may take a while...",
        value = 0,
        {
          tryCatch(
            {
              result <- execution_result()
              rtype <- result_type()

              if (is.null(result) || rtype == "none") {
                showNotification("No results to download", type = "warning")
                return()
              }

              doc <- officer::read_docx()
              incProgress(0.1, detail = "Creating document...")

              if (rtype == "plot") {
                # Word에 그래프 추가
                plots <- if (is.list(result) && !inherits(result, c("gg", "ggplot"))) {
                  result
                } else {
                  list(result)
                }

                for (i in seq_along(plots)) {
                  if (i > 1) {
                    doc <- officer::body_add_break(doc)
                  }
                  doc <- officer::body_add_par(doc, paste("Plot", i), style = "heading 2")
                  doc <- officer::body_add_gg(doc, plots[[i]], width = 6, height = 4)
                  incProgress(0.6 / length(plots), detail = paste("Adding plot", i))
                }
              } else if (rtype == "flextable") {
                # Word에 flextable 추가 (이미 포맷팅된 테이블)
                doc <- officer::body_add_par(doc, "Analysis Results", style = "heading 2")
                doc <- flextable::body_add_flextable(doc, result)
                incProgress(0.7, detail = "Adding table")
              } else if (rtype == "table") {
                # Word에 테이블 추가
                df <- as.data.frame(result)
                ft <- flextable::flextable(df)
                ft <- flextable::autofit(ft)

                doc <- officer::body_add_par(doc, "Analysis Results", style = "heading 2")
                doc <- flextable::body_add_flextable(doc, ft)
                incProgress(0.7, detail = "Adding table")
              }

              incProgress(0.2, detail = "Saving file...")
              print(doc, target = file)

              showNotification("Word document downloaded successfully", type = "message")
            },
            error = function(e) {
              showNotification(
                paste("Error generating Word document:", e$message),
                type = "error",
                duration = 10
              )
            }
          )
        }
      )
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      withProgress(
        message = "Generating Excel file...",
        detail = "This may take a while...",
        value = 0,
        {
          tryCatch(
            {
              result <- execution_result()
              rtype <- result_type()

              if (is.null(result) || rtype == "none") {
                showNotification("No results to download", type = "warning")
                return()
              }

              incProgress(0.1, detail = "Creating workbook...")

              if (rtype == "plot") {
                # Excel은 그래프를 직접 지원하지 않음
                showNotification(
                  "Excel format does not support plots. Please use PowerPoint or Word for plot downloads.",
                  type = "warning",
                  duration = 10
                )
                return()
              } else if (rtype == "table") {
                # Excel에 테이블 추가
                df <- as.data.frame(result)

                # 워크북 생성
                wb <- openxlsx::createWorkbook()

                # 결과 시트 추가
                openxlsx::addWorksheet(wb, "Results")
                openxlsx::writeData(wb, "Results", df, rowNames = FALSE)
                incProgress(0.7, detail = "Adding table")

                incProgress(0.2, detail = "Saving file...")
                openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

                showNotification("Excel file downloaded successfully", type = "message")
              }
            },
            error = function(e) {
              showNotification(
                paste("Error generating Excel file:", e$message),
                type = "error",
                duration = 10
              )
            }
          )
        }
      )
    }
  )

  output$download_txt <- downloadHandler(
    filename = function() {
      paste0("result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      tryCatch(
        {
          result <- execution_result()
          code <- current_code()
          rtype <- result_type()

          # Create text content
          content_lines <- c()

          # Add code section
          if (!is.null(code) && code != "") {
            content_lines <- c(
              content_lines,
              paste(rep("=", 70), collapse = ""),
              "R CODE",
              paste(rep("=", 70), collapse = ""),
              "",
              code,
              ""
            )
          }

          # Add result section
          if (!is.null(result)) {
            content_lines <- c(
              content_lines,
              paste(rep("=", 70), collapse = ""),
              "RESULT",
              paste(rep("=", 70), collapse = ""),
              ""
            )

            if (rtype == "table") {
              # Convert table to text
              result_text <- capture.output(print(as.data.frame(result)))
              content_lines <- c(content_lines, result_text)
            } else if (rtype == "plot") {
              content_lines <- c(content_lines, "[Plot result - cannot be displayed in text format]")
            } else {
              # Other results (text, list, etc.)
              result_text <- capture.output(print(result))
              content_lines <- c(content_lines, result_text)
            }
          }

          # Write to file
          writeLines(content_lines, file)

          showNotification("TXT file downloaded successfully", type = "message")
        },
        error = function(e) {
          showNotification(
            paste("Error generating TXT file:", e$message),
            type = "error",
            duration = 10
          )
        }
      )
    }
  )

  # Clear chat
  observeEvent(input$clear_chat, {
    chat_history(list())
    display_history(list())
    current_code("")
    execution_result(NULL)
    result_type("none")
    token_usage(list(
      input_tokens = 0,
      output_tokens = 0,
      total_tokens = 0,
      total_cost = 0
    ))
  })

  # Return reactive values (optional)
  return(reactive({
    list(
      code = current_code(),
      result = execution_result(),
      type = result_type()
    )
  }))
}
