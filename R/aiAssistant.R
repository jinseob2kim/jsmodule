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
#' @importFrom shinyWidgets pickerInput actionBttn noUiSliderInput

aiAssistantUI <- function(id, show_api_config = TRUE) {
  ns <- NS(id)

  tagList(
    # Enable shinyjs
    shinyjs::useShinyjs(),

    # CSS for AI Assistant
    tags$style(HTML(paste0("
      /* Card component */
      .ai-card {
        background: #F9FAED;
        border: 1px solid #DEDCA6;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
        margin-bottom: 1.5rem;
      }

      .ai-card-header {
        background: linear-gradient(135deg, #1E3C1E 0%, #4A774A 100%);
        color: white;
        padding: 1rem 1.25rem;
        border-radius: 8px 8px 0 0;
        border-bottom: 1px solid rgba(255, 255, 255, 0.1);
      }

      .ai-card-header h5,
      .ai-card-header h4 {
        margin: 0;
        color: white;
        font-weight: 600;
      }

      .ai-card-body {
        padding: 1.5rem;
      }

      /* Chat message bubbles */
      .user-message {
        background: #007bff;
        color: white;
        padding: 0.5rem 0.75rem;
        margin: 0.25rem 0;
        border-radius: 1rem 1rem 0.25rem 1rem;
        max-width: 80%;
        margin-left: auto;
        word-wrap: break-word;
        word-break: break-word;
        overflow-wrap: break-word;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
      }

      .user-message > div {
        word-wrap: break-word;
        word-break: break-word;
        overflow-wrap: break-word;
      }

      .user-message .mt-2 {
        margin-top: 0.25rem !important;
        word-wrap: break-word;
        word-break: break-word;
        overflow-wrap: break-word;
        white-space: pre-wrap;
      }

      .ai-message {
        background: #f1f3f4;
        color: #1A1A1A;
        padding: 0.5rem 0.75rem;
        margin: 0.25rem 0;
        border-radius: 1rem 1rem 1rem 0.25rem;
        max-width: 80%;
        word-wrap: break-word;
        word-break: break-word;
        overflow-wrap: break-word;
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
      }

      .ai-message .mt-2 {
        margin-top: 0.25rem !important;
      }

      .ai-message .mb-0 {
        margin-bottom: 0 !important;
      }

      .error-message {
        background: linear-gradient(135deg, #d9534f 0%, #c9302c 100%);
        color: white;
        padding: 0.5rem 0.75rem;
        margin: 0.25rem 0;
        border-radius: 0.75rem;
        border-left: 4px solid #a94442;
      }

      .error-message .mt-2 {
        margin-top: 0.25rem !important;
      }

      /* Chat container with custom scrollbar */
      .chat-container {
        max-height: 450px;
        overflow-y: auto;
        padding: 1.5rem;
        background: #FCFCFC;
        border-radius: 6px;
        border: 1px solid #ddd;
      }

      .chat-container::-webkit-scrollbar {
        width: 8px;
      }

      .chat-container::-webkit-scrollbar-track {
        background: #E3F0E3;
        border-radius: 10px;
      }

      .chat-container::-webkit-scrollbar-thumb {
        background: #4A774A;
        border-radius: 10px;
      }

      /* Code editor container */
      .code-editor-container {
        border-radius: 6px;
        overflow: hidden;
        border: 1px solid #ddd;
      }

      /* Form inputs focus state - jsmodule green */
      #", ns("provider"), ":focus,
      #", ns("api_key_input"), ":focus,
      #", ns("user_input"), ":focus {
        border-color: #4A774A;
        outline: 0;
        box-shadow: 0 0 0 0.2rem rgba(74, 119, 74, 0.15);
      }

      /* Compact input area - remove extra margins */
      .shiny-input-container {
        margin-bottom: 0 !important;
      }

      .form-group {
        margin-bottom: 0 !important;
      }
    "))),

    # API Configuration Section
    if (show_api_config) {
      div(
        class = "ai-card",
        div(
          class = "ai-card-header",
          tags$h5(icon("key"), " API Configuration")
        ),
        div(
          class = "ai-card-body",
        fluidRow(
          column(6,
            tags$div(
              tags$label(
                "AI Provider ",
                tags$a(
                  icon("info-circle"),
                  id = ns("provider_info"),
                  href = "#",
                  style = "color: #4A774A; cursor: pointer; text-decoration: none;",
                  onclick = "return false;",
                  `data-toggle` = "tooltip",
                  `data-placement` = "top",
                  `data-html` = "true",
                  title = ""
                )
              ),
              shinyWidgets::pickerInput(
                ns("provider"),
                NULL,
                choices = c(
                  "Anthropic (Claude)" = "anthropic",
                  "OpenAI (GPT)" = "openai",
                  "Google (Gemini)" = "google"
                ),
                selected = "anthropic",
                options = list(
                  style = "btn-default"
                )
              ),
              tags$script(HTML(sprintf("
                (function() {
                  var providerId = '%s';
                  var infoId = '%s';

                  var providerMeta = {
                    anthropic: {
                      tooltip: '<strong>Anthropic Claude</strong><br/>Base URL: https://api.anthropic.com/v1/messages<br/>Models: Claude 3.5 Sonnet, Claude 3 Opus, etc.<br/><em>Click to get API key</em>',
                      url: 'https://console.anthropic.com/settings/keys'
                    },
                    openai: {
                      tooltip: '<strong>OpenAI</strong><br/>Base URL: https://api.openai.com/v1/chat/completions<br/>Models: GPT-5, GPT-4o, etc.<br/><em>Click to get API key</em>',
                      url: 'https://platform.openai.com/api-keys'
                    },
                    google: {
                      tooltip: '<strong>Google Gemini</strong><br/>Base URL: https://generativelanguage.googleapis.com/v1beta<br/>Models: Gemini 3 Pro, Gemini 2.5 Pro, Gemini 2.5 Flash, etc.<br/><em>Click to get API key</em>',
                      url: 'https://aistudio.google.com/app/apikey'
                    }
                  };

                  function getProvider() {
                    var el = document.getElementById(providerId);
                    return el && el.value ? el.value : 'anthropic';
                  }

                  function ensureShinyValue(provider) {
                    if (!window.Shiny || typeof Shiny.setInputValue !== 'function') {
                      return;
                    }
                    var current;
                    if (typeof Shiny.getInputValue === 'function') {
                      current = Shiny.getInputValue(providerId);
                    } else if (Shiny.shinyapp && Shiny.shinyapp.$inputValues) {
                      current = Shiny.shinyapp.$inputValues[providerId];
                    }
                    if (typeof current === 'undefined') {
                      Shiny.setInputValue(providerId, provider, { priority: 'event' });
                    }
                  }

                  function refreshTooltip(provider) {
                    var meta = providerMeta[provider] || providerMeta.anthropic;
                    var $info = $('#' + infoId);
                    if (!$info.length || !meta.tooltip) {
                      return;
                    }
                    $info.attr('data-original-title', meta.tooltip);
                    if (typeof $.fn.tooltip === 'function') {
                      if ($info.data('bs.tooltip')) {
                        $info.tooltip('destroy');
                      }
                      $info.tooltip({ html: true, placement: 'top' });
                    }
                  }

                  function onProviderChange() {
                    var provider = getProvider();
                    refreshTooltip(provider);
                    ensureShinyValue(provider);
                  }

                  function bindProviderSelect() {
                    var $picker = $('#' + providerId);
                    if (!$picker.length) {
                      return;
                    }
                    $picker.off('.aiProvider')
                      .on('changed.bs.select.aiProvider', onProviderChange)
                      .on('change.aiProvider', onProviderChange);
                    onProviderChange();
                  }

                  function bindInfoButton() {
                    $('#' + infoId).off('.aiProvider').on('click.aiProvider', function(e) {
                      e.preventDefault();
                      e.stopPropagation();
                      var provider = getProvider();
                      var meta = providerMeta[provider];
                      if (meta && meta.url) {
                        window.open(meta.url, '_blank');
                      }
                    });
                  }

                  $(document).on('shiny:connected', function() {
                    bindProviderSelect();
                    bindInfoButton();
                  });
                  $(document).on('shiny:recalculated', bindProviderSelect);
                  $(function() {
                    bindProviderSelect();
                    bindInfoButton();
                  });
                })();
              ", ns("provider"), ns("provider_info"))))
            ),
            uiOutput(ns("model_selector"))
          ),
          column(6,
            tags$label("API Key"),
            div(
              style = "display: flex; gap: 10px; align-items: flex-end;",
              div(
                style = "flex: 1;",
                passwordInput(
                  ns("api_key_input"),
                  NULL,
                  placeholder = "Enter your API key or leave empty for env var"
                )
              ),
              shinyWidgets::actionBttn(
                ns("check_api_key"),
                "Check",
                icon = icon("key"),
                style = "material-flat",
                color = "primary",
                size = "sm"
              )
            ),
            tags$small(
              class = "text-muted d-block",
              "Env vars: ",
              tags$code("ANTHROPIC_API_KEY"),
              ", ",
              tags$code("OPENAI_API_KEY"),
              ", ",
              tags$code("GOOGLE_API_KEY")
            )
          )
        ),
        uiOutput(ns("api_status")),
        # System Prompt Section
        tags$hr(),
        tags$div(
          style = "margin-top: 15px;",
          checkboxInput(
            ns("use_custom_prompt"),
            "Use Custom System Prompt",
            value = FALSE
          ),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("use_custom_prompt")),
            textAreaInput(
              ns("custom_prompt"),
              "Custom System Prompt",
              placeholder = "Enter custom instructions for the AI...",
              rows = 6,
              width = "100%"
            )
          )
        ),
        # Max Tokens Section
        tags$hr(),
        tags$div(
          style = "margin-top: 15px;",
          numericInput(
            ns("max_tokens"),
            "Max Response Tokens",
            value = 8192,
            min = 1024,
            max = 16384,
            step = 512,
            width = "100%"
          ),
          tags$small(
            class = "text-muted",
            "Controls maximum length of AI responses (1,024 - 16,384, step: 512)"
          )
        )
        )
      )
    } else {
      uiOutput(ns("env_config_status"))
    },

    # Main Interface
    fluidRow(
      # Chat Panel
      column(6,
        div(
          class = "ai-card",
          style = "min-height: 750px;",
          div(
            class = "ai-card-header",
            tags$h5(icon("comments"), " AI Chat Assistant")
          ),
          div(
            class = "ai-card-body",
          tags$p(
            class = "text-muted mb-3",
            "Ask for statistical analysis. ",
            tags$em("Example: 'Create survival curve by rx'")
          ),
          # Token usage display
          uiOutput(ns("token_display")),
          # Chat history
          div(
            id = ns("chat_container"),
            class = "chat-container my-3",
            uiOutput(ns("chat_history"))
          ),
          # Input area - compact layout
          div(
            style = "display: flex; gap: 0.5rem; align-items: flex-start;",
            div(
              style = "flex: 1;",
              textAreaInput(
                ns("user_input"),
                NULL,
                placeholder = "Type your message... (Press Send button or Ctrl+Enter to send)",
                width = "100%",
                rows = 3
              )
            ),
            div(
              style = "display: flex; flex-direction: column; gap: 0.5rem; min-width: 100px;",
              shinyWidgets::actionBttn(
                ns("send_btn"),
                "Send",
                icon = icon("paper-plane"),
                style = "material-flat",
                color = "primary",
                size = "sm",
                block = TRUE
              ),
              shinyWidgets::actionBttn(
                ns("clear_chat"),
                "Clear",
                icon = icon("trash"),
                style = "material-flat",
                color = "warning",
                size = "sm",
                block = TRUE
              )
            )
          ),
          tags$script(HTML(sprintf("
            $(document).ready(function() {
              // Ctrl+Enter to send (optional shortcut)
              $('#%s').on('keydown', function(e) {
                if (e.keyCode === 13 && e.ctrlKey) {
                  e.preventDefault();
                  $('#%s').click();
                }
              });
            });
          ", ns("user_input"), ns("send_btn"))))
          )
        )
      ),

      # Code & Results Panel
      column(6,
        # Code Editor Card
        div(
          class = "ai-card",
          style = "margin-bottom: 1.5rem;",
          div(
            class = "ai-card-header",
            tags$h5(icon("code"), " Generated Code")
          ),
          div(
            class = "ai-card-body",
            div(
              class = "code-editor-container",
              shinyAce::aceEditor(
                outputId = ns("code_editor"),
                value = "",
                mode = "r",
                theme = "monokai",
                height = "350px",
                fontSize = 13,
                showLineNumbers = TRUE,
                highlightActiveLine = TRUE,
                readOnly = FALSE,
                showPrintMargin = FALSE,
                placeholder = "No code generated yet. You can edit the code here."
              )
            ),
            div(
              style = "display: flex; gap: 10px; margin-top: 1rem;",
              shinyWidgets::actionBttn(
                ns("run_code"),
                "Run Code",
                icon = icon("play"),
                style = "material-flat",
                color = "success",
                size = "sm"
              ),
              shinyWidgets::actionBttn(
                ns("copy_code"),
                "Copy Code",
                icon = icon("copy"),
                style = "material-flat",
                color = "default",
                size = "sm"
              ),
              shinyWidgets::actionBttn(
                ns("save_chat"),
                "Save Chat",
                icon = icon("save"),
                style = "material-flat",
                color = "primary",
                size = "sm"
              )
            )
          )
        ),

        # Results Card
        div(
          class = "ai-card",
          div(
            class = "ai-card-header",
            tags$h5(icon("chart-line"), " Analysis Results")
          ),
          div(
            class = "ai-card-body",
          uiOutput(ns("result_output")),
          tags$hr(),
          tags$h6("Download Options", class = "mt-3 mb-3"),
          div(
            style = "display: flex; gap: 0.5rem; flex-wrap: wrap;",
            uiOutput(ns("download_pptx_ui")),
            uiOutput(ns("download_word_ui")),
            uiOutput(ns("download_excel_ui")),
            uiOutput(ns("download_txt_ui"))
          ),
          uiOutput(ns("ppt_size_ui"))
          )
        )
      )
    ),

    # Copy code script
    tags$script(HTML(sprintf("
      $(document).on('click', '#%s', function() {
        var editor = ace.edit('%s');
        var code = editor.getValue();
        navigator.clipboard.writeText(code).then(function() {
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

      // Download chat history
      Shiny.addCustomMessageHandler('downloadChat', function(message) {
        var blob = new Blob([message.content], {type: 'application/json'});
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = message.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      });

      // Real-time validation for max_tokens input
      $(document).ready(function() {
        $('#%s').on('input change', function() {
          var value = parseInt($(this).val());
          var inputGroup = $(this).closest('.form-group');

          if (isNaN(value) || value < 1024 || value > 16384) {
            $(this).css('border-color', '#dc3545');
            $(this).css('box-shadow', '0 0 0 0.2rem rgba(220,53,69,.25)');

            // Show or update error message
            if (inputGroup.find('.token-error').length === 0) {
              inputGroup.append('<small class=\"token-error text-danger\">Value must be between 1,024 and 16,384</small>');
            }
          } else {
            $(this).css('border-color', '');
            $(this).css('box-shadow', '');
            inputGroup.find('.token-error').remove();
          }
        });
      });
    ", ns("copy_code"), ns("code_editor"), ns("copy_success"), ns("max_tokens"))))
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

  allowed_packages <- c(
    "jstable", "jskm", "jsmodule", "survival",
    "ggplot2", "ggpubr", "pROC", "data.table",
    "DT", "gridExtra", "GGally", "forestploter",
    "MatchIt", "timeROC"
  )

  # Reactive values for model list and settings
  available_models <- reactiveVal(list())
  selected_model <- reactiveVal(NULL)

  # Get max tokens from input or default
  get_max_tokens <- reactive({
    if (!is.null(input$max_tokens)) {
      return(as.integer(input$max_tokens))
    }
    return(8192L)  # Default value
  })

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

  # Reset models when provider changes
  observeEvent(input$provider, {
    req(input$provider)
    message("[DEBUG] Provider changed to: ", input$provider)

    # Clear models and selection when provider changes
    available_models(NULL)
    selected_model(NULL)

    # Show notification
    provider_name <- switch(input$provider,
      "anthropic" = "Anthropic Claude",
      "openai" = "OpenAI GPT",
      "google" = "Google Gemini",
      "Unknown"
    )

    showNotification(
      paste0("Switched to ", provider_name, ". Please check your API key to load models."),
      type = "message",
      duration = 3
    )
  }, ignoreInit = TRUE)

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
        "openai" = models[grepl("gpt-5|gpt-4", models)][1],
        "google" = models[grepl("gemini-3|gemini-2", models)][1],
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
              "openai" = models[grepl("gpt-5|gpt-4", models)][1],
              "google" = models[grepl("gemini-3|gemini-2", models)][1],
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

    # Get current selection or use first model as default
    current_selection <- isolate(selected_model())
    if (is.null(current_selection) && length(models) > 0) {
      current_selection <- models[1]
      selected_model(current_selection)
    }

    tagList(
      shinyWidgets::pickerInput(
        session$ns("selected_model"),
        "Model",
        choices = models,
        selected = current_selection,
        options = list(
          `live-search` = TRUE,
          style = "btn-default"
        )
      ),
      tags$hr(style = "margin-top: 20px; margin-bottom: 15px;"),
      shinyWidgets::actionBttn(
        session$ns("apply_config"),
        "Apply Configuration",
        icon = icon("save"),
        style = "material-flat",
        color = "success",
        size = "sm",
        block = TRUE
      )
    )
  })

  # Update selected model when user changes selection
  observeEvent(input$selected_model, {
    req(input$selected_model)
    selected_model(input$selected_model)
    message("[DEBUG] Model selection changed to: ", input$selected_model)
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

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

  # Reactive system prompt (user can customize)
  system_prompt_text <- reactive({
    use_custom <- input$use_custom_prompt
    if (isTRUE(use_custom)) {
      custom <- input$custom_prompt
      if (!is.null(custom) && nchar(trimws(custom)) > 0) {
        custom
      } else {
        stats_guide_text
      }
    } else {
      stats_guide_text
    }
  })

  # Reactive values
  chat_history <- reactiveVal(list())  # Recent 5 conversations for API (10 messages)
  full_chat_history <- reactiveVal(list())  # Full conversation history for saving
  display_history <- reactiveVal(list())
  current_code <- reactiveVal("")
  execution_result <- reactiveVal(NULL)
  result_type <- reactiveVal("none")  # "plot", "table", "text", "error", "none", "loading"

  # Token tracking
  token_usage <- reactiveVal(list(
    input_tokens = 0,
    output_tokens = 0,
    total_tokens = 0,
    total_cost = 0
  ))

  # Helper: Determine result type and store result
  # Unified logic for classifying and storing execution results
  determine_result_type <- function(res, store_result = TRUE) {
    result_info <- list()

    # Check for NULL or empty result
    if (is.null(res)) {
      result_info$type <- "unknown"
      result_info$value <- NULL
      result_info$message <- "No result was generated. The code executed but did not return a value."

      if (store_result) {
        execution_result(NULL)
        result_type("unknown")
      }
      return(result_info)
    }

    # Single plot (comprehensive plot class support)
    # - ggplot/gg: ggplot2, ggpubr, jskm, pROC::ggroc
    # - ggmatrix: GGally::ggpairs
    # - gtable/gTree/grob: grid graphics, gridExtra::grid.arrange/arrangeGrob, forestploter::forest
    # - recordedplot: base R plot()
    if (inherits(res, c("ggplot", "gg", "gtable", "gTree", "grob", "recordedplot", "ggmatrix", "forestplot"))) {
      result_info$type <- "plot"
      result_info$value <- list(res)
      result_info$message <- "Plot generated successfully. The plot is now displayed in the Results panel."

    # Multiple plots
    } else if (is.list(res) && length(res) > 0 &&
               all(sapply(res, function(x) inherits(x, c("ggplot", "gg", "gtable", "gTree", "grob", "recordedplot", "ggmatrix", "forestplot"))))) {
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

    # Unrecognized type - mark as unknown
    } else if (is.list(res) && length(res) == 0) {
      result_info$type <- "unknown"
      result_info$value <- NULL
      result_info$message <- paste0(
        "Empty list result. Object class: ", paste(class(res), collapse = ", "),
        "\nThe code executed but produced an empty result."
      )

    # Everything else - try to display as text but mark potential issues
    } else {
      # Check if it's a recognizable object that we should warn about
      obj_class <- paste(class(res), collapse = ", ")

      # Common plot types that might not be captured
      if (any(grepl("plot|graph|chart|figure", obj_class, ignore.case = TRUE))) {
        result_info$type <- "unknown"
        result_info$value <- NULL
        result_info$message <- paste0(
          "Result type not fully supported: ", obj_class,
          "\nThis appears to be a plot but was not recognized. ",
          "The plot may have been displayed but cannot be saved or exported."
        )
      } else {
        # Try to display as text
        result_info$type <- "text"
        result_info$value <- res
        result_info$message <- paste(capture.output(print(res)), collapse = "\n")
      }
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
      "openai" = "gpt-5-turbo",
      "google" = "gemini-3-flash",
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
      "options",  # Global settings
      "install\\.packages", "remove\\.packages",  # Package management
      "eval\\(", "evalq\\(",  # Nested eval
      "assign\\(.+envir\\s*=\\s*\\.GlobalEnv",  # Global env assignment
      "<<-"  # Super assignment
    )

    for (pattern in dangerous_patterns) {
      if (grepl(pattern, code, perl = TRUE)) {
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

  create_execution_env <- function() {
    env <- new.env(parent = .GlobalEnv)
    env$out <- data()
    env$out.label <- if (!is.null(data_label)) data_label() else NULL

    safe_library <- function(package, ..., character.only = FALSE) {
      pkg_name <- if (character.only) {
        if (!is.character(package) || length(package) != 1) {
          stop("When character.only = TRUE, 'package' must be a single character string.")
        }
        package
      } else {
        deparse(substitute(package))
      }

      pkg_name <- trimws(pkg_name[1])
      if (!nzchar(pkg_name)) {
        stop("Package name cannot be empty.")
      }

      if (!pkg_name %in% allowed_packages) {
        stop(sprintf(
          "Package '%s' is not allowed. Allowed packages: %s",
          pkg_name,
          paste(allowed_packages, collapse = ", ")
        ))
      }

      base::library(pkg_name, character.only = TRUE, ...)
      invisible(TRUE)
    }

    env$library <- safe_library
    env$require <- function(package, ..., character.only = FALSE) {
      safe_library(package, ..., character.only = character.only)
      TRUE
    }

    env
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

    # Add allowed packages information
    context <- paste0(context,
      "\n## Allowed R Packages\n",
      "Only these packages can be loaded with library():\n",
      "- ", paste(allowed_packages, collapse = ", "), "\n"
    )

    return(context)
  })

  # Helper function for null coalescing
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # AI API call function
  call_ai <- function(user_message, conversation_history = list(), context_info = NULL) {
    # [DEBUG] Log function entry
    cat(sprintf("[DEBUG-API] call_ai() entered: message length=%d, first 100 chars='%s'\n",
                nchar(user_message),
                substr(user_message, 1, 100)),
        file = stderr())

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
      system_prompt_text(), "\n\n",
      "## Current Project Context\n",
      data_context(),
      context_section
    )

    messages <- c(
      conversation_history,
      list(list(role = "user", content = user_message))
    )

    # [DEBUG] Log messages array before API call
    last_user_msg <- messages[[length(messages)]]$content
    cat(sprintf("[DEBUG-API] Messages array created: last user message length=%d, first 100 chars='%s'\n",
                nchar(last_user_msg),
                substr(last_user_msg, 1, 100)),
        file = stderr())

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
            max_tokens = get_max_tokens(),
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

      # [DEBUG] Log API response
      cat(sprintf("[DEBUG-API] API response received: assistant message length=%d\n",
                  nchar(assistant_message)),
          file = stderr())

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
            max_tokens = get_max_tokens(),
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

      # [DEBUG] Log API response
      cat(sprintf("[DEBUG-API] OpenAI response received: assistant message length=%d\n",
                  nchar(assistant_message)),
          file = stderr())

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
              maxOutputTokens = get_max_tokens()
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

          # [DEBUG] Log API response
          cat(sprintf("[DEBUG-API] Google response received: assistant message length=%d\n",
                      nchar(assistant_message)),
              file = stderr())

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
        message = "Failed to parse Google API response. Please retry. If the issue persists, try a different model or simplify your question."
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

    # Validate max_tokens
    max_tokens_value <- get_max_tokens()
    if (max_tokens_value < 1024 || max_tokens_value > 16384) {
      showNotification(
        sprintf("Max Response Tokens must be between 1,024 and 16,384 (current: %s)",
                format(max_tokens_value, big.mark = ",")),
        type = "error",
        duration = 5
      )
      return()
    }

    # [DEBUG] Log original input
    cat(sprintf("[DEBUG] Step 1 - Input received: length=%d, first 100 chars='%s'\n",
                nchar(user_msg),
                substr(user_msg, 1, 100)),
        file = stderr())

    # Immediately clear and disable input field
    updateTextAreaInput(session, "user_input", value = "")
    shinyjs::disable("user_input")
    shinyjs::disable("send_btn")

    # Disable code editor
    shinyjs::runjs(sprintf("
      var editor = ace.edit('%s');
      editor.setReadOnly(true);
      editor.container.style.opacity = '0.6';
      editor.container.style.pointerEvents = 'none';
    ", session$ns("code_editor")))

    # Update display history immediately (show user message right away)
    new_display <- c(display_history(),
                     list(list(role = "user", content = user_msg)))
    display_history(new_display)

    # [DEBUG] Log display history update
    cat(sprintf("[DEBUG] Step 2 - Display history updated: message length=%d\n",
                nchar(new_display[[length(new_display)]]$content)),
        file = stderr())

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

        # [DEBUG] Log before API call
        cat(sprintf("[DEBUG] Step 3 - Before API call: message length=%d, first 100 chars='%s'\n",
                    nchar(user_msg),
                    substr(user_msg, 1, 100)),
            file = stderr())
        showNotification("Calling AI API...", type = "message", duration = 2)

        result <- call_ai(user_msg, chat_history(), context_info = context_info)

        # [DEBUG] Log after API call
        cat(sprintf("[DEBUG] Step 4 - After API call: status=%s\n",
                    if (result$success) "SUCCESS" else "FAILED"),
            file = stderr())
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
      # [DEBUG] Log before updating chat history
      cat(sprintf("[DEBUG] Step 5 - Before chat_history update: user_msg length=%d\n",
                  nchar(user_msg)),
          file = stderr())

      # Update full chat history (no limit)
      full_history <- c(full_chat_history(),
                        list(list(role = "user", content = user_msg)),
                        list(list(role = "assistant", content = response$message)))
      full_chat_history(full_history)

      # Update API history (keep only last 5 conversations = 10 messages)
      new_history <- c(chat_history(),
                       list(list(role = "user", content = user_msg)),
                       list(list(role = "assistant", content = response$message)))

      # Keep only last 10 messages (5 user-assistant pairs)
      if (length(new_history) > 10) {
        new_history <- tail(new_history, 10)
      }

      chat_history(new_history)

      # [DEBUG] Log after chat_history update
      user_msg_in_history <- new_history[[length(new_history) - 1]]$content
      cat(sprintf("[DEBUG] Step 6 - After chat_history update: stored user_msg length=%d\n",
                  nchar(user_msg_in_history)),
          file = stderr())

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

    # Re-enable input field and code editor after response
    shinyjs::enable("user_input")
    shinyjs::enable("send_btn")

    # Re-enable code editor
    shinyjs::runjs(sprintf("
      var editor = ace.edit('%s');
      editor.setReadOnly(false);
      editor.container.style.opacity = '1';
      editor.container.style.pointerEvents = 'auto';
    ", session$ns("code_editor")))
  })

  # Display token usage
  output$token_display <- renderUI({
    usage <- token_usage()

    if (usage$total_tokens == 0) {
      return(NULL)
    }

    tags$div(
      class = "token-badge mb-2",
      tags$div(
        icon("chart-bar"), " ",
        tags$strong("Tokens:"),
        sprintf(" %s", format(usage$total_tokens, big.mark = ","))
      ),
      tags$div(
        class = "mt-1",
        icon("dollar-sign"), " ",
        tags$strong("Cost:"),
        sprintf(" $%.4f", usage$total_cost)
      )
    )
  })

  # Display chat history
  output$chat_history <- renderUI({
    history <- display_history()
    if (length(history) == 0) {
      return(tags$div(
        class = "text-center text-muted py-4",
        icon("comments", class = "fa-3x mb-3"),
        tags$p("Start a conversation with your AI assistant...")
      ))
    }

    # [DEBUG] Log rendering
    cat(sprintf("[DEBUG] Step 7 - Rendering chat history: %d messages\n", length(history)),
        file = stderr())
    for (i in seq_along(history)) {
      if (history[[i]]$role == "user") {
        cat(sprintf("[DEBUG] Step 7.%d - User message: length=%d, first 100 chars='%s'\n",
                    i, nchar(history[[i]]$content), substr(history[[i]]$content, 1, 100)),
            file = stderr())
      }
    }

    tagList(
      lapply(history, function(msg) {
        if (msg$role == "user") {
          tags$div(
            class = "user-message",
            tags$div(
              tags$strong(icon("user"), " You"),
              tags$div(
                class = "mt-2",
                style = "white-space: pre-wrap; word-wrap: break-word; word-break: break-word; overflow-wrap: break-word;",
                msg$content
              )
            )
          )
        } else if (msg$role == "assistant") {
          tags$div(
            class = "ai-message",
            tags$div(
              tags$strong(icon("robot"), " AI Assistant"),
              tags$pre(
                class = "mt-2 mb-0",
                style = "white-space: pre-wrap; background: transparent; border: none; color: inherit;",
                msg$content
              )
            )
          )
        } else {
          tags$div(
            class = "error-message",
            tags$strong(icon("exclamation-triangle"), " Error"),
            tags$div(
              class = "mt-2",
              style = "white-space: pre-wrap;",
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
    showNotification("Code copied to clipboard", type = "message", duration = 3)
  })

  # Ask AI to fix error
  observeEvent(input$fix_error, {
    # Get current code and error
    code <- current_code()
    result <- execution_result()

    if (!is.null(result) && !is.null(result$message)) {
      # Build detailed error report
      error_details <- paste(
        "Error message:",
        result$message,
        sep = "\n"
      )

      if (!is.null(result$call) && result$call != "Unknown") {
        error_details <- paste(
          error_details,
          sprintf("\nFailed call: %s", result$call),
          sep = ""
        )
      }

      if (!is.null(result$class)) {
        error_details <- paste(
          error_details,
          sprintf("\nError type: %s", result$class),
          sep = ""
        )
      }

      if (!is.null(result$traceback) && length(result$traceback) > 0) {
        traceback_text <- paste(result$traceback, collapse = "\n")
        error_details <- paste(
          error_details,
          sprintf("\n\nTraceback:\n%s", traceback_text),
          sep = ""
        )
      }

      # Create full error report message
      error_msg <- sprintf(
"I ran this code and got an error:

```r
%s
```

%s

Please help me fix this error.",
        code,
        error_details
      )

      # Insert into chat input
      updateTextAreaInput(session, "user_input", value = error_msg)

      # Scroll to chat input
      shinyjs::runjs(sprintf("document.getElementById('%s').scrollIntoView({behavior: 'smooth', block: 'center'});", session$ns("user_input")))
    }
  })

  # Handle "Ask AI to Fix" for no result (unknown type)
  observeEvent(input$fix_no_result, {
    code <- current_code()

    # Build message for AI
    no_result_msg <- sprintf(
"I ran this code but it didn't produce any result:

```r
%s
```

The code executed without errors, but no result was displayed in the Results panel. This might be because:
- The result was not properly assigned to the 'result' variable
- An unsupported object type was returned
- The code only performed side effects without returning a value
- A function returned NULL or an empty result

Please fix the code to ensure it returns a proper result that can be displayed and exported.",
      code
    )

    # Insert into chat input
    updateTextAreaInput(session, "user_input", value = no_result_msg)

    # Scroll to chat input
    shinyjs::runjs(sprintf("document.getElementById('%s').scrollIntoView({behavior: 'smooth', block: 'center'});", session$ns("user_input")))
  })

  # Save chat history
  observeEvent(input$save_chat, {
    history <- full_chat_history()  # Use full history, not limited chat_history
    if (length(history) == 0) {
      cat(stderr(), "[DEBUG] No chat history to save\n")
      showNotification("No chat history to save", type = "warning")
      return(NULL)
    }

    # Convert to structured format
    chat_data <- list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      provider = get_provider(),
      model = selected_model(),
      messages = history
    )

    # Convert to JSON string
    json_content <- jsonlite::toJSON(chat_data, pretty = TRUE, auto_unbox = TRUE)

    # Send to JavaScript for download
    session$sendCustomMessage(
      type = "downloadChat",
      message = list(
        content = as.character(json_content),
        filename = paste0("chat_history_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
      )
    )

    cat(stderr(), sprintf("[DEBUG] Chat history saved: %d messages\n", length(history)))
  })

  # Run code
  observeEvent(input$run_code, {
    # Get code from editor (user may have edited it)
    code <- input$code_editor
    req(code != "")

    safety_check <- validate_code_safety(code)
    if (!safety_check$safe) {
      result_type("error")
      execution_result(list(
        error = TRUE,
        message = safety_check$reason
      ))
      showNotification(safety_check$reason, type = "error", duration = 5)
      return()
    }

    # Update current_code with the edited version
    current_code(code)

    # Clear previous results and show loading state
    execution_result(NULL)
    result_type("loading")

    # Prepare environment with data and restricted helpers
    env <- create_execution_env()

    # Execute code
    exec_result <- tryCatch({
      parsed <- parse(text = code)
      res <- NULL
      line_num <- 1
      for (expr in parsed) {
        res <- eval(expr, envir = env)
        line_num <- line_num + length(attr(expr, "srcref")[[1]])
      }
      # Check for result variable
      if (exists("result", envir = env)) {
        res <- get("result", envir = env)
      }
      list(success = TRUE, value = res)
    }, error = function(e) {
      # Get detailed error information
      error_call <- if (!is.null(e$call)) deparse(e$call) else "Unknown"
      error_class <- paste(class(e), collapse = ", ")

      # Try to get line number from traceback
      trace_lines <- capture.output(traceback())

      # Build detailed error info
      list(
        success = FALSE,
        message = e$message,
        call = error_call,
        class = error_class,
        traceback = trace_lines
      )
    })

    if (!exec_result$success) {
      result_type("error")
      execution_result(list(
        error = TRUE,
        message = exec_result$message,
        call = exec_result$call,
        class = exec_result$class,
        traceback = exec_result$traceback
      ))
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
      plot_obj <- plots[[1]]
      # Use grid.draw for gtable/gTree objects, print for others
      if (inherits(plot_obj, c("gtable", "gTree")) && !inherits(plot_obj, "gg")) {
        grid::grid.draw(plot_obj)
      } else {
        print(plot_obj)
      }
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

    if (rtype == "loading") {
      return(tags$div(
        style = "text-align: center; padding: 40px;",
        icon("spinner", class = "fa-spin fa-3x", style = "color: #4A774A;"),
        tags$p(
          style = "margin-top: 15px; color: #666; font-size: 16px;",
          "Running code..."
        )
      ))
    }

    if (rtype == "error") {
      return(tags$div(
        # Main error message
        tags$div(
          style = "color: red; margin-bottom: 10px;",
          tags$strong("Execution Error: "), result$message
        ),
        # Collapsible error details
        tags$details(
          style = "margin-bottom: 15px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 10px;",
          tags$summary(
            style = "cursor: pointer; font-weight: bold; color: #856404;",
            icon("info-circle"), " Error Details (click to expand)"
          ),
          tags$div(
            style = "margin-top: 10px; font-family: monospace; font-size: 12px;",
            if (!is.null(result$call) && result$call != "Unknown") {
              tags$div(
                tags$strong("Failed call: "),
                tags$code(result$call),
                tags$br()
              )
            },
            if (!is.null(result$class)) {
              tags$div(
                tags$strong("Error type: "),
                tags$code(result$class),
                tags$br()
              )
            },
            if (!is.null(result$traceback) && length(result$traceback) > 0) {
              tags$div(
                tags$strong("Traceback:"),
                tags$pre(
                  style = "background: #f8f9fa; padding: 10px; margin-top: 5px; overflow-x: auto;",
                  paste(result$traceback, collapse = "\n")
                )
              )
            }
          )
        ),
        # Ask AI button
        shinyWidgets::actionBttn(
          session$ns("fix_error"),
          "Ask AI to Fix",
          icon = icon("robot"),
          style = "material-flat",
          color = "danger",
          size = "sm"
        )
      ))
    }

    # Handle unknown/unrecognized result types
    if (rtype == "unknown") {
      return(tags$div(
        class = "alert alert-warning",
        style = "background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 15px; margin-bottom: 15px;",
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          icon("exclamation-triangle", style = "color: #856404; font-size: 24px; margin-right: 10px;"),
          tags$strong("No Result Generated", style = "color: #856404; font-size: 16px;")
        ),
        tags$p(
          style = "margin-bottom: 10px; color: #856404;",
          "The code executed but did not produce a recognizable result."
        ),
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: bold; color: #856404; margin-bottom: 10px;",
            icon("info-circle"), " Why did this happen? (click to expand)"
          ),
          tags$div(
            style = "margin-top: 10px;",
            tags$p("This might happen when:"),
            tags$ul(
              tags$li("The result was not properly assigned to the ", tags$code("result"), " variable"),
              tags$li("An unsupported object type was returned"),
              tags$li("The code only performed side effects without returning a value"),
              tags$li("A function returned ", tags$code("NULL"), " or an empty result")
            )
          )
        ),
        tags$div(
          style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #ffc107;",
          tags$strong("Suggestion:"),
          tags$p(
            style = "margin-top: 5px;",
            "Click the button below to ask AI to fix the code automatically."
          )
        ),
        shinyWidgets::actionBttn(
          session$ns("fix_no_result"),
          "Ask AI to Fix",
          icon = icon("robot"),
          style = "material-flat",
          color = "warning",
          size = "sm"
        )
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
          column(6,
            shinyWidgets::noUiSliderInput(
              session$ns("ppt_width"),
              "PPT Width",
              min = 5, max = 20, value = 10, step = 0.5,
              color = "#4A774A",
              tooltips = TRUE
            )
          ),
          column(6,
            shinyWidgets::noUiSliderInput(
              session$ns("ppt_height"),
              "PPT Height",
              min = 5, max = 15, value = 7.5, step = 0.5,
              color = "#4A774A",
              tooltips = TRUE
            )
          )
        ),
        div(
          style = "margin-top: 10px;",
          actionButton(
            session$ns("reset_ppt_size"),
            "Reset",
            icon = icon("undo"),
            class = "btn-secondary btn-sm"
          )
        )
      )
    }
  })

  # Reset PPT size to default values
  observeEvent(input$reset_ppt_size, {
    shinyWidgets::updateNoUiSliderInput(
      session = session,
      inputId = "ppt_width",
      value = 10
    )
    shinyWidgets::updateNoUiSliderInput(
      session = session,
      inputId = "ppt_height",
      value = 7.5
    )
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

                # Handle different plot types
                plot_obj <- plots[[i]]
                if (inherits(plot_obj, c("gtable", "gTree")) && !inherits(plot_obj, "gg")) {
                  # Use grid.draw for gtable/gTree objects
                  doc <- officer::ph_with(doc, rvg::dml(code = grid::grid.draw(plot_obj)),
                                         location = officer::ph_location(width = w, height = h, left = 0, top = 0.5))
                } else {
                  # Use print for ggplot and other objects
                  doc <- officer::ph_with(doc, rvg::dml(code = print(plot_obj)),
                                         location = officer::ph_location(width = w, height = h, left = 0, top = 0.5))
                }

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
