# AI Assistant Module

AI-powered statistical analysis code generation module for jsmodule package.

## Overview

The AI Assistant module provides an interactive chat interface that generates R code for statistical analysis. It integrates seamlessly with jsmodule's gadgets and supports multiple AI providers (Anthropic Claude, OpenAI GPT, Google Gemini).

## Quick Start

### 1. API Key Setup

Add your API key to `.Renviron` file:

```r
# Open .Renviron file
usethis::edit_r_environ()

# Add one of the following lines:
ANTHROPIC_API_KEY=your_key_here
OPENAI_API_KEY=your_key_here
GOOGLE_API_KEY=your_key_here

# Save and restart R session
```

### 2. Basic Usage

#### Option A: Use with jsBasicGadget

```r
library(jsmodule)

# Launch gadget with AI Assistant included
jsBasicGadget()

# Navigate to "AI Assistant" tab
```

#### Option B: Standalone Shiny App

```r
library(shiny)
library(jsmodule)
library(survival)

ui <- fluidPage(
  titlePanel("AI Statistical Assistant"),
  aiAssistantUI("ai")
)

server <- function(input, output, session) {
  data <- reactive(colon)
  data.label <- reactive(jstable::mk.lev(colon))

  callModule(aiAssistant, "ai",
    data = data,
    data_label = data.label
  )
}

shinyApp(ui, server)
```

## Features

### Code Generation
- Statistical analysis code (regression, survival analysis, descriptive statistics)
- Visualization code (ggplot2, jskm, forestplot)
- Table generation (jstable, DT)
- Follows jsmodule conventions and best practices

### Multiple AI Providers
- **Anthropic Claude** (default): claude-3-7-sonnet, claude-3-5-sonnet, claude-3-opus
- **OpenAI GPT**: gpt-4o, gpt-4-turbo, gpt-3.5-turbo
- **Google Gemini**: gemini-2.0-flash-exp, gemini-1.5-pro, gemini-1.5-flash

### Export Options
- **Word (.docx)**: Tables with formatted layout
- **PowerPoint (.pptx)**: Plots as editable vector graphics
- **Excel (.xlsx)**: Tables with data preservation
- **R Script (.R)**: Complete reproducible code

### Safety Features
- Sandboxed code execution (only allowed packages)
- Pre-execution code review and editing
- Error handling with AI-assisted fixes
- No file system or network access

## Important Notes

### Data Access
- The AI can only access data provided through the `data` parameter
- Data is referred to as `out` in generated code
- File upload data is automatically reactive

### Allowed Packages
Generated code can only use these packages:
```
jstable, jskm, jsmodule, survival, ggplot2, ggpubr,
pROC, data.table, DT, gridExtra, GGally, forestploter,
MatchIt, timeROC
```

### Variable Structure
The module automatically generates variable structure information:
- Factor variables
- Numeric variables
- Custom structures (if provided via `data_varStruct` parameter)

### API Key Resolution Order
1. Explicit `api_key` argument in `callModule()`
2. UI input (if `show_api_config = TRUE`)
3. Environment variables (`.Renviron` file)

### API Configuration Modes

The `show_api_config` parameter controls how API keys are managed:

#### `show_api_config = TRUE` (Default)
- **Use Case**: Development, personal use, or when users provide their own API keys
- **Behavior**:
  - Shows Settings panel in the UI
  - Users can select AI provider and model
  - Users can enter API key directly in the interface
  - API key input takes precedence over `.Renviron` file
- **Security Note**: API keys entered in UI are only stored in browser memory and never saved to disk
- **Recommendation**: Suitable for local development and single-user applications

```r
# Development mode - users can configure in UI
aiAssistantUI("ai", show_api_config = TRUE)  # Default

callModule(aiAssistant, "ai",
  data = data,
  data_label = data.label,
  show_api_config = TRUE
)
```

#### `show_api_config = FALSE`
- **Use Case**: Production deployment, shared applications, or pre-configured environments
- **Behavior**:
  - Hides Settings panel completely
  - Only uses `.Renviron` file or explicit `api_key` argument
  - No UI elements for API configuration
- **Security Note**: Prevents users from seeing or modifying API keys
- **Recommendation**: Mandatory for production deployments with shared API keys

```r
# Production mode - API key from .Renviron only
aiAssistantUI("ai", show_api_config = FALSE)

callModule(aiAssistant, "ai",
  data = data,
  data_label = data.label,
  show_api_config = FALSE
)
```

## Advanced Usage

### Custom Variable Structure

```r
server <- function(input, output, session) {
  data <- reactive(lung)
  data.label <- reactive(jstable::mk.lev(lung))

  # Define custom variable roles
  var_struct <- reactive({
    list(
      variable = names(lung),
      Base = c("age", "sex", "ph.ecog"),
      Event = "status",
      Time = "time"
    )
  })

  callModule(aiAssistant, "ai",
    data = data,
    data_label = data.label,
    data_varStruct = var_struct
  )
}
```

### Analysis Context

Provide background information to improve AI responses:

```r
callModule(aiAssistant, "ai",
  data = data,
  data_label = data.label,
  analysis_context = reactive({
    "NCCTG lung cancer trial data.
     Primary outcome: time to death (status/time).
     Focus on performance status (ph.ecog) as predictor."
  })
)
```

### Production Deployment

Hide API configuration UI for production:

```r
ui <- fluidPage(
  aiAssistantUI("ai", show_api_config = FALSE)
)

server <- function(input, output, session) {
  callModule(aiAssistant, "ai",
    data = data,
    data_label = data.label,
    show_api_config = FALSE  # Use only .Renviron
  )
}
```

## Troubleshooting

### API Key Not Found
**Problem**: "API key not configured" error
**Solution**:
1. Check `.Renviron` file has correct variable name
2. Restart R session after editing `.Renviron`
3. Verify key is valid (test in terminal: `Sys.getenv("ANTHROPIC_API_KEY")`)

### Code Execution Errors
**Problem**: Generated code fails to execute
**Solution**:
1. Click "Ask AI to Fix" button for automatic correction
2. Review code in editor before execution
3. Check data has required variables
4. Verify packages are installed

### Summary Results Too Fragmented
**Problem**: `summary()` results split into many pieces
**Solution**: This is now fixed in the latest version. Update jsmodule package.

### Text Output Shows Escape Sequences
**Problem**: `\n` visible instead of line breaks
**Solution**: This is now fixed in the latest version. Update jsmodule package.

## Best Practices

### 1. Be Specific in Questions
❌ Bad: "analyze this data"
✅ Good: "perform linear regression with wt.loss as outcome and age, sex, ph.ecog as predictors"

### 2. Review Generated Code
Always review code in the editor before clicking "Run Code"

### 3. Provide Context
Use `analysis_context` parameter to give AI background about your data

### 4. Use Appropriate Model
- Use faster models (Sonnet, GPT-4o) for simple tasks
- Use advanced models (Opus, GPT-4) for complex analyses

### 5. Iterative Refinement
Ask follow-up questions to refine code rather than starting over

## Limitations

1. **No External Data Access**: Cannot read files or connect to databases
2. **Limited Package Scope**: Only allowed packages can be used
3. **Context Window**: Very long conversations may need to be cleared
4. **Visualization Preview**: Some complex plots may not render immediately
5. **Statistical Expertise**: AI provides code, not statistical consulting

## Examples

### Example 1: Descriptive Statistics
```
Q: "Create a Table 1 comparing baseline characteristics by treatment group (rx)"
```

### Example 2: Survival Analysis
```
Q: "Perform Cox regression with time and status as survival outcome,
    adjusting for age, sex, and ph.ecog"
```

### Example 3: Visualization
```
Q: "Create a Kaplan-Meier plot stratified by treatment group with risk table"
```

### Example 4: Model Diagnostics
```
Q: "Check VIF for multicollinearity in the linear model with wt.loss ~ age + sex + ph.ecog"
```

## Security Considerations

### Code Execution Security

#### Environment-Aware Execution (Development vs Production)

The AI Assistant module implements **environment-aware code execution** to balance security and usability:

**Development Mode** (Default):
- Uses standard `eval()` for code execution
- Easier debugging and development
- All console output visible
- Suitable for local, trusted environments

**Production Mode**:
- Uses `RAppArmor::eval.secure()` for sandboxed execution (Linux only)
- Enhanced security with resource limits:
  - 1GB RAM limit
  - 1MB file size limit
  - 10 second timeout
  - No new process creation
- Prevents system command execution
- Required for public deployments

**Environment Detection**:
The module automatically detects production environments using:
1. `DEPLOYMENT_ENV` environment variable (`production` or `development`)
2. shinyapps.io deployment detection
3. RStudio Connect detection
4. `.production` marker file

**Setting Deployment Mode**:

For local development (default):
```r
# No setup needed - defaults to development mode
# Or explicitly set in .Renviron:
DEPLOYMENT_ENV=development
```

For production deployment:
```r
# Add to .Renviron file:
DEPLOYMENT_ENV=production
```

Or create a marker file:
```bash
# In your app directory
touch .production
```

**Linux Server Setup** (for RAppArmor):
```bash
# Install AppArmor
sudo apt-get install apparmor apparmor-utils libapparmor-dev

# Install R package
R -e "install.packages('RAppArmor')"
```

**Platform Support**:
- ✅ **Linux**: Full RAppArmor sandboxing available
- ⚠️ **macOS/Windows**: Falls back to standard eval with warning in production mode
- Recommendation: Deploy on Linux servers for maximum security

#### Basic Security Features
- **Package Whitelist**: Only approved packages allowed
- **Pre-execution Review**: Code can be edited before execution
- **Error Handling**: Safe error messages without system information

### API Key Security

**⚠️ IMPORTANT: API Key Handling**

**How API Keys are Used**:
- API keys are read from environment variables (`.Renviron`) or UI input
- When entered in UI, keys exist only in the current R session memory
- API calls are made using the `httr` package to AI provider APIs

**This is Open Source**:
- All code is publicly available and auditable at https://github.com/jinseob2kim/jsmodule
- No hidden API key storage or transmission
- You can review the code yourself

✅ **What this module does NOT do with API keys**:
- Never saves them anywhere
- Never logs them
- Never transmits them except to the AI provider

✅ **What IS sent to AI providers**:
- Your prompts and questions
- Data structure information (variable names, types, sample statistics)
- Previous conversation history
- Generated code (for error fixing)

**NOT sent**:
- Raw data values (unless explicitly included in your question)
- File system information

#### Best Practices by Deployment Type

**For Personal/Desktop Use** (Recommended):
```r
# Store API key in .Renviron (user's home directory)
# This keeps the key private to your user account
ANTHROPIC_API_KEY=your_key_here
```

**For Team/Shared Use**:
- Each team member should use their own API key in `.Renviron`
- Set `show_api_config = TRUE` to allow individual configuration
- Do NOT share API keys between users

**For Public Web Applications**:
- ⚠️ **NOT RECOMMENDED**: Do not deploy with `show_api_config = TRUE` publicly
- If you must deploy publicly, consider these alternatives:
  1. Implement server-side API proxy (requires custom backend)
  2. Use authentication to limit access
  3. Set strict usage quotas and monitoring

#### API Key Storage Locations

1. **`.Renviron` file** (Recommended for personal use):
   - Location: `~/.Renviron` (user home directory)
   - Security: Only accessible by your user account
   - Persistence: Survives R session restarts

2. **UI Input** (Development only):
   - Location: Browser memory (temporary)
   - Security: Lost when browser tab closes
   - Persistence: No - must re-enter each session

3. **`api_key` argument** (Advanced use):
   - Location: R script or code
   - Security: ⚠️ Avoid - keys visible in code
   - Persistence: Depends on where code is stored

#### Compliance Considerations

If you're working with sensitive data:
1. ✅ Data structure and variable names are sent to AI provider
2. ✅ Statistical summaries may be sent
3. ⚠️ Avoid including actual data values in questions
4. ⚠️ Review your organization's AI usage policy
5. ⚠️ Consider data anonymization before analysis

### Recommended Security Setup

**For Maximum Security**:
```r
# 1. Store API key in .Renviron (never in code)
usethis::edit_r_environ()
# Add: ANTHROPIC_API_KEY=your_key

# 2. Use show_api_config = FALSE in production
aiAssistantUI("ai", show_api_config = FALSE)

# 3. Never commit .Renviron to version control
# Add to .gitignore:
# .Renviron
# .Renviron.local

# 4. Rotate API keys regularly (every 90 days recommended)

# 5. Monitor API usage through provider's dashboard
```

## Support

For issues or feature requests, please file an issue at:
https://github.com/jinseob2kim/jsmodule/issues

## License

Same as jsmodule package license.
