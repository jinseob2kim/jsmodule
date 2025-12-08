#' Get AI prompt from package
#'
#' @param name Prompt name (without .md extension)
#' @return Character string containing the prompt text
#' @keywords internal
#' @noRd
get_prompt <- function(name = "default") {
  file_name <- paste0(name, ".md")

  # Try to find prompt file
  prompt_path <- system.file("prompts", file_name, package = "jsmodule")

  # Fallback for development (devtools::load_all)
  if (prompt_path == "" || !file.exists(prompt_path)) {
    prompt_path <- file.path("inst/prompts", file_name)
  }

  # Check if file exists
  if (!file.exists(prompt_path)) {
    stop("Prompt file not found: ", file_name, call. = FALSE)
  }

  # Read and return
  paste(readLines(prompt_path, warn = FALSE), collapse = "\n")
}


#' List available prompts
#'
#' @return Character vector of available prompt names
#' @keywords internal
#' @noRd
list_prompts <- function() {
  prompt_dir <- system.file("prompts", package = "jsmodule")

  # Fallback for development
  if (prompt_dir == "" || !dir.exists(prompt_dir)) {
    prompt_dir <- "inst/prompts"
  }

  if (!dir.exists(prompt_dir)) {
    return(character(0))
  }

  files <- list.files(prompt_dir, pattern = "\\.md$")
  sub("\\.md$", "", files)
}
