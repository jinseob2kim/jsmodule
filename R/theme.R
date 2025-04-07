#' @title Include jsmodule CSS styling
#' @description
#' Adds the custom `style.css` file bundled with the jsmodule package to a Shiny UI.
#' This allows consistent styling (e.g., bold navbar title, font tweaks, spacing) across
#' all Shiny applications using this package.
#' @returns An HTML `<link>` tag that loads the CSS into a Shiny UI
#' @details This function is meant to be used inside the UI of a Shiny app.
#' It automatically locates and includes the `style.css` file found in `inst/assets/` of the jsmodule package installation.
#' @examples
#' \dontrun{
#' use_jsmodule_style()
#' }
#' @seealso
#'  \code{\link[htmltools]{include}}
#' @rdname use_jsmodule_style
#' @export
#' @importFrom htmltools includeCSS
use_jsmodule_style <- function() {
  css_path <- system.file("assets/style.css", package = "jsmodule")
  htmltools::includeCSS(css_path)
}
