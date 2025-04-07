#' @return HTML dependency that includes the CSS file
#' @export
use_jsmodule_style <- function() {
  css_path <- system.file("assets/style.css", package = "jsmodule")
  htmltools::includeCSS(css_path)
}
